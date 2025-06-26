use std::ops::RangeInclusive;
use wast::core::{FuncKind, Instruction};
use wast::parser::{self, ParseBuffer};

/// Decides if a given string is a well-formed text-format Wasm instruction
///
/// Uses wast ParseBuffer to convert string into buffer and wast parser to parse buffer as Instruction
///
/// # Parameters
/// s: A string slice representing a Wasm instruction
///
/// # Returns
/// true: if the instruction is syntactically well-formed; false otherwise
pub fn is_well_formed_instr(s: &str) -> bool {
    //get rid of comments and spaces (clippy says not to use nth...)
    let s = s
        .split(";;")
        .next()
        .expect("Split unexpectedly produced empty iterator")
        .trim();
    //manually check for empty line
    if s.is_empty() {
        return true;
    }
    let buf = match ParseBuffer::new(s) {
        Ok(b) => b,
        Err(_) => return false,
    };
    parser::parse::<Instruction>(&buf).is_ok()
}

/// Represents the kind of frame in a WebAssembly function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FrameKind {
    Block,
    Loop,
    Else,
}

/// Represents a frame entry (like "block end" pair, etc.)
/// To bind the kind and the range.
/// The range is inclusive, containing both start line number and end line number.
/// The line begins at 0.
#[derive(Debug, Clone)]
pub struct FrameEntry {
    pub kind: FrameKind,
    pub range: RangeInclusive<usize>,
}

/// This function tries to match frames in a WebAssembly function written in WAT format.
///
/// ### Parameters
/// `wat` A wat function in string.
///
/// ### Returns
/// If successful, returns a vector of `FrameEntry` representing the frames in the function.
/// The order is not guaranteed.
/// Or an error with a message will be returned.
///
/// ### Warning
/// This is a simplified version and does not cover all frames in a WebAssembly function.
/// There is no support for `try` or `catch` frames. etc.
///
/// And even this function returns an Ok, it does not mean that the function is well-formed.
/// This function just does its best to match the frames in the function.
///
/// ### Example
///
/// ```
/// # use codillon::wast_utils::{frame_match, FrameKind};
/// let wat = "func\nblock\ni32.const 42\ndrop\nend";
/// let entries = frame_match(wat).unwrap();
/// assert_eq!(entries.len(), 1);
/// assert_eq!(entries[0].kind, FrameKind::Block);
/// assert_eq!(entries[0].range, 1..=4);
/// ```
pub fn frame_match(wat: &str) -> Result<Vec<FrameEntry>, Box<dyn core::error::Error>> {
    // This is the entries that will be returned.
    let mut entries = Vec::new();

    // Use stacks as memory of the beginning of any frame.
    let mut kind_stack = Vec::new();
    let mut line_stack = Vec::new();

    let mut buf = ParseBuffer::new(wat)?;
    // ParseBuffer does not track spans by default, so we need to enable it.
    buf.track_instr_spans(true);
    let func = parser::parse::<wast::core::Func>(&buf)?;

    let expression = if let FuncKind::Inline {
        locals: _,
        expression,
    } = func.kind
    {
        expression
    } else {
        return Err("Function is not inline".into());
    };

    for (instr, span) in expression.instrs.iter().zip(
        expression
            .instr_spans
            .iter()
            .next()
            .expect("Expected at least one span slice"),
    ) {
        match instr {
            Instruction::Block(_) => {
                kind_stack.push(FrameKind::Block);
                // There is a performance concern here, we should convert offsets to line numbers in batch if
                // this becomes a bottleneck. But for now, we will just use the library function.
                line_stack.push(span.linecol_in(wat).0);
            }
            Instruction::Loop(_) => {
                kind_stack.push(FrameKind::Loop);
                line_stack.push(span.linecol_in(wat).0);
            }
            Instruction::Else(_) => {
                let last_frame_kind = kind_stack.pop();
                let span_start = line_stack.pop();
                if let Some(frame_kind) = last_frame_kind {
                    let span_start = span_start.unwrap(); // Will never panic
                    let span_end = span.linecol_in(wat).0 - 1;
                    entries.push(FrameEntry {
                        kind: frame_kind,
                        range: span_start..=span_end,
                    });
                } else {
                    return Err("Else without matching block".into());
                }
                let span_start = span.linecol_in(wat).0;
                kind_stack.push(FrameKind::Else);
                line_stack.push(span_start);
            }
            Instruction::End(_) => {
                let last_frame_kind = kind_stack.pop();
                let span_start = line_stack.pop();
                if let Some(frame_kind) = last_frame_kind {
                    let span_start = span_start.unwrap();
                    let span_end = span.linecol_in(wat).0;
                    entries.push(FrameEntry {
                        kind: frame_kind,
                        range: span_start..=span_end,
                    });
                } else {
                    return Err("End without matching block".into());
                }
            }
            _ => {
                // Ignore other instructions
            }
        }
    }
    Ok(entries)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_frame_match() {
        let s = r#"func
block
i32.const 42
drop
end
block
loop
i32.const 43
drop
else
i32.const 44
drop
end
end
"#;
        let entries = frame_match(s).unwrap();
        assert_eq!(entries.len(), 4);
        assert_eq!(entries[0].kind, FrameKind::Block);
        assert_eq!(entries[0].range, 1..=4);
        assert_eq!(entries[1].kind, FrameKind::Loop);
        assert_eq!(entries[1].range, 6..=8);
        assert_eq!(entries[2].kind, FrameKind::Else);
        assert_eq!(entries[2].range, 9..=12);
        assert_eq!(entries[3].kind, FrameKind::Block);
        assert_eq!(entries[3].range, 5..=13);
    }

    #[test]
    fn test_frame_match1() {
        let s = r#"func
block
i32.const 42
drop
end
block
loop
i32.const 43
drop
else
i32.const 44
drop
end
end
end
"#;
        assert!(
            frame_match(s).is_err()
                && frame_match(s)
                    .unwrap_err()
                    .to_string()
                    .contains("End without matching block")
        );
    }

    #[test]
    fn test_is_well_formed_instr() {
        //well-formed instructions
        assert!(is_well_formed_instr("i32.add"));
        assert!(is_well_formed_instr("i32.const 5"));
        //not well-formed "instructions"
        assert!(!is_well_formed_instr("i32.bogus"));
        assert!(!is_well_formed_instr("i32.const"));
        assert!(!is_well_formed_instr("i32.const x"));
        //not well-formed "instructions": multiple instructions per line, folded instructions
        assert!(!is_well_formed_instr("i32.const 4 i32.const 5"));
        assert!(!is_well_formed_instr("(i32.const 4)"));
        assert!(!is_well_formed_instr(
            "(i32.add (i32.const 4) (i32.const 5))"
        ));
        //spaces before and after, comments, and empty lines are well-formed
        assert!(is_well_formed_instr("    i32.const 5"));
        assert!(is_well_formed_instr("i32.const 5     "));
        assert!(is_well_formed_instr(";;Hello"));
        assert!(is_well_formed_instr("i32.const 5   ;;this is a const"));
        assert!(is_well_formed_instr(""));
    }
}
