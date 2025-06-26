use std::ops::RangeInclusive;
use wast::core::{FuncKind, Instruction};
use wasm_tools::parse_binary_wasm;
use wast::core::{Instruction, Module};
use wast::parser::{self, ParseBuffer};

/// Decides if a given string is a well-formed text-format Wasm instruction
/// (only accepts plain instructions)
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
    let Ok(buf) = ParseBuffer::new(s) else {
        return false;
    };
    parser::parse::<Instruction>(&buf).is_ok()
}

/// Decides if a given string is a well-formed text-format Wasm function
///
/// Uses wast ParseBuffer to convert string into buffer and wast parser to parse buffer as Module
/// Encodes Module to binary Wasm and wasmparser parses binary Wasm
///
/// # Parameters
/// s: A string slice representing a Wasm function
///
/// # Returns
/// true: if the function is syntactically well-formed; false otherwise
///
/// # Assumptions
/// Each instruction is plain
pub fn is_well_formed_func(lines: &str) -> bool {
    //wrap as module
    let func = format!("module (func {lines})");
    let Ok(buf) = ParseBuffer::new(&func) else {
        return false;
    };
    let Ok(mut module) = parser::parse::<Module>(&buf) else {
        return false;
    };
    let Ok(bin) = module.encode() else {
        return false;
    };
    let parser = wasmparser::Parser::new(0);
    parse_binary_wasm(parser, &bin).is_ok()
}

/// Represents the kind of frame in a WebAssembly function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FrameKind {
    Block,
    Loop,
    Else,
    If,
}

/// Represents a frame entry (like "block end" pair, etc.) which binds the kind and range of a frame.
/// The range is inclusive, containing both start line number and end line number.
/// The line begins at 0.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FrameEntry {
    pub kind: FrameKind,
    pub range: RangeInclusive<usize>,
}

/// Tries to match frames in a WebAssembly function written in WAT format as much as possible
///
/// ### Parameters
/// `wat` A wat function in string.
///
/// ### Returns
/// A vector of `FrameEntry` representing the frames in the function.
/// The order of returned frames is not guaranteed.
///
/// ### Warning
/// This is a simplified version and does not cover all frames in a WebAssembly function.
/// There is no support for `try` or `catch` frames. etc.
/// For now, we only support "loop", "if", "else", "block"
///
/// It does not check the wast func is well-formed.
/// It just does its best to match the frames in the function.
///
/// ### Example
///
/// ```
/// # use codillon::utils::{frame_match, FrameKind};
/// let wat = "func\nblock\ni32.const 42\ndrop\nend";
/// let entries = frame_match(wat);
/// assert_eq!(entries.len(), 1);
/// assert_eq!(entries[0].kind, FrameKind::Block);
/// assert_eq!(entries[0].range, 1..=4);
/// ```
pub fn frame_match(wat: &str) -> Vec<FrameEntry> {
    // This is the entries that will be returned.
    let mut entries = Vec::new();

    // Use stacks as memory of frame begining borders.
    let mut kind_stack = Vec::new();
    let mut line_stack = Vec::new();

    let mut buf = if let Ok(buf) = ParseBuffer::new(wat) {
        buf
    } else {
        return entries;
    };
    // ParseBuffer does not track spans by default, so we need to enable span tracing manually.
    buf.track_instr_spans(true);

    let func = if let Ok(func) = parser::parse::<wast::core::Func>(&buf) {
        func
    } else {
        return entries;
    };

    let expression = if let FuncKind::Inline {
        locals: _,
        expression,
    } = func.kind
    {
        expression
    } else {
        return entries;
    };

    for (instr, span) in expression.instrs.iter().zip(
        expression
            .instr_spans
            .expect("Expected Spans for instructions"),
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
            Instruction::If(_) => {
                kind_stack.push(FrameKind::If);
                line_stack.push(span.linecol_in(wat).0);
            }
            Instruction::Else(_) => {
                let last_frame_kind = kind_stack.pop();
                let last_span_start = line_stack.pop();
                if let Some(FrameKind::If) = last_frame_kind {
                    let span_start = last_span_start.unwrap(); // Will never panic
                    let span_end = span.linecol_in(wat).0 - 1;
                    entries.push(FrameEntry {
                        kind: FrameKind::If,
                        range: span_start..=span_end,
                    });
                } else {
                    // Unmatched else
                    return entries;
                }
                let span_start = span.linecol_in(wat).0;
                kind_stack.push(FrameKind::Else);
                line_stack.push(span_start);
            }
            Instruction::End(_) => {
                let last_frame_kind = kind_stack.pop();
                let last_span_start = line_stack.pop();
                if let Some(frame_kind) = last_frame_kind {
                    let span_start = last_span_start.unwrap();
                    let span_end = span.linecol_in(wat).0;
                    entries.push(FrameEntry {
                        kind: frame_kind,
                        range: span_start..=span_end,
                    });
                } else {
                    // Unmatched End
                    return entries;
                }
            }
            _ => (), // Ignore other instructions
        }
    }
    entries
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_frame_match() {
        let well_formed = r#"func         ;; 0
block           ;; 1
i32.const 42    ;; 2
drop            ;; 3
end             ;; 4
block           ;; 5
loop            ;; 6
if              ;; 7
i32.const 43    ;; 8
drop            ;; 9
else            ;; 10
i32.const 44    ;; 11
drop            ;; 12
end             ;; 13
end             ;; 14
end             ;; 15
"#;

        let entries = frame_match(well_formed);

        let expected_entries = vec![
            FrameEntry {
                kind: FrameKind::Block,
                range: 1..=4,
            },
            FrameEntry {
                kind: FrameKind::Block,
                range: 5..=15,
            },
            FrameEntry {
                kind: FrameKind::If,
                range: 7..=9,
            },
            FrameEntry {
                kind: FrameKind::Loop,
                range: 6..=14,
            },
            FrameEntry {
                kind: FrameKind::Else,
                range: 10..=13,
            },
        ];

        assert_eq!(entries.len(), expected_entries.len());

        // Check that all expected entries are present
        for expected in &expected_entries {
            assert!(
                entries.iter().any(|entry| entry == expected),
                "Missing expected entry: {:?}",
                expected
            );
        }
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
    #[test]
    fn test_is_well_formed_func() {
        //well-formed function
        assert!(is_well_formed_func("block\nend\n"));
        assert!(is_well_formed_func("i32.const 1\ni32.const 2\ni32.add"));
        assert!(is_well_formed_func("i64.const 42\ndrop"));
        //indentation
        assert!(is_well_formed_func("block\n  i32.const 0\nend"));
        assert!(is_well_formed_func(
            "i32.const 1\nif\n  i32.const 42\nelse\n  i32.const 99\nend"
        ));
        //nested blocks
        assert!(is_well_formed_func(
            "block\n  i32.const 1\n  block\n    i32.const 2\n    i32.add\n  end\nend"
        ));
        assert!(is_well_formed_func("loop\n  br 0\nend"));
        assert!(is_well_formed_func("i32.const 10\ni32.const 10\ni32.eq"));
        //not well-formed function (assuming that each instruction is plain)
        //mismatched frame
        assert!(!is_well_formed_func("block\n"));
        assert!(!is_well_formed_func("else\ni32.const 1\nend"));
        assert!(!is_well_formed_func("i32.const 1\nend"));
        assert!(!is_well_formed_func("block\ni32.const 1\nend\nend"));
        //unrecognized instructions
        assert!(!is_well_formed_func("i32.const 1\ni32.adx"));
    }
}
