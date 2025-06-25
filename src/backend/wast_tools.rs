use wast::{core::{FuncKind::{self}, Instruction}, parser::{self, ParseBuffer}};
use std::{ops::RangeInclusive};


/// ### FrameKind
/// Represents the kind of frame in a WebAssembly function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FrameKind {
    Block,
    Loop,
    Else,
}



#[derive(Debug, Clone)]
pub struct FrameEntry
{
    pub kind: FrameKind,
    pub range: RangeInclusive<usize> // the first and last line number
}

/// ### Parameters
/// `wat` A wat function string
/// 
/// ### Returns
/// A vector of `FrameEntry` representing the frames in the function.
/// 
/// ### Warning
/// This is a simplified version and may not cover all frames in a WebAssembly function.
/// There is no support for `try` or `catch` frames. etc.
pub fn frame_match(wat: &str) -> Result<Vec<FrameEntry>, Box<dyn core::error::Error>> {
    let mut entries = Vec::new();
    let mut kind_stack = Vec::new();
    let mut line_stack = Vec::new();

    let mut buf = ParseBuffer::new(&wat)?;
    buf.track_instr_spans(true);
    let func = parser::parse::<wast::core::Func>(&buf)?;

    let expression = if let FuncKind::Inline { locals: _, expression } = func.kind
    {
        expression
    }
    else
    {
        return Err("Function is not inline".into());
    };

    for (instr, span) in expression.instrs.iter().zip(expression.instr_spans
    .iter().next().unwrap())    
    {
        match instr {
            Instruction::Block(_) => {
                kind_stack.push(FrameKind::Block);
                // There is a performance concern here, we should convert offsets to line numbers in bunch if
                // this has become a bottleneck. But for now, we will just use the library function.
                line_stack.push(span.linecol_in(wat).0);
            }
            Instruction::Loop(_) => {
                kind_stack.push(FrameKind::Loop);
                line_stack.push(span.linecol_in(wat).0);
            }
            Instruction::Else(_) => {
                let top_kind = kind_stack.pop();
                let span_start = line_stack.pop();
                if let Some(frame_kind) = top_kind {
                    let span_start = span_start.unwrap();
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
                let top_kind = kind_stack.pop();
                let span_start = line_stack.pop();
                if let Some(frame_kind) = top_kind {
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
        let s = 
r#"func
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
}

