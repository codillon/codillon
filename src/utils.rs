use std::ops::RangeInclusive;
use wasm_tools::parse_binary_wasm;
use wast::core::{Expression, Instruction, Module};
use wast::parser::{self, ParseBuffer};

use super::document::InstrKind;

/// Decides if a given string is a well-formed text-format Wasm instruction or an empty line. Line comment ignored.
/// (only accepts plain instructions)
///
/// Uses wast ParseBuffer to convert string into buffer and wast parser to parse buffer as Instruction
///
/// # Parameters
/// s: A string slice representing a Wasm instruction Line
///
/// # Returns
/// Ok(InstrInfo) if well-formed or empty; Err(()) otherwise
pub(crate) fn is_well_formed_instrline(s: &str) -> Result<InstrKind, ()> {
    //get rid of comments and spaces (clippy says not to use nth...)
    let s = s
        .split(";;")
        .next()
        .expect("Split unexpectedly produced empty iterator")
        .trim();
    //manually check for empty line
    if s.is_empty() {
        return Ok(InstrKind::Other);
    }
    let Ok(buf) = ParseBuffer::new(s) else {
        return Err(());
    };
    let instr = parser::parse::<Instruction>(&buf).map_err(|_| ())?;
    match instr {
        Instruction::Block(_) | Instruction::Loop(_) => Ok(InstrKind::Entry),
        Instruction::If(_) => Ok(InstrKind::If),
        Instruction::Else(_) => Ok(InstrKind::Else),
        Instruction::End(_) => Ok(InstrKind::End),
        _ => Ok(InstrKind::Other),
    }
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
/// The range is inclusive, containing both start instr number and end instr number.
/// The start number begins at 0.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Frame {
    pub kind: FrameKind,
    pub range: RangeInclusive<usize>,
}

/// Tries to match frames in a WebAssembly Expr written in Text format as much as possible
///
/// ### Parameters
/// `expr` An expr parsed by wast  tool
///
/// ### Returns
/// A vector of `Frame` representing the frames in the expr.
/// The order of returned frames is not guaranteed.
///
/// ### Warning
/// This is a simplified version and does not cover all frames in a WebAssembly function.
/// There is no support for `try` or `catch` frames. etc.
/// For now, we only support "loop", "if", "else", "block"
///
/// It does not check the wast expr is well-formed.
/// It just does its best to match the frames in the expr.
///
/// ### Example
///
/// ```
/// # use codillon::utils::{frame_match, FrameKind};
/// # use wast::core::Expression;
/// # use wast::parser::{self, ParseBuffer};
/// let wat = "block\ni32.const 42\ndrop\nend";
/// let buf = ParseBuffer::new(wat).unwrap();
/// let expr = parser::parse::<Expression>(&buf).unwrap();
/// let entries = frame_match(&expr);
/// assert_eq!(entries.len(), 1);
/// assert_eq!(entries[0].kind, FrameKind::Block);
/// assert_eq!(entries[0].range, 0..=3);
/// ```
pub fn frame_match(expr: &Expression) -> Vec<Frame> {
    // This is the entries that will be returned.
    let mut frames = Vec::new();

    // Use stacks as memory of frame begining borders.
    let mut frame_border_stack = Vec::new();

    for (idx, instr) in expr.instrs.iter().enumerate() {
        match instr {
            Instruction::Block(_) => {
                frame_border_stack.push((FrameKind::Block, idx));
            }
            Instruction::Loop(_) => {
                frame_border_stack.push((FrameKind::Loop, idx));
            }
            Instruction::If(_) => {
                frame_border_stack.push((FrameKind::If, idx));
            }
            Instruction::Else(_) => {
                if let Some((FrameKind::If, last_span_start)) = frame_border_stack.pop() {
                    let last_span_end = idx - 1;
                    frames.push(Frame {
                        kind: FrameKind::If,
                        range: last_span_start..=last_span_end,
                    });
                } else {
                    // Unmatched else
                    return frames;
                }
                frame_border_stack.push((FrameKind::Else, idx));
            }
            Instruction::End(_) => {
                if let Some((last_frame_kind, last_span_start)) = frame_border_stack.pop() {
                    frames.push(Frame {
                        kind: last_frame_kind,
                        range: last_span_start..=idx,
                    });
                } else {
                    // Unmatched end
                    return frames;
                }
            }
            _ => (), // Ignore other instructions
        }
    }
    frames
}

#[cfg(test)]
mod tests {
    use wast::core::Expression;

    use super::*;

    #[test]
    fn test_frame_match() {
        let well_formed = r#"block           ;; 0
i32.const 42    ;; 1
drop            ;; 2
end             ;; 3
block           ;; 4
loop            ;; 5
if              ;; 6
i32.const 43    ;; 7
drop            ;; 8
else            ;; 9
i32.const 44    ;; 10
drop            ;; 11
end             ;; 12
end             ;; 13
end             ;; 14
"#;

        let buf = ParseBuffer::new(well_formed).unwrap();
        let expr = parser::parse::<Expression>(&buf).unwrap();
        let frames = frame_match(&expr);

        let expected_frames = vec![
            Frame {
                kind: FrameKind::Block,
                range: 0..=3,
            },
            Frame {
                kind: FrameKind::Block,
                range: 4..=14,
            },
            Frame {
                kind: FrameKind::If,
                range: 6..=8,
            },
            Frame {
                kind: FrameKind::Loop,
                range: 5..=13,
            },
            Frame {
                kind: FrameKind::Else,
                range: 9..=12,
            },
        ];

        assert_eq!(frames.len(), expected_frames.len());

        // Check that all expected entries are present
        for expected in &expected_frames {
            assert!(
                frames.iter().any(|entry| entry == expected),
                "Missing expected entry: {expected:?}",
            );
        }
    }

    #[test]
    fn test_is_well_formed_instr() {
        //well-formed instructions
        assert!(is_well_formed_instrline("i32.add").is_ok());
        assert!(is_well_formed_instrline("i32.const 5").is_ok());
        //not well-formed "instructions"
        assert!(is_well_formed_instrline("i32.bogus").is_err());
        assert!(is_well_formed_instrline("i32.const").is_err());
        assert!(is_well_formed_instrline("i32.const x").is_err());
        //not well-formed "instructions": multiple instructions per line, folded instructions
        assert!(is_well_formed_instrline("i32.const 4 i32.const 5").is_err());
        assert!(is_well_formed_instrline("(i32.const 4)").is_err());
        assert!(is_well_formed_instrline("(i32.add (i32.const 4) (i32.const 5))").is_err());
        //spaces before and after, comments, and empty lines are well-formed
        assert!(is_well_formed_instrline("    i32.const 5").is_ok());
        assert!(is_well_formed_instrline("i32.const 5     ").is_ok());
        assert!(is_well_formed_instrline(";;Hello").is_ok());
        assert!(is_well_formed_instrline("i32.const 5   ;;this is a const").is_ok());

        //Empty Instr are well-formed
        assert!(is_well_formed_instrline("").is_ok());
        assert!(is_well_formed_instrline("   ;; Hello").is_ok());

        //Check InstrKind
        assert!(matches!(
            is_well_formed_instrline("block ;; Hello"),
            Ok(InstrKind::Entry)
        ));
        assert!(matches!(
            is_well_formed_instrline("loop ;; Hello"),
            Ok(InstrKind::Entry)
        ));
        assert!(matches!(
            is_well_formed_instrline("if ;; Hello"),
            Ok(InstrKind::If)
        ));
        assert!(matches!(
            is_well_formed_instrline("else ;; Hello"),
            Ok(InstrKind::Else)
        ));
        assert!(matches!(
            is_well_formed_instrline("end ;; Hello"),
            Ok(InstrKind::End)
        ));
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
