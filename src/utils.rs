use std::ops::RangeInclusive;
use wasm_tools::parse_binary_wasm;
use wast::core::{Instruction, Module};
use wast::parser::{self, ParseBuffer};

use super::document::InstrKind;
use crate::document::InstrInfo;

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
        Instruction::Block(_) | Instruction::Loop(_) | Instruction::If(_) => Ok(InstrKind::Entry),
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

/// Represents a frame entry (like "block end" pair, etc.), with range recorded.
/// The range is inclusive, containing both start instr number and end instr number.
/// The start number begins at 0.
pub type Frame = RangeInclusive<usize>;

/// Match frames for a vec of Codeline(Instruction or empty).
///
/// ### Returns
/// If all instructions are well-formed and no inconsistent frames:
/// A vector of `Frame` representing the frames in the expr.
/// The order of returned frames is not guaranteed.
///
/// Otherwise: Err(())
///
/// ### Warning
/// This is a simplified version and does not cover all frames in a WebAssembly function.
/// There is no support for `try` or `catch` frames. etc.
/// For now, we only support "block", "loop", "if", "else"
pub fn frame_match(instrs: impl Iterator<Item = InstrInfo>) -> Result<Vec<Frame>, ()> {
    let mut frames = Vec::new();
    // Use stacks as memory of frame begining borders.
    let mut frame_border_stack = Vec::new();

    for (idx, instr) in instrs.enumerate() {
        match instr.kind {
            InstrKind::Entry => {
                frame_border_stack.push(idx);
            }
            InstrKind::Else => {
                if let Some(last_span_start) = frame_border_stack.pop() {
                    let last_span_end = idx - 1;
                    frames.push(last_span_start..=last_span_end);
                } else {
                    // Unmatched else
                    return Err(());
                }
                frame_border_stack.push(idx);
            }
            InstrKind::End => {
                if let Some(last_span_start) = frame_border_stack.pop() {
                    frames.push(last_span_start..=idx);
                } else {
                    // Unmatched end
                    return Err(());
                }
            }
            _ => (), // Ignore other instructions
        }
    }
    Ok(frames)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_frame_match() {
        // Create a vector of InstrInfo based on the comment indices
        let instrs = vec![
            InstrInfo {
                kind: InstrKind::Entry,
                ..Default::default()
            }, // block (0)
            InstrInfo {
                kind: InstrKind::Other,
                ..Default::default()
            }, // i32.const 42 (1)
            InstrInfo {
                kind: InstrKind::Other,
                ..Default::default()
            }, // drop (2)
            InstrInfo {
                kind: InstrKind::End,
                ..Default::default()
            }, // end (3)
            InstrInfo {
                kind: InstrKind::Entry,
                ..Default::default()
            }, // block (4)
            InstrInfo {
                kind: InstrKind::Entry,
                ..Default::default()
            }, // loop (5)
            InstrInfo {
                kind: InstrKind::Entry,
                ..Default::default()
            }, // if (6)
            InstrInfo {
                kind: InstrKind::Other,
                ..Default::default()
            }, // i32.const 43 (7)
            InstrInfo {
                kind: InstrKind::Other,
                ..Default::default()
            }, // drop (8)
            InstrInfo {
                kind: InstrKind::Else,
                ..Default::default()
            }, // else (9)
            InstrInfo {
                kind: InstrKind::Other,
                ..Default::default()
            }, // i32.const 44 (10)
            InstrInfo {
                kind: InstrKind::Other,
                ..Default::default()
            }, // drop (11)
            InstrInfo {
                kind: InstrKind::End,
                ..Default::default()
            }, // end (12)
            InstrInfo {
                kind: InstrKind::End,
                ..Default::default()
            }, // end (13)
            InstrInfo {
                kind: InstrKind::End,
                ..Default::default()
            }, // end (14)
        ];

        let frames = frame_match(instrs.into_iter()).unwrap();

        // Define expected frames - note that the Frame type is now just a RangeInclusive<usize>
        let expected_frames = vec![
            0..=3,  // block
            4..=14, // block
            6..=8,  // if
            5..=13, // loop
            9..=12, // else
        ];

        assert_eq!(frames.len(), expected_frames.len());

        // Check that all expected entries are present
        for expected in &expected_frames {
            assert!(
                frames.contains(expected),
                "Missing expected frame: {:?}",
                expected
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
            Ok(InstrKind::Entry)
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
