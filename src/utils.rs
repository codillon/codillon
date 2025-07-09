use std::ops::RangeInclusive;

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
    // Remove all comments and whitespace from each line
    let lines = lines
        .lines()
        .map(|line| line.split(";;").next().unwrap_or("").trim())
        .collect::<Vec<_>>()
        .join("\n");

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
/// Match frames for strs(Instruction or empty).
///
/// ### Panics
/// When the input has unmatched frames
///
/// ### Warning
/// This is a simplified version and does not cover all frames in a WebAssembly function.
/// There is no support for `try` or `catch` frames. etc.
/// For now, we only support "block", "loop", "if", "else"
pub fn frame_match<'a>(instrs: impl Iterator<Item = &'a str>) -> Vec<Frame> {
    let mut frames = Vec::new();
    // Use stacks as memory of frame begining borders.
    let mut frame_border_stack = Vec::new();

    for (idx, instr) in instrs.enumerate() {
        match instr.split(";;").next().unwrap_or("").trim() {
            "block" | "if" | "loop" => {
                frame_border_stack.push(idx);
            }
            "else" => {
                if let Some(last_span_start) = frame_border_stack.pop() {
                    let last_span_end = idx - 1;
                    frames.push(last_span_start..=last_span_end);
                } else {
                    panic!("Unmatched else");
                }
                frame_border_stack.push(idx);
            }
            "end" => {
                if let Some(last_span_start) = frame_border_stack.pop() {
                    frames.push(last_span_start..=idx);
                } else {
                    panic!("Unmatched end");
                }
            }
            _ => (), // Ignore other instructions
        }
    }
    frames
}

#[cfg(test)]
mod tests {
    use super::*;

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
        // allow comment
        assert!(!is_well_formed_func(
            "block\ni32.const 1 ;; Comment\nend\nend"
        ));
    }

    #[test]
    fn test_frame_match() {
        // Test case 1: Simple block
        let instrs1 = ["block", "i32.const 42", "drop", "end"];
        let frames1 = frame_match(instrs1.iter().cloned());
        let expected1 = [0..=3]; // block from 0 to 3
        assert_eq!(frames1, expected1);

        // Test case 2: Complex nested structure
        let instrs2 = [
            "block",        // 0
            "i32.const 42", // 1
            "drop",         // 2
            "end",          // 3
            "block",        // 4
            "loop",         // 5
            "if",           // 6
            "i32.const 43", // 7
            "drop",         // 8
            "else",         // 9
            "i32.const 44", // 10
            "drop",         // 11
            "end",          // 12
            "end",          // 13
            "end",          // 14
        ];

        let frames2 = frame_match(instrs2.iter().cloned());
        let expected2 = [
            0..=3,  // first block
            4..=14, // second block
            6..=8,  // if part
            5..=13, // loop
            9..=12, // else part
        ];

        // Sort both vectors for comparison since order might vary
        let mut frames2_sorted = frames2;
        let mut expected2_sorted = expected2;
        frames2_sorted.sort_by_key(|r| *r.start());
        expected2_sorted.sort_by_key(|r| *r.start());

        assert_eq!(frames2_sorted, expected2_sorted);
    }
}
