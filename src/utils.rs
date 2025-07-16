use anyhow::{Result, anyhow};
use std::ops::RangeInclusive;
use wasm_tools::parse_binary_wasm;
use wasmparser::{Parser, ValType};
use wast::core::{Instruction, Module};
use wast::parser::{self, ParseBuffer};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstrInfo {
    If,
    Else,
    End,
    OtherStructured, // block, loop, or try_table
    Other,           // any other instruction
}

impl From<Instruction<'_>> for InstrInfo {
    fn from(instr: Instruction<'_>) -> Self {
        match instr {
            Instruction::If(_) => InstrInfo::If,
            Instruction::Else(_) => InstrInfo::Else,
            Instruction::End(_) => InstrInfo::End,
            Instruction::Block(_) | Instruction::Loop(_) | Instruction::TryTable(_) => {
                InstrInfo::OtherStructured
            }
            Instruction::Try(_) | Instruction::Catch(_) | Instruction::CatchAll => {
                panic!("legacy-exceptions not supported");
            }
            _ => InstrInfo::Other,
        }
    }
}

/// Parse one code line as instruction
/// (only accepts plain instructions)
///
/// Uses wast ParseBuffer to convert string into buffer and wast parser to parse buffer as Instruction
///
/// # Parameters
/// s: A string slice representing a Wasm instruction
///
/// # Returns
/// Err: Malformed
/// Ok(None): empty
/// Ok(Some(InstrInfo)): instruction of given category
pub fn parse_instr(s: &str) -> Result<Option<InstrInfo>> {
    //get rid of comments and spaces (clippy says not to use nth...)
    let s = s
        .split(";;")
        .next()
        .expect("split produced empty iterator")
        .trim();
    Ok(if s.is_empty() {
        // is there an instruction on this line?
        None
    } else {
        Some(parser::parse::<Instruction>(&ParseBuffer::new(s)?)?.into())
    })
}

/// Decides if a given string is a well-formed text-format Wasm function
///
/// Uses wast ParseBuffer to convert string into buffer and wast parser to parse buffer as Module
/// Encodes Module to binary Wasm and wasmparser parses binary Wasm
///
/// # Parameters
/// lines: A string slice representing a Wasm function
///
/// # Returns
/// true: if the function is syntactically well-formed; false otherwise
///
/// # Assumptions
/// Each instruction is plain
pub fn is_well_formed_func(lines: &str) -> bool {
    let parse_text = || {
        let text = format!("module (func {lines})");
        let binary = parser::parse::<Module>(&ParseBuffer::new(&text)?)?.encode()?;
        parse_binary_wasm(Parser::new(0), &binary)
    };
    parse_text().is_ok()
}

/// Returns input and output types for each instruction in a binary Wasm module
///
/// # Parameters
/// wasm_bin: a binary Wasm module
///
/// # Returns
/// A vector of strings where each string represents the input and output types for one instruction
///
/// # Assumptions
/// The function is valid
pub fn print_operands(wasm_bin: &[u8]) -> Result<Vec<String>> {
    let mut validator = wasmparser::Validator::new();
    let parser = wasmparser::Parser::new(0);
    let mut result = Vec::new();
    let dummy_offset = 1; // no need to track offsets, but validator has safety checks against 0

    for payload in parser.parse_all(wasm_bin) {
        if let wasmparser::ValidPayload::Func(func, body) = validator.payload(&payload?)? {
            let mut func_validator =
                func.into_validator(wasmparser::FuncValidatorAllocations::default());
            for op in body.get_operators_reader()? {
                let op = op?;
                let (pop_count, push_count) = op
                    .operator_arity(&func_validator.visitor(dummy_offset))
                    .ok_or(anyhow!("could not determine operator arity"))?;
                let prev_height = func_validator.operand_stack_height();
                let inputs = (prev_height - pop_count..prev_height)
                    .filter_map(|i| func_validator.get_operand_type(i as usize).flatten())
                    .map(valtype_to_str)
                    .collect::<Vec<_>>()
                    .join(" ");
                func_validator.op(dummy_offset, &op)?;
                let new_height = func_validator.operand_stack_height();
                let outputs = (new_height - push_count..new_height)
                    .filter_map(|i| func_validator.get_operand_type(i as usize).flatten())
                    .map(valtype_to_str)
                    .collect::<Vec<_>>()
                    .join(" ");
                result.push(format!("Inputs: [{inputs}] Returns: [{outputs}]"));
            }
        }
    }
    //remove the entry associated with the `end` at end of function body
    result.pop();

    Ok(result)
}

fn valtype_to_str(ty: ValType) -> &'static str {
    match ty {
        ValType::I32 => "i32",
        ValType::I64 => "i64",
        ValType::F32 => "f32",
        ValType::F64 => "f64",
        ValType::V128 => "v128",
        ValType::Ref(_) => "ref",
    }
}

/// Represents a frame entry (like "block end" pair, etc.), with range recorded.
/// The range is inclusive, containing both start instr number and end instr number.
/// The start number begins at 0.
pub type Frame = RangeInclusive<usize>;

/// Fix frames by deactivated unmatched instrs and append **end** after the last instruction
///
/// ### Returns
/// A tuple of a vector containing the deactivated indices, and the number of **end** appended at the end.
pub fn fix_frames(instrs: &[Option<InstrInfo>]) -> (Vec<usize>, usize) {
    let mut deactivated = Vec::new();
    // Use stacks as memory of frame begining borders.
    let mut frame_border_stack = Vec::new();

    for (idx, instr) in instrs.iter().enumerate() {
        let Some(instr) = instr else {
            continue;
        };
        match instr {
            InstrInfo::If | InstrInfo::OtherStructured => frame_border_stack.push((instr, idx)),
            InstrInfo::Else => {
                if let Some(prev) = frame_border_stack.last().cloned()
                    && matches!(prev.0, InstrInfo::If)
                {
                    frame_border_stack.pop();
                    frame_border_stack.push((instr, idx));
                } else {
                    deactivated.push(idx);
                }
            }
            InstrInfo::End => {
                if !frame_border_stack.is_empty() {
                    frame_border_stack.pop();
                } else {
                    deactivated.push(idx);
                }
            }
            _ => (),
        }
    }

    (deactivated, frame_border_stack.len())
}

/// Match frames for (Instruction or empty).
///
/// ### Panics
/// When the input has unmatched frames
pub fn match_frames(instrs: &[Option<InstrInfo>]) -> Vec<Frame> {
    let mut frames = Vec::new();
    // Use stacks as memory of frame begining borders.
    let mut frame_border_stack = Vec::new();

    for (idx, instr) in instrs.iter().enumerate() {
        if let Some(instr) = instr {
            match instr {
                InstrInfo::If | InstrInfo::OtherStructured => {
                    frame_border_stack.push((instr, idx));
                }
                InstrInfo::Else => {
                    if let Some((prev, last_span_start)) = frame_border_stack.pop()
                        && matches!(prev, InstrInfo::If)
                    {
                        let last_span_end = idx - 1;
                        frames.push(last_span_start..=last_span_end);
                    } else {
                        panic!("Unmatched else");
                    }
                    frame_border_stack.push((instr, idx));
                }
                InstrInfo::End => {
                    if let Some((_, last_span_start)) = frame_border_stack.pop() {
                        frames.push(last_span_start..=idx);
                    } else {
                        panic!("Unmatched end");
                    }
                }
                _ => (), // Ignore other instructions
            }
        }
    }
    frames
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;

    #[test]
    fn test_is_well_formed_instr() -> Result<()> {
        //well-formed instructions
        assert!(parse_instr("i32.add")? == Some(InstrInfo::Other));
        assert!(parse_instr("i32.const 5")? == Some(InstrInfo::Other));
        //not well-formed "instructions"
        assert!(parse_instr("i32.bogus").is_err());
        assert!(parse_instr("i32.const").is_err());
        assert!(parse_instr("i32.const x").is_err());
        //not well-formed "instructions": multiple instructions per line, folded instructions
        assert!(parse_instr("i32.const 4 i32.const 5").is_err());
        assert!(parse_instr("(i32.const 4)").is_err());
        assert!(parse_instr("(i32.add (i32.const 4) (i32.const 5))").is_err());
        //spaces before and after, comments, and empty lines are well-formed
        assert!(parse_instr("    i32.const 5")? == Some(InstrInfo::Other));
        assert!(parse_instr("    i32.const 5 ;; hello ")? == Some(InstrInfo::Other));
        assert!(parse_instr("i32.const 5     ")? == Some(InstrInfo::Other));
        assert!(parse_instr(";;Hello")?.is_none());
        assert!(parse_instr("   ;; Hello ")?.is_none());
        assert!(parse_instr("i32.const 5   ;;this is a const")? == Some(InstrInfo::Other));
        assert!(parse_instr("")?.is_none());
        assert!(parse_instr("   ")?.is_none());
        assert!(parse_instr("if")? == Some(InstrInfo::If));
        assert!(parse_instr("if (result i32)")? == Some(InstrInfo::If));
        assert!(parse_instr("   else   ")? == Some(InstrInfo::Else));
        assert!(parse_instr("   end   ")? == Some(InstrInfo::End));
        assert!(parse_instr("   block   ")? == Some(InstrInfo::OtherStructured));
        assert!(parse_instr("   loop   ")? == Some(InstrInfo::OtherStructured));
        assert!(parse_instr("   try_table   ")? == Some(InstrInfo::OtherStructured));
        Ok(())
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

    #[test]
    fn test_fix_frames() {
        let instrs1 = ["block", "i32.const 42", "drop"];
        let (deativated, appended) = fix_frames(
            &instrs1
                .into_iter()
                .map(|instr| parse_instr(instr).unwrap())
                .collect::<Vec<_>>(),
        );
        assert!(deativated.is_empty() && appended == 1);

        let instrs2 = [
            "if",    // 0
            "block", // 1
            "else",  // 2
            "end",   // 3
        ];

        let (deativated, appended) = fix_frames(
            &instrs2
                .into_iter()
                .map(|instr| parse_instr(instr).unwrap())
                .collect::<Vec<_>>(),
        );
        assert!(deativated.len() == 1 && *deativated.first().unwrap() == 2 && appended == 1);
    }

    #[test]
    fn test_frame_match() {
        // Test case 1: Simple block
        let instrs1 = ["block", "i32.const 42", "drop", "end"];
        let frames1 = match_frames(
            &instrs1
                .into_iter()
                .map(|instr| parse_instr(instr).unwrap())
                .collect::<Vec<_>>(),
        );
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

        let mut frames2 = match_frames(
            &instrs2
                .into_iter()
                .map(|instr| parse_instr(instr).unwrap())
                .collect::<Vec<_>>(),
        );
        let mut expected2 = [
            0..=3,  // first block
            4..=14, // second block
            6..=8,  // if part
            5..=13, // loop
            9..=12, // else part
        ];

        // Sort both vectors for comparison since order might vary
        frames2.sort_by_key(|r| *r.start());
        expected2.sort_by_key(|r| *r.start());

        assert_eq!(frames2, expected2);
    }
    #[test]
    fn test_print_operands() -> Result<()> {
        //block instruction with params and results
        let output = vec![
            "Inputs: [] Returns: [i32]",
            "Inputs: [] Returns: [i32]",
            "Inputs: [i32 i32] Returns: [i32 i32]",
            "Inputs: [i32 i32] Returns: [i32]",
            "Inputs: [i32] Returns: [i32]",
            "Inputs: [i32] Returns: []",
        ]
        .into_iter()
        .map(String::from)
        .collect::<Vec<String>>();
        let lines =
            "i32.const 1\ni32.const 2\nblock (param i32 i32) (result i32)\ni32.add\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        assert_eq!(print_operands(&wasm_bin)?, output);
        //if else with params and results
        let output = vec![
            "Inputs: [] Returns: [i32]",
            "Inputs: [i32] Returns: []",
            "Inputs: [] Returns: [i32]",
            "Inputs: [i32] Returns: []",
            "Inputs: [] Returns: [i32]",
            "Inputs: [i32] Returns: [i32]",
            "Inputs: [i32] Returns: []",
        ]
        .into_iter()
        .map(String::from)
        .collect::<Vec<String>>();
        let lines = "i32.const 1\nif (result i32)\ni32.const 1\nelse\ni32.const 2\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        assert_eq!(print_operands(&wasm_bin)?, output);
        //loop with param and return
        let output = vec![
            "Inputs: [] Returns: [i32]",
            "Inputs: [i32] Returns: [i32]",
            "Inputs: [] Returns: [i32]",
            "Inputs: [i32 i32] Returns: [i32]",
            "Inputs: [i32] Returns: []",
            "Inputs: [] Returns: [i32]",
            "Inputs: [i32] Returns: [i32]",
            "Inputs: [i32] Returns: []",
        ]
        .into_iter()
        .map(String::from)
        .collect::<Vec<String>>();
        let lines = "i32.const 10\nloop (param i32) (result i32)\ni32.const 1\ni32.sub\nbr_if 1\ni32.const 2\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        assert_eq!(print_operands(&wasm_bin)?, output);
        //nested block and if
        let output = vec![
            "Inputs: [] Returns: [i32]",
            "Inputs: [i32] Returns: [i32]",
            "Inputs: [i32] Returns: []",
            "Inputs: [] Returns: [i32]",
            "Inputs: [i32] Returns: []",
            "Inputs: [] Returns: [i32]",
            "Inputs: [i32] Returns: [i32]",
            "Inputs: [i32] Returns: [i32]",
            "Inputs: [i32] Returns: []",
        ]
        .into_iter()
        .map(String::from)
        .collect::<Vec<String>>();
        let lines = "i32.const 10\nblock (param i32) (result i32)\nif (result i32)\ni32.const 1\nelse\ni32.const 2\nend\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        assert_eq!(print_operands(&wasm_bin)?, output);
        //empty block
        let output = vec!["Inputs: [] Returns: []", "Inputs: [] Returns: []"]
            .into_iter()
            .map(String::from)
            .collect::<Vec<String>>();
        let lines = "block\nend";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        assert_eq!(print_operands(&wasm_bin)?, output);
        Ok(())
    }
}
