use anyhow::{Result, anyhow};
use wasm_tools::parse_binary_wasm;
use wasmparser::ValType;
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
/// lines: A string slice representing a Wasm function
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

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;

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
