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
pub fn is_well_formed_func(lines: &str) -> bool {
    //wrap as module
    let func = "module\n(func\n".to_string() + lines + ")";
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
    }
}
