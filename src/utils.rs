use std::str::FromStr;

use ouroboros::self_referencing;
use wasm_tools::parse_binary_wasm;
use wast::parser::{self, ParseBuffer};

// Wrap wast instruction for future use.
#[self_referencing]
pub struct Instruction {
    raw_string: String,
    #[borrows(raw_string)]
    #[covariant]
    inner_buffer: ParseBuffer<'this>,
    #[borrows(inner_buffer)]
    #[covariant]
    inner_instr: wast::core::Instruction<'this>,
}

impl std::str::FromStr for Instruction {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let buf = ParseBuffer::new(s).map_err(|err| err.message())?;
        if let Err(err) = parser::parse::<wast::core::Instruction>(&buf) {
            return Err(err.message());
        }
        let result = InstructionBuilder {
            raw_string: s.to_string(),
            inner_buffer_builder: |raw_string| ParseBuffer::new(raw_string).unwrap(),
            inner_instr_builder: |inner_buffer| {
                parser::parse::<wast::core::Instruction>(inner_buffer).unwrap()
            },
        }
        .build();
        Ok(result)
    }
}

impl Instruction {
    pub fn as_wast_instruction(&self) -> &wast::core::Instruction {
        self.borrow_inner_instr()
    }
}

/// Parse one line of code
///
/// Uses wast ParseBuffer to convert string into buffer and wast parser to parse buffer as Instruction
///
/// # Parameters
/// s: A string slice representing a Wasm instruction
///
/// # Returns
///
/// Ok(Some(Instruction)): if the line is a wellformed instr
/// Ok(None): if the line is empty(ok with comment)
/// Err: if the line is malformed
pub fn parse_instr(s: &str) -> Result<Option<Instruction>, String> {
    //get rid of comments and spaces (clippy says not to use nth...)
    let s = s
        .split(";;")
        .next()
        .expect("Split unexpectedly produced empty iterator")
        .trim();
    //manually check for empty line
    if s.is_empty() {
        return Ok(None);
    }

    Instruction::from_str(s).map(Some)
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
    let Ok(mut module) = parser::parse::<wast::core::Module>(&buf) else {
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
        assert!(matches!(
            parse_instr("i32.add")
                .unwrap()
                .unwrap()
                .as_wast_instruction(),
            wast::core::Instruction::I32Add
        ));
        //not well-formed "instructions"
        assert!(parse_instr("i32.bogus").is_err());
        assert!(parse_instr("i32.const").is_err());
        assert!(parse_instr("i32.const x").is_err());
        //not well-formed "instructions": multiple instructions per line, folded instructions
        assert!(parse_instr("i32.const 4 i32.const 5").is_err());
        assert!(parse_instr("(i32.const 4)").is_err());
        assert!(parse_instr("(i32.add (i32.const 4) (i32.const 5))").is_err());
        //spaces before and after, comments, and empty lines are well-formed

        assert!(matches!(
            parse_instr("    i32.const 5")
                .unwrap()
                .unwrap()
                .as_wast_instruction(),
            wast::core::Instruction::I32Const(_)
        ));
        assert!(matches!(
            parse_instr("i32.const 5     ")
                .unwrap()
                .unwrap()
                .as_wast_instruction(),
            wast::core::Instruction::I32Const(_)
        ));
        assert!(matches!(
            parse_instr("i32.const 5   ;;this is a const")
                .unwrap()
                .unwrap()
                .as_wast_instruction(),
            wast::core::Instruction::I32Const(_)
        ));
        //empty line is not well-formed
        assert!(parse_instr(";;Hello").unwrap().is_none());
        assert!(parse_instr("").unwrap().is_none());
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
