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

impl Instruction {
    pub fn from_str(s: &str) -> Option<Instruction> {
        let Ok(buf) = ParseBuffer::new(&s) else {
            return None;
        };
        if parser::parse::<wast::core::Instruction>(&buf).is_err() {
            return None;
        };
        let result = InstructionBuilder {
            raw_string: s.to_string(),
            inner_buffer_builder: |raw_string| ParseBuffer::new(raw_string).unwrap(),
            inner_instr_builder: |inner_buffer| {
                parser::parse::<wast::core::Instruction>(&inner_buffer).unwrap()
            },
        }
        .build();
        Some(result)
    }

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
/// Some(Instruction): if the line is a wellformed instr
/// None: if the line is empty or malformed
pub fn parse_instr(s: &str) -> Option<Instruction> {
    //get rid of comments and spaces (clippy says not to use nth...)
    let s = s
        .split(";;")
        .next()
        .expect("Split unexpectedly produced empty iterator")
        .trim();
    //manually check for empty line
    if s.is_empty() {
        return None;
    }

    Instruction::from_str(s)
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
            parse_instr("i32.add").unwrap().as_wast_instruction(),
            wast::core::Instruction::I32Add
        ));
        //not well-formed "instructions"
        assert!(parse_instr("i32.bogus").is_none());
        assert!(parse_instr("i32.const").is_none());
        assert!(parse_instr("i32.const x").is_none());
        //not well-formed "instructions": multiple instructions per line, folded instructions
        assert!(parse_instr("i32.const 4 i32.const 5").is_none());
        assert!(parse_instr("(i32.const 4)").is_none());
        assert!(parse_instr("(i32.add (i32.const 4) (i32.const 5))").is_none());
        //spaces before and after, comments, and empty lines are well-formed

        assert!(matches!(
            parse_instr("    i32.const 5")
                .unwrap()
                .as_wast_instruction(),
            wast::core::Instruction::I32Const(_)
        ));
        assert!(matches!(
            parse_instr("i32.const 5     ")
                .unwrap()
                .as_wast_instruction(),
            wast::core::Instruction::I32Const(_)
        ));
        assert!(matches!(
            parse_instr("i32.const 5   ;;this is a const")
                .unwrap()
                .as_wast_instruction(),
            wast::core::Instruction::I32Const(_)
        ));
        //empty line is not well-formed
        assert!(parse_instr(";;Hello").is_none());
        assert!(parse_instr("").is_none());
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
