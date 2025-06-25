use wast::core::Instruction;
use wast::parser::{self, ParseBuffer};

/// Decides if a given string is a well-formed text-format Wasm instruction
///
/// Uses ParseBuffer to convert string into single instruction buffer and parser to parse buffer
///
/// # Parameters
/// s: A string slice representing a Wasm instruction
///
/// # Returns
/// true: if the instruction is syntactically well-formed; false otherwise
pub fn is_well_formed_instr(s: &str) -> bool {
    //manually check for empty line and line comments (block comments not supported)
    if s.is_empty() || s.starts_with(";;") {
        return true;
    }
    //manually check for spaces at the beginning and end
    if s.starts_with(" ") || s.ends_with(" ") {
        return false;
    }
    let buf = match ParseBuffer::new(s) {
        Ok(b) => b,
        Err(_) => return false,
    };
    parser::parse::<Instruction>(&buf).is_ok()
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
        //not well-formed "instructions": multiple instructions per line, folded instructions,
        //spaces before and after
        assert!(!is_well_formed_instr("i32.const 4 i32.const 5"));
        assert!(!is_well_formed_instr("(i32.const 4)"));
        assert!(!is_well_formed_instr(
            "(i32.add (i32.const 4) (i32.const 5))"
        ));
        assert!(!is_well_formed_instr("    i32.const 5"));
        assert!(!is_well_formed_instr("i32.const 5     "));
        //comments and empty lines are well-formed
        assert!(is_well_formed_instr(";;Hello"));
        assert!(is_well_formed_instr(""));
    }
}
