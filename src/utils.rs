use wast::Wat;
use wast::core::Instruction;
//use wast::core::Module;
use wasm_tools::parse_binary_wasm;
use wast::parser::{self, ParseBuffer};

/// Decides if a given string is a well-formed text-format Wasm instruction
///
/// Uses wast ParseBuffer to convert string into buffer and wast parser to parse buffer as Instruction
///
/// # Parameters
/// s: A string slice representing a Wasm instruction
///
/// # Returns
/// true: if the instruction is syntactically well-formed; false otherwise
pub fn is_well_formed_instr(s: &str) -> bool {
    //get rid of comments
    let s = s.split(";;").next().unwrap();
    //get rid of spaces
    let trimmed = s.trim();
    //manually check for empty line
    if trimmed.is_empty() {
        return true;
    }
    let buf = match ParseBuffer::new(trimmed) {
        Ok(b) => b,
        Err(_) => return false,
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
    let func = "(module \n(func\nblock\nend\n".to_string() + lines + "))";
    println!("{}", func);
    let buf = match ParseBuffer::new(&func) {
        Ok(b) => b,
        Err(_) => {
            println!("cannot make buffer");
            return false;
        }
    };

    let wat = match parser::parse::<Wat>(&buf) {
        Ok(w) => w,
        Err(_) => return false,
    };
    let mut module = match wat {
        Wat::Module(m) => m,
        Wat::Component(_) => panic!("No components :("),
    };
    /*let mut module = match parser::parse::<Module>(&buf) {
        Ok(m) => m,
        Err(_) => {
            println!("cannot parse as module");
            return false;
        },
    };*/
    let bin = match module.encode() {
        Ok(b) => b,
        Err(_) => {
            println!("cannot encode to binary");
            return false;
        }
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
        assert!(is_well_formed_func(""));
    }
}
