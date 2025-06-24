use wast::Wat;
use wast::parser::{self, ParseBuffer};

/// Decides if a given string is a well-formed text-format Wasm instruction
///
/// Wraps instruction in function and parses using wast parser
///
/// # Parameters
/// s: A string slice representing a Wasm instruction
///
/// # Returns
/// true: if the instruction is syntactically well-formed; false otherwise
pub fn is_well_formed_instr(s: &str) -> bool {
    let wat = "(func ".to_string() + s + ")";
    let buf = match ParseBuffer::new(&wat) {
        Ok(b) => b,
        Err(_) => return false,
    };
    parser::parse::<Wat>(&buf).is_ok()
}
