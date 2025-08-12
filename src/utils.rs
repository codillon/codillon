use crate::editor::EditlineSidecar;
use anyhow::{Result, anyhow};
use self_cell::self_cell;
use std::ops::RangeInclusive;
use wasm_tools::parse_binary_wasm;
use wasmparser::{Operator, Parser, Payload, ValType, ValidPayload, Validator};
use wast::core::{Instruction, Module};
use wast::parser::{self, ParseBuffer};

//Lines that can have Operators are each associated with an Operator and a usize that represents its idx in the editor
type Ops<'a> = Result<Vec<(Operator<'a>, usize)>>;

self_cell!(
    pub struct OkModule {
        owner: Vec<u8>,
        #[covariant]
        dependent: Ops,
    }

    impl {Debug}
);

impl OkModule {
    //assumes only one function
    pub fn build(wasm_bin: Vec<u8>, sidecars: Vec<&EditlineSidecar>) -> Result<Self> {
        Ok(OkModule::new(wasm_bin, |wasm_bin| {
            //first, collect all operators
            let parser = Parser::new(0);
            let mut ops = Vec::new();

            for payload in parser.parse_all(wasm_bin) {
                if let Payload::CodeSectionEntry(body) = payload? {
                    for op in body.get_operators_reader()?.into_iter() {
                        ops.push(op);
                    }
                }
            }

            //pop the function's end operator off the Vec<Operators>
            ops.pop();

            //match each operator with its idx in the editor
            let mut ops_with_indices = Vec::new();
            let mut line_idx = 0;
            for op in ops {
                if op.is_err() {
                    continue;
                }
                while !sidecars[line_idx].can_have_op() {
                    line_idx += 1;
                }
                ops_with_indices.push((op?, line_idx));
                line_idx += 1;
            }

            Ok(ops_with_indices)
        }))
    }

    pub fn borrow_binary(&self) -> &[u8] {
        self.borrow_owner()
    }

    pub fn borrow_ops(&self) -> &Ops<'_> {
        self.borrow_dependent()
    }
}

pub fn str_to_binary(s: &str) -> Result<Vec<u8>> {
    let txt = format!("module (func\n{s})");
    Ok(parser::parse::<Module>(&ParseBuffer::new(&txt)?)?.encode()?)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstrInfo {
    If,
    Else,
    End,
    OtherStructured, // block, loop, or try_table
    Other,           // any other instruction
    EmptyorMalformed,
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
/// InstrInfo: instruction (or malformed "instruction") of given category
pub fn parse_instr(s: &str) -> InstrInfo {
    //get rid of comments and spaces (clippy says not to use nth...)
    let s = s
        .split(";;")
        .next()
        .expect("split produced empty iterator")
        .trim();
    if s.is_empty() {
        // is there an instruction on this line?
        InstrInfo::EmptyorMalformed
    } else if let Ok(buf) = ParseBuffer::new(s)
        && let Ok(instr) = parser::parse::<Instruction>(&buf)
    {
        instr.into()
    } else {
        InstrInfo::EmptyorMalformed
    }
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

/// Return value of collect_operands - represents params and results of each instruction
pub type InstrOps = Vec<(Vec<(ValType, usize)>, Vec<ValType>)>;

/// Returns input and output types for each instruction in a binary Wasm module
///
/// # Parameters
/// wasm_bin: a binary Wasm module
/// ops: a vector of Operators and their idx in the editor
///
/// # Returns
/// Err if function is not valid, else:
/// A vector of tuples where:
/// the first element is a (ValType, usize) tuple representing an operand this instruction pops and the idx in the editor where this operand was pushed
/// the second element is a ValType representing an operand this instruction pushes
pub fn collect_operands<'a>(wasm_bin: &[u8], ops: &Vec<(Operator<'a>, usize)>) -> Result<InstrOps> {
    let mut validator = Validator::new();
    let parser = Parser::new(0);
    let mut result = Vec::new();
    let dummy_offset = 1; // no need to track offsets, but validator has safety checks against 0

    for payload in parser.parse_all(wasm_bin) {
        if let ValidPayload::Func(func, _) = validator.payload(&payload?)? {
            let mut func_validator: wasmparser::FuncValidator<wasmparser::ValidatorResources> =
                func.into_validator(wasmparser::FuncValidatorAllocations::default());
            let mut idx_stack: Vec<usize> = Vec::new(); // simulated operand stack to track idx where each operand is pushed
            for (op, idx) in ops {
                let (pop_count, push_count) = op
                    .operator_arity(&func_validator.visitor(dummy_offset))
                    .ok_or(anyhow!("could not determine operator arity"))?;
                let prev_height = func_validator.operand_stack_height();
                if pop_count > prev_height {
                    return Err(anyhow!("expected to pop operand, but empty stack"));
                }
                let mut inputs = (prev_height - pop_count..prev_height)
                    .filter_map(|i| {
                        let valtype = func_validator.get_operand_type(i as usize).flatten();
                        let idx = idx_stack.pop();
                        match (valtype, idx) {
                            (Some(val), Some(idx)) => Some((val, idx)),
                            _ => None,
                        }
                    })
                    .collect::<Vec<_>>();

                //reverse inputs so that operands pushed more recently are towards the left (for UI)
                inputs.reverse();

                func_validator.op(dummy_offset, op)?;

                let new_height = func_validator.operand_stack_height();
                if push_count > new_height {
                    return Err(anyhow!(
                        "expected operator to push operand that is not on the stack"
                    ));
                }
                let outputs = (new_height - push_count..new_height)
                    .filter_map(|i| func_validator.get_operand_type(i as usize).flatten())
                    .collect::<Vec<_>>();
                for _ in &outputs {
                    idx_stack.push(*idx);
                }

                result.push((inputs, outputs));
            }
        }
    }

    Ok(result)
}

/// Represents a frame entry (like "block end" pair, etc.), with range recorded.
/// The range is inclusive, containing both start instr number and end instr number.
/// The start number begins at 0.
pub type Frame = RangeInclusive<usize>;

/// Fix frames by deactivated unmatched instrs and append **end** after the last instruction
///
/// ### Returns
/// A tuple of a vector containing the deactivated indices, and the number of **end** appended at the end.
pub fn fix_frames(instrs: &[InstrInfo]) -> (Vec<usize>, usize) {
    let mut deactivated = Vec::new();
    // Use stacks as memory of frame begining borders.
    let mut frame_border_stack = Vec::new();

    for (idx, instr) in instrs.iter().enumerate() {
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
pub fn match_frames(instrs: &[InstrInfo]) -> Vec<Frame> {
    let mut frames = Vec::new();
    // Use stacks as memory of frame begining borders.
    let mut frame_border_stack = Vec::new();

    for (idx, instr) in instrs.iter().enumerate() {
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
    if !frame_border_stack.is_empty() {
        panic!("Unmatched frames: {frame_border_stack:?}");
    }
    frames
}

pub trait FmtError {
    type T;
    fn fmt_err(self) -> anyhow::Result<Self::T, anyhow::Error>;
}
impl<T, Q: core::fmt::Debug> FmtError for Result<T, Q> {
    type T = T;
    fn fmt_err(self) -> anyhow::Result<T, anyhow::Error> {
        self.map_err(|e| anyhow::Error::msg(format!("{e:?}")))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;

    #[test]
    fn test_is_well_formed_instr() -> Result<()> {
        //well-formed instructions
        assert!(parse_instr("i32.add") == InstrInfo::Other);
        assert!(parse_instr("i32.const 5") == InstrInfo::Other);
        //not well-formed "instructions"
        assert!(parse_instr("i32.bogus") == InstrInfo::EmptyorMalformed);
        assert!(parse_instr("i32.const") == InstrInfo::EmptyorMalformed);
        assert!(parse_instr("i32.const x") == InstrInfo::EmptyorMalformed);
        //not well-formed "instructions": multiple instructions per line, folded instructions
        assert!(parse_instr("i32.const 4 i32.const 5") == InstrInfo::EmptyorMalformed);
        assert!(parse_instr("(i32.const 4)") == InstrInfo::EmptyorMalformed);
        assert!(
            parse_instr("(i32.add (i32.const 4) (i32.const 5))") == InstrInfo::EmptyorMalformed
        );
        //spaces before and after, comments, and empty lines are well-formed
        assert!(parse_instr("    i32.const 5") == InstrInfo::Other);
        assert!(parse_instr("    i32.const 5 ;; hello ") == InstrInfo::Other);
        assert!(parse_instr("i32.const 5     ") == InstrInfo::Other);
        assert!(parse_instr(";;Hello") == InstrInfo::EmptyorMalformed);
        assert!(parse_instr("   ;; Hello ") == InstrInfo::EmptyorMalformed);
        assert!(parse_instr("i32.const 5   ;;this is a const") == InstrInfo::Other);
        assert!(parse_instr("") == InstrInfo::EmptyorMalformed);
        assert!(parse_instr("   ") == InstrInfo::EmptyorMalformed);
        assert!(parse_instr("if") == InstrInfo::If);
        assert!(parse_instr("if (result i32)") == InstrInfo::If);
        assert!(parse_instr("   else   ") == InstrInfo::Else);
        assert!(parse_instr("   end   ") == InstrInfo::End);
        assert!(parse_instr("   block   ") == InstrInfo::OtherStructured);
        assert!(parse_instr("   loop   ") == InstrInfo::OtherStructured);
        assert!(parse_instr("   try_table   ") == InstrInfo::OtherStructured);
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
        let (deativated, appended) =
            fix_frames(&instrs1.into_iter().map(parse_instr).collect::<Vec<_>>());
        assert!(deativated.is_empty() && appended == 1);

        let instrs2 = [
            "if",    // 0
            "block", // 1
            "else",  // 2
            "end",   // 3
        ];

        let (deativated, appended) =
            fix_frames(&instrs2.into_iter().map(parse_instr).collect::<Vec<_>>());
        assert!(deativated.len() == 1 && *deativated.first().unwrap() == 2 && appended == 1);
    }

    #[test]
    fn test_frame_match() {
        // Test case 1: Simple block
        let instrs1 = ["block", "i32.const 42", "drop", "end"];
        let frames1 = match_frames(&instrs1.into_iter().map(parse_instr).collect::<Vec<_>>());
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

        let mut frames2 = match_frames(&instrs2.into_iter().map(parse_instr).collect::<Vec<_>>());
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
    fn test_collect_operands() -> Result<()> {
        //block instruction with params and results
        let output = [
            (vec![], vec![ValType::I32]),
            (vec![], vec![ValType::I32]),
            (
                vec![(ValType::I32, 0), (ValType::I32, 1)],
                vec![ValType::I32, ValType::I32],
            ),
            (
                vec![(ValType::I32, 2), (ValType::I32, 2)],
                vec![ValType::I32],
            ),
            (vec![(ValType::I32, 3)], vec![ValType::I32]),
            (vec![(ValType::I32, 4)], vec![]),
        ];
        let lines =
            "i32.const 1\ni32.const 2\nblock (param i32 i32) (result i32)\ni32.add\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let sidecars = [
            EditlineSidecar::new(0, InstrInfo::Other, true),
            EditlineSidecar::new(1, InstrInfo::Other, true),
            EditlineSidecar::new(2, InstrInfo::OtherStructured, true),
            EditlineSidecar::new(3, InstrInfo::Other, true),
            EditlineSidecar::new(4, InstrInfo::End, true),
            EditlineSidecar::new(5, InstrInfo::Other, true),
        ];
        let sidecars = sidecars.iter().collect();
        let module = OkModule::build(wasm_bin, sidecars)?;
        assert_eq!(
            collect_operands(
                module.borrow_binary(),
                module.borrow_ops().as_ref().expect("ops")
            )?,
            output
        );

        //if else with params and results
        let output = [
            (vec![], vec![ValType::I32]),
            (vec![(ValType::I32, 0)], vec![]),
            (vec![], vec![ValType::I32]),
            (vec![(ValType::I32, 2)], vec![]),
            (vec![], vec![ValType::I32]),
            (vec![(ValType::I32, 4)], vec![ValType::I32]),
            (vec![(ValType::I32, 5)], vec![]),
        ];
        let lines = "i32.const 1\nif (result i32)\ni32.const 1\nelse\ni32.const 2\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let sidecars = [
            EditlineSidecar::new(0, InstrInfo::Other, true),
            EditlineSidecar::new(1, InstrInfo::If, true),
            EditlineSidecar::new(2, InstrInfo::Other, true),
            EditlineSidecar::new(3, InstrInfo::Else, true),
            EditlineSidecar::new(4, InstrInfo::Other, true),
            EditlineSidecar::new(5, InstrInfo::End, true),
            EditlineSidecar::new(6, InstrInfo::Other, true),
        ];
        let sidecars = sidecars.iter().collect();
        let module = OkModule::build(wasm_bin, sidecars)?;
        assert_eq!(
            collect_operands(
                module.borrow_binary(),
                module.borrow_ops().as_ref().expect("ops")
            )?,
            output
        );

        //loop with param and return
        let output = [
            (vec![], vec![ValType::I32]),
            (vec![(ValType::I32, 0)], vec![ValType::I32]),
            (vec![], vec![ValType::I32]),
            (
                vec![(ValType::I32, 1), (ValType::I32, 2)],
                vec![ValType::I32],
            ),
            (vec![(ValType::I32, 3)], vec![]),
            (vec![], vec![ValType::I32]),
            (vec![(ValType::I32, 5)], vec![ValType::I32]),
            (vec![(ValType::I32, 6)], vec![]),
            /*  "Inputs: [] Returns: [i32]",
            "Inputs: [i32] Returns: [i32]",
            "Inputs: [] Returns: [i32]",
            "Inputs: [i32 i32] Returns: [i32]",
            "Inputs: [i32] Returns: []",
            "Inputs: [] Returns: [i32]",
            "Inputs: [i32] Returns: [i32]",
            "Inputs: [i32] Returns: []",*/
        ];
        let lines = "i32.const 10\nloop (param i32) (result i32)\ni32.const 1\ni32.sub\nbr_if 1\ni32.const 2\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let sidecars = [
            EditlineSidecar::new(0, InstrInfo::Other, true),
            EditlineSidecar::new(1, InstrInfo::OtherStructured, true),
            EditlineSidecar::new(2, InstrInfo::Other, true),
            EditlineSidecar::new(3, InstrInfo::Other, true),
            EditlineSidecar::new(4, InstrInfo::Other, true),
            EditlineSidecar::new(5, InstrInfo::Other, true),
            EditlineSidecar::new(6, InstrInfo::End, true),
            EditlineSidecar::new(7, InstrInfo::Other, true),
        ];
        let sidecars = sidecars.iter().collect();
        let module = OkModule::build(wasm_bin, sidecars)?;
        assert_eq!(
            collect_operands(
                module.borrow_binary(),
                module.borrow_ops().as_ref().expect("ops")
            )?,
            output
        );

        //nested block and if
        let output = [
            (vec![], vec![ValType::I32]),
            (vec![(ValType::I32, 0)], vec![ValType::I32]),
            (vec![(ValType::I32, 1)], vec![]),
            (vec![], vec![ValType::I32]),
            (vec![(ValType::I32, 3)], vec![]),
            (vec![], vec![ValType::I32]),
            (vec![(ValType::I32, 5)], vec![ValType::I32]),
            (vec![(ValType::I32, 6)], vec![ValType::I32]),
            (vec![(ValType::I32, 7)], vec![]),
            /*              "Inputs: [] Returns: [i32]",
            "Inputs: [i32] Returns: [i32]",
            "Inputs: [i32] Returns: []",
            "Inputs: [] Returns: [i32]",
            "Inputs: [i32] Returns: []",
            "Inputs: [] Returns: [i32]",
            "Inputs: [i32] Returns: [i32]",
            "Inputs: [i32] Returns: [i32]",
            "Inputs: [i32] Returns: []",*/
        ];
        let lines = "i32.const 10\nblock (param i32) (result i32)\nif (result i32)\ni32.const 1\nelse\ni32.const 2\nend\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let sidecars = [
            EditlineSidecar::new(0, InstrInfo::Other, true),
            EditlineSidecar::new(1, InstrInfo::OtherStructured, true),
            EditlineSidecar::new(2, InstrInfo::If, true),
            EditlineSidecar::new(3, InstrInfo::Other, true),
            EditlineSidecar::new(4, InstrInfo::Else, true),
            EditlineSidecar::new(5, InstrInfo::Other, true),
            EditlineSidecar::new(6, InstrInfo::End, true),
            EditlineSidecar::new(7, InstrInfo::End, true),
            EditlineSidecar::new(8, InstrInfo::Other, true),
        ];
        let sidecars = sidecars.iter().collect();
        let module = OkModule::build(wasm_bin, sidecars)?;
        assert_eq!(
            collect_operands(
                module.borrow_binary(),
                module.borrow_ops().as_ref().expect("ops")
            )?,
            output
        );

        //empty block
        let output = [(vec![], vec![]), (vec![], vec![])];
        let lines = "block\nend";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let sidecars = [
            EditlineSidecar::new(0, InstrInfo::OtherStructured, true),
            EditlineSidecar::new(1, InstrInfo::End, true),
        ];
        let sidecars = sidecars.iter().collect();
        let module = OkModule::build(wasm_bin, sidecars)?;
        assert_eq!(
            collect_operands(
                module.borrow_binary(),
                module.borrow_ops().as_ref().expect("ops")
            )?,
            output
        );

        Ok(())
    }
}
