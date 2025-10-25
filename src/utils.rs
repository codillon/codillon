use crate::line::LineInfo;
use anyhow::{Result, anyhow};
use std::ops::{Deref, RangeInclusive};
use wasm_encoder::{
    CodeSection, ExportSection, FunctionSection, GlobalSection, Instruction as EncoderInstruction,
    TypeSection,
    reencode::{Reencode, RoundtripReencoder},
};
use wasm_tools::parse_binary_wasm;
use wasmparser::{Operator, Parser, ValType, ValidPayload, Validator};
use wast::{
    core::{Instruction, Module},
    parser::{self, ParseBuffer},
};

pub struct CodillonInstruction<'a> {
    pub op: Operator<'a>,
    pub line_idx: usize,
}

pub struct InstructionTable<'a> {
    pub table: Vec<CodillonInstruction<'a>>,
}

impl<'a> InstructionTable<'a> {
    /// Returns input and output types for each instruction in a binary Wasm module
    ///
    /// # Parameters
    /// wasm_bin: a binary Wasm module
    ///
    /// # Returns
    /// Err if function is not valid, else:
    /// a TypesTable which is a vector of CodillonType
    /// TODO: insert synthetic instructions to make function well-typed
    pub fn to_types_table(&self, wasm_bin: &[u8]) -> Result<TypesTable> {
        let mut validator = Validator::new();
        let parser = Parser::new(0);
        let dummy_offset = 1; // no need to track offsets, but validator has safety checks against 0
        let mut result = Vec::new();

        for payload in parser.parse_all(wasm_bin) {
            if let ValidPayload::Func(func, _) = validator.payload(&payload?)? {
                let mut func_validator: wasmparser::FuncValidator<wasmparser::ValidatorResources> =
                    func.into_validator(wasmparser::FuncValidatorAllocations::default());
                let mut idx_stack: Vec<usize> = Vec::new(); // simulated operand stack to track idx where each operand is pushed
                for instr in &self.table {
                    let op = &instr.op;
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
                                (Some(val), Some(idx)) => Some(InputType {
                                    instr_type: val,
                                    origin_idx: idx,
                                }),
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
                        idx_stack.push(instr.line_idx);
                    }
                    result.push(CodillonType { inputs, outputs });
                }
            }
        }
        Ok(TypesTable { _table: result })
    }

    pub fn build_executable_binary(&self) -> Result<Vec<u8>> {
        const MAX_STEP_COUNT: i32 = 1000;

        let mut module: wasm_encoder::Module = Default::default();

        // Encode the type section.
        let mut types = TypeSection::new();
        let params = vec![];
        let results = vec![];
        types.ty().function(params, results);
        module.section(&types);

        // Encode the function section.
        let mut functions = FunctionSection::new();
        let type_index = 0;
        functions.function(type_index);
        module.section(&functions);

        // Encode the global section.
        let mut global = GlobalSection::new();
        global.global(
            wasm_encoder::GlobalType {
                val_type: wasm_encoder::ValType::I32,
                mutable: true,
                shared: false,
            },
            &wasm_encoder::ConstExpr::i32_const(0),
        );
        module.section(&global);

        // Encode the export section.
        let mut exports = ExportSection::new();
        exports.export("main", wasm_encoder::ExportKind::Func, 0);
        module.section(&exports);

        // Encode the code section.
        let mut codes = CodeSection::new();
        let locals = vec![];
        let mut f = wasm_encoder::Function::new(locals);
        for codillon_instruction in &self.table {
            let instruction = RoundtripReencoder.instruction(codillon_instruction.op.clone())?;
            // Instrument to prevent infinite execution.
            f.instruction(&EncoderInstruction::GlobalGet(0));
            f.instruction(&EncoderInstruction::I32Const(1));
            f.instruction(&EncoderInstruction::I32Add);
            f.instruction(&EncoderInstruction::GlobalSet(0));
            f.instruction(&EncoderInstruction::GlobalGet(0));
            f.instruction(&EncoderInstruction::I32Const(MAX_STEP_COUNT));
            f.instruction(&EncoderInstruction::I32GtU);
            f.instruction(&EncoderInstruction::If(wasm_encoder::BlockType::Empty));
            // Throw unreachable if step count reached.
            f.instruction(&EncoderInstruction::Unreachable);
            f.instruction(&EncoderInstruction::End);
            f.instruction(&instruction);
        }
        f.instruction(&EncoderInstruction::End);
        codes.function(&f);
        module.section(&codes);

        let wasm = module.finish();
        Ok(wasm)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct InputType {
    pub instr_type: ValType,
    pub origin_idx: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CodillonType {
    inputs: Vec<InputType>,
    outputs: Vec<ValType>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypesTable {
    _table: Vec<CodillonType>,
}

pub fn str_to_binary(s: String) -> Result<Vec<u8>> {
    let txt = format!("module (func (export \"main\")\n{s})");
    let binding = ParseBuffer::new(&txt)?;
    let mut module = parser::parse::<Module>(&binding)?;

    Ok(module.encode()?)
}

#[derive(Debug, PartialEq, Eq)]
pub enum InstrKind {
    If,
    Else,
    End,
    OtherStructured, // block, loop, or try_table
    Other,           // any other instruction
    Empty,
    Malformed(String), // explanation
}

impl From<Instruction<'_>> for InstrKind {
    fn from(instr: Instruction<'_>) -> Self {
        match instr {
            Instruction::If(_) => InstrKind::If,
            Instruction::Else(_) => InstrKind::Else,
            Instruction::End(_) => InstrKind::End,
            Instruction::Block(_) | Instruction::Loop(_) | Instruction::TryTable(_) => {
                InstrKind::OtherStructured
            }
            Instruction::Try(_) | Instruction::Catch(_) | Instruction::CatchAll => {
                panic!("legacy-exceptions not supported");
            }
            _ => InstrKind::Other,
        }
    }
}

impl From<wast::Error> for InstrKind {
    fn from(e: wast::Error) -> Self {
        InstrKind::Malformed(format!("{e}").lines().next().unwrap_or_default().into())
    }
}

// Find the line comment separator in these string slices.
pub fn find_comment(s1: &str, s2: &str) -> Option<usize> {
    if let Some(idx) = s1.find(";;") {
        Some(idx)
    } else if s1.bytes().last() == Some(b';') && s2.as_bytes().first() == Some(&b';') {
        Some(s1.len() - 1)
    } else {
        s2.find(";;").map(|idx| s1.len() + idx)
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
/// InstrKind: instruction (or malformed "instruction") of given category
pub fn parse_instr(s: &str) -> InstrKind {
    let s = s.trim(); // get rid of spaces
    if s.is_empty() {
        InstrKind::Empty // no instruction on this line
    } else {
        match ParseBuffer::new(s) {
            Ok(buf) => match parser::parse::<Instruction>(&buf) {
                Ok(instr) => instr.into(),
                Err(e) => e.into(),
            },
            Err(e) => e.into(),
        }
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
// TODO: use Operators in OkModule instead of re-parsing the binary, and annotate each Operator in OkModule with its type instead of returning a vector
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

pub trait LineInfos {
    fn is_empty(&self) -> bool;
    fn len(&self) -> usize;
    fn info(&self, index: usize) -> impl Deref<Target = LineInfo>;
    fn synthetic_ends(&self) -> usize;
}

pub trait LineInfosMut: LineInfos {
    fn set_active_status(&mut self, index: usize, is_active: bool);
    fn set_synthetic_ends(&mut self, num: usize);
}

/// Fix frames by deactivated unmatched instrs and appending ends as necessary to close all open frames
pub fn fix_frames(instrs: &mut impl LineInfosMut) {
    // Use stacks as memory of frame begining borders.
    let mut frame_stack: Vec<InstrKind> = Vec::new();

    for i in 0..instrs.len() {
        let mut is_active: bool = true;
        match instrs.info(i).kind {
            InstrKind::If => frame_stack.push(InstrKind::If),
            InstrKind::OtherStructured => frame_stack.push(InstrKind::OtherStructured),
            InstrKind::Else => {
                if let Some(frame_entry) = frame_stack.last()
                    && *frame_entry == InstrKind::If
                {
                    frame_stack.pop();
                    frame_stack.push(InstrKind::Else);
                } else {
                    is_active = false;
                }
            }
            InstrKind::End => {
                if frame_stack.is_empty() {
                    is_active = false;
                } else {
                    frame_stack.pop();
                }
            }
            InstrKind::Other | InstrKind::Empty | InstrKind::Malformed(_) => (),
        }

        instrs.set_active_status(i, is_active);
    }

    instrs.set_synthetic_ends(frame_stack.len())
}

/// Match frames for (Instruction or empty).
///
/// ### Panics
/// When the input has unmatched frames
pub fn match_frames(instrs: &[InstrKind]) -> Vec<Frame> {
    let mut frames = Vec::new();
    // Use stacks as memory of frame begining borders.
    let mut frame_border_stack = Vec::new();

    for (idx, instr) in instrs.iter().enumerate() {
        match instr {
            InstrKind::If | InstrKind::OtherStructured => {
                frame_border_stack.push((instr, idx));
            }
            InstrKind::Else => {
                if let Some((prev, last_span_start)) = frame_border_stack.pop()
                    && matches!(prev, InstrKind::If)
                {
                    let last_span_end = idx - 1;
                    frames.push(last_span_start..=last_span_end);
                } else {
                    panic!("Unmatched else");
                }
                frame_border_stack.push((instr, idx));
            }
            InstrKind::End => {
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
    use wasmparser::BlockType;

    impl<const N: usize> LineInfos for ([LineInfo; N], usize) {
        fn is_empty(&self) -> bool {
            N == 0
        }

        fn len(&self) -> usize {
            N
        }

        #[allow(refining_impl_trait)]
        fn info(&self, index: usize) -> &LineInfo {
            &self.0[index]
        }

        fn synthetic_ends(&self) -> usize {
            0
        }
    }

    impl<const N: usize> LineInfosMut for ([LineInfo; N], usize) {
        fn set_active_status(&mut self, index: usize, is_active: bool) {
            self.0[index].active = is_active;
        }

        fn set_synthetic_ends(&mut self, num: usize) {
            self.1 = num;
        }
    }

    fn malf(s: &str) -> InstrKind {
        InstrKind::Malformed(String::from(s))
    }

    #[test]
    fn test_is_well_formed_instr() -> Result<()> {
        //well-formed instructions
        assert_eq!(parse_instr("i32.add"), InstrKind::Other);
        assert_eq!(parse_instr("i32.const 5"), InstrKind::Other);
        //not well-formed "instructions"
        assert_eq!(
            parse_instr("i32.bogus"),
            malf("unknown operator or unexpected token")
        );
        assert_eq!(parse_instr("i32.const"), malf("expected a i32"));
        assert_eq!(parse_instr("i32.const x"), malf("expected a i32"));
        //not well-formed "instructions": multiple instructions per line, folded instructions
        assert_eq!(
            parse_instr("i32.const 4 i32.const 5"),
            malf("extra tokens remaining after parse")
        );
        assert_eq!(
            parse_instr("(i32.const 4)"),
            malf("expected an instruction")
        );
        assert_eq!(
            parse_instr("(i32.add (i32.const 4) (i32.const 5))"),
            malf("expected an instruction")
        );
        //spaces before and after, comments, and empty lines are well-formed
        assert_eq!(parse_instr("    i32.const 5"), InstrKind::Other);
        assert_eq!(parse_instr("    i32.const 5 ;; hello "), InstrKind::Other);
        assert_eq!(parse_instr("i32.const 5     "), InstrKind::Other);
        assert_eq!(parse_instr(";;Hello"), malf("expected an instruction"));
        assert_eq!(parse_instr("   ;; Hello "), malf("expected an instruction"));
        assert_eq!(
            parse_instr("i32.const 5   ;;this is a const"),
            InstrKind::Other
        );
        assert_eq!(parse_instr(""), InstrKind::Empty);
        assert_eq!(parse_instr("   "), InstrKind::Empty);
        assert_eq!(parse_instr("if"), InstrKind::If);
        assert_eq!(parse_instr("if (result i32)"), InstrKind::If);
        assert_eq!(parse_instr("   else   "), InstrKind::Else);
        assert_eq!(parse_instr("   end   "), InstrKind::End);
        assert_eq!(parse_instr("   block   "), InstrKind::OtherStructured);
        assert_eq!(parse_instr("   loop   "), InstrKind::OtherStructured);
        assert_eq!(parse_instr("   try_table   "), InstrKind::OtherStructured);
        Ok(())
    }

    #[test]
    fn test_find_comment() {
        assert_eq!(find_comment("abc", "def"), None);
        assert_eq!(find_comment("abc", ";;def"), Some(3));
        assert_eq!(find_comment(";;abc", ";;def"), Some(0));
        assert_eq!(find_comment(";;abc", "def"), Some(0));
        assert_eq!(find_comment(";", "def"), None);
        assert_eq!(find_comment(";", ";def"), Some(0));
        assert_eq!(find_comment("abc;", ";def"), Some(3));
        assert_eq!(find_comment("abc;;", ""), Some(3));
        assert_eq!(find_comment("abc;;", ";"), Some(3));
        assert_eq!(find_comment("abc;", ";"), Some(3));
        assert_eq!(find_comment("abc; ", ";"), None);
        assert_eq!(find_comment("abc; ", "; ;;"), Some(7));
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
        let mut infos1 = (
            instrs1.map(|x| LineInfo {
                kind: parse_instr(x),
                active: false,
            }),
            0,
        );
        fix_frames(&mut infos1);
        assert!(infos1.1 == 1 && infos1.0.iter().all(|x| x.active));

        let instrs2 = [
            "if",    // 0
            "block", // 1
            "else",  // 2
            "end",   // 3
        ];
        let mut infos2 = (
            instrs2.map(|x| LineInfo {
                kind: parse_instr(x),
                active: false,
            }),
            0,
        );
        fix_frames(&mut infos2);
        assert!(infos2.1 == 1 && infos2.0.map(|x| x.active) == [true, true, false, true]);

        let instrs3 = ["end"];
        let mut infos3 = (
            instrs3.map(|x| LineInfo {
                kind: parse_instr(x),
                active: false,
            }),
            0,
        );
        fix_frames(&mut infos3);
        assert!(infos3.1 == 0 && !infos3.0[0].active);
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
        let output = TypesTable {
            _table: vec![
                CodillonType {
                    inputs: vec![],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![
                        InputType {
                            instr_type: ValType::I32,
                            origin_idx: 0,
                        },
                        InputType {
                            instr_type: ValType::I32,
                            origin_idx: 1,
                        },
                    ],
                    outputs: vec![ValType::I32, ValType::I32],
                },
                CodillonType {
                    inputs: vec![
                        InputType {
                            instr_type: ValType::I32,
                            origin_idx: 2,
                        },
                        InputType {
                            instr_type: ValType::I32,
                            origin_idx: 2,
                        },
                    ],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 3,
                    }],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 4,
                    }],
                    outputs: vec![],
                },
            ],
        };
        let lines =
            "i32.const 1\ni32.const 2\nblock (param i32 i32) (result i32)\ni32.add\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = InstructionTable {
            table: vec![
                CodillonInstruction {
                    op: Operator::I32Const { value: 1 },
                    line_idx: 0,
                },
                CodillonInstruction {
                    op: Operator::I32Const { value: 2 },
                    line_idx: 1,
                },
                CodillonInstruction {
                    op: Operator::Block {
                        blockty: BlockType::FuncType(1),
                    },
                    line_idx: 2,
                },
                CodillonInstruction {
                    op: Operator::I32Add,
                    line_idx: 3,
                },
                CodillonInstruction {
                    op: Operator::End,
                    line_idx: 4,
                },
                CodillonInstruction {
                    op: Operator::Drop,
                    line_idx: 5,
                },
            ],
        };
        assert_eq!(instruction_table.to_types_table(&wasm_bin)?, output);

        //if else with params and results
        let output = TypesTable {
            _table: vec![
                CodillonType {
                    inputs: vec![],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 0,
                    }],
                    outputs: vec![],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 2,
                    }],
                    outputs: vec![],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 4,
                    }],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 5,
                    }],
                    outputs: vec![],
                },
            ],
        };
        let lines = "i32.const 1\nif (result i32)\ni32.const 1\nelse\ni32.const 2\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = InstructionTable {
            table: vec![
                CodillonInstruction {
                    op: Operator::I32Const { value: 1 },
                    line_idx: 0,
                },
                CodillonInstruction {
                    op: Operator::If {
                        blockty: BlockType::Type(ValType::I32),
                    },
                    line_idx: 1,
                },
                CodillonInstruction {
                    op: Operator::I32Const { value: 1 },
                    line_idx: 2,
                },
                CodillonInstruction {
                    op: Operator::Else,
                    line_idx: 3,
                },
                CodillonInstruction {
                    op: Operator::I32Const { value: 2 },
                    line_idx: 4,
                },
                CodillonInstruction {
                    op: Operator::End,
                    line_idx: 5,
                },
                CodillonInstruction {
                    op: Operator::Drop,
                    line_idx: 6,
                },
            ],
        };
        assert_eq!(instruction_table.to_types_table(&wasm_bin)?, output);

        //loop with param and return
        let output = TypesTable {
            _table: vec![
                CodillonType {
                    inputs: vec![],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 0,
                    }],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![
                        InputType {
                            instr_type: ValType::I32,
                            origin_idx: 1,
                        },
                        InputType {
                            instr_type: ValType::I32,
                            origin_idx: 2,
                        },
                    ],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 3,
                    }],
                    outputs: vec![],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 5,
                    }],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 6,
                    }],
                    outputs: vec![],
                },
            ],
        };
        let lines = "i32.const 10\nloop (param i32) (result i32)\ni32.const 1\ni32.sub\nbr_if 1\ni32.const 2\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = InstructionTable {
            table: vec![
                CodillonInstruction {
                    op: Operator::I32Const { value: 10 },
                    line_idx: 0,
                },
                CodillonInstruction {
                    op: Operator::Loop {
                        blockty: BlockType::FuncType(1),
                    },
                    line_idx: 1,
                },
                CodillonInstruction {
                    op: Operator::I32Const { value: 1 },
                    line_idx: 2,
                },
                CodillonInstruction {
                    op: Operator::I32Sub,
                    line_idx: 3,
                },
                CodillonInstruction {
                    op: Operator::BrIf { relative_depth: 1 },
                    line_idx: 4,
                },
                CodillonInstruction {
                    op: Operator::I32Const { value: 2 },
                    line_idx: 5,
                },
                CodillonInstruction {
                    op: Operator::End,
                    line_idx: 6,
                },
                CodillonInstruction {
                    op: Operator::Drop,
                    line_idx: 7,
                },
            ],
        };
        assert_eq!(instruction_table.to_types_table(&wasm_bin)?, output);

        //nested block and if
        let output = TypesTable {
            _table: vec![
                CodillonType {
                    inputs: vec![],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 0,
                    }],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 1,
                    }],
                    outputs: vec![],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 3,
                    }],
                    outputs: vec![],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 5,
                    }],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 6,
                    }],
                    outputs: vec![ValType::I32],
                },
                CodillonType {
                    inputs: vec![InputType {
                        instr_type: ValType::I32,
                        origin_idx: 7,
                    }],
                    outputs: vec![],
                },
            ],
        };
        let lines = "i32.const 10\nblock (param i32) (result i32)\nif (result i32)\ni32.const 1\nelse\ni32.const 2\nend\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = InstructionTable {
            table: vec![
                CodillonInstruction {
                    op: Operator::I32Const { value: 10 },
                    line_idx: 0,
                },
                CodillonInstruction {
                    op: Operator::Block {
                        blockty: BlockType::FuncType(1),
                    },
                    line_idx: 1,
                },
                CodillonInstruction {
                    op: Operator::If {
                        blockty: BlockType::Type(ValType::I32),
                    },
                    line_idx: 2,
                },
                CodillonInstruction {
                    op: Operator::I32Const { value: 1 },
                    line_idx: 3,
                },
                CodillonInstruction {
                    op: Operator::Else,
                    line_idx: 4,
                },
                CodillonInstruction {
                    op: Operator::I32Const { value: 2 },
                    line_idx: 5,
                },
                CodillonInstruction {
                    op: Operator::End,
                    line_idx: 6,
                },
                CodillonInstruction {
                    op: Operator::End,
                    line_idx: 7,
                },
                CodillonInstruction {
                    op: Operator::Drop,
                    line_idx: 8,
                },
            ],
        };
        assert_eq!(instruction_table.to_types_table(&wasm_bin)?, output);

        //empty block
        let output = TypesTable {
            _table: vec![
                CodillonType {
                    inputs: vec![],
                    outputs: vec![],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![],
                },
            ],
        };
        let lines = "block\nend";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = InstructionTable {
            table: vec![
                CodillonInstruction {
                    op: Operator::Block {
                        blockty: BlockType::Empty,
                    },
                    line_idx: 0,
                },
                CodillonInstruction {
                    op: Operator::End,
                    line_idx: 1,
                },
            ],
        };
        assert_eq!(instruction_table.to_types_table(&wasm_bin)?, output);

        Ok(())
    }
}
