use crate::{
    line::{Activity, LineInfo},
    syntax::{InstrKind, SyntheticWasm},
};
use anyhow::{Result, anyhow};
use std::ops::{Deref, RangeInclusive};
use wasm_encoder::{
    CodeSection, ExportSection, FunctionSection, GlobalSection, Instruction as EncoderInstruction,
    TypeSection,
    reencode::{Reencode, RoundtripReencoder},
};
use wasm_tools::parse_binary_wasm;
use wasmparser::{Operator, ValType, ValidPayload, Validator};
use wast::{
    core::Module,
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
        let parser = wasmparser::Parser::new(0);
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
        let mut module: wasm_encoder::Module = Default::default();

        // Encode the type section.
        let mut types = TypeSection::new();
        let params = vec![];
        let results = vec![];
        types.ty().function(params, results);
        types.ty().function(vec![wasm_encoder::ValType::I32], vec![]);
        types.ty().function(vec![wasm_encoder::ValType::I32, wasm_encoder::ValType::I32], vec![]);
        module.section(&types);

        // Encode the instrumentation functions as imports.
        let mut imports = wasm_encoder::ImportSection::new();
        imports.import("codillon_debug", "step", wasm_encoder::EntityType::Function(1));
        imports.import("codillon_debug", "set_local", wasm_encoder::EntityType::Function(2));
        imports.import("codillon_debug", "set_global", wasm_encoder::EntityType::Function(2));
        imports.import("codillon_debug", "push_i32", wasm_encoder::EntityType::Function(1));
        imports.import("codillon_debug", "pop_one", wasm_encoder::EntityType::Function(0));
        module.section(&imports);

        // Encode the main function section.
        let mut functions = FunctionSection::new();
        let main_type_index = 0;
        functions.function(main_type_index);
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
        exports.export("main", wasm_encoder::ExportKind::Func, 5);
        module.section(&exports);

        // Encode the code section.
        let mut codes = CodeSection::new();
        // Avoid conflict for the temporary variable used for LocalTee duplication
        let local_index = 0u32;
        let locals = vec![(local_index + 1, wasm_encoder::ValType::I32)];
        let mut f = wasm_encoder::Function::new(locals);
        for codillon_instruction in &self.table {
            // Step after each instruction evaluation
            let line_idx = codillon_instruction.line_idx as i32;
            f.instruction(&EncoderInstruction::I32Const(line_idx));
            f.instruction(&EncoderInstruction::Call(0));

            let instruction = RoundtripReencoder.instruction(codillon_instruction.op.clone())?;
            match &codillon_instruction.op {
                wasmparser::Operator::LocalSet { local_index } |
                wasmparser::Operator::LocalTee { local_index } => {
                    f.instruction(&instruction);
                    f.instruction(&EncoderInstruction::I32Const(*local_index as i32));
                    f.instruction(&EncoderInstruction::LocalGet(*local_index));
                    f.instruction(&EncoderInstruction::Call(1));
                }
                wasmparser::Operator::GlobalSet { global_index } => {
                    f.instruction(&instruction);
                    f.instruction(&EncoderInstruction::I32Const(*global_index as i32));
                    f.instruction(&EncoderInstruction::GlobalGet(*global_index));
                    f.instruction(&EncoderInstruction::Call(2));
                }
                wasmparser::Operator::I32Const { value: _ } => {
                    f.instruction(&instruction);
                    f.instruction(&EncoderInstruction::LocalTee(local_index));
                    f.instruction(&EncoderInstruction::LocalGet(local_index));
                    f.instruction(&EncoderInstruction::Call(3));
                }
                _ => {
                    f.instruction(&instruction);
                }
            }
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
    let txt = format!("module {s}");
    let binding = ParseBuffer::new(&txt)?;
    let mut module = parser::parse::<Module>(&binding)?;

    Ok(module.encode()?)
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
        parse_binary_wasm(wasmparser::Parser::new(0), &binary)
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
    let parser = wasmparser::Parser::new(0);
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
}

pub trait LineInfosMut: LineInfos {
    fn set_active_status(&mut self, index: usize, new_val: Activity);
    fn set_synthetic_before(&mut self, index: usize, synth: SyntheticWasm);
    fn push(&mut self);
}

#[derive(PartialEq, Clone, Debug)]
pub struct FrameInfo {
    pub indent: usize,
    pub start: usize,
    pub end: usize,
    pub unclosed: bool,
    pub kind: InstrKind,
}

pub trait FrameInfosMut: LineInfos {
    fn set_indent(&mut self, index: usize, num: usize);
    fn set_frame_count(&mut self, count: usize);
    fn set_frame_info(&mut self, num: usize, frame: FrameInfo);
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
pub(crate) mod tests {
    use super::*;
    use wasmparser::BlockType;

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
