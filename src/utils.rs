use anyhow::Result;
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

enum InstrumentationFuncs {
    SetLocalI32(u32),
    SetGlobalI32(u32),
    SetMemoryI32,
    SetMemoryF32,
    PushI32,
    PushF32,
    PushI64,
    PushF64,
    Other,
}
#[repr(u32)]
enum InstrumentImports {
    Step,
    PopI,
    SetLocalI32,
    SetGlobalI32,
    SetMemoryI32,
    SetMemoryF32,
    PushI32,
    PushF32,
    PushI64,
    PushF64,
}
impl InstrumentImports {
    pub const TYPE_INDICES: &'static [(&'static str, u32)] = &[
        ("step", 1),
        ("pop_i", 0),
        ("set_local_i32", 5),
        ("set_global_i32", 5),
        ("set_memory_i32", 6),
        ("set_memory_f32", 7),
        ("push_i32", 1),
        ("push_f32", 2),
        ("push_i64", 3),
        ("push_f64", 4),
    ];
}

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
            if let ValidPayload::Func(func, body) = validator.payload(&payload?)? {
                let mut func_validator: wasmparser::FuncValidator<wasmparser::ValidatorResources> =
                    func.into_validator(wasmparser::FuncValidatorAllocations::default());
                let mut idx_stack: Vec<(usize, usize)> = Vec::new(); // simulated operand stack to track where each operand is pushed
                let mut reader = body.get_operators_reader()?;
                for instr in &self.table {
                    let op = &instr.op;
                    debug_assert_eq!(op, &reader.read()?); // ensure wasm_bin matches the instruction table
                    let (pop_count, push_count) = op
                        .operator_arity(&func_validator.visitor(dummy_offset))
                        .unwrap_or((0, 0));
                    let prev_height = func_validator.operand_stack_height();
                    let inputs = (0..pop_count)
                        .map(|i| {
                            if pop_count < prev_height + i + 1 {
                                let idx = (pop_count - i - 1) as usize;
                                Some(InputType {
                                    instr_type: func_validator
                                        .get_operand_type(idx)
                                        .flatten()
                                        .expect("operand"),
                                    origin: idx_stack[(prev_height + i - pop_count) as usize],
                                })
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();

                    for _ in 0..pop_count {
                        idx_stack.pop();
                    }

                    let _ = func_validator.op(dummy_offset, op);

                    let new_height = func_validator.operand_stack_height();
                    let outputs = (0..push_count)
                        .map(|i| {
                            idx_stack.push((instr.line_idx, i as usize));
                            if push_count < new_height + i + 1 {
                                Some(
                                    func_validator
                                        .get_operand_type((push_count - i - 1) as usize)
                                        .flatten()
                                        .expect("result operand"),
                                )
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();

                    result.push(CodillonType { inputs, outputs });
                }
            }
        }
        Ok(TypesTable { table: result })
    }

    fn classify(operation: &wasmparser::Operator, op_type: &CodillonType) -> InstrumentationFuncs {
        use wasmparser::Operator::*;
        match operation {
            // Special Functions
            LocalSet { local_index } | LocalTee { local_index } => match op_type.outputs[0] {
                Some(wasmparser::ValType::I32) => InstrumentationFuncs::SetLocalI32(*local_index),
                _ => InstrumentationFuncs::Other,
            },
            GlobalSet { global_index } => match op_type.outputs[0] {
                Some(wasmparser::ValType::I32) => InstrumentationFuncs::SetGlobalI32(*global_index),
                _ => InstrumentationFuncs::Other,
            },
            I32Store { .. } | I32Store8 { .. } | I32Store16 { .. } => {
                InstrumentationFuncs::SetMemoryI32
            }
            F32Store { .. } => InstrumentationFuncs::SetMemoryF32,
            // Match based on outputs
            _ => match op_type.outputs.as_slice() {
                [Some(wasmparser::ValType::I32)] => InstrumentationFuncs::PushI32,
                [Some(wasmparser::ValType::I64)] => InstrumentationFuncs::PushI64,
                [Some(wasmparser::ValType::F32)] => InstrumentationFuncs::PushF32,
                [Some(wasmparser::ValType::F64)] => InstrumentationFuncs::PushF64,
                _ => InstrumentationFuncs::Other,
            },
        }
    }

    fn instr_func_types(&self) -> TypeSection {
        // Encode the type section.
        let mut types = TypeSection::new();
        // 0: (i32) -> ()
        types
            .ty()
            .function(vec![wasm_encoder::ValType::I32], vec![]);
        // 1: (i32) -> (i32)
        types.ty().function(
            vec![wasm_encoder::ValType::I32],
            vec![wasm_encoder::ValType::I32],
        );
        // 2: (f32) -> (f32)
        types.ty().function(
            vec![wasm_encoder::ValType::F32],
            vec![wasm_encoder::ValType::F32],
        );
        // 3: (i64) -> (i64)
        types.ty().function(
            vec![wasm_encoder::ValType::I64],
            vec![wasm_encoder::ValType::I64],
        );
        // 4: (f64) -> (f64)
        types.ty().function(
            vec![wasm_encoder::ValType::F64],
            vec![wasm_encoder::ValType::F64],
        );
        // 5: (i32, i32) -> ()
        types.ty().function(
            vec![wasm_encoder::ValType::I32, wasm_encoder::ValType::I32],
            vec![],
        );
        // 6: (i32, i32) -> (i32, i32)
        types.ty().function(
            vec![wasm_encoder::ValType::I32, wasm_encoder::ValType::I32],
            vec![wasm_encoder::ValType::I32, wasm_encoder::ValType::I32],
        );
        // 7: (i32, f32) -> (i32, f32)
        types.ty().function(
            vec![wasm_encoder::ValType::I32, wasm_encoder::ValType::F32],
            vec![wasm_encoder::ValType::I32, wasm_encoder::ValType::F32],
        );
        types
    }

    fn instr_imports(&self) -> wasm_encoder::ImportSection {
        // Encode the instrumentation functions as imports.
        let mut imports = wasm_encoder::ImportSection::new();
        for (name, type_idx) in InstrumentImports::TYPE_INDICES.iter() {
            imports.import(
                "codillon_debug",
                name,
                wasm_encoder::EntityType::Function(*type_idx),
            );
        }
        imports
    }

    pub fn build_executable_binary(&self, types: &TypesTable) -> Result<Vec<u8>> {
        let mut module: wasm_encoder::Module = Default::default();
        module.section(&self.instr_func_types());
        module.section(&self.instr_imports());

        // Encode the main function section.
        let mut functions = FunctionSection::new();
        functions.function(0);
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
        exports.export(
            "main",
            wasm_encoder::ExportKind::Func,
            InstrumentImports::TYPE_INDICES.len() as u32,
        );
        module.section(&exports);

        // Encode the code section.
        let mut codes = CodeSection::new();
        let locals = vec![];
        let mut f = wasm_encoder::Function::new(locals);
        let pop_debug = |func: &mut wasm_encoder::Function, num_pop: i32| {
            func.instruction(&EncoderInstruction::I32Const(num_pop));
            func.instruction(&EncoderInstruction::Call(InstrumentImports::PopI as u32));
        };
        for (i, codillon_instruction) in self.table.iter().enumerate() {
            let line_idx = codillon_instruction.line_idx as i32;
            let instruction = RoundtripReencoder.instruction(codillon_instruction.op.clone())?;
            let value_type = &types.table[i];
            let operation_type = Self::classify(&codillon_instruction.op, value_type);
            // Instrumentation that needs to occur before execution
            match operation_type {
                InstrumentationFuncs::SetMemoryI32 => {
                    f.instruction(&EncoderInstruction::Call(
                        InstrumentImports::SetMemoryI32 as u32,
                    ));
                }
                InstrumentationFuncs::SetMemoryF32 => {
                    f.instruction(&EncoderInstruction::Call(
                        InstrumentImports::SetMemoryF32 as u32,
                    ));
                }
                _ => {}
            }
            pop_debug(&mut f, value_type.inputs.len() as i32);
            f.instruction(&instruction);
            // Instrumentation that needs to occur after execution
            match operation_type {
                InstrumentationFuncs::SetLocalI32(local_index) => {
                    f.instruction(&EncoderInstruction::I32Const(local_index as i32));
                    f.instruction(&EncoderInstruction::LocalGet(local_index));
                    f.instruction(&EncoderInstruction::Call(
                        InstrumentImports::SetLocalI32 as u32,
                    ));
                }
                InstrumentationFuncs::SetGlobalI32(global_index) => {
                    f.instruction(&EncoderInstruction::I32Const(global_index as i32));
                    f.instruction(&EncoderInstruction::GlobalGet(global_index));
                    f.instruction(&EncoderInstruction::Call(
                        InstrumentImports::SetGlobalI32 as u32,
                    ));
                }
                InstrumentationFuncs::PushI32 => {
                    f.instruction(&EncoderInstruction::Call(InstrumentImports::PushI32 as u32));
                }
                InstrumentationFuncs::PushF32 => {
                    f.instruction(&EncoderInstruction::Call(InstrumentImports::PushF32 as u32));
                }
                InstrumentationFuncs::PushI64 => {
                    f.instruction(&EncoderInstruction::Call(InstrumentImports::PushI64 as u32));
                }
                InstrumentationFuncs::PushF64 => {
                    f.instruction(&EncoderInstruction::Call(InstrumentImports::PushF64 as u32));
                }
                _ => {}
            }
            // Step after each instruction evaluation
            f.instruction(&EncoderInstruction::I32Const(line_idx));
            f.instruction(&EncoderInstruction::Call(InstrumentImports::Step as u32));
            f.instruction(&EncoderInstruction::I32Eqz);
            f.instruction(&EncoderInstruction::If(wasm_encoder::BlockType::Empty));
            f.instruction(&EncoderInstruction::Return);
            f.instruction(&EncoderInstruction::End);
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
    pub origin: (usize, usize), // line index + push# within the line
}

#[derive(Debug, PartialEq, Eq)]
pub struct CodillonType {
    inputs: Vec<Option<InputType>>,
    outputs: Vec<Option<ValType>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypesTable {
    table: Vec<CodillonType>,
}

pub fn str_to_binary(mut txt: String) -> Result<Vec<u8>> {
    txt.insert_str(0, "module ");
    let wasm_bin = parser::parse::<Module>(&ParseBuffer::new(&txt)?)?.encode()?;
    parse_binary_wasm(wasmparser::Parser::new(0), &wasm_bin)?; // make sure binary is well-formed

    Ok(wasm_bin)
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
    fn test_str_to_binary_for_one_function() {
        fn is_well_formed_func(s: &str) -> bool {
            str_to_binary(format!("(func {s})")).is_ok()
        }

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
    fn test_types_table() -> Result<()> {
        //block instruction with params and results
        let output = TypesTable {
            table: vec![
                CodillonType {
                    inputs: vec![],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![
                        Some(InputType {
                            instr_type: ValType::I32,
                            origin: (0, 0),
                        }),
                        Some(InputType {
                            instr_type: ValType::I32,
                            origin: (1, 0),
                        }),
                    ],
                    outputs: vec![Some(ValType::I32), Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![
                        Some(InputType {
                            instr_type: ValType::I32,
                            origin: (2, 0),
                        }),
                        Some(InputType {
                            instr_type: ValType::I32,
                            origin: (2, 1),
                        }),
                    ],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (3, 0),
                    })],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (4, 0),
                    })],
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
            table: vec![
                CodillonType {
                    inputs: vec![],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (0, 0),
                    })],
                    outputs: vec![],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (2, 0),
                    })],
                    outputs: vec![],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (4, 0),
                    })],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (5, 0),
                    })],
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
            table: vec![
                CodillonType {
                    inputs: vec![],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (0, 0),
                    })],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![
                        Some(InputType {
                            instr_type: ValType::I32,
                            origin: (1, 0),
                        }),
                        Some(InputType {
                            instr_type: ValType::I32,
                            origin: (2, 0),
                        }),
                    ],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (3, 0),
                    })],
                    outputs: vec![],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (5, 0),
                    })],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (6, 0),
                    })],
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
            table: vec![
                CodillonType {
                    inputs: vec![],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (0, 0),
                    })],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (1, 0),
                    })],
                    outputs: vec![],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (3, 0),
                    })],
                    outputs: vec![],
                },
                CodillonType {
                    inputs: vec![],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (5, 0),
                    })],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (6, 0),
                    })],
                    outputs: vec![Some(ValType::I32)],
                },
                CodillonType {
                    inputs: vec![Some(InputType {
                        instr_type: ValType::I32,
                        origin: (7, 0),
                    })],
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
            table: vec![
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

    #[test]
    fn test_types_table_invalid_input() -> Result<()> {
        // valid example first
        {
            let wasm_bin = str_to_binary("(func i32.const 4 i32.const 5 i32.add)".to_string())?;
            let instruction_table = InstructionTable {
                table: vec![
                    CodillonInstruction {
                        op: Operator::I32Const { value: 4 },
                        line_idx: 1,
                    },
                    CodillonInstruction {
                        op: Operator::I32Const { value: 5 },
                        line_idx: 2,
                    },
                    CodillonInstruction {
                        op: Operator::I32Add,
                        line_idx: 3,
                    },
                ],
            };
            let expected_output = TypesTable {
                table: vec![
                    CodillonType {
                        inputs: vec![],
                        outputs: vec![Some(ValType::I32)],
                    },
                    CodillonType {
                        inputs: vec![],
                        outputs: vec![Some(ValType::I32)],
                    },
                    CodillonType {
                        inputs: vec![
                            Some(InputType {
                                instr_type: ValType::I32,
                                origin: (1, 0),
                            }),
                            Some(InputType {
                                instr_type: ValType::I32,
                                origin: (2, 0),
                            }),
                        ],
                        outputs: vec![Some(ValType::I32)],
                    },
                ],
            };
            assert_eq!(
                instruction_table.to_types_table(&wasm_bin)?,
                expected_output
            );
        }

        // missing one param
        {
            let wasm_bin = str_to_binary("(func i32.const 4 i32.add)".to_string())?;
            let instruction_table = InstructionTable {
                table: vec![
                    CodillonInstruction {
                        op: Operator::I32Const { value: 4 },
                        line_idx: 1,
                    },
                    CodillonInstruction {
                        op: Operator::I32Add,
                        line_idx: 3,
                    },
                ],
            };
            let expected_output = TypesTable {
                table: vec![
                    CodillonType {
                        inputs: vec![],
                        outputs: vec![Some(ValType::I32)],
                    },
                    CodillonType {
                        inputs: vec![
                            None,
                            Some(InputType {
                                instr_type: ValType::I32,
                                origin: (1, 0),
                            }),
                        ],
                        outputs: vec![None],
                    },
                ],
            };
            assert_eq!(
                instruction_table.to_types_table(&wasm_bin)?,
                expected_output
            );
        }

        // unknown arity
        {
            let wasm_bin = str_to_binary("(func call 1)".to_string())?;
            let instruction_table = InstructionTable {
                table: vec![CodillonInstruction {
                    op: Operator::Call { function_index: 1 },
                    line_idx: 1,
                }],
            };
            let expected_output = TypesTable {
                table: vec![CodillonType {
                    inputs: vec![],
                    outputs: vec![],
                }],
            };
            assert_eq!(
                instruction_table.to_types_table(&wasm_bin)?,
                expected_output
            );
        }

        // heterogeneous params (valid)
        {
            let wasm_bin =
                str_to_binary("(func f32.const 6 f32.const 2 i32.const 1 select)".to_string())?;
            let instruction_table = InstructionTable {
                table: vec![
                    CodillonInstruction {
                        op: Operator::F32Const { value: 6.0.into() },
                        line_idx: 1,
                    },
                    CodillonInstruction {
                        op: Operator::F32Const { value: 2.0.into() },
                        line_idx: 2,
                    },
                    CodillonInstruction {
                        op: Operator::I32Const { value: 1 },
                        line_idx: 3,
                    },
                    CodillonInstruction {
                        op: Operator::Select,
                        line_idx: 4,
                    },
                ],
            };
            let expected_output = TypesTable {
                table: vec![
                    CodillonType {
                        inputs: vec![],
                        outputs: vec![Some(ValType::F32)],
                    },
                    CodillonType {
                        inputs: vec![],
                        outputs: vec![Some(ValType::F32)],
                    },
                    CodillonType {
                        inputs: vec![],
                        outputs: vec![Some(ValType::I32)],
                    },
                    CodillonType {
                        inputs: vec![
                            Some(InputType {
                                instr_type: ValType::F32,
                                origin: (1, 0),
                            }),
                            Some(InputType {
                                instr_type: ValType::F32,
                                origin: (2, 0),
                            }),
                            Some(InputType {
                                instr_type: ValType::I32,
                                origin: (3, 0),
                            }),
                        ],
                        outputs: vec![Some(ValType::F32)],
                    },
                ],
            };
            assert_eq!(
                instruction_table.to_types_table(&wasm_bin)?,
                expected_output
            );
        }

        // heterogeneous params (invalid)
        {
            let wasm_bin =
                str_to_binary("(func f32.const 6 i32.const 2 i32.const 1 select)".to_string())?;
            let instruction_table = InstructionTable {
                table: vec![
                    CodillonInstruction {
                        op: Operator::F32Const { value: 6.0.into() },
                        line_idx: 1,
                    },
                    CodillonInstruction {
                        op: Operator::I32Const { value: 2 },
                        line_idx: 2,
                    },
                    CodillonInstruction {
                        op: Operator::I32Const { value: 1 },
                        line_idx: 3,
                    },
                    CodillonInstruction {
                        op: Operator::Select,
                        line_idx: 4,
                    },
                ],
            };
            let expected_output = TypesTable {
                table: vec![
                    CodillonType {
                        inputs: vec![],
                        outputs: vec![Some(ValType::F32)],
                    },
                    CodillonType {
                        inputs: vec![],
                        outputs: vec![Some(ValType::I32)],
                    },
                    CodillonType {
                        inputs: vec![],
                        outputs: vec![Some(ValType::I32)],
                    },
                    CodillonType {
                        inputs: vec![
                            Some(InputType {
                                instr_type: ValType::F32,
                                origin: (1, 0),
                            }),
                            Some(InputType {
                                instr_type: ValType::I32,
                                origin: (2, 0),
                            }),
                            Some(InputType {
                                instr_type: ValType::I32,
                                origin: (3, 0),
                            }),
                        ],
                        outputs: vec![None],
                    },
                ],
            };
            assert_eq!(
                instruction_table.to_types_table(&wasm_bin)?,
                expected_output
            );
        }

        Ok(())
    }
}
