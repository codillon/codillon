use anyhow::{Context, Result, bail};
use wasm_encoder::{
    CodeSection, ExportSection, FunctionSection, GlobalSection, Instruction as EncoderInstruction,
    TypeSection, ValType as EncoderValType,
    reencode::{Reencode, RoundtripReencoder},
};
use wasm_tools::parse_binary_wasm;
use wasmparser::{
    FuncValidator, HeapType, Operator, ValType, ValidPayload, Validator, WasmModuleResources,
};
use wast::{
    core::Module,
    parser::{self, ParseBuffer},
};

use crate::syntax::{LineInfos, LineInfosMut};
use EncoderInstruction::*;
use InstrumentationFuncs::*;

enum InstrumentationFuncs {
    SetLocalI32(u32),
    SetLocalF32(u32),
    SetLocalI64(u32),
    SetLocalF64(u32),
    SetGlobalI32(u32),
    SetGlobalF32(u32),
    SetGlobalI64(u32),
    SetGlobalF64(u32),
    SetMemoryI32,
    SetMemoryF32,
    SetMemoryI64,
    SetMemoryF64,
    PushI32,
    PushF32,
    PushI64,
    PushF64,
    CallFunc(u32),
    EndFunc,
    Other,
}
#[repr(u32)]
enum InstrImports {
    Step,
    PopI,
    SetLocalI32,
    SetLocalF32,
    SetLocalI64,
    SetLocalF64,
    SetGlobalI32,
    SetGlobalF32,
    SetGlobalI64,
    SetGlobalF64,
    SetMemoryI32,
    SetMemoryF32,
    SetMemoryI64,
    SetMemoryF64,
    PushI32,
    PushF32,
    PushI64,
    PushF64,
}
impl InstrImports {
    const TYPE_INDICES: &'static [(&'static str, u32)] = &[
        ("step", 1),
        ("pop_i", 0),
        ("set_local_i32", 5),
        ("set_local_f32", 10),
        ("set_local_i64", 11),
        ("set_local_f64", 12),
        ("set_global_i32", 5),
        ("set_global_f32", 10),
        ("set_global_i64", 11),
        ("set_global_f64", 12),
        ("set_memory_i32", 6),
        ("set_memory_f32", 7),
        ("set_memory_i64", 8),
        ("set_memory_f64", 9),
        ("push_i32", 1),
        ("push_f32", 2),
        ("push_i64", 3),
        ("push_f64", 4),
    ];
    const FUNC_SIGS: &'static [(&'static [EncoderValType], &'static [EncoderValType])] = &[
        // 0: (i32) -> ()
        (&[EncoderValType::I32], &[]),
        // 1: (i32) -> (i32)
        (&[EncoderValType::I32], &[EncoderValType::I32]),
        // 2: (f32) -> (f32)
        (&[EncoderValType::F32], &[EncoderValType::F32]),
        // 3: (i64) -> (i64)
        (&[EncoderValType::I64], &[EncoderValType::I64]),
        // 4: (f64) -> (f64)
        (&[EncoderValType::F64], &[EncoderValType::F64]),
        // 5: (i32, i32) -> ()
        (&[EncoderValType::I32, EncoderValType::I32], &[]),
        // 6: (i32, i32) -> (i32, i32)
        (
            &[EncoderValType::I32, EncoderValType::I32],
            &[EncoderValType::I32, EncoderValType::I32],
        ),
        // 7: (i32, f32) -> (i32, f32)
        (
            &[EncoderValType::I32, EncoderValType::F32],
            &[EncoderValType::I32, EncoderValType::F32],
        ),
        // 8: (i32, i64) -> (i32, i64)
        (
            &[EncoderValType::I32, EncoderValType::I64],
            &[EncoderValType::I32, EncoderValType::I64],
        ),
        // 9: (i32, f64) -> (i32, f64)
        (
            &[EncoderValType::I32, EncoderValType::F64],
            &[EncoderValType::I32, EncoderValType::F64],
        ),
        // 10: (i32, f32) -> ()
        (&[EncoderValType::I32, EncoderValType::F32], &[]),
        // 11: (i32, i64) -> ()
        (&[EncoderValType::I32, EncoderValType::I64], &[]),
        // 12: (i32, f64) -> ()
        (&[EncoderValType::I32, EncoderValType::F64], &[]),
    ];
}

pub struct GeneralOperator<'a> {
    prepended: Vec<Operator<'a>>,
    op: Operator<'a>,
    untyped: bool,
}

pub struct Aligned<T> {
    pub op: T,
    pub line_idx: usize,
}

pub struct RawFunction<'a> {
    pub type_idx: usize,
    pub locals: Vec<(u32, ValType)>,
    pub operators: Vec<Aligned<Operator<'a>>>,
}

pub struct ValidFunction<'a> {
    pub type_idx: usize,
    pub locals: Vec<(u32, ValType)>,
    pub operators: Vec<Aligned<GeneralOperator<'a>>>,
}

pub struct RawModule<'a> {
    pub types: Vec<wasmparser::FuncType>,
    pub imports: Vec<wasmparser::Import<'a>>,
    pub memory: Vec<wasmparser::MemoryType>,
    pub globals: Vec<wasmparser::Global<'a>>,
    pub functions: Vec<RawFunction<'a>>,
}

pub struct ValidModule<'a> {
    pub types: Vec<wasmparser::FuncType>,
    pub imports: Vec<wasmparser::Import<'a>>,
    pub memory: Vec<wasmparser::MemoryType>,
    pub globals: Vec<wasmparser::Global<'a>>,
    pub functions: Vec<ValidFunction<'a>>,
}

impl<'a> From<Aligned<Operator<'a>>> for Aligned<GeneralOperator<'a>> {
    fn from(val: Aligned<Operator<'a>>) -> Self {
        Self {
            op: GeneralOperator {
                prepended: vec![],
                op: val.op,
                untyped: false,
            },
            line_idx: val.line_idx,
        }
    }
}

const DUMMY_OFFSET: usize = 1; // no need to track offsets, but validator has safety checks against 0

impl<'a> RawModule<'a> {
    pub fn new(
        editor: &impl LineInfos,
        wasm_bin: &'a [u8],
        function_ranges: &[(usize, usize)],
    ) -> Result<Self> {
        use wasmparser::*;
        let parser = Parser::new(0);
        let mut functions: Vec<RawFunction> = Vec::new();
        let mut types: Vec<FuncType> = Vec::new();
        let mut func_type_indices: Vec<usize> = Vec::new();
        let mut imports: Vec<Import> = Vec::new();
        let mut memory: Vec<MemoryType> = Vec::new();
        let mut globals: Vec<Global> = Vec::new();
        let mut func_locals: Vec<Vec<(u32, ValType)>> = Vec::new();
        let mut func_ops: Vec<Vec<Operator<'a>>> = Vec::new();

        for payload in parser.parse_all(wasm_bin) {
            match payload? {
                Payload::TypeSection(reader) => {
                    for ft in reader.into_iter_err_on_gc_types().flatten() {
                        types.push(ft);
                    }
                }
                Payload::ImportSection(reader) => {
                    for import in reader.into_imports().flatten() {
                        imports.push(import);
                    }
                }
                Payload::FunctionSection(reader) => {
                    for func_type_idx in reader.into_iter().flatten() {
                        func_type_indices.push(func_type_idx as usize);
                    }
                }
                Payload::MemorySection(reader) => {
                    for mem in reader.into_iter_with_offsets().flatten() {
                        memory.push(mem.1);
                    }
                }
                Payload::GlobalSection(reader) => {
                    for global in reader.into_iter_with_offsets().flatten() {
                        globals.push(global.1);
                    }
                }
                Payload::CodeSectionEntry(body) => {
                    let mut locals: Vec<(u32, ValType)> = Vec::new();
                    let local_reader = body.get_locals_reader()?;
                    for local in local_reader {
                        locals.push(local?);
                    }
                    func_locals.push(locals);
                    let mut ops = Vec::new();
                    for op in body.get_operators_reader()?.into_iter() {
                        ops.push(op?);
                    }
                    //include the function's end opcode
                    func_ops.push(ops);
                }
                _ => {}
            }
        }
        for (func_idx, (func_start, func_end)) in function_ranges.iter().enumerate() {
            //match each operator with its idx in the editor
            let mut aligned_ops = Vec::new();
            let mut ops_iter = func_ops[func_idx].clone().into_iter();
            for line_idx in *func_start..=*func_end {
                for _ in 0..editor.info(line_idx).num_ops() {
                    let op = ops_iter.next().context("not enough operators")?;
                    aligned_ops.push(Aligned { op, line_idx });
                }
            }
            match ops_iter.next() {
                Some(end @ wasmparser::Operator::End) => {
                    aligned_ops.push(Aligned {
                        op: end,
                        line_idx: *func_end,
                    });
                }
                Some(_) => {
                    bail!("not enough instructions");
                }
                None => {
                    bail!("not enough operators");
                }
            }
            let locals = func_locals.get(func_idx).cloned().unwrap_or_default();
            functions.push(RawFunction {
                type_idx: func_type_indices[func_idx],
                locals,
                operators: aligned_ops,
            });
        }

        Ok(RawModule {
            types,
            imports,
            memory,
            globals,
            functions,
        })
    }

    pub fn fix_validity(
        self,
        wasm_bin: &'a [u8],
        editor: &mut impl LineInfosMut,
    ) -> Result<ValidModule<'a>> {
        let parser = wasmparser::Parser::new(0);
        let mut validator = Validator::new();
        let mut allocs = wasmparser::FuncValidatorAllocations::default();

        let mut ret = ValidModule {
            types: self.types,
            imports: self.imports,
            memory: self.memory,
            globals: self.globals,
            functions: Vec::with_capacity(self.functions.len()),
        };

        let mut raw_functions = self.functions.into_iter();

        for payload in parser.parse_all(wasm_bin) {
            if let ValidPayload::Func(func, body) = validator.payload(&payload?)? {
                let RawFunction {
                    type_idx,
                    locals,
                    operators,
                } = raw_functions.next().expect("one function");

                #[cfg(debug_assertions)]
                Self::assert_bodies_match(&locals, &operators, &body)?;

                let mut func_validator = func.into_validator(allocs);

                for (count, ty) in &locals {
                    func_validator.define_locals(DUMMY_OFFSET, *count, *ty)?;
                }

                let mut valid_function = ValidFunction {
                    type_idx,
                    locals,
                    operators: Vec::with_capacity(operators.len()),
                };

                for op in operators {
                    match func_validator.try_op(DUMMY_OFFSET, &op.op) {
                        Ok(()) => valid_function.operators.push(op.into()),
                        Err(e) => {
                            editor.set_invalid(op.line_idx, Some(e.message().to_string()));
                            /* make a valid version of this operator */
                            let validized = Self::make_valid(func_validator.clone(), op)?;
                            for prepended_op in &validized.op.prepended {
                                func_validator
                                    .op(DUMMY_OFFSET, prepended_op)
                                    .expect("prepended op is valid");
                            }
                            func_validator
                                .op(DUMMY_OFFSET, &validized.op.op)
                                .expect("validized op now valid");
                            valid_function.operators.push(validized);
                        }
                    };
                }

                ret.functions.push(valid_function);
                allocs = func_validator.into_allocations();
            }
        }

        Ok(ret)
    }

    // Take an invalid Operator and make it valid, either by
    // inserting the appropriate drops and consts before it,
    // or by disabling it.
    fn make_valid<T: WasmModuleResources + Clone>(
        func_validator: FuncValidator<T>,
        op: Aligned<Operator>,
    ) -> Result<Aligned<GeneralOperator>> {
        // match "i32 " or "(ref func) "
        fn first_type(s: &str) -> Result<&str> {
            let mut paren_count = 0;
            for (i, ch) in s.bytes().enumerate() {
                match ch {
                    b' ' if paren_count == 0 => {
                        return Ok(&s[0..i]);
                    }
                    b'(' => paren_count += 1,
                    b')' => paren_count -= 1,
                    _ => (),
                }
            }
            panic!("no end of type found");
        }

        // given a string describing a Wasm valtype, synthesize
        // the corresponding "const" instruction to produce
        // a value of that type. E.g. "i32" produces an "i32.const 0" instruction.
        // Some types do not have a default value (e.g. "(ref func")).
        fn type_str_to_default_value(s: &str) -> Option<Operator<'static>> {
            match s {
                "i32" => Some(Operator::I32Const { value: 0 }),
                "i64" => Some(Operator::I64Const { value: 0 }),
                "f32" => Some(Operator::F32Const { value: 0f32.into() }),
                "f64" => Some(Operator::F64Const { value: 0f64.into() }),
                "v128" => Some(Operator::V128Const {
                    value: 0u128.into(),
                }),
                "funcref" => Some(Operator::RefNull {
                    hty: HeapType::FUNC,
                }),
                "externref" => Some(Operator::RefNull {
                    hty: HeapType::EXTERN,
                }),
                _ => None,
            }
        }

        // Disable this operator (there's no way to make it valid in context),
        // by replacing it with a nop unless it's the beginning of a block instruction,
        // in which case preserve the block structure.
        fn bailout(op: &Aligned<GeneralOperator<'_>>) -> Aligned<GeneralOperator<'static>> {
            Aligned {
                line_idx: op.line_idx,
                op: GeneralOperator {
                    prepended: vec![],
                    untyped: true,
                    op: match op.op.op {
                        Operator::Block { .. } => Operator::Block {
                            blockty: wasmparser::BlockType::Empty,
                        },
                        Operator::Loop { .. } => Operator::Loop {
                            blockty: wasmparser::BlockType::Empty,
                        },
                        Operator::If { .. } => Operator::If {
                            blockty: wasmparser::BlockType::Empty,
                        },
                        _ => Operator::Nop,
                    },
                },
            }
        }

        let mut general_op: Aligned<GeneralOperator> = op.into();

        // First, drop all operands accessible on stack
        let drop_count = func_validator.operand_stack_height() as usize
            - func_validator.get_control_frame(0).unwrap().height;
        for _ in 0..drop_count {
            general_op.op.prepended.push(Operator::Drop);
        }

        // Attempt to validate the operator in this context,
        // adding one param to the stack to satisfy each
        // "type mismatch" error message from the validator.
        loop {
            let mut validator_copy = func_validator.clone();

            for prepended_op in &general_op.op.prepended {
                validator_copy
                    .op(DUMMY_OFFSET, prepended_op)
                    .expect("prepended op is valid");
            }

            match validator_copy.try_op(DUMMY_OFFSET, &general_op.op.op) {
                Ok(()) => {
                    return Ok(general_op);
                }
                Err(e) => {
                    match e.message().strip_prefix("type mismatch: expected ") {
                        None => {
                            // some reason for the invalidity other than a missing/mismatched param
                            // -> replace operator with something valid in this context
                            return Ok(bailout(&general_op));
                        }
                        Some(suffix) => match type_str_to_default_value(first_type(suffix)?) {
                            // Invalidity is because of a missing or mismatched param.
                            // Can we insert a default value?
                            Some(op) => general_op.op.prepended.insert(drop_count, op),
                            None => {
                                // No default value possible.
                                general_op.op.untyped = true;
                                match &general_op.op.op {
                                    // If the operator is an end, need to preserve block structure,
                                    // but we can do this by marking block (which is almost ending anyway)
                                    // unreachable. This will make the stack polymorphic and the `end`
                                    // can pop whatever it needs.
                                    Operator::End => general_op
                                        .op
                                        .prepended
                                        .insert(drop_count, Operator::Unreachable),
                                    // A polymorphic operator (e.g. `drop` or `select`) can take any param,
                                    // so here give it an i32.
                                    _ if suffix == "a type but nothing on stack" => general_op
                                        .op
                                        .prepended
                                        .insert(drop_count, Operator::I32Const { value: 0 }),
                                    // Otherwise, replace operator with something valid in this context.
                                    _ => return Ok(bailout(&general_op)),
                                }
                            }
                        },
                    }
                }
            }
        }
    }

    fn assert_bodies_match(
        locals: &[(u32, ValType)],
        ops: &[Aligned<Operator<'a>>],
        body: &wasmparser::FunctionBody<'a>,
    ) -> Result<()> {
        let mut locals_reader = body.get_locals_reader()?;

        for (count, ty) in locals {
            let (body_count, body_ty) = locals_reader.read()?;
            assert_eq!(*count, body_count);
            assert_eq!(*ty, body_ty);
        }

        let mut ops_reader = body.get_operators_reader()?;
        for op in ops {
            assert_eq!(&op.op, &ops_reader.read()?);
        }
        ops_reader.finish()?;
        assert!(&ops_reader.eof());
        Ok(())
    }
}

#[derive(Default)]
struct SimulatedStack {
    idx_stack: Vec<(usize, usize)>,
}

impl SimulatedStack {
    // Given an operator, validate it and return its type (the param and result types).
    fn op(
        &mut self,
        op: &Operator<'_>,
        validator: &mut wasmparser::FuncValidator<wasmparser::ValidatorResources>,
        line_idx: usize,
        untyped: bool,
    ) -> Result<CodillonType> {
        let (pop_count, push_count) = op
            .operator_arity(&validator.visitor(DUMMY_OFFSET))
            .expect("arity");
        let (pop_count, push_count) = (pop_count as usize, push_count as usize);
        let pre_instr_height = validator.operand_stack_height() as usize;
        let frame_base_height = validator.get_control_frame(0).expect("top frame").height;
        assert!(pre_instr_height >= frame_base_height);
        let accessible_operands = pre_instr_height - frame_base_height;

        let untyped = untyped || pop_count > accessible_operands; // can happen after unreachable

        let inputs = if untyped {
            vec![]
        } else {
            (0..pop_count)
                .map(|i| InputType {
                    instr_type: validator
                        .get_operand_type(pop_count - i - 1)
                        .flatten()
                        .unwrap(),
                    origin: self.idx_stack[pre_instr_height + i - pop_count],
                })
                .collect::<Vec<_>>()
        };

        for _ in 0..std::cmp::min(pop_count, accessible_operands) {
            self.idx_stack.pop();
        }

        validator.op(DUMMY_OFFSET, op)?;

        let post_instr_height = validator.operand_stack_height() as usize;
        let frame_base_height = match validator.get_control_frame(0) {
            Some(f) => f.height,
            _ => 0,
        }; // might be the terminating end of a function, in which case no stack frames remaining
        assert!(post_instr_height >= frame_base_height);
        let accessible_operands = post_instr_height - frame_base_height;
        assert!(push_count <= accessible_operands);

        let outputs = (0..push_count)
            .map(|i| {
                self.idx_stack.push((line_idx, i));
                validator
                    .get_operand_type(push_count - i - 1)
                    .flatten()
                    .expect("result operand")
            })
            .collect::<Vec<_>>();

        Ok(CodillonType {
            inputs,
            outputs,
            input_arity: untyped.then(|| pop_count.try_into().unwrap_or(0)),
        })
    }
}

impl<'a> ValidModule<'a> {
    /// Computes the param and result types for each operator in the module.
    ///
    pub fn to_types_table(&self, wasm_bin: &[u8]) -> Result<TypesTable> {
        let parser = wasmparser::Parser::new(0);
        let mut validator = Validator::new();
        let mut ret = TypesTable {
            functions: Vec::with_capacity(self.functions.len()),
        };
        let mut allocs = wasmparser::FuncValidatorAllocations::default();
        let mut function_index = 0;

        for payload in parser.parse_all(wasm_bin) {
            if let ValidPayload::Func(func, _) = validator.payload(&payload?)? {
                let mut validator = func.into_validator(allocs);
                let types = Self::function_into_types_table(
                    &mut validator,
                    self.functions.get(function_index).unwrap(),
                )?;
                ret.functions.push(types);
                allocs = validator.into_allocations();
                function_index += 1;
            }
        }
        Ok(ret)
    }

    fn function_into_types_table(
        func_validator: &mut wasmparser::FuncValidator<wasmparser::ValidatorResources>,
        valid_func: &ValidFunction<'_>,
    ) -> Result<TypedFunction> {
        let mut stack = SimulatedStack::default();

        let mut types = Vec::with_capacity(valid_func.operators.len());

        for (count, ty) in &valid_func.locals {
            func_validator.define_locals(DUMMY_OFFSET, *count, *ty)?;
        }

        for op in &valid_func.operators {
            for pre in &op.op.prepended {
                stack.op(pre, func_validator, op.line_idx, false)?;
            }

            types.push(stack.op(&op.op.op, func_validator, op.line_idx, op.op.untyped)?);
        }

        Ok(TypedFunction { types })
    }

    fn classify(operation: &wasmparser::Operator, op_type: &CodillonType) -> InstrumentationFuncs {
        use wasmparser::Operator::*;
        match operation {
            // Special Functions
            Call { function_index } => CallFunc(*function_index),
            Return | End => EndFunc,
            LocalSet { local_index } | LocalTee { local_index } => {
                match op_type.inputs.first().expect("local op type") {
                    InputType {
                        instr_type: ValType::I32,
                        ..
                    } => SetLocalI32(*local_index),
                    InputType {
                        instr_type: ValType::F32,
                        ..
                    } => SetLocalF32(*local_index),
                    InputType {
                        instr_type: ValType::I64,
                        ..
                    } => SetLocalI64(*local_index),
                    InputType {
                        instr_type: ValType::F64,
                        ..
                    } => SetLocalF64(*local_index),
                    _ => Other,
                }
            }
            GlobalSet { global_index } => match op_type.inputs.first().expect("global op type") {
                InputType {
                    instr_type: ValType::I32,
                    ..
                } => SetGlobalI32(*global_index),
                InputType {
                    instr_type: ValType::F32,
                    ..
                } => SetGlobalF32(*global_index),
                InputType {
                    instr_type: ValType::I64,
                    ..
                } => SetGlobalI64(*global_index),
                InputType {
                    instr_type: ValType::F64,
                    ..
                } => SetGlobalF64(*global_index),
                _ => Other,
            },
            I32Store { .. } | I32Store8 { .. } | I32Store16 { .. } => SetMemoryI32,
            F32Store { .. } => SetMemoryF32,
            I64Store { .. } | I64Store8 { .. } | I64Store16 { .. } => SetMemoryI64,
            F64Store { .. } => SetMemoryF64,
            // Match based on outputs
            _ => match op_type.outputs.as_slice() {
                [wasmparser::ValType::I32] => PushI32,
                [wasmparser::ValType::I64] => PushI64,
                [wasmparser::ValType::F32] => PushF32,
                [wasmparser::ValType::F64] => PushF64,
                _ => Other,
            },
        }
    }

    fn instr_func_types(&self) -> TypeSection {
        // Encode the type section.
        let mut types = TypeSection::new();
        for (params_slice, results_slice) in InstrImports::FUNC_SIGS.iter() {
            types
                .ty()
                .function(params_slice.to_vec(), results_slice.to_vec());
        }
        for ty in &self.types {
            types.ty().function(
                ty.params()
                    .iter()
                    .map(parser_to_encoder)
                    .collect::<Vec<_>>()
                    .clone(),
                ty.results()
                    .iter()
                    .map(parser_to_encoder)
                    .collect::<Vec<_>>()
                    .clone(),
            );
        }
        types
    }

    fn instr_imports(&self) -> (wasm_encoder::ImportSection, u32) {
        use wasm_encoder::EntityType;
        use wasmparser::*;
        use web_sys::console::log_1;
        // Encode the instrumentation functions as imports.
        let mut imports = wasm_encoder::ImportSection::new();
        for (name, type_idx) in InstrImports::TYPE_INDICES.iter() {
            imports.import("codillon_debug", name, EntityType::Function(*type_idx));
        }
        let mut num_imports = InstrImports::TYPE_INDICES.len() as u32;
        for Import { name, module, ty } in &self.imports {
            match *ty {
                TypeRef::Func(type_idx) | TypeRef::FuncExact(type_idx) => {
                    imports.import(
                        module,
                        name,
                        EntityType::Function(type_idx + InstrImports::FUNC_SIGS.len() as u32),
                    );
                    num_imports += 1;
                }
                TypeRef::Memory(MemoryType {
                    initial,
                    memory64,
                    shared,
                    maximum,
                    page_size_log2,
                }) => {
                    imports.import(
                        module,
                        name,
                        EntityType::Memory(wasm_encoder::MemoryType {
                            minimum: initial,
                            maximum,
                            memory64,
                            shared,
                            page_size_log2,
                        }),
                    );
                }
                TypeRef::Global(GlobalType {
                    content_type,
                    mutable,
                    shared,
                }) => {
                    imports.import(
                        module,
                        name,
                        EntityType::Global(wasm_encoder::GlobalType {
                            val_type: parser_to_encoder(&content_type),
                            shared,
                            mutable,
                        }),
                    );
                }
                _ => {
                    log_1(&format!("unsupported import {name} from module {module}").into());
                }
            };
        }
        (imports, num_imports)
    }

    fn build_globals(&self) -> wasm_encoder::GlobalSection {
        use wasm_encoder::ConstExpr;
        use wasmparser::Operator::*;
        let mut globals = GlobalSection::new();
        for wasmparser::Global { ty, init_expr } in &self.globals {
            let mut init_reader = init_expr.get_operators_reader();
            if let Ok(op) = init_reader.read() {
                let initial_expression = match op {
                    I32Const { value } => ConstExpr::i32_const(value),
                    F32Const { value } => ConstExpr::f32_const(value.into()),
                    I64Const { value } => ConstExpr::i64_const(value),
                    F64Const { value } => ConstExpr::f64_const(value.into()),
                    V128Const { value } => ConstExpr::v128_const(value.into()),
                    _ => ConstExpr::empty(),
                };
                globals.global(
                    wasm_encoder::GlobalType {
                        val_type: parser_to_encoder(&ty.content_type),
                        mutable: ty.mutable,
                        shared: ty.shared,
                    },
                    &initial_expression,
                );
            }
        }
        globals
    }

    fn build_memory(&self) -> wasm_encoder::MemorySection {
        let mut memory = wasm_encoder::MemorySection::new();
        for &wasmparser::MemoryType {
            memory64,
            shared,
            initial,
            maximum,
            page_size_log2,
        } in &self.memory
        {
            memory.memory(wasm_encoder::MemoryType {
                minimum: initial,
                maximum,
                shared,
                memory64,
                page_size_log2,
            });
        }
        memory
    }

    pub fn build_executable_binary(&self, types: &TypesTable) -> Result<Vec<u8>> {
        let mut module: wasm_encoder::Module = Default::default();
        module.section(&self.instr_func_types());
        let (imports, num_imports) = self.instr_imports();
        module.section(&imports);

        // Encode the function section.
        let mut functions = FunctionSection::new();
        let func_type_offset = InstrImports::FUNC_SIGS.len();
        for func in &self.functions {
            functions.function((func.type_idx + func_type_offset) as u32);
        }
        module.section(&functions);
        module.section(&self.build_memory());
        module.section(&self.build_globals());

        // Encode the export section.
        let mut exports = ExportSection::new();
        exports.export("main", wasm_encoder::ExportKind::Func, num_imports);
        module.section(&exports);

        // Encode the code section.
        let mut codes = CodeSection::new();
        for func_idx in 0..self.functions.len() {
            let _ = self.build_function(func_idx, &mut codes, types);
        }
        module.section(&codes);

        let wasm = module.finish();
        Ok(wasm)
    }

    fn build_function(
        &self,
        func_idx: usize,
        codes: &mut CodeSection,
        types: &TypesTable,
    ) -> Result<(), anyhow::Error> {
        let num_instr_imports = InstrImports::TYPE_INDICES.len() as u32;
        let function = self.functions.get(func_idx).expect("valid func idx");
        let mut f = wasm_encoder::Function::new(
            function
                .locals
                .iter()
                .map(|(count, value)| (*count, parser_to_encoder(value)))
                .collect::<Vec<(u32, EncoderValType)>>(),
        );
        let pop_debug = |func: &mut wasm_encoder::Function, num_pop: i32| {
            func.instruction(&I32Const(num_pop));
            func.instruction(&Call(InstrImports::PopI as u32));
        };
        self.record_params(func_idx, &mut f);
        for (i, codillon_instruction) in function.operators.iter().enumerate() {
            if !codillon_instruction.op.prepended.is_empty() {
                f.instruction(&Unreachable);
                for preop in &codillon_instruction.op.prepended {
                    f.instruction(&RoundtripReencoder.instruction(preop.clone())?);
                }
            }
            let line_idx = codillon_instruction.line_idx as i32;
            let instruction = RoundtripReencoder.instruction(codillon_instruction.op.op.clone())?;
            let value_type = &types.functions.get(func_idx).unwrap().types[i];
            let operation_type = Self::classify(&codillon_instruction.op.op, value_type);
            if !value_type.inputs.is_empty() {
                pop_debug(&mut f, value_type.inputs.len() as i32);
            }
            if let CallFunc(function_idx) = operation_type {
                f.instruction(&Call(function_idx + num_instr_imports));
            } else if matches!(operation_type, EndFunc) {
                // Don't need to pop on returns
                pop_debug(&mut f, 0);
                f.instruction(&instruction);
            } else {
                // Instrumentation that needs to occur before execution
                Self::instrument_memory_ops(&mut f, &operation_type);
                f.instruction(&instruction);
                // Instrumentation that needs to occur after execution
                Self::instrument_local_ops(&mut f, &operation_type);
                Self::instrument_global_ops(&mut f, &operation_type);
                Self::instrument_push_ops(&mut f, &operation_type);
            }
            if !matches!(
                codillon_instruction.op.op,
                wasmparser::Operator::End
                    | wasmparser::Operator::Return
                    | wasmparser::Operator::Unreachable
            ) {
                // Step after each instruction evaluation
                f.instruction(&I32Const(line_idx));
                f.instruction(&Call(InstrImports::Step as u32));
                f.instruction(&I32Eqz);
                f.instruction(&If(wasm_encoder::BlockType::Empty));
                // Trap when run out of steps
                f.instruction(&Unreachable);
                f.instruction(&End);
            }
        }
        codes.function(&f);
        Ok(())
    }

    fn record_params(&self, func_idx: usize, f: &mut wasm_encoder::Function) {
        for (param_idx, param) in self.get_func_type(func_idx).params().iter().enumerate() {
            f.instruction(&I32Const(param_idx as i32));
            f.instruction(&LocalGet(param_idx as u32));
            match param {
                ValType::I32 => {
                    f.instruction(&Call(InstrImports::SetLocalI32 as u32));
                }
                ValType::F32 => {
                    f.instruction(&Call(InstrImports::SetLocalF32 as u32));
                }
                ValType::I64 => {
                    f.instruction(&Call(InstrImports::SetLocalI64 as u32));
                }
                ValType::F64 => {
                    f.instruction(&Call(InstrImports::SetLocalF64 as u32));
                }
                _ => {}
            }
        }
    }

    fn instrument_memory_ops(
        f: &mut wasm_encoder::Function,
        operation_type: &InstrumentationFuncs,
    ) {
        match operation_type {
            SetMemoryI32 => {
                f.instruction(&Call(InstrImports::SetMemoryI32 as u32));
            }
            SetMemoryF32 => {
                f.instruction(&Call(InstrImports::SetMemoryF32 as u32));
            }
            SetMemoryI64 => {
                f.instruction(&Call(InstrImports::SetMemoryI64 as u32));
            }
            SetMemoryF64 => {
                f.instruction(&Call(InstrImports::SetMemoryF64 as u32));
            }
            _ => {}
        }
    }
    fn instrument_local_ops(f: &mut wasm_encoder::Function, operation_type: &InstrumentationFuncs) {
        match *operation_type {
            SetLocalI32(local_index) => {
                f.instruction(&I32Const(local_index as i32));
                f.instruction(&LocalGet(local_index));
                f.instruction(&Call(InstrImports::SetLocalI32 as u32));
            }
            SetLocalF32(local_index) => {
                f.instruction(&I32Const(local_index as i32));
                f.instruction(&LocalGet(local_index));
                f.instruction(&Call(InstrImports::SetLocalF32 as u32));
            }
            SetLocalI64(local_index) => {
                f.instruction(&I32Const(local_index as i32));
                f.instruction(&LocalGet(local_index));
                f.instruction(&Call(InstrImports::SetLocalI64 as u32));
            }
            SetLocalF64(local_index) => {
                f.instruction(&I32Const(local_index as i32));
                f.instruction(&LocalGet(local_index));
                f.instruction(&Call(InstrImports::SetLocalF64 as u32));
            }
            _ => {}
        }
    }
    fn instrument_global_ops(
        f: &mut wasm_encoder::Function,
        operation_type: &InstrumentationFuncs,
    ) {
        match *operation_type {
            SetGlobalI32(global_index) => {
                f.instruction(&I32Const(global_index as i32));
                f.instruction(&GlobalGet(global_index));
                f.instruction(&Call(InstrImports::SetGlobalI32 as u32));
            }
            SetGlobalF32(global_index) => {
                f.instruction(&I32Const(global_index as i32));
                f.instruction(&GlobalGet(global_index));
                f.instruction(&Call(InstrImports::SetGlobalF32 as u32));
            }
            SetGlobalI64(global_index) => {
                f.instruction(&I32Const(global_index as i32));
                f.instruction(&GlobalGet(global_index));
                f.instruction(&Call(InstrImports::SetGlobalI64 as u32));
            }
            SetGlobalF64(global_index) => {
                f.instruction(&I32Const(global_index as i32));
                f.instruction(&GlobalGet(global_index));
                f.instruction(&Call(InstrImports::SetGlobalF64 as u32));
            }
            _ => {}
        }
    }
    fn instrument_push_ops(f: &mut wasm_encoder::Function, operation_type: &InstrumentationFuncs) {
        match operation_type {
            PushI32 => {
                f.instruction(&Call(InstrImports::PushI32 as u32));
            }
            PushF32 => {
                f.instruction(&Call(InstrImports::PushF32 as u32));
            }
            PushI64 => {
                f.instruction(&Call(InstrImports::PushI64 as u32));
            }
            PushF64 => {
                f.instruction(&Call(InstrImports::PushF64 as u32));
            }
            _ => {}
        }
    }

    pub fn get_func_type(&self, func_idx: usize) -> &wasmparser::FuncType {
        &self.types[self.functions[func_idx].type_idx]
    }
}

fn parser_to_encoder(value: &wasmparser::ValType) -> EncoderValType {
    match value {
        ValType::I32 => EncoderValType::I32,
        ValType::I64 => EncoderValType::I64,
        ValType::F32 => EncoderValType::F32,
        ValType::F64 => EncoderValType::F64,
        ValType::V128 => EncoderValType::V128,
        // Reference types not yet supported
        _ => panic!("unsupported valtype"),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct InputType {
    pub instr_type: ValType,
    pub origin: (usize, usize), // line index + push# within the line
}

#[derive(Debug, PartialEq, Eq)]
pub struct CodillonType {
    pub inputs: Vec<InputType>,
    pub outputs: Vec<ValType>,
    pub input_arity: Option<u8>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypedFunction {
    pub types: Vec<CodillonType>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypesTable {
    pub functions: Vec<TypedFunction>,
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
