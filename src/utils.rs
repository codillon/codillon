use anyhow::{Context, Result, bail};
use indexmap::IndexMap;
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
    CallFunc(u32),
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
    RecordI32,
    RecordF32,
    RecordI64,
    RecordF64,
}
impl InstrImports {
    const TYPE_INDICES: &'static [(&'static str, u32)] = &[
        ("step", 1),
        ("pop_i", 0),
        ("set_local_i32", 2),
        ("set_local_f32", 7),
        ("set_local_i64", 8),
        ("set_local_f64", 9),
        ("set_global_i32", 2),
        ("set_global_f32", 7),
        ("set_global_i64", 8),
        ("set_global_f64", 9),
        ("set_memory_i32", 3),
        ("set_memory_f32", 4),
        ("set_memory_i64", 5),
        ("set_memory_f64", 6),
        ("record_i32", 2),
        ("record_f32", 7),
        ("record_i64", 8),
        ("record_f64", 9),
    ];
    const FUNC_SIGS: &'static [(&'static [EncoderValType], &'static [EncoderValType])] = &[
        // 0: (i32) -> ()
        (&[EncoderValType::I32], &[]),
        // 1: (i32) -> (i32)
        (&[EncoderValType::I32], &[EncoderValType::I32]),
        // 2: (i32, i32) -> ()
        (&[EncoderValType::I32, EncoderValType::I32], &[]),
        // 3: (i32, i32) -> (i32, i32)
        (
            &[EncoderValType::I32, EncoderValType::I32],
            &[EncoderValType::I32, EncoderValType::I32],
        ),
        // 4: (i32, f32) -> (i32, f32)
        (
            &[EncoderValType::I32, EncoderValType::F32],
            &[EncoderValType::I32, EncoderValType::F32],
        ),
        // 5: (i32, i64) -> (i32, i64)
        (
            &[EncoderValType::I32, EncoderValType::I64],
            &[EncoderValType::I32, EncoderValType::I64],
        ),
        // 6: (i32, f64) -> (i32, f64)
        (
            &[EncoderValType::I32, EncoderValType::F64],
            &[EncoderValType::I32, EncoderValType::F64],
        ),
        // 7: (i32, f32) -> ()
        (&[EncoderValType::I32, EncoderValType::F32], &[]),
        // 8: (i32, i64) -> ()
        (&[EncoderValType::I32, EncoderValType::I64], &[]),
        // 9: (i32, f64) -> ()
        (&[EncoderValType::I32, EncoderValType::F64], &[]),
    ];
}

struct HelperFunc {
    name: &'static str,
    params: &'static [wasmparser::ValType],
    results: &'static [wasmparser::ValType],
    reason: &'static str,
}
const HELPER_IMPORTS: &[(&str, &[HelperFunc])] = &[(
    "helpers",
    &[HelperFunc {
        name: "draw_point",
        params: &[ValType::F64, ValType::F64],
        results: &[],
        reason: "expected type (f64, f64) -> ()",
    }],
)];

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
    pub imports: wasm_encoder::ImportSection,
    pub memory: Vec<wasmparser::MemoryType>,
    pub globals: Vec<wasmparser::Global<'a>>,
    pub functions: Vec<ValidFunction<'a>>,
    pub num_func_imports: u32,
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
        import_lines: &[usize],
    ) -> Result<ValidModule<'a>> {
        let parser = wasmparser::Parser::new(0);
        let mut validator = Validator::new();
        let mut allocs = wasmparser::FuncValidatorAllocations::default();
        let (imports, num_func_imports) = self.instr_imports(import_lines, editor);

        let mut ret = ValidModule {
            types: self.types,
            imports,
            memory: self.memory,
            globals: self.globals,
            functions: Vec::with_capacity(self.functions.len()),
            num_func_imports,
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

    fn check_import(
        import_module: &str,
        import_name: &str,
        ty: &wasmparser::FuncType,
    ) -> Option<String> {
        // Check if module name exists
        for (module, components) in HELPER_IMPORTS {
            if *module == import_module {
                for HelperFunc {
                    name,
                    params,
                    results,
                    reason,
                } in *components
                {
                    // Check if component name exists
                    if *name == import_name {
                        // Check if function type matches
                        return if ty.params() == *params && ty.results() == *results {
                            None
                        } else {
                            Some(reason.to_string())
                        };
                    }
                }
                return Some(format!(
                    "component {import_name} not found in module {module}"
                ));
            }
        }
        Some(format!("module {import_module} not found"))
    }

    fn instr_imports(
        &self,
        import_lines: &[usize],
        editor: &mut impl LineInfosMut,
    ) -> (wasm_encoder::ImportSection, u32) {
        use wasm_encoder::EntityType;
        use wasmparser::*;
        use web_sys::console::log_1;
        // Encode the instrumentation functions as imports.
        let mut imports = wasm_encoder::ImportSection::new();
        for (name, type_idx) in InstrImports::TYPE_INDICES.iter() {
            imports.import("codillon_debug", name, EntityType::Function(*type_idx));
        }
        let mut num_func_imports = 0;
        for (import_idx, Import { module, name, ty }) in self.imports.iter().enumerate() {
            match *ty {
                TypeRef::Func(type_idx) | TypeRef::FuncExact(type_idx) => {
                    let reason = Self::check_import(module, name, &self.types[type_idx as usize]);
                    editor.set_invalid(import_lines[import_idx], reason);
                    imports.import(
                        module,
                        name,
                        EntityType::Function(type_idx + InstrImports::FUNC_SIGS.len() as u32),
                    );
                    num_func_imports += 1;
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
        (imports, num_func_imports)
    }
}

#[derive(Default)]
struct SimulatedStack {
    idx_stack: Vec<(usize, usize)>,
}

// Opcodes that make the rest of the frame unreachable and pop all accessible operands.
// (Unfortunate that we have to hardcode this.)
fn is_unreachable_op(op: &Operator<'_>) -> bool {
    matches!(
        op,
        Operator::Return
            | Operator::Unreachable
            | Operator::Throw { .. }
            | Operator::ThrowRef
            | Operator::Br { .. }
            | Operator::BrTable { .. }
            | Operator::Rethrow { .. }
    )
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

        let pop_count = if is_unreachable_op(op) {
            accessible_operands
        } else {
            pop_count
        };

        let inputs = (0..pop_count)
            .map(|i| {
                if untyped || (pop_count - i - 1) >= accessible_operands {
                    None
                } else {
                    Some(InputType {
                        instr_type: validator
                            .get_operand_type(pop_count - i - 1)
                            .flatten()
                            .unwrap(),
                        origin: self.idx_stack[pre_instr_height + i - pop_count],
                    })
                }
            })
            .collect::<Vec<_>>();

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

        Ok(CodillonType { inputs, outputs })
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
            LocalSet { local_index } | LocalTee { local_index } => {
                match op_type.inputs.first().expect("local op type") {
                    Some(ty) => match ty {
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
                    },
                    None => panic!("classify run on unreachable op"),
                }
            }
            GlobalSet { global_index } => match op_type.inputs.first().expect("global op type") {
                Some(ty) => match ty {
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
                None => panic!("classify run on unreachable op"),
            },
            I32Store { .. } | I32Store8 { .. } | I32Store16 { .. } => SetMemoryI32,
            F32Store { .. } => SetMemoryF32,
            I64Store { .. } | I64Store8 { .. } | I64Store16 { .. } => SetMemoryI64,
            F64Store { .. } => SetMemoryF64,
            _ => Other,
        }
    }
    fn build_type_section(&self) -> TypeSection {
        let mut type_section = TypeSection::new();
        // Instrumentation function types
        for (params_slice, results_slice) in InstrImports::FUNC_SIGS.iter() {
            type_section
                .ty()
                .function(params_slice.to_vec(), results_slice.to_vec());
        }
        // User-defined types
        for ty in &self.types {
            type_section.ty().function(
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
        type_section
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
        // Maps ouputs (list of ValTypes) to dynamic_func_idx
        let mut types_map: IndexMap<Vec<ValType>, usize> =
            IndexMap::with_capacity(self.functions.len());
        let mut type_section = self.build_type_section();
        let mut dynamic_type_idx = (InstrImports::FUNC_SIGS.len() + self.types.len()) as u32;
        let mut function_section = FunctionSection::new();
        for func in &self.functions {
            function_section.function((func.type_idx + InstrImports::FUNC_SIGS.len()) as u32);
        }
        let mut dynamic_func_idx = InstrImports::TYPE_INDICES.len()
            + self.num_func_imports as usize
            + self.functions.len();
        for func in &types.functions {
            for CodillonType { outputs, .. } in &func.types {
                if !outputs.is_empty() && !types_map.contains_key(outputs) {
                    let mut outputs_params =
                        outputs.iter().map(parser_to_encoder).collect::<Vec<_>>();
                    outputs_params.append(&mut vec![EncoderValType::I32; outputs.len()]);
                    type_section.ty().function(
                        outputs_params,
                        outputs.iter().map(parser_to_encoder).collect::<Vec<_>>(),
                    );
                    function_section.function(dynamic_type_idx);
                    types_map.insert(outputs.clone(), dynamic_func_idx);
                    dynamic_type_idx += 1;
                    dynamic_func_idx += 1;
                }
            }
        }

        // Encode the export section.
        let mut exports = ExportSection::new();
        let num_instr_imports = InstrImports::TYPE_INDICES.len() as u32;
        if !self.functions.is_empty() {
            exports.export(
                "main",
                wasm_encoder::ExportKind::Func,
                num_instr_imports + self.num_func_imports,
            );
        }
        module.section(&type_section);
        module.section(&self.imports);
        module.section(&function_section);
        module.section(&self.build_memory());
        module.section(&self.build_globals());
        module.section(&exports);
        // Encode the code section.
        let mut codes = CodeSection::new();
        for func_idx in 0..self.functions.len() {
            self.build_function(func_idx, &mut codes, types, &types_map)?;
        }
        for outputs in types_map.keys() {
            self.dynamically_generate_function(outputs, &mut codes)?;
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
        types_map: &IndexMap<Vec<ValType>, usize>,
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
        let pop_debug = |f: &mut wasm_encoder::Function, num_pop: i32| {
            f.instruction(&I32Const(num_pop));
            f.instruction(&Call(InstrImports::PopI as u32));
        };
        let record = |f: &mut wasm_encoder::Function, outputs: &Vec<ValType>| {
            for _ in 0..outputs.len() {
                f.instruction(&I32Const(0));
            }
            f.instruction(&Call(types_map[outputs] as u32));
        };
        self.record_params(func_idx, &mut f);
        let mut unreachable = false;
        for (i, codillon_instruction) in function.operators.iter().enumerate() {
            // Skip unreachable operators until end of frame.
            if codillon_instruction.op.op == Operator::End {
                // End of frame; need to include this operator and all after.
                unreachable = false;
            } else if unreachable {
                // Otherwise, skip operator.
                continue;
            }

            let line_idx = codillon_instruction.line_idx as i32;
            let instruction = RoundtripReencoder.instruction(codillon_instruction.op.op.clone())?;
            let value_type = &types.functions.get(func_idx).unwrap().types[i];
            let operation_type = Self::classify(&codillon_instruction.op.op, value_type);

            if !codillon_instruction.op.prepended.is_empty() {
                // Originally invalid operator that was "validized" for type-analysis purposes.
                // Traps at runtime.
                f.instruction(&Unreachable);
                unreachable = true;
            } else if !value_type.inputs.is_empty() {
                pop_debug(&mut f, value_type.inputs.len() as i32);
            }

            if codillon_instruction.op.op == Operator::End
                || codillon_instruction.op.op == Operator::Return
            {
                // Call dynamically generated type to record return values
                if !value_type.outputs.is_empty() {
                    record(&mut f, &value_type.outputs);
                }
                f.instruction(&instruction);
                continue;
            } else if unreachable {
                continue;
            }

            if let CallFunc(function_idx) = operation_type {
                f.instruction(&Call(function_idx + num_instr_imports));
            } else {
                // Instrumentation that needs to occur before execution
                Self::instrument_memory_ops(&mut f, &operation_type);
                f.instruction(&instruction);
                // Instrumentation that needs to occur after execution
                Self::instrument_local_ops(&mut f, &operation_type);
                Self::instrument_global_ops(&mut f, &operation_type);
                if let Other = operation_type
                    && !value_type.outputs.is_empty()
                {
                    record(&mut f, &value_type.outputs);
                }
            }

            if is_unreachable_op(&codillon_instruction.op.op) {
                // Operator that will render the rest of frame unreachable.
                unreachable = true;
                continue;
            }

            // Step after each instruction evaluation
            f.instruction(&I32Const(line_idx));
            f.instruction(&Call(InstrImports::Step as u32));
            f.instruction(&I32Eqz);
            f.instruction(&If(wasm_encoder::BlockType::Empty));
            // Trap when run out of steps
            f.instruction(&Unreachable);
            f.instruction(&End);
        }
        codes.function(&f);
        Ok(())
    }

    fn dynamically_generate_function(
        &self,
        outputs: &[ValType],
        codes: &mut CodeSection,
    ) -> Result<(), anyhow::Error> {
        let n = outputs.len();
        let mut f = wasm_encoder::Function::new(vec![]);
        for (output_idx, output) in outputs.iter().enumerate() {
            f.instruction(&LocalGet((n + output_idx) as u32));
            f.instruction(&LocalGet(output_idx as u32));
            match output {
                ValType::I32 => {
                    f.instruction(&Call(InstrImports::RecordI32 as u32));
                }
                ValType::F32 => {
                    f.instruction(&Call(InstrImports::RecordF32 as u32));
                }
                ValType::I64 => {
                    f.instruction(&Call(InstrImports::RecordI64 as u32));
                }
                ValType::F64 => {
                    f.instruction(&Call(InstrImports::RecordF64 as u32));
                }
                _ => {}
            }
        }
        for output_idx in 0..outputs.len() {
            f.instruction(&LocalGet(output_idx as u32));
        }
        f.instruction(&End);
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
    pub inputs: Vec<Option<InputType>>,
    pub outputs: Vec<ValType>,
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

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use wasm_encoder::ImportSection;
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
        type CodillonInstruction<'a> = Aligned<Operator<'a>>;

        fn ins(inputs: Vec<InputType>) -> CodillonType {
            CodillonType {
                inputs: inputs.into_iter().map(Some).collect(),
                outputs: Vec::new(),
            }
        }

        fn outs(outputs: Vec<ValType>) -> CodillonType {
            CodillonType {
                inputs: Vec::new(),
                outputs,
            }
        }

        fn inout(inputs: Vec<InputType>, outputs: Vec<ValType>) -> CodillonType {
            CodillonType {
                inputs: inputs.into_iter().map(Some).collect(),
                outputs,
            }
        }

        fn force_valid(raw: RawModule<'_>) -> ValidModule<'_> {
            let mut ret = ValidModule {
                types: vec![],
                imports: ImportSection::new(),
                memory: vec![],
                globals: vec![],
                functions: vec![],
                num_func_imports: 0,
            };

            for func in raw.functions {
                let mut valid = ValidFunction {
                    type_idx: 0,
                    locals: func.locals,
                    operators: vec![],
                };
                for op in func.operators {
                    valid.operators.push(op.into());
                }
                ret.functions.push(valid);
            }

            ret
        }

        //block instruction with params and results
        let output = TypesTable {
            functions: vec![TypedFunction {
                types: vec![
                    outs(vec![ValType::I32]),
                    outs(vec![ValType::I32]),
                    inout(
                        vec![
                            InputType {
                                instr_type: ValType::I32,
                                origin: (0, 0),
                            },
                            InputType {
                                instr_type: ValType::I32,
                                origin: (1, 0),
                            },
                        ],
                        vec![ValType::I32, ValType::I32],
                    ),
                    inout(
                        vec![
                            InputType {
                                instr_type: ValType::I32,
                                origin: (2, 0),
                            },
                            InputType {
                                instr_type: ValType::I32,
                                origin: (2, 1),
                            },
                        ],
                        vec![ValType::I32],
                    ),
                    inout(
                        vec![InputType {
                            instr_type: ValType::I32,
                            origin: (3, 0),
                        }],
                        vec![ValType::I32],
                    ),
                    ins(vec![InputType {
                        instr_type: ValType::I32,
                        origin: (4, 0),
                    }]),
                    ins(vec![]),
                ],
            }],
        };
        let lines =
            "i32.const 1\ni32.const 2\nblock (param i32 i32) (result i32)\ni32.add\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = RawModule {
            types: vec![],
            imports: vec![],
            memory: vec![],
            globals: vec![],
            functions: vec![RawFunction {
                type_idx: 0,
                locals: vec![],
                operators: vec![
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
                    CodillonInstruction {
                        op: Operator::End,
                        line_idx: 6,
                    },
                ],
            }],
        };
        assert_eq!(
            force_valid(instruction_table).to_types_table(&wasm_bin)?,
            output
        );

        //if else with params and results
        let output = TypesTable {
            functions: vec![TypedFunction {
                types: vec![
                    outs(vec![ValType::I32]),
                    ins(vec![InputType {
                        instr_type: ValType::I32,
                        origin: (0, 0),
                    }]),
                    outs(vec![ValType::I32]),
                    ins(vec![InputType {
                        instr_type: ValType::I32,
                        origin: (2, 0),
                    }]),
                    outs(vec![ValType::I32]),
                    inout(
                        vec![InputType {
                            instr_type: ValType::I32,
                            origin: (4, 0),
                        }],
                        vec![ValType::I32],
                    ),
                    ins(vec![InputType {
                        instr_type: ValType::I32,
                        origin: (5, 0),
                    }]),
                    ins(vec![]),
                ],
            }],
        };
        let lines = "i32.const 1\nif (result i32)\ni32.const 1\nelse\ni32.const 2\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = RawModule {
            types: vec![],
            imports: vec![],
            memory: vec![],
            globals: vec![],
            functions: vec![RawFunction {
                type_idx: 0,
                locals: vec![],
                operators: vec![
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
                    CodillonInstruction {
                        op: Operator::End,
                        line_idx: 7,
                    },
                ],
            }],
        };
        assert_eq!(
            force_valid(instruction_table).to_types_table(&wasm_bin)?,
            output
        );

        //loop with param and return
        let output = TypesTable {
            functions: vec![TypedFunction {
                types: vec![
                    outs(vec![ValType::I32]),
                    inout(
                        vec![InputType {
                            instr_type: ValType::I32,
                            origin: (0, 0),
                        }],
                        vec![ValType::I32],
                    ),
                    outs(vec![ValType::I32]),
                    inout(
                        vec![
                            InputType {
                                instr_type: ValType::I32,
                                origin: (1, 0),
                            },
                            InputType {
                                instr_type: ValType::I32,
                                origin: (2, 0),
                            },
                        ],
                        vec![ValType::I32],
                    ),
                    ins(vec![InputType {
                        instr_type: ValType::I32,
                        origin: (3, 0),
                    }]),
                    outs(vec![ValType::I32]),
                    inout(
                        vec![InputType {
                            instr_type: ValType::I32,
                            origin: (5, 0),
                        }],
                        vec![ValType::I32],
                    ),
                    ins(vec![InputType {
                        instr_type: ValType::I32,
                        origin: (6, 0),
                    }]),
                    ins(vec![]),
                ],
            }],
        };
        let lines = "i32.const 10\nloop (param i32) (result i32)\ni32.const 1\ni32.sub\nbr_if 1\ni32.const 2\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = RawModule {
            types: vec![],
            imports: vec![],
            memory: vec![],
            globals: vec![],
            functions: vec![RawFunction {
                type_idx: 0,
                locals: vec![],
                operators: vec![
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
                    CodillonInstruction {
                        op: Operator::End,
                        line_idx: 8,
                    },
                ],
            }],
        };
        assert_eq!(
            force_valid(instruction_table).to_types_table(&wasm_bin)?,
            output
        );

        //nested block and if
        let output = TypesTable {
            functions: vec![TypedFunction {
                types: vec![
                    outs(vec![ValType::I32]),
                    inout(
                        vec![InputType {
                            instr_type: ValType::I32,
                            origin: (0, 0),
                        }],
                        vec![ValType::I32],
                    ),
                    ins(vec![InputType {
                        instr_type: ValType::I32,
                        origin: (1, 0),
                    }]),
                    outs(vec![ValType::I32]),
                    ins(vec![InputType {
                        instr_type: ValType::I32,
                        origin: (3, 0),
                    }]),
                    outs(vec![ValType::I32]),
                    inout(
                        vec![InputType {
                            instr_type: ValType::I32,
                            origin: (5, 0),
                        }],
                        vec![ValType::I32],
                    ),
                    inout(
                        vec![InputType {
                            instr_type: ValType::I32,
                            origin: (6, 0),
                        }],
                        vec![ValType::I32],
                    ),
                    ins(vec![InputType {
                        instr_type: ValType::I32,
                        origin: (7, 0),
                    }]),
                    ins(vec![]),
                ],
            }],
        };
        let lines = "i32.const 10\nblock (param i32) (result i32)\nif (result i32)\ni32.const 1\nelse\ni32.const 2\nend\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = RawModule {
            types: vec![],
            imports: vec![],
            memory: vec![],
            globals: vec![],
            functions: vec![RawFunction {
                type_idx: 0,
                locals: vec![],
                operators: vec![
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
                    CodillonInstruction {
                        op: Operator::End,
                        line_idx: 9,
                    },
                ],
            }],
        };
        assert_eq!(
            force_valid(instruction_table).to_types_table(&wasm_bin)?,
            output
        );

        //empty block
        let output = TypesTable {
            functions: vec![TypedFunction {
                types: vec![ins(vec![]), ins(vec![]), ins(vec![])],
            }],
        };
        let lines = "block\nend";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = RawModule {
            types: vec![],
            imports: vec![],
            memory: vec![],
            globals: vec![],
            functions: vec![RawFunction {
                type_idx: 0,
                locals: vec![],
                operators: vec![
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
                    CodillonInstruction {
                        op: Operator::End,
                        line_idx: 2,
                    },
                ],
            }],
        };
        assert_eq!(
            force_valid(instruction_table).to_types_table(&wasm_bin)?,
            output
        );

        Ok(())
    }
    #[test]
    fn test_parse_memory_section() -> Result<()> {
        let mut mem_exists = false;
        let test_mem = wasmparser::MemoryType {
            memory64: true,
            shared: false,
            initial: 3,
            maximum: Some(5),
            page_size_log2: Some(0),
        };
        let valid_module = ValidModule {
            types: vec![],
            imports: ImportSection::new(),
            memory: vec![test_mem],
            globals: vec![],
            functions: vec![],
            num_func_imports: 0,
        };
        let wasm_bin = valid_module.build_executable_binary(&TypesTable { functions: vec![] })?;
        let mem_payload = wasmparser::Parser::new(0)
            .parse_all(&wasm_bin)
            .nth(4)
            .expect("failed to get memory payload");
        if let wasmparser::Payload::MemorySection(reader) = mem_payload? {
            assert_eq!(
                reader.into_iter().next().expect("failed to get memory")?,
                test_mem
            );
            mem_exists = true;
        }
        assert!(mem_exists);
        Ok(())
    }

    use crate::line::{Activity, LineInfo};
    use crate::symbolic::parse_line_symbols;
    use crate::syntax::{
        SyntheticWasm, find_function_ranges, find_import_lines, fix_syntax, parse_line,
    };

    #[derive(Default)]
    struct FakeTextLine {
        instr_text: String,
        info: LineInfo,
    }

    #[derive(Default)]
    struct FakeTextBuffer {
        lines: Vec<FakeTextLine>,
    }

    impl FakeTextLine {
        fn new(s: &str) -> Self {
            let kind = parse_line(s);
            let symbols = parse_line_symbols(s, &kind);
            Self {
                instr_text: String::from(s),
                info: LineInfo {
                    kind,
                    symbols,
                    ..Default::default()
                },
            }
        }
    }

    impl FakeTextBuffer {
        fn push_line(&mut self, string: &str) {
            self.lines.push(FakeTextLine::new(string))
        }
    }

    impl LineInfos for FakeTextBuffer {
        fn is_empty(&self) -> bool {
            self.lines.is_empty()
        }

        fn len(&self) -> usize {
            self.lines.len()
        }

        #[allow(refining_impl_trait)]
        fn info(&self, index: usize) -> &LineInfo {
            &self.lines[index].info
        }
    }

    impl LineInfosMut for FakeTextBuffer {
        fn set_active_status(&mut self, index: usize, new_val: Activity) {
            self.lines[index].info.active = new_val
        }

        fn set_synthetic_before(&mut self, index: usize, synth: SyntheticWasm) {
            self.lines[index].info.synthetic_before = synth
        }

        fn push(&mut self) {
            self.push_line("")
        }

        fn set_invalid(&mut self, index: usize, reason: Option<String>) {
            self.lines[index].info.invalid = reason
        }
    }

    fn test_editor_flow(editor: &mut FakeTextBuffer) -> Result<String> {
        fix_syntax(editor);

        let function_ranges = find_function_ranges(editor);

        // build text of module
        let mut well_formed_str = String::new();
        for line in editor.lines.iter() {
            for str_idx in 0..line.info.num_well_formed_strs() {
                well_formed_str.push_str(line.info.well_formed_str(str_idx, &line.instr_text));
                if str_idx + 1 != line.info.num_well_formed_strs() {
                    well_formed_str.push(' ');
                }
            }
            well_formed_str.push('\n');
        }

        let wasm_bin = str_to_binary(well_formed_str)?;

        let raw_module =
            RawModule::new(editor, &wasm_bin, &function_ranges).context("RawModule::new")?;
        let validized = raw_module
            .fix_validity(&wasm_bin, editor, &find_import_lines(editor))
            .context("fix_validity")?;
        let types = validized
            .to_types_table(&wasm_bin)
            .context("to_types_table")?;
        let runnable = validized
            .build_executable_binary(&types)
            .context("build_executable_binary")?;
        wasmparser::validate(&runnable).context("validate")?;

        wasmprinter::print_bytes(&runnable).context("print_bytes")
    }

    const EXPECTED_FIELDS: &str = r#"  (type (;0;) (func (param i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (param i32 i32)))
  (type (;3;) (func (param i32 i32) (result i32 i32)))
  (type (;4;) (func (param i32 f32) (result i32 f32)))
  (type (;5;) (func (param i32 i64) (result i32 i64)))
  (type (;6;) (func (param i32 f64) (result i32 f64)))
  (type (;7;) (func (param i32 f32)))
  (type (;8;) (func (param i32 i64)))
  (type (;9;) (func (param i32 f64)))
  (type (;10;) (func))
  (import "codillon_debug" "step" (func (;0;) (type 1)))
  (import "codillon_debug" "pop_i" (func (;1;) (type 0)))
  (import "codillon_debug" "set_local_i32" (func (;2;) (type 2)))
  (import "codillon_debug" "set_local_f32" (func (;3;) (type 7)))
  (import "codillon_debug" "set_local_i64" (func (;4;) (type 8)))
  (import "codillon_debug" "set_local_f64" (func (;5;) (type 9)))
  (import "codillon_debug" "set_global_i32" (func (;6;) (type 2)))
  (import "codillon_debug" "set_global_f32" (func (;7;) (type 7)))
  (import "codillon_debug" "set_global_i64" (func (;8;) (type 8)))
  (import "codillon_debug" "set_global_f64" (func (;9;) (type 9)))
  (import "codillon_debug" "set_memory_i32" (func (;10;) (type 3)))
  (import "codillon_debug" "set_memory_f32" (func (;11;) (type 4)))
  (import "codillon_debug" "set_memory_i64" (func (;12;) (type 5)))
  (import "codillon_debug" "set_memory_f64" (func (;13;) (type 6)))
  (import "codillon_debug" "record_i32" (func (;14;) (type 2)))
  (import "codillon_debug" "record_f32" (func (;15;) (type 7)))
  (import "codillon_debug" "record_i64" (func (;16;) (type 8)))
  (import "codillon_debug" "record_f64" (func (;17;) (type 9)))
  (export "main" (func 18))
"#;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_fixes() -> Result<()> {
        // empty function
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func)");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + r#"  (func (;18;) (type 10))
)
"#;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // syntax error (synthesizes closing paren)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + r#"  (func (;18;) (type 10))
)
"#;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // syntax error (synthesizes closing "func)")
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + r#"  (func (;18;) (type 10))
)
"#;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // syntax error (missing (func) field but has instructions)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("i64.const 17");
            editor.push_line("drop");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + r#"  (func (;18;) (type 10)
    i64.const 17
    i32.const 0
    call 19
    i32.const 0
    call 0
    i32.eqz
    if ;; label = @1
      unreachable
    end
    i32.const 1
    call 1
    drop
    i32.const 1
    call 0
    i32.eqz
    if ;; label = @1
      unreachable
    end
  )
  (func (;19;) (type 11) (param i64 i32) (result i64)
    local.get 1
    local.get 0
    call 16
    local.get 0
  )
)
"#;
            let expected = expected.replace(
                "  (type (;10;) (func))\n",
                "  (type (;10;) (func))\n  (type (;11;) (func (param i64 i32) (result i64)))\n",
            );
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // well-formed block
        const JUST_BLOCK_END: &str = r#"    block ;; label = @1
      i32.const 1
      call 0
      i32.eqz
      if ;; label = @2
        unreachable
      end
    end
  )
)
"#;
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("block");
            editor.push_line("end");
            editor.push_line(")");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + "  (func (;18;) (type 10)\n"
                + JUST_BLOCK_END;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // syntax error (missing `end`) -- should be same as the well-formed block
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("block");
            editor.push_line(")");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + "  (func (;18;) (type 10)\n"
                + JUST_BLOCK_END;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // validation error (missing operand)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("i32.const 137");
            editor.push_line("i32.add");
            editor.push_line("drop");
            editor.push_line(")");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + r#"  (func (;18;) (type 10)
    i32.const 137
    i32.const 0
    call 19
    i32.const 1
    call 0
    i32.eqz
    if ;; label = @1
      unreachable
    end
    unreachable
  )
  (func (;19;) (type 11) (param i32 i32) (result i32)
    local.get 1
    local.get 0
    call 14
    local.get 0
  )
)
"#;
            let expected = expected.replace(
                "  (type (;10;) (func))\n",
                "  (type (;10;) (func))\n  (type (;11;) (func (param i32 i32) (result i32)))\n",
            );
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // syntax error (func has inline import but also instructions -- will deactivate the instructions)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("(import \"hello\" \"goodbye\")");
            editor.push_line("i32.const 7");
            editor.push_line("i32.const 8");
            editor.push_line("i32.const 9");
            editor.push_line(")");
            editor.push_line("(func)");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + r#"  (import "hello" "goodbye" (func (;18;) (type 10)))
  (export "main" (func 19))
  (func (;19;) (type 10))
)
"#;
            let expected = expected.replace("  (export \"main\" (func 18))\n", "");
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // syntax error (consumed symbolic reference no defined - module-level, local, label)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("block $b");
            editor.push_line("local.get $x"); // invalid local
            editor.push_line("call $b"); // invalid module-level
            editor.push_line("br $l"); // invalid label
            editor.push_line("end");
            editor.push_line(")");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + "  (func (;18;) (type 10)\n"
                + JUST_BLOCK_END;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // syntax error (label on "end"/"else" doesn't match that introduced by the frame)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("block $y");
            editor.push_line("loop $x");
            editor.push_line("end $y"); // mismatched frame: dropped
            editor.push_line("end $x");
            editor.push_line("end $y");
            editor.push_line(")");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + r#"  (func (;18;) (type 10)
    block ;; label = @1
      i32.const 1
      call 0
      i32.eqz
      if ;; label = @2
        unreachable
      end
      loop ;; label = @2
        i32.const 2
        call 0
        i32.eqz
        if ;; label = @3
          unreachable
        end
      end
    end
  )
)
"#;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // validation error (impossible branch -- will be disabled)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("br 144");
            editor.push_line(")");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + r#"  (func (;18;) (type 10)
    nop
    i32.const 1
    call 0
    i32.eqz
    if ;; label = @1
      unreachable
    end
  )
)
"#;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // validation error (bad memory reference -- will be disabled)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(memory 0)");
            editor.push_line("(func");
            editor.push_line("i32.load 144");
            editor.push_line("drop");
            editor.push_line(")");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + r#"  (func (;18;) (type 10)
    nop
    i32.const 2
    call 0
    i32.eqz
    if ;; label = @1
      unreachable
    end
    unreachable
  )
)
"#;
            let expected = expected.replace(
                "  (export \"main\" (func 18))",
                "  (memory (;0;) 0)\n  (export \"main\" (func 18))",
            );

            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // branch instruction renders rest of frame unreachable and pops all accessible operands
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("loop");
            editor.push_line("i32.const 5");
            editor.push_line("br 0");
            editor.push_line("end");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + r#"  (func (;18;) (type 10)
    loop ;; label = @1
      i32.const 0
      call 0
      i32.eqz
      if ;; label = @2
        unreachable
      end
      i32.const 5
      i32.const 0
      call 19
      i32.const 1
      call 0
      i32.eqz
      if ;; label = @2
        unreachable
      end
      i32.const 1
      call 1
      br 0 (;@1;)
    end
  )
  (func (;19;) (type 11) (param i32 i32) (result i32)
    local.get 1
    local.get 0
    call 14
    local.get 0
  )
)
"#;
            let expected = expected.replace(
                "  (type (;10;) (func))\n",
                "  (type (;10;) (func))\n  (type (;11;) (func (param i32 i32) (result i32)))\n",
            );
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // validation error
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("i32.const 4");
            editor.push_line(")");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + r#"  (func (;18;) (type 10)
    i32.const 4
    i32.const 0
    call 19
    i32.const 1
    call 0
    i32.eqz
    if ;; label = @1
      unreachable
    end
    unreachable
  )
  (func (;19;) (type 11) (param i32 i32) (result i32)
    local.get 1
    local.get 0
    call 14
    local.get 0
  )
)
"#;
            let expected = expected.replace(
                "  (type (;10;) (func))\n",
                "  (type (;10;) (func))\n  (type (;11;) (func (param i32 i32) (result i32)))\n",
            );
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("call $test");
            editor.push_line("i32.add"); // missing drop
            editor.push_line(")");
            editor.push_line("(func $test (result i32 i32)");
            editor.push_line("i32.const 9");
            editor.push_line("i32.const 10");
            editor.push_line(")");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + r#"  (func (;18;) (type 10)
    call 19
    i32.const 1
    call 0
    i32.eqz
    if ;; label = @1
      unreachable
    end
    i32.const 2
    call 1
    i32.add
    i32.const 0
    call 21
    i32.const 2
    call 0
    i32.eqz
    if ;; label = @1
      unreachable
    end
    unreachable
  )
  (func (;19;) (type 11) (result i32 i32)
    i32.const 9
    i32.const 0
    call 21
    i32.const 5
    call 0
    i32.eqz
    if ;; label = @1
      unreachable
    end
    i32.const 10
    i32.const 0
    call 21
    i32.const 6
    call 0
    i32.eqz
    if ;; label = @1
      unreachable
    end
    i32.const 2
    call 1
    i32.const 0
    i32.const 0
    call 20
  )
  (func (;20;) (type 12) (param i32 i32 i32 i32) (result i32 i32)
    local.get 2
    local.get 0
    call 14
    local.get 3
    local.get 1
    call 14
    local.get 0
    local.get 1
  )
  (func (;21;) (type 13) (param i32 i32) (result i32)
    local.get 1
    local.get 0
    call 14
    local.get 0
  )
)
"#;
            let expected = expected.replace(
                "  (type (;10;) (func))\n",
                "  (type (;10;) (func))\n  (type (;11;) (func (result i32 i32)))\n  (type (;12;) (func (param i32 i32 i32 i32) (result i32 i32)))\n  (type (;13;) (func (param i32 i32) (result i32)))\n",
            );
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // local declaration mixed with other module parts
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("(param i32) (local i32)"); // not allowed
            editor.push_line("(local i32 i32 i32)"); // allowed
            editor.push_line("(local f64)"); // also allowed
            editor.push_line(")");
            let expected = String::from("(module\n")
                + EXPECTED_FIELDS
                + r#"  (func (;18;) (type 10)
    (local i32 i32 i32 f64)
  )
)
"#;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        Ok(())
    }

    #[test]
    fn test_import_validation() -> Result<()> {
        // Test case 1: module name not found
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(import \"not_helpers\" \"draw_point\" (func (param f64 f64)))");
            let _ = test_editor_flow(&mut editor)?;
            assert_eq!(
                editor.lines[0]
                    .info
                    .invalid
                    .as_ref()
                    .expect("nonexistent module name should be invalid"),
                "module not_helpers not found"
            );
        }

        // Test case 2: component name not found
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(import \"helpers\" \"not_draw_point\" (func (param f64 f64)))");
            let _ = test_editor_flow(&mut editor)?;
            assert_eq!(
                editor.lines[0]
                    .info
                    .invalid
                    .as_ref()
                    .expect("nonexistent component name should be invalid"),
                "component not_draw_point not found in module helpers"
            );
        }

        // Test case 3: wrong function type
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(import \"helpers\" \"draw_point\" (func (param i32)))");
            let _ = test_editor_flow(&mut editor)?;
            assert_eq!(
                editor.lines[0]
                    .info
                    .invalid
                    .as_ref()
                    .expect("wrong function type should be invalid"),
                "expected type (f64, f64) -> ()"
            );
        }

        // Test case 4: correct import
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(import \"helpers\" \"draw_point\" (func (param f64, f64)))");
            let _ = test_editor_flow(&mut editor)?;
            assert!(
                editor.lines[0].info.invalid.is_none(),
                "draw_point should be correctly imported"
            );
        }

        Ok(())
    }
}
