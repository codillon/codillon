use crate::syntax::{
    FrameInfo, FrameInfosMut, InstrKind, LineInfos, LineInfosMut, LineKind, ModulePart,
};
use EncoderInstruction::*;
use anyhow::{Result, bail};
use indexmap::IndexMap;
use wasm_encoder::{
    CodeSection, Instruction as EncoderInstruction, ValType as EncoderValType,
    reencode::{Reencode, RoundtripReencoder},
};
use wasm_tools::parse_binary_wasm;
use wasmparser::{
    FuncValidator, Global, GlobalType, HeapType, Operator, ValType, ValidPayload, Validator,
    WasmFeatures, WasmModuleResources,
};
use wast::{
    core::Module,
    parser::{self, ParseBuffer},
};

#[repr(u32)]
enum InstrImports {
    Step,
    RecordI32,
    RecordF32,
    RecordI64,
    RecordF64,
}
impl InstrImports {
    const TYPE_INDICES: &'static [(&'static str, u32)] = &[
        ("step", 0), // indices relative to end of user-provided type section
        ("record_i32", 1),
        ("record_f32", 2),
        ("record_i64", 3),
        ("record_f64", 4),
    ];
    const FUNC_SIGS: &'static [(&'static [EncoderValType], &'static [EncoderValType])] = &[
        // 0: i32 -> ()
        (&[EncoderValType::I32], &[]),
        // 1: (i32, i32) -> ()
        (&[EncoderValType::I32, EncoderValType::I32], &[]),
        // 2: (f32, i32) -> ()
        (&[EncoderValType::F32, EncoderValType::I32], &[]),
        // 3: (i64, i32) -> ()
        (&[EncoderValType::I64, EncoderValType::I32], &[]),
        // 4: (f64, i32) -> ()
        (&[EncoderValType::F64, EncoderValType::I32], &[]),
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
    &[
        HelperFunc {
            name: "draw_point",
            params: &[ValType::F64, ValType::F64],
            results: &[],
            reason: "expected type (f64, f64) -> ()",
        },
        HelperFunc {
            name: "clear_canvas",
            params: &[],
            results: &[],
            reason: "expected type () -> ()",
        },
        HelperFunc {
            name: "set_color",
            params: &[ValType::I32, ValType::I32, ValType::I32],
            results: &[],
            reason: "expected type (i32, i32, i32) -> ()",
        },
        HelperFunc {
            name: "set_extent",
            params: &[ValType::F64, ValType::F64, ValType::F64, ValType::F64],
            results: &[],
            reason: "expected type (f64, f64, f64, f64) -> ()",
        },
        HelperFunc {
            name: "set_radius",
            params: &[ValType::F64],
            results: &[],
            reason: "expected type (f64) -> ()",
        },
    ],
)];

#[derive(Debug)]
pub struct GeneralOperator<'a> {
    prepended: Vec<Operator<'a>>,
    op: Operator<'a>,
    untyped: bool,
}

#[derive(Debug)]
pub struct Aligned<T> {
    pub inner: T,
    pub line_idx: usize,
}

pub struct RawFunction<'a> {
    pub type_idx: u32,
    pub params: Vec<ValType>,
    pub locals: Vec<Aligned<ValType>>,
    pub operators: Vec<Aligned<Operator<'a>>>,
}

#[derive(Debug)]
pub struct ValidFunction<'a> {
    pub type_idx: u32,
    pub params: Vec<ValType>,
    pub locals: Vec<Aligned<ValType>>,
    pub operators: Vec<Aligned<GeneralOperator<'a>>>,
}

pub struct RawModule<'a> {
    pub types: Vec<wasmparser::FuncType>,
    pub imports: Vec<wasmparser::Import<'a>>,
    pub memory: Vec<wasmparser::MemoryType>,
    pub globals: Vec<Aligned<wasmparser::Global<'a>>>,
    pub functions: Vec<RawFunction<'a>>,
}

#[derive(Debug)]
pub struct ValidModule<'a> {
    pub types: Vec<wasmparser::FuncType>,
    pub imports: Vec<wasmparser::Import<'a>>,
    pub memory: Vec<wasmparser::MemoryType>,
    pub globals: Vec<Aligned<wasmparser::Global<'a>>>,
    pub functions: Vec<ValidFunction<'a>>,
}

impl<'a> From<Aligned<Operator<'a>>> for Aligned<GeneralOperator<'a>> {
    fn from(val: Aligned<Operator<'a>>) -> Self {
        Self {
            inner: GeneralOperator {
                prepended: vec![],
                op: val.inner,
                untyped: false,
            },
            line_idx: val.line_idx,
        }
    }
}

const DUMMY_OFFSET: usize = 1; // no need to track offsets, but validator has safety checks against 0

// current features (notable exceptions: SIMD, GC, function references, exceptions)
const CODILLON_WASM_FEATURES: WasmFeatures = WasmFeatures::WASM1
    .union(WasmFeatures::BULK_MEMORY)
    .union(WasmFeatures::REFERENCE_TYPES)
    .union(WasmFeatures::SIGN_EXTENSION)
    .union(WasmFeatures::SATURATING_FLOAT_TO_INT)
    .union(WasmFeatures::MULTI_VALUE)
    .union(WasmFeatures::CUSTOM_PAGE_SIZES)
    .union(WasmFeatures::EXTENDED_CONST)
    .union(WasmFeatures::MEMORY64)
    .union(WasmFeatures::MULTI_MEMORY)
    .union(WasmFeatures::RELAXED_SIMD)
    .union(WasmFeatures::TAIL_CALL)
    .union(WasmFeatures::THREADS)
    .union(WasmFeatures::WIDE_ARITHMETIC);

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
        let mut func_type_indices: Vec<u32> = Vec::new();
        let mut imports: Vec<Import> = Vec::new();
        let mut memory: Vec<MemoryType> = Vec::new();
        let mut globals: Vec<Aligned<Global>> = Vec::new();
        type FuncInfo<'a> = (Vec<Aligned<ValType>>, Vec<Aligned<Operator<'a>>>);
        let mut funcs: Vec<FuncInfo> = Vec::with_capacity(function_ranges.len());

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
                        func_type_indices.push(func_type_idx);
                    }
                }
                Payload::MemorySection(reader) => {
                    for mem in reader.into_iter_with_offsets().flatten() {
                        memory.push(mem.1);
                    }
                }
                Payload::GlobalSection(reader) => {
                    for global in reader.into_iter_with_offsets().flatten() {
                        globals.push(Aligned {
                            line_idx: 0,
                            inner: global.1,
                        });
                    }
                }
                Payload::CodeSectionEntry(body) => {
                    let mut locals = Vec::new();
                    let locals_reader = body.get_locals_reader()?;
                    for entry in locals_reader {
                        let (num, ty) = entry?;
                        for _ in 0..num {
                            locals.push(Aligned {
                                line_idx: 0,
                                inner: ty,
                            });
                        }
                    }
                    let mut ops = Vec::new();
                    for op in body.get_operators_reader()?.into_iter() {
                        ops.push(Aligned {
                            line_idx: 0,
                            inner: op?,
                        });
                    }
                    funcs.push((locals, ops));
                }
                _ => {}
            }
        }

        // align globals
        {
            let mut globals_iter = globals.iter_mut();
            for line_no in 0..editor.len() {
                if editor.info(line_no).is_active()
                    && let LineKind::Other(parts) = &editor.info(line_no).kind
                    && parts.first() == Some(&ModulePart::Global)
                {
                    debug_assert_eq!(parts.len(), 1);
                    globals_iter
                        .next()
                        .expect("not enough enough globals in module")
                        .line_idx = line_no;
                }
            }
            assert!(globals_iter.next().is_none(), "too many globals in module");
        }

        assert_eq!(
            function_ranges.len(),
            funcs.len(),
            "function count mismatch"
        );

        for (func_idx, ((mut locals, mut ops), (start_line, end_line))) in
            funcs.into_iter().zip(function_ranges).enumerate()
        {
            // align locals
            let mut locals_iter = locals.iter_mut();
            for line_no in *start_line..=*end_line {
                if editor.info(line_no).is_active()
                    && let LineKind::Other(parts) = &editor.info(line_no).kind
                    && let Some(ModulePart::Local(num)) = parts.first()
                {
                    debug_assert_eq!(parts.len(), 1);
                    for _ in 0..*num {
                        locals_iter
                            .next()
                            .expect("not enough locals in function")
                            .line_idx = line_no;
                    }
                }
            }
            assert!(locals_iter.next().is_none(), "too many locals in function");

            // align ops
            let mut ops_iter = ops.iter_mut();
            for line_no in *start_line..=*end_line {
                for _ in 0..editor.info(line_no).num_ops() {
                    ops_iter
                        .next()
                        .expect("not enough ops in function")
                        .line_idx = line_no;
                }
            }
            match ops_iter.next() {
                Some(Aligned {
                    line_idx,
                    inner: wasmparser::Operator::End,
                }) => {
                    *line_idx = *end_line;
                }
                Some(_) => {
                    bail!("too many ops in function");
                }
                None => {
                    bail!("not enough ops in function (missing end)");
                }
            }
            functions.push(RawFunction {
                type_idx: func_type_indices[func_idx],
                params: types[func_type_indices[func_idx] as usize]
                    .params()
                    .to_vec(),
                locals,
                operators: ops,
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
        let mut validator = Validator::new_with_features(CODILLON_WASM_FEATURES);
        let mut allocs = wasmparser::FuncValidatorAllocations::default();

        /* Disable unlinkable imports */
        let mut imports = Vec::new();
        for (import_idx, import @ wasmparser::Import { module, name, ty }) in
            self.imports.into_iter().enumerate()
        {
            match ty {
                wasmparser::TypeRef::Func(type_idx) => {
                    match Self::check_func_import(module, name, &self.types[type_idx as usize]) {
                        Some(reason) => editor.set_invalid(import_lines[import_idx], Some(reason)),
                        None => imports.push(import),
                    }
                }
                _ => {
                    editor.set_invalid(
                        import_lines[import_idx],
                        Some(String::from("unsupported import kind")),
                    );
                }
            };
        }

        let mut ret = ValidModule {
            types: self.types,
            imports,
            memory: self.memory,
            globals: self.globals,
            functions: Vec::with_capacity(self.functions.len()),
        };

        let mut raw_functions = self.functions.into_iter();

        /* "Validize" each function */
        for payload in parser.parse_all(wasm_bin) {
            if let ValidPayload::Func(func, body) = validator.payload(&payload?)? {
                let RawFunction {
                    type_idx,
                    params,
                    locals,
                    operators,
                } = raw_functions.next().expect("function count mismatch");

                #[cfg(debug_assertions)]
                Self::assert_bodies_match(&locals, &operators, &body)?;

                let mut func_validator = func.into_validator(allocs);

                for ty in &locals {
                    func_validator.define_locals(DUMMY_OFFSET, 1, ty.inner)?;
                }

                let mut valid_function = ValidFunction {
                    type_idx,
                    params,
                    locals,
                    operators: Vec::with_capacity(operators.len()),
                };

                for op in operators {
                    match func_validator.try_op(DUMMY_OFFSET, &op.inner) {
                        Ok(()) => valid_function.operators.push(op.into()),
                        Err(e) => {
                            editor.set_invalid(op.line_idx, Some(e.message().to_string()));
                            /* make a valid version of this operator */
                            let validized = Self::make_valid(func_validator.clone(), op)?;
                            for prepended_op in &validized.inner.prepended {
                                func_validator
                                    .op(DUMMY_OFFSET, prepended_op)
                                    .expect("prepended op is valid");
                            }
                            func_validator
                                .op(DUMMY_OFFSET, &validized.inner.op)
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
                inner: GeneralOperator {
                    prepended: vec![],
                    untyped: true,
                    op: match op.inner.op {
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
            general_op.inner.prepended.push(Operator::Drop);
        }

        // Attempt to validate the operator in this context,
        // adding one param to the stack to satisfy each
        // "type mismatch" error message from the validator.
        loop {
            let mut validator_copy = func_validator.clone();

            for prepended_op in &general_op.inner.prepended {
                validator_copy
                    .op(DUMMY_OFFSET, prepended_op)
                    .expect("prepended op is valid");
            }

            match validator_copy.try_op(DUMMY_OFFSET, &general_op.inner.op) {
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
                            Some(op) => general_op.inner.prepended.insert(drop_count, op),
                            None => {
                                // No default value possible.
                                general_op.inner.untyped = true;
                                match &general_op.inner.op {
                                    // If the operator is an end, need to preserve block structure,
                                    // but we can do this by marking block (which is almost ending anyway)
                                    // unreachable. This will make the stack polymorphic and the `end`
                                    // can pop whatever it needs.
                                    Operator::End => general_op
                                        .inner
                                        .prepended
                                        .insert(drop_count, Operator::Unreachable),
                                    // A polymorphic operator (e.g. `drop` or `select`) can take any param,
                                    // so here give it an i32.
                                    _ if suffix == "a type but nothing on stack" => general_op
                                        .inner
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
        locals: &[Aligned<ValType>],
        ops: &[Aligned<Operator<'a>>],
        body: &wasmparser::FunctionBody<'a>,
    ) -> Result<()> {
        let mut local_idx = 0;
        let locals_reader = body.get_locals_reader()?;
        for entry in locals_reader {
            let (num, ty) = entry?;
            for _ in 0..num {
                assert!(local_idx < locals.len());
                assert_eq!(ty, locals[local_idx].inner);
                local_idx += 1;
            }
        }

        let mut ops_reader = body.get_operators_reader()?;
        for op in ops {
            assert_eq!(&op.inner, &ops_reader.read()?);
        }
        ops_reader.finish()?;
        assert!(&ops_reader.eof());
        Ok(())
    }

    fn check_func_import(
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
                    "function ‘{import_name}’ not found in module {module}"
                ));
            }
        }
        Some(format!("module ‘{import_module}’ not found"))
    }
}

#[derive(Default)]
struct SimulatedStack {
    slot_idx_stack: Vec<usize>,
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
        slots: &mut Vec<Slot>,
        untyped: bool,
    ) -> Result<OperatorType> {
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
                    Some(SlotUse(
                        self.slot_idx_stack[pre_instr_height + i - pop_count],
                    ))
                }
            })
            .collect::<Vec<_>>();

        for _ in 0..std::cmp::min(pop_count, accessible_operands) {
            self.slot_idx_stack.pop();
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
                slots.push(Slot(
                    validator
                        .get_operand_type(push_count - i - 1)
                        .flatten()
                        .expect("result operand"),
                ));
                self.slot_idx_stack.push(slots.len() - 1);
                // XXX: advisory connection to "original" global or local slot for a {global/local}.get?
                SlotUse(slots.len() - 1)
            })
            .collect::<Vec<_>>();

        Ok(OperatorType { inputs, outputs })
    }
}

impl<'a> ValidModule<'a> {
    /// Computes the param and result types for each operator in the module.
    ///
    pub fn to_types_table(&self, wasm_bin: &[u8]) -> Result<TypedModule> {
        let parser = wasmparser::Parser::new(0);
        let mut validator = Validator::new_with_features(CODILLON_WASM_FEATURES);
        let mut ret = TypedModule {
            slots: Vec::new(),
            globals: Vec::with_capacity(self.globals.len()),
            funcs: Vec::with_capacity(self.functions.len()),
        };
        let mut allocs = wasmparser::FuncValidatorAllocations::default();

        for Aligned {
            inner:
                Global {
                    ty: GlobalType { content_type, .. },
                    ..
                },
            ..
        } in &self.globals
        {
            ret.slots.push(Slot(*content_type));
            ret.globals.push(SlotUse(ret.slots.len() - 1));
        }

        let mut funcs_iter = self.functions.iter();
        for payload in parser.parse_all(wasm_bin) {
            if let ValidPayload::Func(func, _) = validator.payload(&payload?)? {
                let valid_func = funcs_iter.next().expect("not enough funcs in ValidModule");
                let mut validator = func.into_validator(allocs);
                let types = Self::function_into_types_table(
                    &mut validator,
                    &self.types,
                    valid_func,
                    &mut ret.slots,
                )?;
                ret.funcs.push(types);
                allocs = validator.into_allocations();
            }
        }
        assert!(funcs_iter.next().is_none(), "too much funcs in ValidModule");
        Ok(ret)
    }

    fn function_into_types_table(
        func_validator: &mut wasmparser::FuncValidator<wasmparser::ValidatorResources>,
        types: &[wasmparser::FuncType],
        valid_func: &ValidFunction<'_>,
        slots: &mut Vec<Slot>,
    ) -> Result<TypedFunction> {
        let mut ret = TypedFunction {
            params: Vec::with_capacity(valid_func.params.len()),
            locals: Vec::with_capacity(valid_func.locals.len()),
            ops: Vec::with_capacity(valid_func.operators.len()),
        };

        for param_ty in types[valid_func.type_idx as usize].params() {
            slots.push(Slot(*param_ty));
            ret.params.push(SlotUse(slots.len() - 1));
        }

        for ty in &valid_func.locals {
            func_validator.define_locals(DUMMY_OFFSET, 1, ty.inner)?;
            slots.push(Slot(ty.inner));
            ret.locals.push(SlotUse(slots.len() - 1));
        }

        let mut stack = SimulatedStack::default();

        for op in &valid_func.operators {
            for pre in &op.inner.prepended {
                stack.op(pre, func_validator, slots, false)?;
            }

            ret.ops
                .push(stack.op(&op.inner.op, func_validator, slots, op.inner.untyped)?);
        }

        Ok(ret)
    }

    pub fn build_instrumented_binary(
        self,
        types: &TypedModule,
        function_ranges: &[(usize, usize)],
    ) -> Result<Vec<u8>> {
        use wasm_encoder::*;
        let mut module = Module::default();

        /* Make import section (with instrumentation functions prepended) */
        let (import_section, num_func_imports) = {
            let mut import_section = ImportSection::new();
            let mut num_func_imports = 0;

            // First in import section: the "primitive" instrumentation functions
            // All function indices in code will have to be incremented by the number of these.
            for (name, type_idx) in InstrImports::TYPE_INDICES.iter() {
                import_section.import(
                    "codillon_debug",
                    name,
                    // instrumentation types come after the types in the original module
                    EntityType::Function(self.types.len() as u32 + type_idx),
                );
                num_func_imports += 1;
            }

            // Next in import section: the user-defined (original) imports
            for orig_import in &self.imports {
                match orig_import.ty {
                    wasmparser::TypeRef::Func(_) => num_func_imports += 1,
                    _ => panic!("unexpected import type"),
                }
                RoundtripReencoder.parse_import(&mut import_section, *orig_import)?;
            }
            (import_section, num_func_imports)
        };

        /* Build type and function section */
        // These are done together because we are auto-generating "transparent" instrumentation
        // functions (which collect the results of an arbitrary operator) and their types
        // at the same time.
        let (type_section, function_section, result_types_to_func_idx) = {
            let mut type_section = TypeSection::new();
            let mut function_section = FunctionSection::new();

            /* Start type section */

            // First in type section: the user-defined (original) function types
            for ty in &self.types {
                type_section
                    .ty()
                    .func_type(&RoundtripReencoder.func_type(ty.clone())?);
            }

            // Next in type section: the "primitive" instrumentation function types
            for (p, r) in InstrImports::FUNC_SIGS.iter() {
                type_section.ty().function(p.to_vec(), r.to_vec());
            }

            // Last in type section will be the "transparent" instrumention function types (below)

            /* Start function section */

            // First in function section: the user-defined (original) functions
            for func in &self.functions {
                function_section.function(func.type_idx);
            }

            // Last in function section: the "transparent" instrumentation functions (which return their inputs)

            // Map operator result types to the func idx of the "transparent" instrumentation function
            let mut result_types_to_func_idx = IndexMap::new();

            let mut next_type_idx = type_section.len();
            debug_assert_eq!(
                next_type_idx,
                InstrImports::FUNC_SIGS.len() as u32 + self.types.len() as u32
            );
            let mut next_func_idx = num_func_imports + self.functions.len() as u32;
            for func in &types.funcs {
                for OperatorType { outputs, .. } in &func.ops {
                    let results = outputs
                        .iter()
                        .map(|SlotUse(idx)| types.slots[*idx].0)
                        .collect::<Vec<_>>();
                    if !results.is_empty() && !result_types_to_func_idx.contains_key(&results) {
                        // Make function-section entry for new "transparent" instrumentation function
                        // The results are just the results of the instrumented operator.
                        let instr_results = RoundtripReencoder.val_types(results.clone())?;
                        // The params are the same thing, plus a slot index for each operand.
                        let mut instr_params = instr_results.clone();
                        instr_params.extend(vec![EncoderValType::I32; outputs.len()]);

                        // Add the instrumentation function type to the type section...
                        debug_assert_eq!(type_section.len(), next_type_idx);
                        type_section.ty().function(instr_params, instr_results);
                        // ... and add the function itself to the function section...
                        debug_assert_eq!(num_func_imports + function_section.len(), next_func_idx);
                        function_section.function(next_type_idx);
                        // ... and add this relationship to the map.
                        result_types_to_func_idx.insert(results, next_func_idx);

                        next_type_idx += 1;
                        next_func_idx += 1;
                    }
                }
            }
            (type_section, function_section, result_types_to_func_idx)
        };

        // Write finalized sections.
        module.section(&type_section);
        module.section(&import_section);
        module.section(&function_section);

        /* XXX table section: skip for now */

        /* Memory section */
        {
            let mut memory_section = MemorySection::new();
            for memory in &self.memory {
                memory_section.memory(RoundtripReencoder.memory_type(*memory)?);
            }
            module.section(&memory_section);
        }

        /* XXX tag section: skip for now */

        /* Global section */
        {
            let mut global_section = GlobalSection::new();
            for global in &self.globals {
                RoundtripReencoder.parse_global(&mut global_section, global.inner.clone())?;
            }
            module.section(&global_section);
        }

        /* Export section */
        {
            let mut exports = ExportSection::new();
            if !self.functions.is_empty() {
                exports.export("main", ExportKind::Func, num_func_imports);
            }
            module.section(&exports);
        }

        /* XXX start section: skip for now */
        /* XXX elem section: skip for now */
        /* XXX datacount section: skip for now */

        /* Code section */
        {
            let mut code_section = CodeSection::new();

            // First in code section: the original functions
            for (func_idx, start_line) in function_ranges.iter().enumerate() {
                self.build_function(
                    func_idx,
                    &mut code_section,
                    types,
                    &result_types_to_func_idx,
                    start_line.0,
                )?;
            }

            // Last in code section: the "transparent" (dynamically generated) instrumentation functions
            for result_type in result_types_to_func_idx.keys() {
                // iterated in same order as inserted
                self.dynamically_generate_function(result_type, &mut code_section)?;
            }

            module.section(&code_section);
        }

        /* XXX data section: skip for now */

        let wasm = module.finish();
        Ok(wasm)
    }

    fn build_function(
        &self,
        func_idx: usize,
        code_section: &mut CodeSection,
        types: &TypedModule,
        result_types_to_func_idx: &IndexMap<Vec<ValType>, u32>,
        start_line: usize,
    ) -> Result<()> {
        use wasm_encoder::Instruction::*;
        let num_instr_imports = InstrImports::TYPE_INDICES.len() as u32;
        let orig_function = &self.functions[func_idx];
        let typed_function = &types.funcs[func_idx];
        let mut new_function = wasm_encoder::Function::new_with_locals_types(
            orig_function
                .locals
                .iter()
                .map(|ty| RoundtripReencoder.val_type(ty.inner).fmt_err())
                .collect::<Result<Vec<_>>>()?,
        );

        let transparent_record_results =
            |f: &mut wasm_encoder::Function, results: &Vec<SlotUse>| {
                if results.is_empty() {
                    return;
                }
                for SlotUse(slot_idx) in results {
                    f.instruction(&I32Const((*slot_idx).try_into().expect("slot -> i32")));
                }
                f.instruction(&Call(
                    result_types_to_func_idx[&results
                        .iter()
                        .map(|SlotUse(slot_idx)| types.slots[*slot_idx].0)
                        .collect::<Vec<_>>()],
                ));
            };

        let primitive_record = |f: &mut wasm_encoder::Function, operand: &SlotUse| {
            use InstrImports::*;
            use ValType::*;
            let SlotUse(slot_idx) = operand;
            f.instruction(&I32Const((*slot_idx).try_into().expect("slot -> i32"))); // slot
            let rec_function = match types.slots[*slot_idx].0 {
                I32 => RecordI32,
                F32 => RecordF32,
                I64 => RecordI64,
                F64 => RecordF64,
                _ => bail!("unhandled ValType for primitive_record"),
            };
            f.instruction(&Call(rec_function as u32));
            Ok(())
        };

        fn step_debug(f: &mut wasm_encoder::Function, line_number: usize) {
            // Step after each instruction evaluation
            f.instruction(&I32Const(line_number.try_into().expect("line_no -> i32")));
            f.instruction(&Call(InstrImports::Step as u32));
        }

        // XXX on function entry, should "enter frame" of the params and locals

        // Record values of params and locals
        for (param_idx, param) in typed_function.params.iter().enumerate() {
            new_function.instruction(&LocalGet(param_idx.try_into().expect("param idx -> u32")));
            primitive_record(&mut new_function, param)?;
        }
        for (local_idx_pre, local) in typed_function.locals.iter().enumerate() {
            let local_idx = local_idx_pre + typed_function.params.len();
            new_function.instruction(&LocalGet(local_idx.try_into().expect("local idx -> u32")));
            primitive_record(&mut new_function, local)?;
        }

        if orig_function.operators.len() > 1
            && let Some(Aligned { line_idx, .. }) = orig_function.operators.first()
        {
            step_debug(&mut new_function, *line_idx);
        } else {
            step_debug(&mut new_function, start_line + 1);
        }

        // Record all operators, translating func idxes as necessary
        for (i, codillon_operator) in orig_function.operators.iter().enumerate() {
            let mut op = RoundtripReencoder.instruction(codillon_operator.inner.op.clone())?;
            let op_type = &typed_function.ops[i];

            if !codillon_operator.inner.prepended.is_empty() {
                // Originally invalid operator that was "validized" for type-analysis purposes.
                // Traps at runtime.
                new_function.instruction(&Unreachable);
            }

            // increment function indices to accommodate added imports
            match op {
                Call(ref mut idx) | RefFunc(ref mut idx) | ReturnCall(ref mut idx) => {
                    *idx += num_instr_imports
                }
                _ => {}
            }

            // the operator itself
            new_function.instruction(&op);

            if i + 1 == orig_function.operators.len() {
                debug_assert!(matches!(op, End));
                continue; // function is over
            }

            // record result operands
            transparent_record_results(&mut new_function, &op_type.outputs);

            // did this operator change a global or local (XXX or memory?)
            match op {
                GlobalSet(idx) => {
                    new_function.instruction(&GlobalGet(idx));
                    primitive_record(&mut new_function, &types.globals[idx as usize])?;
                }
                LocalSet(idx) | LocalTee(idx) => {
                    new_function.instruction(&LocalGet(idx));
                    let local_type = typed_function
                        .params
                        .iter()
                        .chain(&typed_function.locals)
                        .nth(idx as usize)
                        .expect("valid local idx");
                    primitive_record(&mut new_function, local_type)?;
                }
                _ => {}
            }

            step_debug(&mut new_function, codillon_operator.line_idx + 1);
        }
        code_section.function(&new_function);
        Ok(())
    }

    fn dynamically_generate_function(
        &self,
        tys: &[ValType],
        code_section: &mut CodeSection,
    ) -> Result<()> {
        use InstrImports::*;
        use ValType::*;
        let n = tys.len();
        let mut f = wasm_encoder::Function::new(vec![]);
        for (i, ty) in tys.iter().enumerate() {
            f.instruction(&LocalGet(i as u32)); // value
            f.instruction(&LocalGet((n + i) as u32)); // slot idx
            let rec_function = match ty {
                I32 => RecordI32,
                F32 => RecordF32,
                I64 => RecordI64,
                F64 => RecordF64,
                _ => bail!("unexpected output type"),
            };
            f.instruction(&Call(rec_function as u32));
        }
        for output_idx in 0..tys.len() {
            f.instruction(&LocalGet(output_idx as u32)); // restore values to stack
        }
        f.instruction(&End);
        code_section.function(&f);
        Ok(())
    }
}

// Slot represents anywhere that a value can go, e.g. a global, local, or stack operand.
#[derive(Debug, PartialEq, Eq)]
pub struct Slot(ValType);

// A SlotUse represents any input from, or output to, a slot (identified by its global index).
#[derive(Debug, PartialEq, Eq)]
pub struct SlotUse(usize);

// An Expression represents a collection of Slots that are initiatialized on entry and restored on exit.
// E.g. local Slots live in an Expression for the function's entire body, and stack operands
// live in an Expression that might be smaller than the function's body.
// pub struct Expression(Vec<usize>);

#[derive(Debug, PartialEq, Eq)]
pub struct OperatorType {
    pub inputs: Vec<Option<SlotUse>>,
    pub outputs: Vec<SlotUse>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypedFunction {
    pub params: Vec<SlotUse>,
    pub locals: Vec<SlotUse>,
    pub ops: Vec<OperatorType>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypedModule {
    pub slots: Vec<Slot>,
    pub globals: Vec<SlotUse>,
    pub funcs: Vec<TypedFunction>,
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

pub fn indent_and_frame(
    code: &mut impl FrameInfosMut,
    _module: &ValidModule,
    _types: &TypedModule,
) {
    struct OpenFrame {
        num: usize,
        start: usize,
        kind: InstrKind,
    }

    let mut frame_stack: Vec<OpenFrame> = Vec::new();
    let mut frame_count = 0;

    let mut indent: i32 = 0;
    for line_no in 0..code.len() {
        let mut indent_adjustment: i32 = 0;
        let ends_before = code.info(line_no).synthetic_before.end_opcodes;

        for _ in 0..ends_before {
            let f = frame_stack.pop().expect("frame ended before line");
            indent -= 1;
            code.set_frame_info(
                f.num,
                FrameInfo {
                    indent: indent.try_into().expect("indent -> usize"),
                    start: f.start,
                    end: line_no,
                    unclosed: true,
                    kind: f.kind,
                },
            );
        }

        let active = code.info(line_no).is_active();
        let line_kind = code.info(line_no).kind.stripped_clone();
        match line_kind {
            LineKind::Instr(kind) if active => match kind {
                InstrKind::If | InstrKind::OtherStructured => {
                    frame_stack.push(OpenFrame {
                        num: frame_count,
                        start: line_no,
                        kind,
                    });
                    frame_count += 1;
                    indent_adjustment = -1;
                    indent += 1;
                }
                InstrKind::Else => {
                    let Some(OpenFrame {
                        num,
                        start,
                        kind: InstrKind::If,
                    }) = frame_stack.pop()
                    else {
                        panic!("else outside if block");
                    };
                    indent_adjustment = -1;
                    indent -= 1;
                    code.set_frame_info(
                        num,
                        FrameInfo {
                            indent: indent.try_into().expect("indent -> usize"),
                            start,
                            end: line_no,
                            unclosed: false,
                            kind: InstrKind::If,
                        },
                    );
                    frame_stack.push(OpenFrame {
                        num: frame_count,
                        start: line_no,
                        kind: InstrKind::Else,
                    });
                    indent += 1;
                    frame_count += 1;
                }
                InstrKind::End => {
                    let Some(OpenFrame { num, start, kind }) = frame_stack.pop() else {
                        panic!("unclosed frame");
                    };
                    indent -= 1;
                    code.set_frame_info(
                        num,
                        FrameInfo {
                            indent: indent.try_into().expect("indent -> usize"),
                            start,
                            end: line_no,
                            unclosed: false,
                            kind,
                        },
                    );
                }
                InstrKind::Other => {}
            },
            LineKind::Instr(_) | LineKind::Empty | LineKind::Malformed(_) | LineKind::Other(_) => {}
        }

        // adjust indentation
        let paren_depths = code.info(line_no).paren_depths();
        indent += paren_depths.0;
        code.set_indent(
            line_no,
            (indent + indent_adjustment)
                .try_into()
                .expect("indent -> usize"),
        );
        indent += paren_depths.1;
    }
    code.set_frame_count(frame_count);
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
    use wasmparser::{BlockType, FuncType};

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

    fn empty_ft() -> FuncType {
        FuncType::new(vec![], vec![])
    }

    #[test]
    fn test_types_table() -> Result<()> {
        type CodillonInstruction<'a> = Aligned<Operator<'a>>;
        use ValType::*;

        fn ins(inputs: Vec<SlotUse>) -> OperatorType {
            OperatorType {
                inputs: inputs.into_iter().map(Some).collect(),
                outputs: Vec::new(),
            }
        }

        fn outs(outputs: Vec<SlotUse>) -> OperatorType {
            OperatorType {
                inputs: Vec::new(),
                outputs,
            }
        }

        fn inout(inputs: Vec<SlotUse>, outputs: Vec<SlotUse>) -> OperatorType {
            OperatorType {
                inputs: inputs.into_iter().map(Some).collect(),
                outputs,
            }
        }

        fn os(ty: ValType) -> Slot {
            Slot(ty)
        }

        fn slot(idx: usize) -> SlotUse {
            SlotUse(idx)
        }

        fn force_valid(raw: RawModule<'_>) -> ValidModule<'_> {
            let mut ret = ValidModule {
                types: raw.types,
                imports: raw.imports,
                memory: raw.memory,
                globals: raw.globals,
                functions: vec![],
            };

            for func in raw.functions {
                let mut valid = ValidFunction {
                    type_idx: 0,
                    params: func.params,
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
        let output = TypedModule {
            slots: vec![os(I32), os(I32), os(I32), os(I32), os(I32), os(I32)],
            globals: vec![],
            funcs: vec![TypedFunction {
                params: vec![],
                locals: vec![],
                ops: vec![
                    outs(vec![slot(0)]),
                    outs(vec![slot(1)]),
                    inout(vec![slot(0), slot(1)], vec![slot(2), slot(3)]),
                    inout(vec![slot(2), slot(3)], vec![slot(4)]),
                    inout(vec![slot(4)], vec![slot(5)]),
                    ins(vec![slot(5)]),
                    ins(vec![]),
                ],
            }],
        };
        let lines =
            "i32.const 1\ni32.const 2\nblock (param i32 i32) (result i32)\ni32.add\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = RawModule {
            types: vec![empty_ft()],
            imports: vec![],
            memory: vec![],
            globals: vec![],
            functions: vec![RawFunction {
                type_idx: 0,
                params: vec![],
                locals: vec![],
                operators: vec![
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 1 },
                        line_idx: 0,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 2 },
                        line_idx: 1,
                    },
                    CodillonInstruction {
                        inner: Operator::Block {
                            blockty: BlockType::FuncType(1),
                        },
                        line_idx: 2,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Add,
                        line_idx: 3,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 4,
                    },
                    CodillonInstruction {
                        inner: Operator::Drop,
                        line_idx: 5,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
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
        let output = TypedModule {
            slots: vec![os(I32), os(I32), os(I32), os(I32)],
            globals: vec![],
            funcs: vec![TypedFunction {
                params: vec![],
                locals: vec![],
                ops: vec![
                    outs(vec![slot(0)]),
                    ins(vec![slot(0)]),
                    outs(vec![slot(1)]),
                    ins(vec![slot(1)]),
                    outs(vec![slot(2)]),
                    inout(vec![slot(2)], vec![slot(3)]),
                    ins(vec![slot(3)]),
                    ins(vec![]),
                ],
            }],
        };
        let lines = "i32.const 1\nif (result i32)\ni32.const 1\nelse\ni32.const 2\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = RawModule {
            types: vec![empty_ft()],
            imports: vec![],
            memory: vec![],
            globals: vec![],
            functions: vec![RawFunction {
                type_idx: 0,
                params: vec![],
                locals: vec![],
                operators: vec![
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 1 },
                        line_idx: 0,
                    },
                    CodillonInstruction {
                        inner: Operator::If {
                            blockty: BlockType::Type(I32),
                        },
                        line_idx: 1,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 1 },
                        line_idx: 2,
                    },
                    CodillonInstruction {
                        inner: Operator::Else,
                        line_idx: 3,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 2 },
                        line_idx: 4,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 5,
                    },
                    CodillonInstruction {
                        inner: Operator::Drop,
                        line_idx: 6,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
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
        let output = TypedModule {
            slots: vec![os(I32), os(I32), os(I32), os(I32), os(I32), os(I32)],
            globals: vec![],
            funcs: vec![TypedFunction {
                params: vec![],
                locals: vec![],
                ops: vec![
                    outs(vec![slot(0)]),
                    inout(vec![slot(0)], vec![slot(1)]),
                    outs(vec![slot(2)]),
                    inout(vec![slot(1), slot(2)], vec![slot(3)]),
                    ins(vec![slot(3)]),
                    outs(vec![slot(4)]),
                    inout(vec![slot(4)], vec![slot(5)]),
                    ins(vec![slot(5)]),
                    ins(vec![]),
                ],
            }],
        };
        let lines = "i32.const 10\nloop (param i32) (result i32)\ni32.const 1\ni32.sub\nbr_if 1\ni32.const 2\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = RawModule {
            types: vec![empty_ft()],
            imports: vec![],
            memory: vec![],
            globals: vec![],
            functions: vec![RawFunction {
                type_idx: 0,
                params: vec![],
                locals: vec![],
                operators: vec![
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 10 },
                        line_idx: 0,
                    },
                    CodillonInstruction {
                        inner: Operator::Loop {
                            blockty: BlockType::FuncType(1),
                        },
                        line_idx: 1,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 1 },
                        line_idx: 2,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Sub,
                        line_idx: 3,
                    },
                    CodillonInstruction {
                        inner: Operator::BrIf { relative_depth: 1 },
                        line_idx: 4,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 2 },
                        line_idx: 5,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 6,
                    },
                    CodillonInstruction {
                        inner: Operator::Drop,
                        line_idx: 7,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
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
        let output = TypedModule {
            slots: vec![os(I32), os(I32), os(I32), os(I32), os(I32), os(I32)],
            globals: vec![],
            funcs: vec![TypedFunction {
                params: vec![],
                locals: vec![],
                ops: vec![
                    outs(vec![slot(0)]),
                    inout(vec![slot(0)], vec![slot(1)]),
                    ins(vec![slot(1)]),
                    outs(vec![slot(2)]),
                    ins(vec![slot(2)]),
                    outs(vec![slot(3)]),
                    inout(vec![slot(3)], vec![slot(4)]),
                    inout(vec![slot(4)], vec![slot(5)]),
                    ins(vec![slot(5)]),
                    ins(vec![]),
                ],
            }],
        };
        let lines = "i32.const 10\nblock (param i32) (result i32)\nif (result i32)\ni32.const 1\nelse\ni32.const 2\nend\nend\ndrop";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = RawModule {
            types: vec![empty_ft()],
            imports: vec![],
            memory: vec![],
            globals: vec![],
            functions: vec![RawFunction {
                type_idx: 0,
                params: vec![],
                locals: vec![],
                operators: vec![
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 10 },
                        line_idx: 0,
                    },
                    CodillonInstruction {
                        inner: Operator::Block {
                            blockty: BlockType::FuncType(1),
                        },
                        line_idx: 1,
                    },
                    CodillonInstruction {
                        inner: Operator::If {
                            blockty: BlockType::Type(I32),
                        },
                        line_idx: 2,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 1 },
                        line_idx: 3,
                    },
                    CodillonInstruction {
                        inner: Operator::Else,
                        line_idx: 4,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 2 },
                        line_idx: 5,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 6,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 7,
                    },
                    CodillonInstruction {
                        inner: Operator::Drop,
                        line_idx: 8,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
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
        let output = TypedModule {
            slots: vec![],
            globals: vec![],
            funcs: vec![TypedFunction {
                params: vec![],
                locals: vec![],
                ops: vec![ins(vec![]), ins(vec![]), ins(vec![])],
            }],
        };
        let lines = "block\nend";
        let func = format!("(module (func {lines}))");
        let wasm_bin = wat::parse_str(func).expect("failed to parse wat to binary wasm");
        let instruction_table = RawModule {
            types: vec![empty_ft()],
            imports: vec![],
            memory: vec![],
            globals: vec![],
            functions: vec![RawFunction {
                type_idx: 0,
                params: vec![],
                locals: vec![],
                operators: vec![
                    CodillonInstruction {
                        inner: Operator::Block {
                            blockty: BlockType::Empty,
                        },
                        line_idx: 0,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 1,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
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
            types: vec![empty_ft()],
            imports: vec![],
            memory: vec![test_mem],
            globals: vec![],
            functions: vec![],
        };
        let function_ranges: Vec<(usize, usize)> = Vec::new();
        let wasm_bin = valid_module.build_instrumented_binary(
            &TypedModule {
                slots: vec![],
                globals: vec![],
                funcs: vec![],
            },
            &function_ranges,
        )?;
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

    #[derive(Default, Debug)]
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
        use anyhow::Context;

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
            .build_instrumented_binary(&types, &function_ranges)
            .context("build_executable_binary")?;

        let instrumented_text = wasmprinter::print_bytes(&runnable).context("print_bytes")?;

        wasmparser::validate(&runnable).context(format!(
            "validate {:?} -> {instrumented_text}",
            editor.lines
        ))?;

        Ok(instrumented_text)
    }

    const EXPECTED_TYPES: &str = r#"  (type (;0;) (func))
  (type (;1;) (func (param i32)))
  (type (;2;) (func (param i32 i32)))
  (type (;3;) (func (param f32 i32)))
  (type (;4;) (func (param i64 i32)))
  (type (;5;) (func (param f64 i32)))
"#;
    const EXPECTED_IMPORTS: &str = r#"  (import "codillon_debug" "step" (func (;0;) (type 1)))
  (import "codillon_debug" "record_i32" (func (;1;) (type 2)))
  (import "codillon_debug" "record_f32" (func (;2;) (type 3)))
  (import "codillon_debug" "record_i64" (func (;3;) (type 4)))
  (import "codillon_debug" "record_f64" (func (;4;) (type 5)))
  (export "main" (func 5))
"#;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_fixes() -> Result<()> {
        // empty function
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func)");
            let expected = String::from("(module\n")
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + r#"  (func (;5;) (type 0)
    i32.const 1
    call 0
  )
)
"#;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // syntax error (synthesizes closing paren)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            let expected = String::from("(module\n")
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + r#"  (func (;5;) (type 0)
    i32.const 1
    call 0
  )
)
"#;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // syntax error (synthesizes closing "func)" on line 1)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(");
            let expected = String::from("(module\n")
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + r#"  (func (;5;) (type 0)
    i32.const 2
    call 0
  )
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
                + EXPECTED_TYPES
                + "  (type (;6;) (func (param i64 i32) (result i64)))\n"
                + EXPECTED_IMPORTS
                + r#"  (func (;5;) (type 0)
    i32.const 0
    call 0
    i64.const 17
    i32.const 0
    call 6
    i32.const 1
    call 0
    drop
    i32.const 2
    call 0
  )
  (func (;6;) (type 6) (param i64 i32) (result i64)
    local.get 0
    local.get 1
    call 3
    local.get 0
  )
)
"#;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // well-formed block
        const JUST_BLOCK_END: &str = r#"    i32.const 1
    call 0
    block ;; label = @1
      i32.const 2
      call 0
    end
    i32.const 3
    call 0
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
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + "  (func (;5;) (type 0)\n"
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
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + "  (func (;5;) (type 0)\n"
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
                + EXPECTED_TYPES
                + "  (type (;6;) (func (param i32 i32) (result i32)))\n"
                + EXPECTED_IMPORTS
                + r#"  (func (;5;) (type 0)
    i32.const 1
    call 0
    i32.const 137
    i32.const 0
    call 6
    i32.const 2
    call 0
    unreachable
    i32.add
    i32.const 3
    call 6
    i32.const 3
    call 0
    drop
    i32.const 4
    call 0
  )
  (func (;6;) (type 6) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call 1
    local.get 0
  )
)
"#;

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
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + r#"  (func (;5;) (type 0)
    i32.const 7
    call 0
  )
)
"#;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // syntax error (consumed symbolic reference not defined - module-level, local, label)
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
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + "  (func (;5;) (type 0)\n"
                + JUST_BLOCK_END;
            let expected = expected.replace("i32.const 3", "i32.const 6");
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
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + r#"  (func (;5;) (type 0)
    i32.const 1
    call 0
    block ;; label = @1
      i32.const 2
      call 0
      loop ;; label = @2
        i32.const 3
        call 0
      end
      i32.const 5
      call 0
    end
    i32.const 6
    call 0
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
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + r#"  (func (;5;) (type 0)
    i32.const 1
    call 0
    nop
    i32.const 2
    call 0
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
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + r#"  (func (;5;) (type 0)
    i32.const 2
    call 0
    nop
    i32.const 3
    call 0
    unreachable
    drop
    i32.const 4
    call 0
  )
)
"#;
            let expected = expected.replace(
                "  (export \"main\" (func 5))",
                "  (memory (;0;) 0)\n  (export \"main\" (func 5))",
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
                + EXPECTED_TYPES
                + "  (type (;6;) (func (param i32 i32) (result i32)))\n"
                + EXPECTED_IMPORTS
                + r#"  (func (;5;) (type 0)
    i32.const 0
    call 0
    loop ;; label = @1
      i32.const 1
      call 0
      i32.const 5
      i32.const 0
      call 6
      i32.const 2
      call 0
      br 0 (;@1;)
      i32.const 3
      call 0
    end
    i32.const 4
    call 0
  )
  (func (;6;) (type 6) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call 1
    local.get 0
  )
)
"#;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // validation error
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("i32.const 4");
            editor.push_line(")");
            let expected = String::from("(module\n")
                + EXPECTED_TYPES
                + "  (type (;6;) (func (param i32 i32) (result i32)))\n"
                + EXPECTED_IMPORTS
                + r#"  (func (;5;) (type 0)
    i32.const 1
    call 0
    i32.const 4
    i32.const 0
    call 6
    i32.const 2
    call 0
    unreachable
  )
  (func (;6;) (type 6) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call 1
    local.get 0
  )
)
"#;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        // additional function with multi-value result type
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
                + r#"  (type (;0;) (func))
  (type (;1;) (func (result i32 i32)))
  (type (;2;) (func (param i32)))
  (type (;3;) (func (param i32 i32)))
  (type (;4;) (func (param f32 i32)))
  (type (;5;) (func (param i64 i32)))
  (type (;6;) (func (param f64 i32)))
  (type (;7;) (func (param i32 i32 i32 i32) (result i32 i32)))
  (type (;8;) (func (param i32 i32) (result i32)))
  (import "codillon_debug" "step" (func (;0;) (type 2)))
  (import "codillon_debug" "record_i32" (func (;1;) (type 3)))
  (import "codillon_debug" "record_f32" (func (;2;) (type 4)))
  (import "codillon_debug" "record_i64" (func (;3;) (type 5)))
  (import "codillon_debug" "record_f64" (func (;4;) (type 6)))
  (export "main" (func 5))
  (func (;5;) (type 0)
    i32.const 1
    call 0
    call 6
    i32.const 0
    i32.const 1
    call 7
    i32.const 2
    call 0
    i32.add
    i32.const 2
    call 8
    i32.const 3
    call 0
    unreachable
  )
  (func (;6;) (type 1) (result i32 i32)
    i32.const 5
    call 0
    i32.const 9
    i32.const 3
    call 8
    i32.const 6
    call 0
    i32.const 10
    i32.const 4
    call 8
    i32.const 7
    call 0
  )
  (func (;7;) (type 7) (param i32 i32 i32 i32) (result i32 i32)
    local.get 0
    local.get 2
    call 1
    local.get 1
    local.get 3
    call 1
    local.get 0
    local.get 1
  )
  (func (;8;) (type 8) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call 1
    local.get 0
  )
)
"#;
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
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + r#"  (func (;5;) (type 0)
    (local i32 i32 i32 f64)
    local.get 0
    i32.const 0
    call 1
    local.get 1
    i32.const 1
    call 1
    local.get 2
    i32.const 2
    call 1
    local.get 3
    i32.const 3
    call 4
    i32.const 1
    call 0
  )
)
"#;
            assert_eq!(expected, test_editor_flow(&mut editor)?);
        }

        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("block (param i32)");
            editor.push_line("end");
            editor.push_line(")");

            let expected = String::from("(module\n")
                + r#"  (type (;0;) (func))
  (type (;1;) (func (param i32)))
  (type (;2;) (func (param i32)))
  (type (;3;) (func (param i32 i32)))
  (type (;4;) (func (param f32 i32)))
  (type (;5;) (func (param i64 i32)))
  (type (;6;) (func (param f64 i32)))
  (type (;7;) (func (param i32 i32) (result i32)))
  (import "codillon_debug" "step" (func (;0;) (type 2)))
  (import "codillon_debug" "record_i32" (func (;1;) (type 3)))
  (import "codillon_debug" "record_f32" (func (;2;) (type 4)))
  (import "codillon_debug" "record_i64" (func (;3;) (type 5)))
  (import "codillon_debug" "record_f64" (func (;4;) (type 6)))
  (export "main" (func 5))
  (func (;5;) (type 0)
    i32.const 1
    call 0
    unreachable
    block (type 1) (param i32) ;; label = @1
      i32.const 1
      call 6
      i32.const 2
      call 0
      unreachable
    end
    i32.const 3
    call 0
  )
  (func (;6;) (type 7) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call 1
    local.get 0
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
                "module ‘not_helpers’ not found"
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
                "function ‘not_draw_point’ not found in module helpers"
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
