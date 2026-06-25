use crate::syntax::{
    FrameInfo, FrameInfosMut, FuncPart, InstrKind, LineInfos, LineInfosMut, LineKind, ModulePart,
    find_function_ranges,
};
use EncoderInstruction::*;
use anyhow::{Result, bail};
use indexmap::IndexMap;
use itertools::{Itertools, zip_eq};
use std::{
    cmp::{max, min},
    collections::HashMap,
    fmt::{Debug, Formatter},
    num::NonZeroU32,
    ops::Range,
};
use wasm_encoder::{
    CodeSection, Instruction as EncoderInstruction, MemArg, ValType as EncoderValType,
    reencode::{Reencode, RoundtripReencoder},
};
use wasm_tools::parse_binary_wasm;
use wasmparser::{
    Frame, FrameKind, FuncValidator, Global, GlobalType, HeapType, Operator, ValType, ValidPayload,
    Validator, WasmFeatures, WasmModuleResources,
};
use wast::{
    core::{
        GlobalType as TextGlobalType, Limits as TextLimits, MemoryType as TextMemoryType, Module,
        ValType as TextValType,
    },
    parser::{self, ParseBuffer},
};

#[repr(u32)]
enum InstrImports {
    RecordStep,
    RecordInvalid,
    RecordI32,
    RecordF32,
    RecordI64,
    RecordF64,
    EnterFunc,
    BeforeCall,
    AfterCall,
    BeforeTailCall,
    EnterBlock,
}
impl InstrImports {
    const TYPE_INDICES: &'static [(&'static str, u32)] = &[
        ("record_step", 0), // indices relative to end of user-provided type section
        ("record_invalid", 1),
        ("record_i32", 2),
        ("record_f32", 3),
        ("record_i64", 4),
        ("record_f64", 5),
        ("enter_func", 6),
        ("before_call", 1),
        ("after_call", 6),
        ("before_tail_call", 1),
        ("enter_block", 6),
    ];
    const FUNC_SIGS: &'static [(&'static [EncoderValType], &'static [EncoderValType])] = &[
        // 0: i32 -> i32
        (&[EncoderValType::I32], &[EncoderValType::I32]),
        // 1: () -> ()
        (&[], &[]),
        // 2: (i32, i32) -> ()
        (&[EncoderValType::I32, EncoderValType::I32], &[]),
        // 3: (f32, i32) -> ()
        (&[EncoderValType::F32, EncoderValType::I32], &[]),
        // 4: (i64, i32) -> ()
        (&[EncoderValType::I64, EncoderValType::I32], &[]),
        // 5: (f64, i32) -> ()
        (&[EncoderValType::F64, EncoderValType::I32], &[]),
        // 6: i32 -> ()
        (&[EncoderValType::I32], &[]),
    ];
}

#[derive(PartialEq, Eq)]
pub enum HelperImportKind<'a> {
    Func {
        params: &'a [TextValType<'a>],
        results: &'a [TextValType<'a>],
    },
    Global(TextGlobalType<'a>),
    Memory(TextMemoryType),
}

struct HelperImport<'a> {
    name: &'static str,
    kind: HelperImportKind<'a>,
    reason: &'static str,
}

const HELPER_IMPORTS: &[(&str, &[HelperImport])] = &[
    (
        "draw",
        &[
            HelperImport {
                name: "point",
                kind: HelperImportKind::Func {
                    params: &[TextValType::F64, TextValType::F64],
                    results: &[],
                },
                reason: "expected type (param f64 f64)",
            },
            HelperImport {
                name: "clear",
                kind: HelperImportKind::Func {
                    params: &[],
                    results: &[],
                },
                reason: "expected empty type",
            },
            HelperImport {
                name: "set_color",
                kind: HelperImportKind::Func {
                    params: &[TextValType::I32, TextValType::I32, TextValType::I32],
                    results: &[],
                },
                reason: "expected type (param i32 i32 i32)",
            },
            HelperImport {
                name: "set_extent",
                kind: HelperImportKind::Func {
                    params: &[
                        TextValType::F64,
                        TextValType::F64,
                        TextValType::F64,
                        TextValType::F64,
                    ],
                    results: &[],
                },
                reason: "expected type (param f64 f64 f64 f64)",
            },
            HelperImport {
                name: "set_radius",
                kind: HelperImportKind::Func {
                    params: &[TextValType::F64],
                    results: &[],
                },
                reason: "expected type (param f64)",
            },
        ],
    ),
    (
        "listen",
        &[
            HelperImport {
                name: "num_samples",
                kind: HelperImportKind::Global(TextGlobalType {
                    ty: TextValType::I32,
                    mutable: false,
                    shared: false,
                }),
                reason: "expected (global i32)",
            },
            HelperImport {
                name: "listen_memory",
                kind: HelperImportKind::Memory(TextMemoryType {
                    limits: TextLimits {
                        is64: false,
                        min: 1,
                        max: None,
                    },
                    shared: false,
                    page_size_log2: None,
                }),
                reason: "expected (memory 1)",
            },
        ],
    ),
];

#[derive(Debug, PartialEq)]
pub enum OpInfo {
    Normal,
    FuncEnd,
    SyntheticElse,
}

#[derive(Debug)]
pub struct GeneralOperator<'a> {
    prepended: Vec<Operator<'a>>,
    op: Operator<'a>,
    untyped: bool,
    info: OpInfo,
}

#[derive(Debug)]
pub struct Aligned<T> {
    pub inner: T,
    pub line_idx: usize,
    pub position_id: u32,
}

pub struct RawFunction<'a> {
    pub type_idx: u32,
    pub lines: (usize, usize),
    pub positions: (u32, u32),
    pub params: Vec<Aligned<ValType>>,
    pub locals: Vec<Aligned<ValType>>,
    pub operators: Vec<Aligned<Operator<'a>>>,
}

#[derive(Debug)]
pub struct ValidFunction<'a> {
    pub type_idx: u32,
    pub lines: (usize, usize),
    pub positions: (u32, u32),
    pub params: Vec<Aligned<ValType>>,
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
                info: OpInfo::Normal,
            },
            line_idx: val.line_idx,
            position_id: val.position_id,
        }
    }
}

const DUMMY_OFFSET: usize = 1; // no need to track offsets, but validator has safety checks against 0

// current features (notable exceptions: SIMD, GC, function references, exceptions, memory64)
const CODILLON_WASM_FEATURES: WasmFeatures = WasmFeatures::WASM1
    .union(WasmFeatures::BULK_MEMORY)
    .union(WasmFeatures::SIGN_EXTENSION)
    .union(WasmFeatures::SATURATING_FLOAT_TO_INT)
    .union(WasmFeatures::MULTI_VALUE)
    .union(WasmFeatures::CUSTOM_PAGE_SIZES)
    .union(WasmFeatures::MULTI_MEMORY)
    .union(WasmFeatures::TAIL_CALL);

impl<'a> RawModule<'a> {
    pub fn new(editor: &impl LineInfos, wasm_bin: &'a [u8]) -> Result<Self> {
        use wasmparser::*;
        let function_ranges = find_function_ranges(editor);
        let parser = Parser::new(0);
        let mut functions: Vec<RawFunction> = Vec::new();
        let mut types: Vec<FuncType> = Vec::new();
        let mut func_type_indices: Vec<u32> = Vec::new();
        let mut imports: Vec<Import> = Vec::new();
        let mut memory: Vec<MemoryType> = Vec::new();
        let mut globals: Vec<Aligned<Global>> = Vec::new();
        type FuncInfo<'a> = (Vec<Aligned<ValType>>, Vec<Aligned<Operator<'a>>>); // locals, ops
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
                            position_id: 0,
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
                                position_id: 0,
                                inner: ty,
                            });
                        }
                    }
                    let mut ops = Vec::new();
                    for op in body.get_operators_reader()?.into_iter() {
                        ops.push(Aligned {
                            line_idx: 0,
                            position_id: 0,
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
                    && let LineKind::Other(ModulePart::Global) = &editor.info(line_no).kind
                {
                    let glob = globals_iter.next().expect("not enough globals in module");
                    glob.line_idx = line_no;
                    glob.position_id = editor.info(line_no).id;
                }
            }
            assert!(globals_iter.next().is_none(), "too many globals in module");
        }

        assert_eq!(
            function_ranges.len(),
            funcs.len(),
            "function count mismatch"
        );

        for (func_idx, ((mut locals, mut operators), (start_line, end_line))) in
            funcs.into_iter().zip(function_ranges).enumerate()
        {
            // align params
            let params = types[func_type_indices[func_idx] as usize]
                .params()
                .to_vec()
                .into_iter()
                .map(|ty| Aligned {
                    line_idx: start_line,
                    position_id: editor.info(start_line).id,
                    inner: ty,
                })
                .collect();

            // align locals
            let mut locals_iter = locals.iter_mut();
            for line_no in start_line..=end_line {
                if editor.info(line_no).is_active()
                    && let LineKind::Other(ModulePart::Func(parts)) = &editor.info(line_no).kind
                    && let Some(FuncPart::Local(num)) = parts.first()
                {
                    debug_assert_eq!(parts.len(), 1);
                    for _ in 0..*num {
                        let loc = locals_iter.next().expect("not enough locals in function");
                        loc.line_idx = line_no;
                        loc.position_id = editor.info(line_no).id;
                    }
                }
            }
            assert!(locals_iter.next().is_none(), "too many locals in function");

            // align ops
            let mut ops_iter = operators.iter_mut();
            for line_no in start_line..=end_line {
                for _ in 0..editor.info(line_no).num_ops() {
                    let op = ops_iter.next().expect("not enough ops in function");
                    op.line_idx = line_no;
                    op.position_id = editor.info(line_no).id;
                }
            }
            match ops_iter.next() {
                Some(Aligned {
                    line_idx,
                    position_id,
                    inner: wasmparser::Operator::End,
                }) => {
                    *line_idx = end_line;
                    *position_id = editor.info(end_line).id;
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
                lines: (start_line, end_line),
                positions: (editor.info(start_line).id, editor.info(end_line).id),
                params,
                locals,
                operators,
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
        editor: &mut impl LineInfosMut,
        wasm_bin: &'a [u8],
    ) -> Result<ValidModule<'a>> {
        let parser = wasmparser::Parser::new(0);
        let mut validator = Validator::new_with_features(CODILLON_WASM_FEATURES);
        let mut allocs = wasmparser::FuncValidatorAllocations::default();

        let mut ret = ValidModule {
            types: self.types,
            imports: self.imports,
            memory: self.memory,
            globals: self.globals,
            functions: Vec::with_capacity(self.functions.len()),
        };

        let mut raw_functions = self.functions.into_iter();

        /* "Validize" each function */
        for payload in parser.parse_all(wasm_bin) {
            if let ValidPayload::Func(func, _body) = validator.payload(&payload?)? {
                let RawFunction {
                    type_idx,
                    lines,
                    positions,
                    params,
                    locals,
                    operators,
                } = raw_functions.next().expect("function count mismatch");

                #[cfg(debug_assertions)]
                Self::assert_bodies_match(&locals, &operators, &_body)?;

                let mut func_validator = func.into_validator(allocs);

                for ty in &locals {
                    func_validator.define_locals(DUMMY_OFFSET, 1, ty.inner)?;
                }

                let mut valid_function = ValidFunction {
                    type_idx,
                    lines,
                    positions,
                    params,
                    locals,
                    operators: Vec::with_capacity(operators.len()),
                };

                for op in operators {
                    match func_validator.try_op(DUMMY_OFFSET, &op.inner) {
                        Ok(()) => valid_function.operators.push(op.into()),
                        Err(e) => {
                            let mut prepend_to_err = String::new();

                            /* expand if-[...]-end to if-[...]-else-end */
                            if op.inner == Operator::End
                                && let Some(Frame {
                                    kind: FrameKind::If,
                                    ..
                                }) = func_validator.get_control_frame(0)
                            {
                                /* does a plain "else" work here? */
                                let plain_else = Aligned {
                                    line_idx: op.line_idx,
                                    position_id: op.position_id,
                                    inner: GeneralOperator {
                                        prepended: vec![],
                                        op: Operator::Else,
                                        info: OpInfo::SyntheticElse,
                                        untyped: false,
                                    },
                                };
                                if func_validator
                                    .try_op(DUMMY_OFFSET, &plain_else.inner.op)
                                    .is_ok()
                                {
                                    valid_function.operators.push(plain_else);
                                    prepend_to_err.push_str("implicit ‘else’ branch: ");
                                // `else` was valid, but implicit else branch has invalid `end`
                                } else {
                                    /* otherwise, insert a valid version of the synthetic else operator */
                                    let mut validized_else = Self::make_valid(
                                        func_validator.clone(),
                                        Aligned {
                                            line_idx: op.line_idx,
                                            position_id: op.position_id,
                                            inner: Operator::Else,
                                        },
                                    )?;
                                    validized_else.inner.info = OpInfo::SyntheticElse;
                                    for prepended_op in &validized_else.inner.prepended {
                                        func_validator
                                            .op(DUMMY_OFFSET, prepended_op)
                                            .expect("prepended op to else is valid");
                                    }
                                    func_validator
                                        .op(DUMMY_OFFSET, &validized_else.inner.op)
                                        .expect("validized else now valid");
                                    valid_function.operators.push(validized_else);
                                }
                            }

                            /* make a valid version of the actual operator */
                            let line_idx = op.line_idx;
                            let validized = Self::make_valid(func_validator.clone(), op)?;
                            for prepended_op in &validized.inner.prepended {
                                func_validator
                                    .op(DUMMY_OFFSET, prepended_op)
                                    .expect("prepended op is valid");
                            }
                            func_validator
                                .op(DUMMY_OFFSET, &validized.inner.op)
                                .expect("validized op now valid");

                            editor.set_invalid(line_idx, Some(prepend_to_err + e.message()));

                            valid_function.operators.push(validized);
                        }
                    };
                }

                let last_op = &mut valid_function.operators.last_mut().unwrap().inner;

                debug_assert_eq!(last_op.op, Operator::End);
                debug_assert_eq!(last_op.info, OpInfo::Normal);
                last_op.info = OpInfo::FuncEnd;

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
            debug_assert!(op.inner.op != Operator::End);
            debug_assert!(op.inner.op != Operator::Else);
            Aligned {
                line_idx: op.line_idx,
                position_id: op.position_id,
                inner: GeneralOperator {
                    prepended: vec![],
                    untyped: true,
                    info: OpInfo::Normal,
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

            if general_op.inner.prepended.len() > 1000 {
                // bomb out instead of infinite loop
                dbg!(general_op);
                bail!("too many prepended ops to fix");
            }

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

    #[cfg(debug_assertions)]
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
}

pub fn check_import(
    import_module: &str,
    import_name: &str,
    import_kind: Option<&HelperImportKind>,
) -> Option<String> {
    // Check if module name exists
    for (module, components) in HELPER_IMPORTS {
        if *module == import_module {
            for HelperImport { name, kind, reason } in *components {
                // Check if component name exists
                if *name == import_name {
                    // Check if type matches
                    return if import_kind == Some(kind) {
                        None
                    } else {
                        Some(reason.to_string())
                    };
                }
            }
            return Some(format!(
                "field ‘{import_name}’ not found in module {module}"
            ));
        }
    }
    Some(format!("module ‘{import_module}’ not found"))
}

#[derive(Default)]
struct SimulatedStack {
    slot_idx_stack: Vec<SlotUse>,
}

// Opcodes that make the rest of the frame unreachable and pop all accessible operands.
// (Unfortunate that we have to hardcode this.)
fn is_unreachable_op(op: &Operator<'_>) -> bool {
    matches!(
        op,
        Operator::Return
            | Operator::ReturnCall { .. }
            | Operator::ReturnCallIndirect { .. }
            | Operator::ReturnCallRef { .. }
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

        #[cfg(debug_assertions)]
        let expect_unreachable = is_unreachable_op(op)
            || (validator.get_control_frame(0).unwrap().unreachable
                && !matches!(op, Operator::End | Operator::Else));

        let pop_count = if is_unreachable_op(op) {
            accessible_operands
        } else {
            pop_count
        };

        let is_select = matches!(op, Operator::Select | Operator::TypedSelect { .. });

        let inputs = (0..pop_count)
            .map(|i| {
                if untyped || (pop_count - i - 1) >= accessible_operands {
                    // XXX special-case for select. Should handle in more principled/general way.
                    if is_select && i == 2 && pop_count - 1 < accessible_operands {
                        self.slot_idx_stack[pre_instr_height + i - pop_count]
                    } else {
                        slots.push(Slot { ty: None });
                        SlotUse::new(slots.len() - 1)
                    }
                } else {
                    self.slot_idx_stack[pre_instr_height + i - pop_count]
                }
            })
            .collect::<Vec<_>>();

        for _ in 0..min(pop_count, accessible_operands) {
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

        #[cfg(debug_assertions)]
        if let Some(f) = validator.get_control_frame(0) {
            debug_assert_eq!(expect_unreachable, f.unreachable);
        }

        let outputs = (0..push_count)
            .map(|i| {
                slots.push(Slot {
                    ty: if is_select {
                        // XXX should handle in more principled/general way
                        slots.get(inputs[0].usize()).and_then(|y| y.ty)
                    } else {
                        validator.get_operand_type(push_count - i - 1).flatten()
                    },
                });
                self.slot_idx_stack.push(SlotUse::new(slots.len() - 1));
                // XXX: advisory connection to "original" global or local slot for a {global/local}.get?
                // XXX: advisory connection to `end` or `loop` op for a br/br_[x]?
                SlotUse::new(slots.len() - 1)
            })
            .collect::<Vec<_>>();

        Ok(OperatorType { inputs, outputs })
    }
}

struct FunctionHelperInfo {
    result_types_to_func_idx: IndexMap<Vec<ValType>, u32>,
    shadow_memory_indices: Vec<usize>,
    bounds_check_to_func_idx: IndexMap<Option<EncoderValType>, u32>,
}

impl<'a> ValidModule<'a> {
    /// Computes the param and result types for each operator in the module.
    ///
    pub fn to_types_table(&self, wasm_bin: &[u8]) -> Result<TypedModule> {
        let parser = wasmparser::Parser::new(0);
        let mut validator = Validator::new_with_features(CODILLON_WASM_FEATURES);
        let mut ret = TypedModule {
            slots: Vec::new(),
            blocks: Vec::new(),
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
            ret.slots.push(Slot {
                ty: Some(*content_type),
            });
            ret.globals.push(SlotUse::new(ret.slots.len() - 1));
        }

        let mut funcs_iter = self.functions.iter();
        for payload in parser.parse_all(wasm_bin) {
            if let ValidPayload::Func(func, _) = validator.payload(&payload?)? {
                let valid_func = funcs_iter.next().expect("not enough funcs in ValidModule");
                let mut validator = func.into_validator(allocs);
                let types = Self::function_into_types_table(
                    &mut validator,
                    valid_func,
                    &mut ret.slots,
                    &mut ret.blocks,
                )?;
                ret.funcs.push(types);
                allocs = validator.into_allocations();
            }
        }
        assert!(funcs_iter.next().is_none(), "too many funcs in ValidModule");
        Ok(ret)
    }

    fn function_into_types_table(
        func_validator: &mut wasmparser::FuncValidator<wasmparser::ValidatorResources>,
        valid_func: &ValidFunction<'_>,
        slots: &mut Vec<Slot>,
        blocks: &mut Vec<(SlotUse, SlotUse)>,
    ) -> Result<TypedFunction> {
        let mut ret = TypedFunction {
            first_slot: SlotUse::new(slots.len()),
            params: Vec::with_capacity(valid_func.params.len()),
            locals: Vec::with_capacity(valid_func.locals.len()),
            ops: Vec::with_capacity(valid_func.operators.len()),
        };

        for param_ty in valid_func.params.iter() {
            slots.push(Slot {
                ty: Some(param_ty.inner),
            });
            ret.params.push(SlotUse::new(slots.len() - 1));
        }

        for ty in &valid_func.locals {
            func_validator.define_locals(DUMMY_OFFSET, 1, ty.inner)?;
            slots.push(Slot { ty: Some(ty.inner) });
            ret.locals.push(SlotUse::new(slots.len() - 1));
        }

        let mut stack = SimulatedStack::default();
        let mut frame_stack = Vec::new();

        for op in &valid_func.operators {
            for pre in &op.inner.prepended {
                stack.op(pre, func_validator, slots, false)?;
            }

            if matches!(
                &op.inner.op,
                Operator::Block { .. } | Operator::Loop { .. } | Operator::If { .. }
            ) {
                frame_stack.push(slots.len());
            }

            let operator_ty = stack.op(&op.inner.op, func_validator, slots, op.inner.untyped)?;

            if op.line_idx == valid_func.lines.1 && op.inner.info != OpInfo::FuncEnd {
                debug_assert!(
                    (op.inner.op == Operator::End)
                        || (op.inner.op == Operator::Else
                            && op.inner.info == OpInfo::SyntheticElse)
                );

                ret.ops.push(OperatorType {
                    inputs: vec![],
                    outputs: vec![],
                }); // don't give type to synthetic else or end
            } else {
                ret.ops.push(operator_ty);
            }

            if op.inner.op == Operator::End && op.inner.info != OpInfo::FuncEnd {
                blocks.push((
                    SlotUse::new(frame_stack.pop().unwrap()),
                    SlotUse::new(slots.len()),
                ));
            }
        }

        debug_assert!(frame_stack.is_empty());

        Ok(ret)
    }

    pub fn build_instrumented_binary(self, types: &TypedModule) -> Result<Vec<u8>> {
        use wasm_encoder::*;
        let mut module = Module::default();

        /* Make import section (with instrumentation functions prepended) */
        let (import_section, num_func_imports) = {
            let mut import_section = ImportSection::new();
            let mut num_func_imports: u32 = 0;

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
                    wasmparser::TypeRef::Global(_) | wasmparser::TypeRef::Memory(_) => {}
                    _ => panic!("unexpected import type"),
                }
                RoundtripReencoder.parse_import(&mut import_section, *orig_import)?;
            }
            (import_section, num_func_imports)
        };

        let shadow_memory_indices: Vec<usize> = self
            .memory
            .iter()
            .enumerate()
            .filter_map(|(i, mem)| (mem.page_size_log2 == Some(0)).then_some(i))
            .collect();

        /* Build type and function section */
        // These are done together because we are auto-generating "transparent" instrumentation
        // functions (which collect the results of an arbitrary operator) and their types
        // at the same time.
        let (
            type_section,
            function_section,
            result_types_to_func_idx,
            step_function_idx,
            bounds_check_to_func_idx,
        ) = {
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

            // The type of the "step" function (N.B. not the same as the record_step import) is i32 -> ();
            // this is now one of the "primitive" instrumentation function types.
            let step_function_type_idx = type_section.len() - 1;

            // Next in type section will be the "transparent" instrumention function types (below)

            /* Start function section */

            // N.B. The imported functions have claimed the first function *indices*.

            // First in the actual function section: the user-defined (original) functions
            for func in &self.functions {
                function_section.function(func.type_idx);
            }

            // Next in function section: the "step" function (called after every operator)
            // This is inserted into the user's module so it can safely use `unreachable`
            // to terminate execution.
            let step_function_idx = num_func_imports + self.functions.len() as u32;
            debug_assert_eq!(function_section.len() as usize, self.functions.len());
            function_section.function(step_function_type_idx);
            let num_inserted_functions = 1;

            // Next in function section: the "transparent" instrumentation functions (which return their inputs)

            // Map operator result types to the func idx of the "transparent" instrumentation function
            let mut result_types_to_func_idx = IndexMap::new();

            let mut next_type_idx = type_section.len();
            let mut next_func_idx =
                num_func_imports + self.functions.len() as u32 + num_inserted_functions;

            debug_assert_eq!(
                next_type_idx as usize,
                InstrImports::FUNC_SIGS.len() + self.types.len()
            );
            debug_assert_eq!(next_type_idx, type_section.len());
            debug_assert_eq!(next_func_idx, num_func_imports + function_section.len());
            for func in &types.funcs {
                for OperatorType { outputs, .. } in &func.ops {
                    let results = outputs
                        .iter()
                        .filter_map(|idx| types.slots[idx.usize()].ty)
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
            // Last in type and function section: dynamically generated functions to do
            // software bounds checks on memory operations for memories with pagesize 1
            let mut bounds_check_to_func_idx = IndexMap::new();
            for func in &self.functions {
                for op in &func.operators {
                    if let Some((mem_idx, _, _, ty)) = bounds_check_info(&op.inner.op)
                        && shadow_memory_indices.contains(&(mem_idx as usize))
                        && !bounds_check_to_func_idx.contains_key(&ty)
                    {
                        let (params, results) = match ty {
                            None => (
                                vec![EncoderValType::I32, EncoderValType::I64], // offset, limit
                                vec![EncoderValType::I32],                      // offset
                            ),
                            Some(ty) => {
                                (
                                    vec![EncoderValType::I32, ty, EncoderValType::I64], // offset, value, limit
                                    vec![EncoderValType::I32, ty], // offset, value
                                )
                            }
                        };
                        debug_assert_eq!(type_section.len(), next_type_idx);
                        type_section.ty().function(params, results);
                        debug_assert_eq!(num_func_imports + function_section.len(), next_func_idx);
                        function_section.function(next_type_idx);
                        bounds_check_to_func_idx.insert(ty, next_func_idx);
                        next_type_idx += 1;
                        next_func_idx += 1;
                    }
                }
            }
            (
                type_section,
                function_section,
                result_types_to_func_idx,
                step_function_idx,
                bounds_check_to_func_idx,
            )
        };

        // Write finalized sections.
        module.section(&type_section);
        module.section(&import_section);
        module.section(&function_section);

        /* XXX table section: skip for now */

        /* Memory section */
        {
            let mut memory_section = MemorySection::new();
            for (mem_idx, memory) in self.memory.iter().enumerate() {
                if shadow_memory_indices.contains(&mem_idx) {
                    // Convert to using 64 KiB pagesize
                    memory_section.memory(wasm_encoder::MemoryType {
                        minimum: memory.initial.div_ceil(65536).max(1),
                        maximum: None,
                        memory64: memory.memory64,
                        shared: memory.shared,
                        page_size_log2: None,
                    });
                } else {
                    memory_section.memory(RoundtripReencoder.memory_type(*memory)?);
                }
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
            for &mem_idx in &shadow_memory_indices {
                global_section.global(
                    wasm_encoder::GlobalType {
                        val_type: EncoderValType::I64,
                        mutable: true,
                        shared: false,
                    },
                    &wasm_encoder::ConstExpr::i64_const(self.memory[mem_idx].initial as i64),
                );
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

            let mut block_count = 0;

            let info = FunctionHelperInfo {
                result_types_to_func_idx,
                shadow_memory_indices,
                bounds_check_to_func_idx,
            };

            // First in code section: the original functions
            for func_idx in 0..self.functions.len() {
                let function = self.build_function(
                    func_idx,
                    types,
                    step_function_idx,
                    &mut block_count,
                    &info,
                )?;
                code_section.function(&function);
            }

            // Next in code section: the "step" function.
            // This is inserted into the instrumented module so that "unreachable" doesn't panic the Rust.
            {
                let mut f = wasm_encoder::Function::new(vec![]);
                f.instruction(&LocalGet(0)); // line number param
                f.instruction(&Call(InstrImports::RecordStep as u32));
                f.instruction(&I32Eqz);
                f.instruction(&If(wasm_encoder::BlockType::Empty));
                // Trap when run out of steps
                f.instruction(&Unreachable);
                f.instruction(&End);
                f.instruction(&End);
                code_section.function(&f);
            }

            // Next in code section: the "transparent" (dynamically generated) instrumentation functions
            for result_type in info.result_types_to_func_idx.keys() {
                // iterated in same order as inserted
                self.dynamically_generate_function(result_type, &mut code_section)?;
            }

            // Last in code section: the dynamically generated memory bounds check functions
            for ty in info.bounds_check_to_func_idx.keys() {
                self.dynamically_generate_bounds_check_function(*ty, &mut code_section)?;
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
        types: &TypedModule,
        step_function_idx: u32,
        block_count: &mut usize,
        info: &FunctionHelperInfo,
    ) -> Result<wasm_encoder::Function> {
        use wasm_encoder::Instruction::*;
        let orig_function = &self.functions[func_idx];
        let typed_function = &types.funcs[func_idx];
        let num_instr_imports = InstrImports::TYPE_INDICES.len() as u32; // also idx of step function
        let mut new_function = wasm_encoder::Function::new_with_locals_types(
            orig_function
                .locals
                .iter()
                .map(|ty| RoundtripReencoder.val_type(ty.inner).fmt_err())
                .collect::<Result<Vec<_>>>()?,
        );

        let transparent_record_results =
            |f: &mut wasm_encoder::Function, results: &Vec<SlotUse>| {
                if results.is_empty() || results.iter().any(|x| types.slots[x.usize()].ty.is_none())
                {
                    return;
                }
                for slot_use in results {
                    f.instruction(&I32Const(slot_use.i32()));
                }
                f.instruction(&Call(
                    info.result_types_to_func_idx[&results
                        .iter()
                        .filter_map(|slot_idx| types.slots[slot_idx.usize()].ty)
                        .collect::<Vec<_>>()],
                ));
            };

        let primitive_record = |f: &mut wasm_encoder::Function, operand: &SlotUse| {
            use InstrImports::*;
            use ValType::*;
            let rec_function = match types.slots[operand.usize()].ty {
                Some(I32) => RecordI32,
                Some(F32) => RecordF32,
                Some(I64) => RecordI64,
                Some(F64) => RecordF64,
                None => return Ok(()), /* don't try to record unknown types in unreachable code */
                _ => bail!("unhandled ValType for primitive_record"),
            };
            f.instruction(&I32Const(operand.u32().try_into().unwrap()));
            f.instruction(&Call(rec_function as u32));
            Ok(())
        };

        let step_debug = |f: &mut wasm_encoder::Function, line_number: usize, below: bool| {
            // Step before each operator evaluation
            let idx = line_number as i32 | if below { 1 << 31 } else { 0 };
            f.instruction(&I32Const(idx));
            f.instruction(&Call(step_function_idx));
        };

        // XXX on function entry, should "enter frame" of the params and locals

        // Record values of globals (XXX not really necessary at start of every function, but, we don't have
        // our own "_start" function to do this in)
        for (global_idx, global) in types.globals.iter().enumerate() {
            new_function.instruction(&GlobalGet(
                global_idx.try_into().expect("global idx -> u32"),
            ));
            primitive_record(&mut new_function, global)?;
        }

        // Signal function entry
        new_function.instruction(&I32Const(func_idx.try_into().expect("func idx -> u32")));
        new_function.instruction(&Call(InstrImports::EnterFunc as u32));

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

        step_debug(&mut new_function, orig_function.lines.0, false);

        // Record all operators, translating func idxes as necessary
        for (i, codillon_operator) in orig_function.operators.iter().enumerate() {
            step_debug(&mut new_function, codillon_operator.line_idx, false);

            let mut op = RoundtripReencoder.instruction(codillon_operator.inner.op.clone())?;
            let op_type = &typed_function.ops[i];

            if codillon_operator.inner.untyped || !codillon_operator.inner.prepended.is_empty() {
                // Originally invalid operator that was "validized" for type-analysis purposes.
                // Traps at runtime.
                new_function.instruction(&Call(InstrImports::RecordInvalid as u32));
                new_function.instruction(&Unreachable);
            }

            // increment function indices to accommodate added imports
            match op {
                Call(ref mut idx) | RefFunc(ref mut idx) | ReturnCall(ref mut idx) => {
                    *idx += num_instr_imports
                }
                _ => {}
            }

            // the structural prepended operators (needed in the case of, e.g., inserting, `else`)
            for pre_op in &codillon_operator.inner.prepended {
                if matches!(pre_op, Operator::Else | Operator::Unreachable) {
                    new_function.instruction(&RoundtripReencoder.instruction(pre_op.clone())?);
                }
            }

            // signal function call or block entry
            match op {
                Call(_) | CallIndirect { .. } => {
                    new_function.instruction(&Call(InstrImports::BeforeCall as u32));
                }
                ReturnCall(_) | ReturnCallIndirect { .. } => {
                    new_function.instruction(&Call(InstrImports::BeforeTailCall as u32));
                }
                _ => (),
            }

            // bounds checking for load and store operations for memories with pagesize 1
            if let Some((mem_idx, offset, width, ty)) =
                bounds_check_info(&codillon_operator.inner.op)
                && let Some(shadow_pos) = info
                    .shadow_memory_indices
                    .iter()
                    .position(|&i| i == mem_idx as usize)
            {
                let shadow_global_idx = (self.globals.len() + shadow_pos) as u32;
                let total_offset = (width as u64 + offset) as i64;
                new_function.instruction(&GlobalGet(shadow_global_idx));
                new_function.instruction(&I64Const(total_offset));
                new_function.instruction(&I64Sub); // limit = size - (offset + width)
                new_function.instruction(&Call(info.bounds_check_to_func_idx[&ty]));
            }

            if i + 1 == orig_function.operators.len() {
                debug_assert!(matches!(op, End));
                debug_assert!(codillon_operator.inner.info == OpInfo::FuncEnd);

                // special handling for the function end (we can't put instrumentation after it,
                // but we know the type of end operator is identity on its inputs)
                transparent_record_results(&mut new_function, &op_type.outputs);

                new_function.instruction(&op);
                continue; // function is over
            }

            // Deny memory.grow requests for memories with pagesize 1
            if let MemoryGrow(mem_index) = op
                && info.shadow_memory_indices.contains(&(mem_index as usize))
            {
                // Discard argument and return -1
                new_function.instruction(&Drop);
                new_function.instruction(&I32Const(-1));
            } else {
                // the operator itself
                new_function.instruction(&op);
            }

            if codillon_operator.line_idx == orig_function.lines.1 {
                // synthetic else or end

                debug_assert!(
                    (codillon_operator.inner.op == Operator::End)
                        || (codillon_operator.inner.op == Operator::Else
                            && codillon_operator.inner.info == OpInfo::SyntheticElse)
                );

                debug_assert!(codillon_operator.inner.info != OpInfo::FuncEnd);
                new_function.instruction(&Call(InstrImports::RecordInvalid as u32));
                new_function.instruction(&Unreachable);
            }

            // record result operands
            transparent_record_results(&mut new_function, &op_type.outputs);

            match op {
                // signal back from function call
                Call(_) | CallIndirect { .. } => {
                    new_function
                        .instruction(&I32Const(func_idx.try_into().expect("func idx -> u32")));
                    new_function.instruction(&Call(InstrImports::AfterCall as u32));

                    // special handling: step this twice so pointer can return to the callsite after complete
                    step_debug(&mut new_function, codillon_operator.line_idx, true);
                }

                // clear slots belonging to a loop on (re-)entry
                Loop(_) => {
                    new_function.instruction(&I32Const(
                        (*block_count).try_into().expect("block idx -> i32"),
                    ));
                    new_function.instruction(&Call(InstrImports::EnterBlock as u32));
                    *block_count += 1;
                }

                // track other kinds of blocks (but no need to clear slots)
                Block(_) | If(_) => *block_count += 1,

                // did this operator change a global or local (XXX or memory?)
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
        }
        Ok(new_function)
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

    fn dynamically_generate_bounds_check_function(
        &self,
        ty: Option<EncoderValType>,
        code_section: &mut CodeSection,
    ) -> Result<()> {
        use wasm_encoder::Instruction::*;
        let limit_idx: u32 = if ty.is_none() { 1 } else { 2 };
        let mut f = wasm_encoder::Function::new(vec![]);
        f.instruction(&LocalGet(0)); // addr
        f.instruction(&I64ExtendI32U);
        f.instruction(&LocalGet(limit_idx)); // limit
        f.instruction(&I64GtS);
        f.instruction(&If(wasm_encoder::BlockType::Empty));
        // Explicitly trigger out of bounds memory access
        f.instruction(&I32Const(-1));
        f.instruction(&I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0, // There must exist at least one memory at index 0
        }));
        f.instruction(&Drop);
        f.instruction(&End);
        f.instruction(&LocalGet(0)); // restore offset
        if ty.is_some() {
            f.instruction(&LocalGet(1)); // restore value
        }
        f.instruction(&End);
        code_section.function(&f);
        Ok(())
    }
}

fn bounds_check_info(op: &Operator<'_>) -> Option<(u32, u64, i32, Option<EncoderValType>)> {
    use Operator::*;
    match op {
        I32Load8S { memarg }
        | I32Load8U { memarg }
        | I64Load8S { memarg }
        | I64Load8U { memarg } => Some((memarg.memory, memarg.offset, 1, None)),
        I32Load16S { memarg }
        | I32Load16U { memarg }
        | I64Load16S { memarg }
        | I64Load16U { memarg } => Some((memarg.memory, memarg.offset, 2, None)),
        I32Load { memarg } | F32Load { memarg } | I64Load32S { memarg } | I64Load32U { memarg } => {
            Some((memarg.memory, memarg.offset, 4, None))
        }
        I64Load { memarg } | F64Load { memarg } => Some((memarg.memory, memarg.offset, 8, None)),
        I32Store8 { memarg } => Some((memarg.memory, memarg.offset, 1, Some(EncoderValType::I32))),
        I32Store16 { memarg } => Some((memarg.memory, memarg.offset, 2, Some(EncoderValType::I32))),
        I32Store { memarg } => Some((memarg.memory, memarg.offset, 4, Some(EncoderValType::I32))),
        I64Store8 { memarg } => Some((memarg.memory, memarg.offset, 1, Some(EncoderValType::I64))),
        I64Store16 { memarg } => Some((memarg.memory, memarg.offset, 2, Some(EncoderValType::I64))),
        I64Store32 { memarg } => Some((memarg.memory, memarg.offset, 4, Some(EncoderValType::I64))),
        I64Store { memarg } => Some((memarg.memory, memarg.offset, 8, Some(EncoderValType::I64))),
        F32Store { memarg } => Some((memarg.memory, memarg.offset, 4, Some(EncoderValType::F32))),
        F64Store { memarg } => Some((memarg.memory, memarg.offset, 8, Some(EncoderValType::F64))),
        _ => None,
    }
}

// Slot represents anywhere that a value can go, e.g. a global, local, or stack operand.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Slot {
    pub ty: Option<ValType>,
}

// A SlotUse represents any input from, or output to, a slot (identified by its global index).
// We use a somewhat fancy representation so that Option<SlotUse> (representing the unknown
// params to an untyped operator, or a valid operator after unreachable) can fit in 4 bytes.
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct SlotUse(NonZeroU32);

impl SlotUse {
    pub fn new(idx: usize) -> Self {
        let idx_as_u32: u32 = idx.try_into().unwrap();
        let inner = NonZeroU32::new(!idx_as_u32).unwrap();
        Self(inner)
    }

    pub fn u32(&self) -> u32 {
        !self.0.get()
    }

    pub fn usize(&self) -> usize {
        self.u32().try_into().unwrap()
    }

    pub fn i32(&self) -> i32 {
        self.u32().try_into().unwrap()
    }
}

impl Debug for SlotUse {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "SlotUse({})", self.u32())
    }
}

#[test]
fn assert_slot_use_small() {
    assert_eq!(core::mem::size_of::<Option<SlotUse>>(), 4);
}

// An Expression represents a collection of Slots that are initiatialized on entry and restored on exit.
// E.g. local Slots live in an Expression for the function's entire body, and stack operands
// live in an Expression that might be smaller than the function's body.
// pub struct Expression(Vec<usize>);

#[derive(Debug, PartialEq, Eq)]
pub struct OperatorType {
    pub inputs: Vec<SlotUse>,
    pub outputs: Vec<SlotUse>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypedFunction {
    pub first_slot: SlotUse,
    pub params: Vec<SlotUse>,
    pub locals: Vec<SlotUse>,
    pub ops: Vec<OperatorType>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypedModule {
    pub slots: Vec<Slot>,
    pub blocks: Vec<(SlotUse, SlotUse)>,
    pub globals: Vec<SlotUse>,
    pub funcs: Vec<TypedFunction>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Coordinate {
    pub position_id: u32,
    pub operand_num: usize,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct SlotConnection {
    pub written: Option<Coordinate>,
    pub read: Option<Coordinate>,
}

#[derive(Default, Debug)]
pub struct SlotConnections {
    pub connections: Vec<SlotConnection>, // same index space as Slot #
    pub first_slot_of_func: Vec<SlotUse>, // indexed by "local" function idx (not including func imports)
    pub blocks: Vec<(SlotUse, SlotUse)>,
    pub bad_connections: Vec<(Coordinate, Coordinate)>,
}

impl SlotConnections {
    pub fn slot_range(&self, local_func_idx: u32) -> Range<usize> {
        let lower_limit = self.first_slot_of_func[local_func_idx as usize];
        let upper_limit = self
            .first_slot_of_func
            .get(local_func_idx as usize + 1)
            .copied()
            .unwrap_or_else(|| SlotUse::new(self.connections.len()));
        Range {
            start: lower_limit.usize(),
            end: upper_limit.usize(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct SlotInfo {
    pub slot: Slot,
    pub used: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AnnotatedOperatorType {
    pub inputs: Vec<SlotInfo>,
    pub outputs: Vec<SlotInfo>,
}

pub fn find_connections(module: &ValidModule, tys: &TypedModule) -> SlotConnections {
    let mut cx = SlotConnections {
        connections: vec![
            SlotConnection {
                written: None,
                read: None
            };
            tys.slots.len()
        ],
        first_slot_of_func: Vec::with_capacity(tys.funcs.len()),
        blocks: tys.blocks.clone(),
        bad_connections: vec![],
    };

    // locate globals
    for (Aligned { position_id, .. }, slot_idx) in zip_eq(&module.globals, &tys.globals) {
        cx.connections[slot_idx.usize()].written = Some(Coordinate {
            position_id: *position_id,
            operand_num: 0,
        });
    }

    for (func, func_tys) in zip_eq(&module.functions, &tys.funcs) {
        cx.first_slot_of_func.push(func_tys.first_slot);

        // locate params
        for (operand_num, slot_idx) in func_tys.params.iter().enumerate() {
            cx.connections[slot_idx.usize()].read = Some(Coordinate {
                position_id: func.positions.0,
                operand_num,
            });
        }

        // locate locals
        let mut local_counter = (None, 0);
        for (Aligned { position_id, .. }, slot_idx) in zip_eq(&func.locals, &func_tys.locals) {
            if local_counter.0 == Some(*position_id) {
                local_counter.1 += 1;
            } else {
                local_counter.0 = Some(*position_id);
                local_counter.1 = 0;
            }
            cx.connections[slot_idx.usize()].written = Some(Coordinate {
                position_id: *position_id,
                operand_num: local_counter.1,
            });
        }

        for (Aligned { position_id, .. }, OperatorType { inputs, outputs }) in
            zip_eq(&func.operators, &func_tys.ops)
        {
            for (operand_num, idx) in outputs.iter().enumerate() {
                cx.connections[idx.usize()].written = Some(Coordinate {
                    position_id: *position_id,
                    operand_num,
                });
            }

            for (operand_num, idx) in inputs.iter().enumerate() {
                cx.connections[idx.usize()].read = Some(Coordinate {
                    position_id: *position_id,
                    operand_num,
                });
            }
        }

        // connect "bad connections"
        let mut stranded: Vec<Coordinate> = Vec::new();
        let mut accessible_heights = vec![0];
        for (
            Aligned {
                position_id,
                inner: GeneralOperator { op, .. },
                ..
            },
            OperatorType { inputs, outputs },
        ) in zip_eq(&func.operators, &func_tys.ops)
        {
            for (operand_num, slot) in inputs.iter().enumerate().rev() {
                if cx.connections[slot.usize()].written.is_none()
                    && stranded.len() > *accessible_heights.last().unwrap()
                    && let Some(where_written) = stranded.pop()
                {
                    cx.bad_connections.push((
                        where_written,
                        Coordinate {
                            position_id: *position_id,
                            operand_num,
                        },
                    ));
                }
            }

            match op {
                Operator::Block { .. } | Operator::Loop { .. } | Operator::If { .. } => {
                    accessible_heights.push(stranded.len())
                }
                Operator::End => stranded.truncate(accessible_heights.pop().unwrap()),
                Operator::Else => stranded.truncate(*accessible_heights.last().unwrap()),
                _ => (),
            }

            for (operand_num, idx) in outputs.iter().enumerate() {
                if cx.connections[idx.usize()].read.is_none() {
                    stranded.push(Coordinate {
                        position_id: *position_id,
                        operand_num,
                    });
                }
            }
        }
    }

    cx
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

pub const FRAME_MARGIN: u16 = 3;
pub const BLOCK_BOUNDARY_INDENT: u16 = 3;

pub fn indent_and_frame(code: &mut impl FrameInfosMut, module: &ValidModule, types: &TypedModule) {
    assert!(code.len() > 0);

    struct OpenFrame {
        end: usize,
        synthetic: bool,
        indent: u16,
        slots: Vec<SlotUse>,
    }

    let mut frames: Vec<FrameInfo> = module
        .functions
        .iter()
        .map(|ValidFunction { lines, .. }| FrameInfo {
            indent: 0,
            start: lines.0,
            end: lines.1,
            unclosed: false,
            kind: InstrKind::OtherStructured,
            wide: !code
                .info(lines.0)
                .synthetic_before
                .module_field_syntax
                .is_empty(),
        })
        .collect();
    let mut frame_stack: Vec<OpenFrame> = Vec::new();

    let mut func_ops_rev = module
        .functions
        .iter()
        .rev()
        .flat_map(|func| func.operators.iter().rev())
        .zip_eq(
            types
                .funcs
                .iter()
                .rev()
                .flat_map(|func| func.ops.iter().rev()),
        )
        .peekable();

    let mut slot_map: HashMap<SlotUse, u16> = HashMap::new(); // unfilled slot -> indent
    let mut paren_indent: i32 = 0;

    fn process_outputs(map: &mut HashMap<SlotUse, u16>, outs: &Vec<SlotUse>) {
        /* resolve dependencies */
        for idx in outs {
            map.remove(idx);
        }
    }

    fn process_inputs(
        map: &mut HashMap<SlotUse, u16>,
        ins: &[SlotUse],
        indent: u16,
        frame_stack: &mut [OpenFrame],
    ) {
        /* insert dependencies */
        for idx in ins {
            debug_assert!(!map.contains_key(idx));
            map.insert(*idx, indent);
            if let Some(f) = frame_stack.last_mut() {
                f.slots.push(*idx);
            }
        }
    }

    for line_no in (0..code.len()).rev() {
        let (active, line_kind, ends_before, paren_depths) = {
            let l = code.info(line_no);
            (
                l.is_active(),
                l.kind.stripped_clone(),
                l.synthetic_before.end_opcodes,
                l.paren_depths(),
            )
        };

        /* compute indent */
        let frame_indent = match frame_stack.last() {
            Some(OpenFrame { indent, .. }) => *indent,
            None => 0,
        };

        let margined_indent = frame_indent + if frame_indent == 0 { 0 } else { FRAME_MARGIN };
        let mut instr_indent: u16 = max(
            margined_indent,
            slot_map.values().max().copied().unwrap_or(0) + 1,
        );
        let mut paren_indent_above = paren_indent;

        paren_indent -= paren_depths.1;
        paren_indent_above -= paren_depths.0;
        paren_indent_above -= paren_depths.1;

        let default_indent: u16 = max(
            match frame_stack.last() {
                Some(OpenFrame { indent, .. }) => *indent + 1,
                None => 0,
            },
            if let LineKind::Other(_) = line_kind {
                paren_indent.try_into().unwrap()
            } else if paren_indent == 0 {
                0
            } else {
                max(instr_indent, paren_indent.try_into().unwrap())
            },
        );

        if !active {
            code.set_indent(line_no, default_indent);
            continue;
        }

        match line_kind {
            LineKind::Instr(kind) => {
                let typed_op = func_ops_rev.next().expect("next operator");
                debug_assert_eq!(typed_op.0.line_idx, line_no);
                debug_assert!(typed_op.0.inner.info == OpInfo::Normal);

                match kind {
                    InstrKind::End => {
                        debug_assert_eq!(typed_op.0.inner.op, Operator::End);
                        frame_stack.push(OpenFrame {
                            end: line_no,
                            synthetic: false,
                            indent: instr_indent,
                            slots: vec![],
                        });

                        /* process synthetic else if present */
                        if let Some((
                            Aligned {
                                line_idx,
                                position_id,
                                inner,
                            },
                            OperatorType { inputs, outputs },
                        )) = func_ops_rev.peek()
                            && *line_idx == line_no
                        {
                            debug_assert_eq!(inner.op, Operator::Else);
                            debug_assert_eq!(inner.info, OpInfo::SyntheticElse);
                            debug_assert_eq!(*position_id, typed_op.0.position_id);
                            debug_assert_eq!(*position_id, code.info(line_no).id);
                            process_outputs(&mut slot_map, outputs);
                            process_inputs(&mut slot_map, inputs, instr_indent, &mut frame_stack);
                            func_ops_rev.next();
                        }
                        instr_indent += FRAME_MARGIN;
                    }
                    InstrKind::If | InstrKind::Loop | InstrKind::OtherStructured => {
                        debug_assert!(matches!(
                            typed_op.0.inner.op,
                            Operator::Block { .. } | Operator::Loop { .. } | Operator::If { .. }
                        ));
                        let f = frame_stack.pop().expect("malformed frame stack");
                        frames.push(FrameInfo {
                            indent: f.indent,
                            start: line_no,
                            end: f.end,
                            unclosed: f.synthetic,
                            kind,
                            wide: false,
                        });
                        instr_indent = f.indent + BLOCK_BOUNDARY_INDENT;
                        process_outputs(&mut slot_map, &f.slots); // clear open slots
                    }
                    InstrKind::Else => {
                        debug_assert!(typed_op.0.inner.op == Operator::Else);
                        let f = frame_stack.pop().expect("malformed frame stack");
                        frames.push(FrameInfo {
                            indent: f.indent,
                            start: line_no,
                            end: f.end,
                            unclosed: f.synthetic,
                            kind,
                            wide: false,
                        });
                        process_outputs(&mut slot_map, &f.slots); // clear open slots
                        frame_stack.push(OpenFrame {
                            end: line_no,
                            synthetic: false,
                            indent: f.indent,
                            slots: vec![],
                        });
                        instr_indent = f.indent + BLOCK_BOUNDARY_INDENT;
                    }
                    InstrKind::Other => {
                        debug_assert!(!matches!(
                            typed_op.0.inner.op,
                            Operator::Block { .. }
                                | Operator::Loop { .. }
                                | Operator::If { .. }
                                | Operator::Else
                                | Operator::End
                        ));
                    }
                }

                process_outputs(&mut slot_map, &typed_op.1.outputs);
                process_inputs(
                    &mut slot_map,
                    &typed_op.1.inputs,
                    instr_indent,
                    &mut frame_stack,
                );
                code.set_indent(line_no, instr_indent);
            }
            LineKind::Malformed(_) => {
                debug_assert!(ends_before == 0);
                code.set_indent(line_no, default_indent);
            }
            LineKind::Empty | LineKind::Other(_) => {
                /* not an instruction */

                /* process function end */
                if let Some((
                    Aligned {
                        inner:
                            GeneralOperator {
                                info: OpInfo::FuncEnd,
                                ..
                            },
                        line_idx,
                        position_id,
                    },
                    OperatorType { inputs, .. },
                )) = func_ops_rev.peek()
                    && *line_idx == line_no
                {
                    debug_assert_eq!(*position_id, code.info(line_no).id);
                    debug_assert!(frame_stack.is_empty());
                    slot_map.clear();
                    process_inputs(&mut slot_map, inputs, 0, &mut frame_stack);
                    func_ops_rev.next();
                }

                /* handle synthetic ends-of-frames */
                for _ in 0..ends_before {
                    let frame_indent = match frame_stack.last() {
                        Some(OpenFrame { indent, .. }) => *indent,
                        None => 0,
                    };

                    frame_stack.push(OpenFrame {
                        end: line_no,
                        synthetic: true,
                        indent: frame_indent + 1,
                        slots: vec![],
                    });

                    let next_synthetic_end = func_ops_rev.next().expect("next synthetic end");
                    debug_assert_eq!(next_synthetic_end.0.inner.op, Operator::End);
                    debug_assert_eq!(next_synthetic_end.0.line_idx, line_no);
                    process_outputs(&mut slot_map, &next_synthetic_end.1.outputs);
                    process_inputs(
                        &mut slot_map,
                        &next_synthetic_end.1.inputs,
                        0,
                        &mut frame_stack,
                    );

                    /* discard prepended else if present */
                    if let Some((
                        Aligned {
                            line_idx,
                            position_id,
                            inner,
                        },
                        OperatorType { inputs, outputs },
                    )) = func_ops_rev.peek()
                        && *line_idx == line_no
                        && inner.info == OpInfo::SyntheticElse
                    {
                        debug_assert_eq!(*position_id, code.info(line_no).id);
                        debug_assert_eq!(inner.op, Operator::Else);
                        process_outputs(&mut slot_map, outputs);
                        process_inputs(&mut slot_map, inputs, 0, &mut frame_stack);
                        func_ops_rev.next();
                    }
                }

                code.set_indent(line_no, default_indent);
            }
        }

        paren_indent = paren_indent_above;
    }

    debug_assert!(func_ops_rev.next().is_none());

    code.set_frames(frames);
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
    use crate::debug::{
        CallFrameOp, ExecutionState, RunLog, SlotContents, TerminationType, WasmValue,
    };
    use regex::Regex;
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

        fn ins(inputs: Vec<usize>) -> OperatorType {
            OperatorType {
                inputs: inputs.into_iter().map(SlotUse::new).collect(),
                outputs: Vec::new(),
            }
        }

        fn outs(outputs: Vec<usize>) -> OperatorType {
            OperatorType {
                inputs: Vec::new(),
                outputs: outputs.into_iter().map(SlotUse::new).collect(),
            }
        }

        fn inout(inputs: Vec<usize>, outputs: Vec<usize>) -> OperatorType {
            OperatorType {
                inputs: inputs.into_iter().map(SlotUse::new).collect(),
                outputs: outputs.into_iter().map(SlotUse::new).collect(),
            }
        }

        fn os(ty: ValType) -> Slot {
            Slot { ty: Some(ty) }
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
                    type_idx: func.type_idx,
                    lines: func.lines,
                    positions: func.positions,
                    params: func.params,
                    locals: func.locals,
                    operators: vec![],
                };
                for op in func.operators {
                    valid.operators.push(op.into());
                }
                assert_eq!(valid.operators.last().unwrap().inner.op, Operator::End);
                valid.operators.last_mut().unwrap().inner.info = OpInfo::FuncEnd;
                ret.functions.push(valid);
            }

            ret
        }

        //block instruction with params and results
        let output = TypedModule {
            slots: vec![os(I32), os(I32), os(I32), os(I32), os(I32), os(I32)],
            blocks: vec![(SlotUse::new(2), SlotUse::new(6))],
            globals: vec![],
            funcs: vec![TypedFunction {
                first_slot: SlotUse::new(0),
                params: vec![],
                locals: vec![],
                ops: vec![
                    outs(vec![0]),
                    outs(vec![1]),
                    inout(vec![0, 1], vec![2, 3]),
                    inout(vec![2, 3], vec![4]),
                    inout(vec![4], vec![5]),
                    ins(vec![5]),
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
                lines: (0, 7),
                positions: (0, 0),
                params: vec![],
                locals: vec![],
                operators: vec![
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 1 },
                        line_idx: 0,
                        position_id: 0,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 2 },
                        line_idx: 1,
                        position_id: 1,
                    },
                    CodillonInstruction {
                        inner: Operator::Block {
                            blockty: BlockType::FuncType(1),
                        },
                        line_idx: 2,
                        position_id: 2,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Add,
                        line_idx: 3,
                        position_id: 3,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 4,
                        position_id: 4,
                    },
                    CodillonInstruction {
                        inner: Operator::Drop,
                        line_idx: 5,
                        position_id: 5,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 6,
                        position_id: 6,
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
            blocks: vec![(SlotUse::new(1), SlotUse::new(4))],
            globals: vec![],
            funcs: vec![TypedFunction {
                first_slot: SlotUse::new(0),
                params: vec![],
                locals: vec![],
                ops: vec![
                    outs(vec![0]),
                    ins(vec![0]),
                    outs(vec![1]),
                    ins(vec![1]),
                    outs(vec![2]),
                    inout(vec![2], vec![3]),
                    ins(vec![3]),
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
                lines: (0, 8),
                positions: (0, 0),
                params: vec![],
                locals: vec![],
                operators: vec![
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 1 },
                        line_idx: 0,
                        position_id: 0,
                    },
                    CodillonInstruction {
                        inner: Operator::If {
                            blockty: BlockType::Type(I32),
                        },
                        line_idx: 1,
                        position_id: 1,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 1 },
                        line_idx: 2,
                        position_id: 2,
                    },
                    CodillonInstruction {
                        inner: Operator::Else,
                        line_idx: 3,
                        position_id: 3,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 2 },
                        line_idx: 4,
                        position_id: 4,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 5,
                        position_id: 5,
                    },
                    CodillonInstruction {
                        inner: Operator::Drop,
                        line_idx: 6,
                        position_id: 6,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 7,
                        position_id: 7,
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
            blocks: vec![(SlotUse::new(1), SlotUse::new(6))],
            globals: vec![],
            funcs: vec![TypedFunction {
                first_slot: SlotUse::new(0),
                params: vec![],
                locals: vec![],
                ops: vec![
                    outs(vec![0]),
                    inout(vec![0], vec![1]),
                    outs(vec![2]),
                    inout(vec![1, 2], vec![3]),
                    ins(vec![3]),
                    outs(vec![4]),
                    inout(vec![4], vec![5]),
                    ins(vec![5]),
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
                lines: (0, 9),
                positions: (0, 0),
                params: vec![],
                locals: vec![],
                operators: vec![
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 10 },
                        line_idx: 0,
                        position_id: 0,
                    },
                    CodillonInstruction {
                        inner: Operator::Loop {
                            blockty: BlockType::FuncType(1),
                        },
                        line_idx: 1,
                        position_id: 1,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 1 },
                        line_idx: 2,
                        position_id: 2,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Sub,
                        line_idx: 3,
                        position_id: 3,
                    },
                    CodillonInstruction {
                        inner: Operator::BrIf { relative_depth: 1 },
                        line_idx: 4,
                        position_id: 4,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 2 },
                        line_idx: 5,
                        position_id: 5,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 6,
                        position_id: 6,
                    },
                    CodillonInstruction {
                        inner: Operator::Drop,
                        line_idx: 7,
                        position_id: 7,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 8,
                        position_id: 8,
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
            blocks: vec![
                (SlotUse::new(2), SlotUse::new(5)),
                (SlotUse::new(1), SlotUse::new(6)),
            ],
            globals: vec![],
            funcs: vec![TypedFunction {
                first_slot: SlotUse::new(0),
                params: vec![],
                locals: vec![],
                ops: vec![
                    outs(vec![0]),
                    inout(vec![0], vec![1]),
                    ins(vec![1]),
                    outs(vec![2]),
                    ins(vec![2]),
                    outs(vec![3]),
                    inout(vec![3], vec![4]),
                    inout(vec![4], vec![5]),
                    ins(vec![5]),
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
                lines: (0, 10),
                positions: (0, 0),
                params: vec![],
                locals: vec![],
                operators: vec![
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 10 },
                        line_idx: 0,
                        position_id: 0,
                    },
                    CodillonInstruction {
                        inner: Operator::Block {
                            blockty: BlockType::FuncType(1),
                        },
                        line_idx: 1,
                        position_id: 1,
                    },
                    CodillonInstruction {
                        inner: Operator::If {
                            blockty: BlockType::Type(I32),
                        },
                        line_idx: 2,
                        position_id: 2,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 1 },
                        line_idx: 3,
                        position_id: 3,
                    },
                    CodillonInstruction {
                        inner: Operator::Else,
                        line_idx: 4,
                        position_id: 4,
                    },
                    CodillonInstruction {
                        inner: Operator::I32Const { value: 2 },
                        line_idx: 5,
                        position_id: 5,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 6,
                        position_id: 6,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 7,
                        position_id: 7,
                    },
                    CodillonInstruction {
                        inner: Operator::Drop,
                        line_idx: 8,
                        position_id: 8,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 9,
                        position_id: 9,
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
            blocks: vec![(SlotUse::new(0), SlotUse::new(0))],
            globals: vec![],
            funcs: vec![TypedFunction {
                first_slot: SlotUse::new(0),
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
                lines: (0, 2),
                positions: (0, 0),
                params: vec![],
                locals: vec![],
                operators: vec![
                    CodillonInstruction {
                        inner: Operator::Block {
                            blockty: BlockType::Empty,
                        },
                        line_idx: 0,
                        position_id: 0,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 1,
                        position_id: 1,
                    },
                    CodillonInstruction {
                        inner: Operator::End,
                        line_idx: 2,
                        position_id: 2,
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
        fn test_mem(valid_module: ValidModule, mem_value: wasmparser::MemoryType) -> Result<()> {
            let mut mem_exists = false;
            let wasm_bin = valid_module.build_instrumented_binary(&TypedModule {
                slots: vec![],
                blocks: vec![],
                globals: vec![],
                funcs: vec![],
            })?;
            let mem_payload = wasmparser::Parser::new(0)
                .parse_all(&wasm_bin)
                .nth(4)
                .expect("failed to get memory payload");
            if let wasmparser::Payload::MemorySection(reader) = mem_payload? {
                assert_eq!(
                    reader.into_iter().next().expect("failed to get memory")?,
                    mem_value
                );
                mem_exists = true;
            }
            assert!(mem_exists);
            Ok(())
        }
        let mut mem_value = wasmparser::MemoryType {
            memory64: false,
            shared: false,
            initial: 3,
            maximum: Some(5),
            page_size_log2: Some(16),
        };
        // Test 65536 pagesize
        {
            let valid_module = ValidModule {
                types: vec![empty_ft()],
                imports: vec![],
                memory: vec![mem_value],
                globals: vec![],
                functions: vec![],
            };
            test_mem(valid_module, mem_value)?;
        }
        // test 1 page size
        {
            mem_value.page_size_log2 = Some(0);
            let valid_module = ValidModule {
                types: vec![empty_ft()],
                imports: vec![],
                memory: vec![mem_value],
                globals: vec![],
                functions: vec![],
            };
            mem_value.initial = 1;
            mem_value.maximum = None;
            mem_value.page_size_log2 = None;
            test_mem(valid_module, mem_value)?;
        }

        Ok(())
    }

    use crate::line::{Activity, LineInfo};
    use crate::symbolic::parse_line_symbols;
    use crate::syntax::{SyntheticWasm, fix_syntax, parse_line};

    #[derive(Default, Debug)]
    struct FakeTextLine {
        instr_text: String,
        info: LineInfo,
    }

    #[derive(Default, Debug)]
    struct FakeTextBuffer {
        lines: Vec<FakeTextLine>,
    }

    impl FakeTextLine {
        fn new(s: &str, id: u32) -> Self {
            let kind = parse_line(s);
            let symbols = parse_line_symbols(s, &kind);
            Self {
                instr_text: String::from(s),
                info: LineInfo {
                    kind,
                    id,
                    symbols,
                    ..Default::default()
                },
            }
        }
    }

    impl FakeTextBuffer {
        fn push_line(&mut self, string: &str) {
            self.lines
                .push(FakeTextLine::new(string, self.lines.len() as u32))
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

        fn set_runtime_error(&mut self, _index: usize, _msg: Option<String>) {}
    }

    impl FrameInfosMut for FakeTextBuffer {
        fn set_indent(&mut self, _index: usize, _num: u16) {}
        fn set_frames(&mut self, _frames: Vec<FrameInfo>) {}
    }

    struct EditorOutput {
        text: String,
        binary: Vec<u8>,
        connections: SlotConnections,
    }

    fn test_editor_flow(editor: &mut FakeTextBuffer) -> Result<EditorOutput> {
        use anyhow::Context;

        fix_syntax(editor);

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
        let raw_module = RawModule::new(editor, &wasm_bin).context("RawModule::new")?;
        let validized = raw_module
            .fix_validity(editor, &wasm_bin)
            .context("fix_validity")?;

        let types = validized
            .to_types_table(&wasm_bin)
            .context("to_types_table")?;

        indent_and_frame(editor, &validized, &types);

        let connections = find_connections(&validized, &types);

        let instrumented_binary = validized
            .build_instrumented_binary(&types)
            .context("build_executable_binary")?;

        let instrumented_text =
            wasmprinter::print_bytes(&instrumented_binary).context("print_bytes")?;

        wasmparser::validate(&instrumented_binary).context(format!(
            "validate {:?} -> {instrumented_text}",
            editor.lines
        ))?;

        let re = Regex::new(r"\(;\d+;\) ")?; // remove wasmprinter idx comments to let tests evolve more easily
        Ok(EditorOutput {
            text: re.replace_all(&instrumented_text, "").to_string(),
            binary: instrumented_binary,
            connections,
        })
    }

    fn test_execution(
        binary: &[u8],
        params: &[wasmtime::Val],
        results: &mut [wasmtime::Val],
    ) -> Result<RunLog> {
        use wasmtime::*;

        let engine = Engine::default();
        let module = Module::new(&engine, binary)?;
        let mut store = Store::new(&engine, RunLog::default());
        let mut linker = Linker::new(&engine);

        type Ctx<'a> = Caller<'a, RunLog>;

        // Match the imports provided in make_imports() (excluding graphics for now)
        linker.func_wrap("codillon_debug", "record_step", |ctx: Ctx<'_>, n| -> i32 {
            ctx.data().wasm_step(n).into()
        })?;
        linker.func_wrap("codillon_debug", "record_invalid", |ctx: Ctx<'_>| {
            ctx.data().set_termination_type(TerminationType::HitInvalid)
        })?;
        linker.func_wrap("codillon_debug", "record_i32", |ctx: Ctx<'_>, v: i32, s| {
            ctx.data().record_slot(v, s)
        })?;
        linker.func_wrap("codillon_debug", "record_f32", |ctx: Ctx<'_>, v: f32, s| {
            ctx.data().record_slot(v, s)
        })?;
        linker.func_wrap("codillon_debug", "record_i64", |ctx: Ctx<'_>, v: i64, s| {
            ctx.data().record_slot(v, s)
        })?;
        linker.func_wrap("codillon_debug", "record_f64", |ctx: Ctx<'_>, v: f64, s| {
            ctx.data().record_slot(v, s)
        })?;
        linker.func_wrap("codillon_debug", "enter_func", |ctx: Ctx<'_>, func_idx| {
            ctx.data().call_frame_op(CallFrameOp::EnterFunc(func_idx));
        })?;
        linker.func_wrap("codillon_debug", "before_call", |ctx: Ctx<'_>| {
            ctx.data().call_frame_op(CallFrameOp::BeforeCall);
        })?;
        linker.func_wrap("codillon_debug", "after_call", |ctx: Ctx<'_>, func_idx| {
            ctx.data().call_frame_op(CallFrameOp::AfterCall(func_idx));
        })?;
        linker.func_wrap("codillon_debug", "before_tail_call", |ctx: Ctx<'_>| {
            ctx.data().call_frame_op(CallFrameOp::BeforeTailCall);
        })?;
        linker.func_wrap(
            "codillon_debug",
            "enter_block",
            |ctx: Ctx<'_>, block_idx| {
                ctx.data().call_frame_op(CallFrameOp::EnterBlock(block_idx));
            },
        )?;

        let instance = linker.instantiate(&mut store, &module)?;
        let main = instance.get_func(&mut store, "main").expect("main export");
        let res = main.call(&mut store, params, results);
        store.data().finish(res.map_err(|x| x.to_string()))?;

        Ok(store.into_data())
    }

    const EXPECTED_TYPES: &str = r#"  (type (func))
  (type (func (param i32) (result i32)))
  (type (func))
  (type (func (param i32 i32)))
  (type (func (param f32 i32)))
  (type (func (param i64 i32)))
  (type (func (param f64 i32)))
  (type (func (param i32)))
"#;
    const EXPECTED_IMPORTS: &str = r#"  (import "codillon_debug" "record_step" (func (type 1)))
  (import "codillon_debug" "record_invalid" (func (type 2)))
  (import "codillon_debug" "record_i32" (func (type 3)))
  (import "codillon_debug" "record_f32" (func (type 4)))
  (import "codillon_debug" "record_i64" (func (type 5)))
  (import "codillon_debug" "record_f64" (func (type 6)))
  (import "codillon_debug" "enter_func" (func (type 7)))
  (import "codillon_debug" "before_call" (func (type 2)))
  (import "codillon_debug" "after_call" (func (type 7)))
  (import "codillon_debug" "before_tail_call" (func (type 2)))
  (import "codillon_debug" "enter_block" (func (type 7)))
"#;

    const EXPECTED_MAIN: &str = r#"  (export "main" (func 11))
"#;

    const EXPECTED_STEP: &str = r#"  (func (type 7) (param i32)
    local.get 0
    call 0
    i32.eqz
    if ;; label = @1
      unreachable
    end
  )
"#;
    const EXPECTED_RECORD_I32: &str = r#"  (func (type 8) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call 2
    local.get 0
  )
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
                + EXPECTED_MAIN
                + r#"  (func (type 0)
    i32.const 0
    call 6
    i32.const 0
    call 12
    i32.const 0
    call 12
  )
"# + EXPECTED_STEP
                + ")\n";

            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::Success, log.termination_type());
        }

        // syntax error (synthesizes closing paren)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            let expected = String::from("(module\n")
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + EXPECTED_MAIN
                + r#"  (func (type 0)
    i32.const 0
    call 6
    i32.const 0
    call 12
    i32.const 1
    call 12
  )
"# + EXPECTED_STEP
                + ")\n";
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::Success, log.termination_type());
        }

        // syntax error (synthesizes closing "func)" on line 1)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(");
            let expected = String::from("(module\n")
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + EXPECTED_MAIN
                + r#"  (func (type 0)
    i32.const 0
    call 6
    i32.const 1
    call 12
    i32.const 1
    call 12
  )
"# + EXPECTED_STEP
                + ")\n";
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::Success, log.termination_type());
        }

        // syntax error (first operator is `else`)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("else");
            let expected = String::from("(module\n")
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + EXPECTED_MAIN
                + r#"  (func (type 0)
    i32.const 0
    call 6
    i32.const 0
    call 12
    i32.const 1
    call 12
  )
"# + EXPECTED_STEP
                + ")\n";
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::Success, log.termination_type());
        }

        // syntax error (missing (func) field but has instructions)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("i64.const 17");
            editor.push_line("drop");
            let expected = String::from("(module\n")
                + EXPECTED_TYPES
                + "  (type (func (param i64 i32) (result i64)))\n"
                + EXPECTED_IMPORTS
                + EXPECTED_MAIN
                + r#"  (func (type 0)
    i32.const 0
    call 6
    i32.const 0
    call 12
    i32.const 0
    call 12
    i64.const 17
    i32.const 0
    call 13
    i32.const 1
    call 12
    drop
    i32.const 2
    call 12
  )
"# + EXPECTED_STEP
                + r#"  (func (type 8) (param i64 i32) (result i64)
    local.get 0
    local.get 1
    call 4
    local.get 0
  )
)
"#;
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::Success, log.termination_type());
        }

        // well-formed block
        const JUST_BLOCK_END: &str = r#"    i32.const 0
    call 6
    i32.const 0
    call 12
    i32.const 1
    call 12
    block ;; label = @1
      i32.const 2
      call 12
    end
    i32.const 3
    call 12
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
                + EXPECTED_MAIN
                + "  (func (type 0)\n"
                + JUST_BLOCK_END
                + EXPECTED_STEP
                + ")\n";
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::Success, log.termination_type());
        }

        // syntax error (missing `end`) -- should be similar to well-formed block
        // (with different line # for the function end, and now invalid on synthetic end)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("block");
            editor.push_line(")");
            let expected = String::from("(module\n")
                + EXPECTED_TYPES
                + EXPECTED_IMPORTS
                + EXPECTED_MAIN
                + "  (func (type 0)\n"
                + &JUST_BLOCK_END.replace("i32.const 3", "call 1\n    unreachable\n    i32.const 2") // different line # for function end
                + EXPECTED_STEP
                + ")\n";
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::HitInvalid, log.termination_type());
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
                + "  (type (func (param i32 i32) (result i32)))\n"
                + EXPECTED_IMPORTS
                + EXPECTED_MAIN
                + r#"  (func (type 0)
    i32.const 0
    call 6
    i32.const 0
    call 12
    i32.const 1
    call 12
    i32.const 137
    i32.const 0
    call 13
    i32.const 2
    call 12
    call 1
    unreachable
    i32.add
    i32.const 3
    call 13
    i32.const 3
    call 12
    drop
    i32.const 4
    call 12
  )
"# + EXPECTED_STEP
                + EXPECTED_RECORD_I32
                + ")\n";

            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::HitInvalid, log.termination_type());
        }

        // (func has inline import -- will deactivate the import)
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
                + "  (type (func (param i32 i32) (result i32)))\n"
                + EXPECTED_IMPORTS
                + EXPECTED_MAIN
                + r#"  (func (type 0)
    i32.const 0
    call 6
    i32.const 0
    call 13
    i32.const 2
    call 13
    i32.const 7
    i32.const 0
    call 14
    i32.const 3
    call 13
    i32.const 8
    i32.const 1
    call 14
    i32.const 4
    call 13
    i32.const 9
    i32.const 2
    call 14
    i32.const 5
    call 13
    call 1
    unreachable
  )
  (func (type 0)
    i32.const 1
    call 6
    i32.const 6
    call 13
    i32.const 6
    call 13
  )
"# + EXPECTED_STEP
                + r#"  (func (type 8) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call 2
    local.get 0
  )
)
"#;
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::HitInvalid, log.termination_type());
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
                + EXPECTED_MAIN
                + "  (func (type 0)\n"
                + &JUST_BLOCK_END.replace("i32.const 2\n", "i32.const 5\n")
                + EXPECTED_STEP
                + ")\n";
            let expected = expected.replace("i32.const 3", "i32.const 6");
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::Success, log.termination_type());
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
                + EXPECTED_MAIN
                + r#"  (func (type 0)
    i32.const 0
    call 6
    i32.const 0
    call 12
    i32.const 1
    call 12
    block ;; label = @1
      i32.const 2
      call 12
      loop ;; label = @2
        i32.const 1
        call 10
        i32.const 4
        call 12
      end
      i32.const 5
      call 12
    end
    i32.const 6
    call 12
  )
"# + EXPECTED_STEP
                + ")\n";
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::Success, log.termination_type());
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
                + EXPECTED_MAIN
                + r#"  (func (type 0)
    i32.const 0
    call 6
    i32.const 0
    call 12
    i32.const 1
    call 12
    call 1
    unreachable
    nop
    i32.const 2
    call 12
  )
"# + EXPECTED_STEP
                + ")\n";
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::HitInvalid, log.termination_type());
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
                + "  (memory 0)\n"
                + EXPECTED_MAIN
                + r#"  (func (type 0)
    i32.const 0
    call 6
    i32.const 1
    call 12
    i32.const 2
    call 12
    call 1
    unreachable
    nop
    i32.const 3
    call 12
    call 1
    unreachable
    drop
    i32.const 4
    call 12
  )
"# + EXPECTED_STEP
                + ")\n";
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::HitInvalid, log.termination_type());
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
                + "  (type (func (param i32 i32) (result i32)))\n"
                + EXPECTED_IMPORTS
                + EXPECTED_MAIN
                + r#"  (func (type 0)
    i32.const 0
    call 6
    i32.const 0
    call 12
    i32.const 0
    call 12
    loop ;; label = @1
      i32.const 0
      call 10
      i32.const 1
      call 12
      i32.const 5
      i32.const 0
      call 13
      i32.const 2
      call 12
      br 0 (;@1;)
      i32.const 3
      call 12
    end
    i32.const 4
    call 12
  )
"# + EXPECTED_STEP
                + EXPECTED_RECORD_I32
                + ")\n";

            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::TooManySteps, log.termination_type());
        }

        // validation error (operands not popped at end of block/function)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("i32.const 4");
            editor.push_line(")");
            let expected = String::from("(module\n")
                + EXPECTED_TYPES
                + "  (type (func (param i32 i32) (result i32)))\n"
                + EXPECTED_IMPORTS
                + EXPECTED_MAIN
                + r#"  (func (type 0)
    i32.const 0
    call 6
    i32.const 0
    call 12
    i32.const 1
    call 12
    i32.const 4
    i32.const 0
    call 13
    i32.const 2
    call 12
    call 1
    unreachable
  )
"# + EXPECTED_STEP
                + EXPECTED_RECORD_I32
                + ")\n";
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::HitInvalid, log.termination_type());
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
                + r#"  (type (func))
  (type (func (result i32 i32)))
"# + EXPECTED_TYPES
                + r#"  (type (func (param i32 i32 i32 i32) (result i32 i32)))
  (type (func (param i32 i32) (result i32)))
  (import "codillon_debug" "record_step" (func (type 2)))
  (import "codillon_debug" "record_invalid" (func (type 3)))
  (import "codillon_debug" "record_i32" (func (type 4)))
  (import "codillon_debug" "record_f32" (func (type 5)))
  (import "codillon_debug" "record_i64" (func (type 6)))
  (import "codillon_debug" "record_f64" (func (type 7)))
  (import "codillon_debug" "enter_func" (func (type 8)))
  (import "codillon_debug" "before_call" (func (type 3)))
  (import "codillon_debug" "after_call" (func (type 8)))
  (import "codillon_debug" "before_tail_call" (func (type 3)))
  (import "codillon_debug" "enter_block" (func (type 8)))
  (export "main" (func 11))
  (func (type 0)
    i32.const 0
    call 6
    i32.const 0
    call 13
    i32.const 1
    call 13
    call 7
    call 12
    i32.const 0
    i32.const 1
    call 14
    i32.const 0
    call 8
    i32.const -2147483647
    call 13
    i32.const 2
    call 13
    i32.add
    i32.const 2
    call 15
    i32.const 3
    call 13
    call 1
    unreachable
  )
  (func (type 1) (result i32 i32)
    i32.const 1
    call 6
    i32.const 4
    call 13
    i32.const 5
    call 13
    i32.const 9
    i32.const 3
    call 15
    i32.const 6
    call 13
    i32.const 10
    i32.const 4
    call 15
    i32.const 7
    call 13
    i32.const 5
    i32.const 6
    call 14
  )
  (func (type 8) (param i32)
    local.get 0
    call 0
    i32.eqz
    if ;; label = @1
      unreachable
    end
  )
  (func (type 9) (param i32 i32 i32 i32) (result i32 i32)
    local.get 0
    local.get 2
    call 2
    local.get 1
    local.get 3
    call 2
    local.get 0
    local.get 1
  )
  (func (type 10) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call 2
    local.get 0
  )
)
"#;
            let expected = expected.replace(
                "  (type (func))\n  (type (func (param i32) (result i32)))",
                "  (type (func (param i32) (result i32)))",
            );
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::HitInvalid, log.termination_type());
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
                + EXPECTED_MAIN
                + r#"  (func (type 0)
    (local i32 i32 i32 f64)
    i32.const 0
    call 6
    local.get 0
    i32.const 0
    call 2
    local.get 1
    i32.const 1
    call 2
    local.get 2
    i32.const 2
    call 2
    local.get 3
    i32.const 3
    call 5
    i32.const 0
    call 12
    i32.const 4
    call 12
  )
"# + EXPECTED_STEP
                + ")\n";
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::Success, log.termination_type());
        }

        // invalid block
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("block (param i32)");
            editor.push_line("end");
            editor.push_line(")");

            let expected = String::from("(module\n")
                + r#"  (type (func))
  (type (func (param i32)))
"# + EXPECTED_TYPES
                + r#"  (type (func (param i32 i32) (result i32)))
  (import "codillon_debug" "record_step" (func (type 2)))
  (import "codillon_debug" "record_invalid" (func (type 3)))
  (import "codillon_debug" "record_i32" (func (type 4)))
  (import "codillon_debug" "record_f32" (func (type 5)))
  (import "codillon_debug" "record_i64" (func (type 6)))
  (import "codillon_debug" "record_f64" (func (type 7)))
  (import "codillon_debug" "enter_func" (func (type 8)))
  (import "codillon_debug" "before_call" (func (type 3)))
  (import "codillon_debug" "after_call" (func (type 8)))
  (import "codillon_debug" "before_tail_call" (func (type 3)))
  (import "codillon_debug" "enter_block" (func (type 8)))
  (export "main" (func 11))
  (func (type 0)
    i32.const 0
    call 6
    i32.const 0
    call 12
    i32.const 1
    call 12
    call 1
    unreachable
    block (type 1) (param i32) ;; label = @1
      i32.const 1
      call 13
      i32.const 2
      call 12
      call 1
      unreachable
    end
    i32.const 3
    call 12
  )
  (func (type 8) (param i32)
    local.get 0
    call 0
    i32.eqz
    if ;; label = @1
      unreachable
    end
  )
  (func (type 9) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call 2
    local.get 0
  )
)
"#;
            let expected = expected.replace(
                "  (type (func))\n  (type (func (param i32) (result i32)))",
                "  (type (func (param i32) (result i32)))",
            );
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::HitInvalid, log.termination_type());
        }

        {
            // issue #168
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(");
            editor.push_line("(func");
            editor.push_line("(local $x i32)");
            editor.push_line("local.get $x");
            editor.push_line("drop");
            editor.push_line(")");
            test_editor_flow(&mut editor)?;
        }

        {
            // issue #191
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(");
            editor.push_line("(func");
            editor.push_line("(local $x i32) (local $y i32)");
            editor.push_line(")");
            test_editor_flow(&mut editor)?;
        }

        {
            // issue #220
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(memory $x 1)");
            editor.push_line("(memory $x 2)");
            editor.push_line("(global $x i32 (i32.const 0))");
            editor.push_line("(global $x i64 (i32.const 0))");
            editor.push_line("(func");
            editor.push_line("(param $x i32) (param $x i32)");
            editor.push_line("(param $x i32)");
            editor.push_line("(local $x f32)");
            editor.push_line(")");
            editor.push_line("(func (param $x i32) (param $x i32)");
            editor.push_line("(");
            editor.push_line("func (param $x i32) (param $x i32)");
            editor.push_line("func)");
            test_editor_flow(&mut editor)?;
        }

        {
            // issue #226 case 1
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("call $g");
            editor.push_line(")");
            editor.push_line("(func $g (param i32) (result i32))");
            test_editor_flow(&mut editor)?;
        }

        {
            // issue #226 case 2
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func $draw (import \"draw\" \"point\") (param f64 f64)");
            editor.push_line("(func $f");
            editor.push_line(")");
            editor.push_line("(func");
            editor.push_line("call $f");
            editor.push_line(")");
            test_editor_flow(&mut editor)?;
        }

        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("call $f");
            editor.push_line(")");
            editor.push_line("(func $f (import \"x\" \"y\"))");
            test_editor_flow(&mut editor)?;
        }

        {
            // end that does double duty (implicit else + end)
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("if (result i32)");
            editor.push_line("end");
            editor.push_line(")");
            test_editor_flow(&mut editor)?;
        }

        {
            // end that does double duty (implicit else + end) II
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("if (result i32)");
            editor.push_line("i32.const 4");
            editor.push_line("end");
            editor.push_line(")");
            test_editor_flow(&mut editor)?;
        }

        {
            // end that does double duty (implicit else + end) III
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("if (result i32 i64 f32)");
            editor.push_line("f32.const 4.0");
            editor.push_line("end");
            editor.push_line(")");
            test_editor_flow(&mut editor)?;
        }

        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("if (param f64) (result i64)");
            editor.push_line("end");
            editor.push_line(")");
            test_editor_flow(&mut editor)?;
        }

        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("i32.const 6");
            editor.push_line("if (param i64)");
            editor.push_line(")");
            test_editor_flow(&mut editor)?;
        }

        {
            // invalid select
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("select");
            editor.push_line(")");
            test_editor_flow(&mut editor)?;
        }

        {
            // unreachable select
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("unreachable");
            editor.push_line("select");
            editor.push_line(")");
            test_editor_flow(&mut editor)?;
        }

        {
            // issue #242
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("call $x");
            editor.push_line(")");
            editor.push_line("nop");
            editor.push_line("(func $x)");
            test_editor_flow(&mut editor)?;
        }

        {
            // bad connections
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func (result f64)");
            editor.push_line("i32.const 4");
            editor.push_line(")");
            let EditorOutput { connections, .. } = test_editor_flow(&mut editor)?;
            assert_eq!(
                connections.connections,
                vec![
                    SlotConnection {
                        written: Some(Coordinate {
                            position_id: 1,
                            operand_num: 0
                        }),
                        read: None
                    },
                    SlotConnection {
                        written: None,
                        read: Some(Coordinate {
                            position_id: 2,
                            operand_num: 0
                        })
                    },
                    SlotConnection {
                        written: Some(Coordinate {
                            position_id: 2,
                            operand_num: 0
                        }),
                        read: None
                    }
                ]
            );
            assert_eq!(connections.first_slot_of_func, vec![SlotUse::new(0)]);
            assert_eq!(
                connections.bad_connections,
                vec![(
                    Coordinate {
                        position_id: 1,
                        operand_num: 0
                    },
                    Coordinate {
                        position_id: 2,
                        operand_num: 0
                    }
                )]
            );
        }

        {
            // bad connections don't escape a block
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func (result f64)");
            editor.push_line("block");
            editor.push_line("i32.const 4");
            editor.push_line("end");
            editor.push_line(")");
            let EditorOutput { connections, .. } = test_editor_flow(&mut editor)?;
            assert_eq!(
                connections.connections,
                vec![
                    SlotConnection {
                        written: Some(Coordinate {
                            position_id: 2,
                            operand_num: 0
                        }),
                        read: None
                    },
                    SlotConnection {
                        written: None,
                        read: Some(Coordinate {
                            position_id: 4,
                            operand_num: 0
                        })
                    },
                    SlotConnection {
                        written: Some(Coordinate {
                            position_id: 4,
                            operand_num: 0
                        }),
                        read: None
                    }
                ]
            );
            assert_eq!(connections.first_slot_of_func, vec![SlotUse::new(0)]);
            assert!(connections.bad_connections.is_empty());
        }

        {
            // bad connections can span a block
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("f32.const 2");
            editor.push_line("block");
            editor.push_line("end");
            editor.push_line("i32.eqz");
            editor.push_line(")");
            let EditorOutput { connections, .. } = test_editor_flow(&mut editor)?;
            assert_eq!(
                connections.bad_connections,
                vec![(
                    Coordinate {
                        position_id: 1,
                        operand_num: 0
                    },
                    Coordinate {
                        position_id: 4,
                        operand_num: 0
                    }
                )]
            );
        }

        {
            // unreachable cleared after `else`
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("i32.const 1");
            editor.push_line("if");
            editor.push_line("unreachable");
            editor.push_line("else");
            editor.push_line("end");
            editor.push_line(")");
            test_editor_flow(&mut editor)?;
        }

        Ok(())
    }

    #[test]
    fn test_import_validation() -> Result<()> {
        // Test case 1: module name not found
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(import \"not_draw\" \"point\" (func (param f64 f64)))");
            test_editor_flow(&mut editor)?;
            assert_eq!(
                editor.lines[0].info.kind,
                LineKind::Malformed("module ‘not_draw’ not found".to_string())
            );
        }

        // Test case 2: component name not found
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(import \"draw\" \"not_point\" (func (param f64 f64)))");
            test_editor_flow(&mut editor)?;
            assert_eq!(
                editor.lines[0].info.kind,
                LineKind::Malformed("field ‘not_point’ not found in module draw".to_string())
            );
        }

        // Test case 3: wrong function type
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(import \"draw\" \"point\" (func (param i32)))");
            test_editor_flow(&mut editor)?;
            assert_eq!(
                editor.lines[0].info.kind,
                LineKind::Malformed("expected type (param f64 f64)".to_string())
            );
        }

        // Test case 4: correct import
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(import \"draw\" \"point\" (func (param f64, f64)))");
            test_editor_flow(&mut editor)?;
            assert!(
                editor.lines[0].info.invalid.is_none(),
                "draw/point should be correctly imported"
            );
        }

        // Test case 5: interleave valid and invalid imports
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(import \"a\" \"b\" (func $fake_func1))");
            editor.push_line("(import \"draw\" \"clear\" (func $clear_canvas))");
            editor.push_line("(import \"c\" \"d\" (func $fake_func2))");
            editor.push_line("(import \"draw\" \"point\" (func $draw_point (param f64 f64)))");
            editor.push_line("(import \"e\" \"f\" (func $fake_func3))");
            editor.push_line("(func");
            editor.push_line("call $fake_func1");
            editor.push_line("f64.const 0");
            editor.push_line("call $fake_func2");
            editor.push_line("f64.const 0");
            editor.push_line("call $fake_func3");
            editor.push_line("call $draw_point");
            editor.push_line("call $fake_func1");
            editor.push_line("call $clear_canvas");
            editor.push_line("call $fake_func2");
            editor.push_line(")");
            test_editor_flow(&mut editor)?;
            assert_eq!(
                editor.lines[0].info.kind,
                LineKind::Malformed("module ‘a’ not found".to_string())
            );
            assert!(
                editor.lines[1].info.invalid.is_none(),
                "clear_canvas should be valid"
            );
            assert_eq!(
                editor.lines[2].info.kind,
                LineKind::Malformed("module ‘c’ not found".to_string())
            );
            assert!(
                editor.lines[3].info.invalid.is_none(),
                "draw_point should be valid"
            );
            assert_eq!(
                editor.lines[4].info.kind,
                LineKind::Malformed("module ‘e’ not found".to_string())
            );
            for i in 6..editor.lines.len() {
                if i % 2 == 0 {
                    assert_eq!(
                        editor.lines[i].info.active,
                        Activity::Inactive("undefined symbolic reference"),
                    );
                } else {
                    assert!(
                        editor.lines[i].info.is_well_formed()
                            && editor.lines[i].info.invalid.is_none(),
                        "import test line {i} should be valid"
                    );
                }
            }
        }

        // Test case 6: valid global and memory imports
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(import \"listen\" \"num_samples\" (global i32))");
            editor.push_line("(import \"listen\" \"listen_memory\" (memory 1))");
            test_editor_flow(&mut editor)?;
            assert!(
                editor.lines[0].info.invalid.is_none(),
                "num_samples global should be valid"
            );
            assert!(
                editor.lines[1].info.invalid.is_none(),
                "listen_memory memory should be valid"
            );
        }

        // Test case 7: wrong types for global and memory imports
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(import \"listen\" \"num_samples\" (global (mut i32)))");
            editor.push_line("(import \"listen\" \"listen_memory\" (memory 2))");
            test_editor_flow(&mut editor)?;
            assert_eq!(
                editor.lines[0].info.kind,
                LineKind::Malformed("expected (global i32)".to_string()),
                "num_samples with wrong mutability should be rejected syntactically"
            );
            assert_eq!(
                editor.lines[1].info.kind,
                LineKind::Malformed("expected (memory 1)".to_string()),
                "listen_memory with wrong size should be rejected syntactically"
            );
        }

        Ok(())
    }

    #[test]
    fn test_bounds_checks() -> Result<()> {
        // Test memory.grow is denied for memory with pagesize 1
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(memory 8 100 (pagesize 1))");
            editor.push_line("(func");
            editor.push_line("(result i32)");
            editor.push_line("i32.const 4");
            editor.push_line("memory.grow");
            editor.push_line(")");

            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let mut results = [wasmtime::Val::I32(0)];
            let log = test_execution(&binary, &[], &mut results)?;
            assert!(text.contains("drop\n    i32.const -1"));
            assert!(!text.contains("memory.grow"));
            assert_eq!(TerminationType::Success, log.termination_type());
            assert_eq!(results[0].i32().expect("return value"), -1);
        }

        // Bounds check occurs for loads in memory with page size 1
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(memory 11 100 (pagesize 1))");
            editor.push_line("(func");
            editor.push_line("(result i32)");
            editor.push_line("i32.const 0");
            editor.push_line("i32.load offset=7");
            editor.push_line(")");

            let expected = String::from("(module\n  (type (func (result i32)))\n")
                + EXPECTED_TYPES
                + r#"  (type (func (param i32 i32) (result i32)))
  (type (func (param i32 i64) (result i32)))
"# + EXPECTED_IMPORTS
                + r#"  (memory 1)
  (global (mut i64) i64.const 11)
  (export "main" (func 11))
  (func (type 0) (result i32)
    i32.const 0
    call 6
    i32.const 1
    call 12
    i32.const 3
    call 12
    i32.const 0
    i32.const 0
    call 13
    i32.const 4
    call 12
    global.get 0
    i64.const 11
    i64.sub
    call 14
    i32.load offset=7
    i32.const 1
    call 13
    i32.const 5
    call 12
    i32.const 2
    call 13
  )
"# + EXPECTED_STEP
                + EXPECTED_RECORD_I32
                + r#"  (func (type 9) (param i32 i64) (result i32)
    local.get 0
    i64.extend_i32_u
    local.get 1
    i64.gt_s
    if ;; label = @1
      i32.const -1
      i32.load
      drop
    end
    local.get 0
  )
)
"#;

            let expected = expected.replace(
                "  (type (func))\n  (type (func (param i32) (result i32)))",
                "  (type (func (param i32) (result i32)))",
            );
            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let mut results = [wasmtime::Val::I32(0)];
            let log = test_execution(&binary, &[], &mut results)?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::Success, log.termination_type());
            assert_eq!(results[0].i32().expect("return value"), 0);
        }

        // Bounds check ocurrs for store in memory with pagesize 1
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(memory 4294967295 (pagesize 1))");
            editor.push_line("(func");
            editor.push_line("i32.const 4294967286");
            editor.push_line("f64.const 7.0");
            editor.push_line("f64.store offset=1");
            editor.push_line(")");

            let expected = String::from("(module\n")
                + EXPECTED_TYPES
                + r#"  (type (func (param i32 i32) (result i32)))
  (type (func (param f64 i32) (result f64)))
  (type (func (param i32 f64 i64) (result i32 f64)))
"# + EXPECTED_IMPORTS
                + r#"  (memory 65536)
  (global (mut i64) i64.const 4294967295)
  (export "main" (func 11))
  (func (type 0)
    i32.const 0
    call 6
    i32.const 1
    call 12
    i32.const 2
    call 12
    i32.const -10
    i32.const 0
    call 13
    i32.const 3
    call 12
    f64.const 0x1.cp+2 (;=7;)
    i32.const 1
    call 14
    i32.const 4
    call 12
    global.get 0
    i64.const 9
    i64.sub
    call 15
    f64.store offset=1
    i32.const 5
    call 12
  )
"# + EXPECTED_STEP
                + EXPECTED_RECORD_I32
                + r#"  (func (type 9) (param f64 i32) (result f64)
    local.get 0
    local.get 1
    call 5
    local.get 0
  )
  (func (type 10) (param i32 f64 i64) (result i32 f64)
    local.get 0
    i64.extend_i32_u
    local.get 2
    i64.gt_s
    if ;; label = @1
      i32.const -1
      i32.load
      drop
    end
    local.get 0
    local.get 1
  )
)
"#;

            let EditorOutput { text, binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(expected, text);
            assert_eq!(TerminationType::Success, log.termination_type());
        }

        {
            // Bounds check success (default pagesize)
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(memory 1)");
            editor.push_line("(func");
            editor.push_line("i32.const 65534");
            editor.push_line("i32.const 0");
            editor.push_line("i32.store16");
            editor.push_line(")");
            let EditorOutput { binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(TerminationType::Success, log.termination_type());
        }

        {
            // Bounds check success (default pagesize)
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(memory 1)");
            editor.push_line("(func");
            editor.push_line("i32.const 65535");
            editor.push_line("i32.const 0");
            editor.push_line("i32.store8");
            editor.push_line(")");
            let EditorOutput { binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(TerminationType::Success, log.termination_type());
        }

        {
            // Bounds check failure (default pagesize)
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(memory 1)");
            editor.push_line("(func");
            editor.push_line("i32.const 65535");
            editor.push_line("i32.const 0");
            editor.push_line("i32.store16");
            editor.push_line(")");
            let EditorOutput { binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(TerminationType::Error, log.termination_type());
        }

        Ok(())
    }

    #[test]
    fn execution_tests() -> Result<()> {
        // Test case 1: check that the stepping in execution tests are working correctly
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("(local i32)");
            editor.push_line("loop");
            editor.push_line("local.get 0");
            editor.push_line("i32.const 1");
            editor.push_line("i32.add");
            editor.push_line("local.tee 0");
            editor.push_line("i32.const 14286"); // Just barely exceeds number of steps at 100,005 steps
            editor.push_line("i32.sub");
            editor.push_line("br_if 0");
            editor.push_line("end");
            editor.push_line(")");

            let EditorOutput { binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(TerminationType::TooManySteps, log.termination_type());
        }

        // helper for SlotContents -> WasmValue to match previous API
        fn c2v(c: Option<SlotContents>) -> Option<WasmValue> {
            c.map(|x| x.val)
        }

        // Test basic "slot" assignments
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("i32.const 5");
            editor.push_line("i32.const 9");
            editor.push_line("i32.add");
            editor.push_line("drop");
            editor.push_line(")");

            let EditorOutput {
                binary,
                connections,
                ..
            } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(TerminationType::Success, log.termination_type());
            assert_eq!(6, log.step_count());

            let mut state = ExecutionState::default();
            state.reset(&connections);

            state.goto_step(&log, &connections, 5, None);
            assert_eq!(state.next_step, 6);
            assert_eq!(connections.connections.len(), 3);
            assert_eq!(state.slots.len(), 3);
            assert_eq!(c2v(state.slot_contents(0)), Some(WasmValue::I32(5)));
            assert_eq!(c2v(state.slot_contents(1)), Some(WasmValue::I32(9)));
            assert_eq!(c2v(state.slot_contents(2)), Some(WasmValue::I32(14)));
            assert_eq!(state.status.line_num, Some(5));
            assert_eq!(state.status.termination, TerminationType::Success);

            state.goto_step(&log, &connections, 2, None);
            assert_eq!(state.next_step, 3);
            assert_eq!(connections.connections.len(), 3);
            assert_eq!(state.slots.len(), 3);
            assert_eq!(c2v(state.slot_contents(0)), Some(WasmValue::I32(5)));
            assert_eq!(c2v(state.slot_contents(1)), None);
            assert_eq!(c2v(state.slot_contents(2)), None);
            assert_eq!(state.status.line_num, Some(2));
            assert_eq!(state.status.termination, TerminationType::Running);
        }

        // Test calls
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("call $x");
            editor.push_line(")");
            editor.push_line("(func $x)");

            let EditorOutput {
                binary,
                connections,
                ..
            } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(TerminationType::Success, log.termination_type());
            assert_eq!(6, log.step_count());

            let mut state = ExecutionState::default();
            state.reset(&connections);

            state.goto_step(&log, &connections, 4, None);
            assert_eq!(state.next_step, 5);
            assert_eq!(connections.connections.len(), 0);
            assert_eq!(state.slots.len(), 0);
            assert_eq!(state.status.line_num, Some(1));
            assert_eq!(state.status.below_line, true);
            assert_eq!(state.status.termination, TerminationType::Running);
        }

        // Test recursive call
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("i32.const 2");
            editor.push_line("call $x");
            editor.push_line(")");
            editor.push_line("(func $x (param $n i32)");
            editor.push_line("local.get $n");
            editor.push_line("i32.eqz");
            editor.push_line("br_if 0");
            editor.push_line("local.get $n");
            editor.push_line("i32.const 1");
            editor.push_line("i32.sub");
            editor.push_line("call $x");
            editor.push_line(")");

            let EditorOutput {
                binary,
                connections,
                ..
            } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(TerminationType::Success, log.termination_type());
            assert_eq!(29, log.step_count());

            let mut state = ExecutionState::default();
            state.reset(&connections);

            state.goto_step(&log, &connections, 22, None);
            assert_eq!(state.next_step, 23);
            assert_eq!(c2v(state.slot_contents(2)), Some(0.into())); // param of $x function

            state.goto_step(&log, &connections, 23, None);
            assert_eq!(state.next_step, 24);
            assert_eq!(c2v(state.slot_contents(2)), Some(1.into())); // param of $x function after stack unwinds

            state.goto_step(&log, &connections, 25, None);
            assert_eq!(state.next_step, 26);
            assert_eq!(c2v(state.slot_contents(2)), Some(2.into())); // param of $x function after stack unwinds again
        }

        // Test annoying tail call (#270)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("  call $f");
            editor.push_line("  drop");
            editor.push_line(")");
            editor.push_line("(func $f (result i64)");
            editor.push_line("  block (result i64)");
            editor.push_line("    i64.const 0");
            editor.push_line("    return_call $f");
            editor.push_line("    i64.const 0");
            editor.push_line("  end");
            editor.push_line(")");

            let EditorOutput { binary, .. } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(TerminationType::TooManySteps, log.termination_type());
        }

        // Test synthetic end with non-unit type (#273)
        {
            let mut editor = FakeTextBuffer::default();
            editor.push_line("(func");
            editor.push_line("  block (result i32)");
            editor.push_line(")");

            let EditorOutput {
                binary,
                connections,
                ..
            } = test_editor_flow(&mut editor)?;
            let log = test_execution(&binary, &[], &mut [])?;
            assert_eq!(TerminationType::HitInvalid, log.termination_type());
            assert_eq!(connections.connections.len(), 2);
            assert!(connections.connections[0].written.is_none());
            assert!(connections.connections[0].read.is_none());
            assert!(connections.connections[1].written.is_none());
            assert!(connections.connections[1].read.is_none());
        }

        Ok(())
    }
}
