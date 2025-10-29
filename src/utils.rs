use crate::line::LineInfo;
use crate::modular::{FuncHeader};
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
    let txt = format!("module {s}");
    let binding = ParseBuffer::new(&txt)?;
    let mut module = parser::parse::<Module>(&binding)?;

    Ok(module.encode()?)
}

#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub enum InstrKind {
    #[default]
    Empty,
    
    FuncHeader,
    ModuHeader,
    FuncModuEnd,

    If,
    Else,
    End,
    OtherStructured,   // block, loop, or try_table
    Other,             // any other instruction
    Malformed(String), // explanation
}

impl InstrKind {
    fn stripped_clone(&self) -> InstrKind {
        use InstrKind::*;

        match self {
            Malformed(_) => Malformed(String::new()),
            If => If,
            Else => Else,
            End => End,
            OtherStructured => OtherStructured,
            Other => Other,
            Empty => Empty,
            FuncHeader => FuncHeader,
            ModuHeader => ModuHeader,
            FuncModuEnd => FuncModuEnd,
        }
    }
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

pub fn is_instr(line: &InstrKind) -> bool {
    !matches!(line, InstrKind::FuncHeader | InstrKind::ModuHeader | InstrKind::FuncModuEnd)
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
    } else if s.starts_with("(func") {
        match ParseBuffer::new(&s[1..]) {
            Ok(buf) => match parser::parse::<FuncHeader>(&buf) {
                Ok(_) => InstrKind::FuncHeader,
                Err(e) => e.into(),
            },
            Err(e) => e.into(),
        }
    } else if s == ")" {
        InstrKind::FuncModuEnd
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
    fn synthetic_ends_len(&self) -> usize;
}

#[derive(PartialEq, Clone, Default, Debug)]
pub struct FrameInfo {
    pub indent: usize,
    pub start: usize,
    pub end: usize,
    pub unclosed: bool,
    pub kind: InstrKind,
}

pub trait LineInfosMut: LineInfos {
    fn set_active_status(&mut self, index: usize, is_active: bool);
    fn set_indent(&mut self, index: usize, num: usize);
    fn set_frame_count(&mut self, count: usize);
    fn set_frame_info(&mut self, num: usize, frame: FrameInfo);
    fn reset_synthetic_ends(&mut self);
    fn add_synthetic_ends(&mut self, list: &mut Vec<(usize, String)>);
}

/// Fix frames by deactivated unmatched instrs and appending ends as necessary to close all open frames
pub fn fix_frames(lines: &mut impl LineInfosMut) {
    struct OpenFrame {
        num: usize,
        line_no: usize,
        kind: InstrKind,
    }

    // Return the number of frames opened
    fn try_open_frame(
        lines: &mut impl LineInfosMut, 
        line_no: usize, 
        kind: &InstrKind, 
        frame_stack: &mut Vec<OpenFrame>, 
        frame_count: usize
    ) -> usize {
        match kind {
            InstrKind::If | InstrKind::Else | InstrKind::OtherStructured | InstrKind::FuncHeader => {
                // Set indentation
                lines.set_indent(line_no, frame_stack.len());

                // Push frame opening
                frame_stack.push(OpenFrame {
                    num: frame_count,
                    line_no,
                    kind: kind.clone(),
                });

                1
            }
            _ => 0,
        }
    }

    // Return the number of frames opened
    fn try_close_frame(
        lines: &mut impl LineInfosMut, 
        line_no: usize, 
        kind: &InstrKind, 
        frame_stack: &mut Vec<OpenFrame>, 
        synthetic: bool,
        synthetic_ends: &mut Vec<(usize, String)>,
    ) -> usize {
        match kind {
            InstrKind::FuncModuEnd => {
                let mut closed_num = 0;

                // Find the last inserted Func opening
                if let Some(pos) = frame_stack.iter().rposition(|f| matches!(f.kind, InstrKind::FuncHeader)) {
                    // Close all frames at and after that
                    while frame_stack.len() > pos {
                        let Some(OpenFrame { num, line_no: start, kind }) = frame_stack.pop() else { break; };
                        let indent = frame_stack.len();

                        if synthetic {
                            // Add synthetic end
                            match kind {
                                InstrKind::FuncHeader => synthetic_ends.push((line_no - 1, ")".to_string())),
                                _ => synthetic_ends.push((line_no - 1, "end".to_string())),
                            }
                        } else {
                            lines.set_indent(line_no, indent);
                        }

                        // Record frame
                        lines.set_frame_info(
                            num,
                            FrameInfo {
                                indent: indent,
                                start: start,
                                end: line_no,
                                unclosed: synthetic,
                                kind,
                            },
                        );
                        closed_num += 1;
                    }

                } else {
                    // No matching open frame
                    lines.set_indent(line_no, frame_stack.len());
                    lines.set_active_status(line_no, false);
                }

                closed_num
            }
            InstrKind::Else | InstrKind::End => {
                if let Some(OpenFrame {
                    num,
                    line_no: start,
                    kind: open_kind,
                }) = frame_stack.last()
                {
                    let can_close = match (kind, open_kind) {
                        (InstrKind::Else, InstrKind::If) => true,
                        (InstrKind::End, InstrKind::If | InstrKind::Else | InstrKind::OtherStructured) => true,
                        _ => false,
                    };

                    if can_close {
                        let indent = frame_stack.len() - 1;

                        if synthetic {
                            // Add synthetic end
                            synthetic_ends.push((line_no - 1, "end".to_string()));
                        } else {
                            lines.set_indent(line_no, indent);
                        }

                        lines.set_frame_info(
                            *num,
                            FrameInfo {
                                indent,
                                start: *start,
                                end: line_no,
                                unclosed: synthetic,
                                kind: open_kind.clone(),
                            },
                        );
                        frame_stack.pop();

                        1
                    } else {
                        lines.set_indent(line_no, frame_stack.len());
                        lines.set_active_status(line_no, false);
                        0
                    }
                } else {
                    // No matching open frame
                    lines.set_indent(line_no, frame_stack.len());
                    lines.set_active_status(line_no, false);
                    0
                }
            }
            _ => 0,
        }
    }

    let mut frame_stack: Vec<OpenFrame> = Vec::new();
    let mut frame_count = 0;
    // (line idx just before synthetic end, synthetic end word)
    let mut synthetic_ends: Vec<(usize, String)> = Vec::new();
    let len = lines.len();
    let mut in_func = false;

    for line_no in 0..lines.len() {
        // Assume this line is active at the beginning
        lines.set_active_status(line_no, true);

        let kind = lines.info(line_no).kind.stripped_clone();
        if !is_instr(&kind) {
            // Function/Module frame
            match kind {
                InstrKind::FuncHeader => {
                    // Close the previous function frame if there's one
                    if in_func {
                        try_close_frame(
                            lines, line_no, 
                            &InstrKind::FuncModuEnd, 
                            &mut frame_stack, 
                            true, &mut synthetic_ends
                        );
                    }

                    // Open new function frame
                    frame_count += try_open_frame(lines, line_no, &kind, &mut frame_stack, frame_count);
                    in_func = true;
                }
                InstrKind::ModuHeader => todo!(),
                InstrKind::FuncModuEnd => {
                    try_close_frame(
                        lines, line_no, 
                        &InstrKind::FuncModuEnd, 
                        &mut frame_stack, 
                        false, &mut synthetic_ends
                    );
                    in_func = false;
                }
                _ => ()
            }
        } else {
            // Instruction line
            // Skip if the instruction is not wrapped in a function
            if !in_func {
                lines.set_indent(line_no, frame_stack.len());
                if lines.info(line_no).is_instr() {
                    lines.set_active_status(line_no, false);
                }
                continue;
            }

            match kind {
                InstrKind::If | InstrKind::OtherStructured => {
                    frame_count += try_open_frame(lines, line_no, &kind, &mut frame_stack, frame_count);
                }
                InstrKind::Else => {
                    let closed_num = try_close_frame(
                        lines, line_no, 
                        &kind, 
                        &mut frame_stack, 
                        false, &mut synthetic_ends
                    );
                    if closed_num > 0 {
                        // Insert the ELSE frame
                        frame_count += try_open_frame(lines, line_no, &kind, &mut frame_stack, frame_count);
                    }
                }
                InstrKind::End => {
                    try_close_frame(
                        lines, line_no, 
                        &kind, 
                        &mut frame_stack, 
                        false, &mut synthetic_ends
                    );
                }
                InstrKind::Other | InstrKind::Empty | InstrKind::Malformed(_) => {
                    lines.set_indent(line_no, frame_stack.len());
                }
                _ => ()
            }
        }
    }

    while !frame_stack.is_empty() {
        try_close_frame(lines, len, &InstrKind::FuncModuEnd, &mut frame_stack, true, &mut synthetic_ends);
    }

    lines.set_frame_count(frame_count);
    lines.add_synthetic_ends(&mut synthetic_ends);
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

    struct TestLineInfos {
        lines: Vec<LineInfo>,
        synthetic_ends: usize,
        frames: Vec<FrameInfo>,
    }

    impl TestLineInfos {
        fn new<const N: usize>(instrs: [&str; N]) -> Self {
            Self {
                lines: instrs
                    .into_iter()
                    .map(|x| LineInfo {
                        kind: parse_instr(x),
                        active: true,
                        ..Default::default()
                    })
                    .collect(),
                synthetic_ends: 0,
                frames: Vec::new(),
            }
        }
    }

    impl LineInfos for TestLineInfos {
        fn is_empty(&self) -> bool {
            self.lines.is_empty()
        }

        fn len(&self) -> usize {
            self.lines.len()
        }

        #[allow(refining_impl_trait)]
        fn info(&self, index: usize) -> &LineInfo {
            &self.lines[index]
        }

        fn synthetic_ends(&self) -> usize {
            self.synthetic_ends
        }
    }

    impl LineInfosMut for TestLineInfos {
        fn set_active_status(&mut self, index: usize, is_active: bool) {
            self.lines[index].active = is_active;
        }

        fn set_synthetic_ends(&mut self, num: usize) {
            self.synthetic_ends = num;
        }

        fn set_indent(&mut self, index: usize, num: usize) {
            self.lines[index].indent = Some(num.try_into().unwrap());
        }

        fn set_frame_info(&mut self, num: usize, frame: FrameInfo) {
            if num >= self.frames.len() {
                self.frames.resize(num + 1, FrameInfo::default());
            }
            self.frames[num] = frame;
        }

        fn set_frame_count(&mut self, count: usize) {
            self.frames.truncate(count);
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
        // function header
        assert_eq!(parse_instr("(func"), InstrKind::FuncHeader);
        assert_eq!(parse_instr("(func)"), malf("extra tokens remaining after parse"));
        assert_eq!(parse_instr("(func $x"), InstrKind::FuncHeader);
        assert_eq!(parse_instr("(func (param $a i32)"), InstrKind::FuncHeader);
        assert_eq!(parse_instr("(func (param) (result i64)"), InstrKind::FuncHeader);
        assert_eq!(parse_instr("  (func (type 0) (local i32)"), InstrKind::FuncHeader);
        assert_eq!(parse_instr("(func i32.const 5"), malf("extra tokens remaining after parse"));
        assert_eq!(parse_instr("(func (i32.const 5)"), malf("extra tokens remaining after parse"));
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
        {
            let mut infos1 = TestLineInfos::new(["block", "i32.const 42", "drop"]);
            fix_frames(&mut infos1);
            assert!(infos1.lines.iter().all(|x| x.active));
            assert_eq!(
                infos1
                    .lines
                    .iter()
                    .map(|x| x.indent.unwrap())
                    .collect::<Vec<_>>(),
                [0, 1, 1]
            );
            assert_eq!(infos1.synthetic_ends, 1);
            assert_eq!(
                infos1.frames,
                [FrameInfo {
                    indent: 0,
                    start: 0,
                    end: 3,
                    unclosed: true,
                    kind: InstrKind::OtherStructured
                }]
            );
        }

        {
            let mut infos2 = TestLineInfos::new([
                "if",    // 0
                "block", // 1
                "else",  // 2
                "end",   // 3
            ]);
            fix_frames(&mut infos2);
            assert_eq!(infos2.synthetic_ends, 1);
            assert_eq!(
                infos2.lines.iter().map(|x| x.active).collect::<Vec<_>>(),
                [true, true, false, true]
            );
            assert_eq!(
                infos2
                    .lines
                    .iter()
                    .map(|x| x.indent.unwrap())
                    .collect::<Vec<_>>(),
                [0, 1, 2, 1]
            );
            assert_eq!(
                infos2.frames,
                [
                    FrameInfo {
                        indent: 0,
                        start: 0,
                        end: 4,
                        unclosed: true,
                        kind: InstrKind::If
                    },
                    FrameInfo {
                        indent: 1,
                        start: 1,
                        end: 3,
                        unclosed: false,
                        kind: InstrKind::OtherStructured
                    }
                ]
            );
        }

        {
            let mut infos3 = TestLineInfos::new(["end"]);
            fix_frames(&mut infos3);
            assert!(!infos3.lines[0].active);
            assert!(infos3.frames.is_empty());
        }

        {
            // Test case 4: Simple block
            let mut infos4 = TestLineInfos::new(["block", "i32.const 42", "drop", "end"]);
            fix_frames(&mut infos4);
            assert_eq!(
                infos4.frames,
                [FrameInfo {
                    indent: 0,
                    start: 0,
                    end: 3,
                    unclosed: false,
                    kind: InstrKind::OtherStructured
                }]
            );
        }

        {
            // Test case 5: Complex nested structure
            let mut infos5 = TestLineInfos::new([
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
            ]);
            fix_frames(&mut infos5);
            assert_eq!(infos5.frames.len(), 5);
            assert_eq!(
                infos5.frames[0],
                FrameInfo {
                    start: 0,
                    end: 3,
                    unclosed: false,
                    indent: 0,
                    kind: InstrKind::OtherStructured
                }
            );
            assert_eq!(
                infos5.frames[1],
                FrameInfo {
                    start: 4,
                    end: 14,
                    unclosed: false,
                    indent: 0,
                    kind: InstrKind::OtherStructured
                }
            );
            assert_eq!(
                infos5.frames[2],
                FrameInfo {
                    start: 5,
                    end: 13,
                    unclosed: false,
                    indent: 1,
                    kind: InstrKind::OtherStructured
                }
            );
            assert_eq!(
                infos5.frames[3],
                FrameInfo {
                    start: 6,
                    end: 9,
                    unclosed: false,
                    indent: 2,
                    kind: InstrKind::If
                }
            );
            assert_eq!(
                infos5.frames[4],
                FrameInfo {
                    start: 9,
                    end: 12,
                    unclosed: false,
                    indent: 2,
                    kind: InstrKind::Else
                }
            );
        }
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
