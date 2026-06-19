// Structures and utilities to support multi-function/multi-module text buffers.

use crate::{
    line::LineInfo,
    symbolic::{
        ModuleIdentifiers, collect_label_symbol, collect_local_symbols, collect_module_symbols,
        symbols_resolved,
    },
    utils::{HelperImportKind, check_import},
};
use anyhow::Result;
use std::collections::HashSet;
use wast::{
    Error,
    core::{
        Export, Func, FuncKind, FunctionType, Global, Imports, InlineExport, InlineImport,
        Instruction, ItemKind, Limits, LocalParser, Memory, MemoryType, Table, TypeUse, ValType,
    },
    kw,
    parser::{self, Cursor, Parse, ParseBuffer, Parser, Peek},
    token::Id,
};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum InstrKind {
    If,
    Else,
    End,
    Loop,
    OtherStructured, // block or try_table
    Other,           // any other instruction
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum FuncPart {
    FuncKeyword,
    Id,
    InlineExport,
    Param,
    Result,
    Local(usize), // number of locals declared
    LParen,
    RParen,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ImportKind {
    Import,
    InlineMemory,
    InlineGlobal,
    InlineFunc,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ModulePart {
    Export,
    Import(ImportKind),
    Memory,
    Table,
    Global,
    Func(Vec<FuncPart>),
}

impl From<FuncPart> for &'static str {
    fn from(val: FuncPart) -> &'static str {
        use FuncPart::*;
        match val {
            LParen => "(",
            RParen => ")",
            FuncKeyword => "func",
            _ => panic!("ModulePart cannot be rendered in text"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LineKind {
    Empty,
    Instr(InstrKind),
    Other(ModulePart),
    Malformed(String), // explanation
}

pub trait LineInfos {
    fn is_empty(&self) -> bool;
    fn len(&self) -> usize;
    fn info(&self, index: usize) -> &crate::line::LineInfo;
}

pub trait LineInfosMut: LineInfos {
    fn set_active_status(&mut self, index: usize, new_val: crate::line::Activity);
    fn set_synthetic_before(&mut self, index: usize, synth: SyntheticWasm);
    fn set_invalid(&mut self, index: usize, reason: Option<String>);
    fn set_runtime_error(&mut self, index: usize, msg: Option<String>);
    fn push(&mut self);
}

#[derive(PartialEq, Clone, Debug)]
pub struct FrameInfo {
    pub indent: u16,
    pub start: usize,
    pub end: usize,
    pub unclosed: bool,
    pub kind: InstrKind,
    pub wide: bool,
}

pub trait FrameInfosMut: LineInfos {
    fn set_indent(&mut self, index: usize, num: u16);
    fn set_frames(&mut self, frames: Vec<FrameInfo>);
}

impl LineKind {
    pub fn stripped_clone(&self) -> LineKind {
        match self {
            LineKind::Malformed(_) => LineKind::Malformed(String::new()),
            _ => self.clone(),
        }
    }
}

impl From<Instruction<'_>> for InstrKind {
    fn from(instr: Instruction<'_>) -> Self {
        match instr {
            Instruction::If(_) => InstrKind::If,
            Instruction::Else(_) => InstrKind::Else,
            Instruction::End(_) => InstrKind::End,
            Instruction::Loop(_) => InstrKind::Loop,
            Instruction::Block(_) | Instruction::TryTable(_) => InstrKind::OtherStructured,
            Instruction::Try(_) | Instruction::Catch(_) | Instruction::CatchAll => {
                panic!("legacy-exceptions not supported");
            }
            _ => InstrKind::Other,
        }
    }
}

fn remap_diagnostic(s: String) -> String {
    const TOKEN_MESSAGE: &str = "unexpected token, expected one of: `i32`, `i64`, `f32`, `f64`";
    if s.starts_with(TOKEN_MESSAGE) {
        return TOKEN_MESSAGE.to_string();
    }
    s.replace("expected a i", "expected an i")
}

impl From<Error> for LineKind {
    fn from(e: Error) -> Self {
        LineKind::Malformed(remap_diagnostic(e.message()))
    }
}

impl<'a> Parse<'a> for ModulePart {
    fn parse(parser: Parser<'a>) -> Result<Self, Error> {
        use ValType::*;
        if parser.peek2::<kw::export>()? {
            if parser.peek::<InlineExport>()? {
                parse_func_parts(&parser, false)
            } else {
                parser.parens(|p| p.parse::<Export>())?;
                Ok(ModulePart::Export)
            }
        } else if parser.peek::<InlineImport>()? {
            Err(parser.error("imports must be on one line"))
        } else if parser.peek2::<kw::import>()? {
            parser.parens(|p| match p.parse::<Imports>()? {
                Imports {
                    items:
                        wast::core::ImportItems::Single {
                            module,
                            name,
                            sig: wast::core::ItemSig { kind, .. },
                        },
                    ..
                } => match kind {
                    ItemKind::Func(ty) => {
                        check_func_import(&parser, module, name, ty, ImportKind::Import)
                    }
                    ItemKind::Global(ty) => {
                        match check_import(module, name, Some(&HelperImportKind::Global(ty))) {
                            None => Ok(ModulePart::Import(ImportKind::Import)),
                            Some(error_message) => Err(parser.error(error_message)),
                        }
                    }
                    ItemKind::Memory(ty) => {
                        match check_import(module, name, Some(&HelperImportKind::Memory(ty))) {
                            None => Ok(ModulePart::Import(ImportKind::Import)),
                            Some(error_message) => Err(parser.error(error_message)),
                        }
                    }
                    _ => Err(parser.error("unsupported import kind")),
                },

                _ => Err(parser.error("unsupported import kind")),
            })
        } else if parser.peek2::<kw::memory>()? {
            parser.parens(|p| match p.parse::<Memory>()? {
                Memory {
                    kind:
                        wast::core::MemoryKind::Import {
                            import: InlineImport { module, field },
                            ty,
                            ..
                        },
                    ..
                } => match check_import(module, field, Some(&HelperImportKind::Memory(ty))) {
                    None => Ok(ModulePart::Import(ImportKind::InlineMemory)),
                    Some(error_message) => Err(parser.error(error_message)),
                },
                Memory {
                    kind:
                        wast::core::MemoryKind::Normal(MemoryType {
                            shared: false,
                            limits: Limits { is64: false, .. },
                            ..
                        }),
                    ..
                } => Ok(ModulePart::Memory),
                _ => Err(parser.error("unsupported memory feature")),
            })
        } else if parser.peek2::<kw::table>()? {
            parser.parens(|p| match p.parse::<Table>()? {
                Table {
                    kind: wast::core::TableKind::Import { .. },
                    ..
                } => Err(parser.error("unsupported import kind")),
                _ => Ok(ModulePart::Table),
            })
        } else if parser.peek2::<kw::global>()? {
            parser.parens(|p| match p.parse::<Global>()? {
                Global {
                    kind: wast::core::GlobalKind::Import(InlineImport { module, field }),
                    ty,
                    ..
                } => match check_import(module, field, Some(&HelperImportKind::Global(ty))) {
                    None => Ok(ModulePart::Import(ImportKind::InlineGlobal)),
                    Some(error_message) => Err(parser.error(error_message)),
                },
                Global {
                    kind: wast::core::GlobalKind::Inline(wast::core::Expression { instrs, .. }),
                    ty:
                        wast::core::GlobalType {
                            ty, shared: false, ..
                        },
                    ..
                } => match instrs.len() {
                    0 => Err(parser.error("expected initializer")),
                    1 => match (ty, &instrs[0]) {
                        (I32, Instruction::I32Const(_)) => Ok(ModulePart::Global),
                        (F32, Instruction::F32Const(_)) => Ok(ModulePart::Global),
                        (I64, Instruction::I64Const(_)) => Ok(ModulePart::Global),
                        (F64, Instruction::F64Const(_)) => Ok(ModulePart::Global),
                        (I32, _) => Err(parser.error("expected i32.const initializer")),
                        (F32, _) => Err(parser.error("expected f32.const initializer")),
                        (I64, _) => Err(parser.error("expected i64.const initializer")),
                        (F64, _) => Err(parser.error("expected f64.const initializer")),
                        _ => Err(parser.error("unsupported global type")),
                    },
                    _ => Err(parser.error("expected single-instruction initializer")),
                },
                _ => Err(parser.error("unsupported global type")),
            })
        } else if parser.step(|cursor| Ok((cursor.peek_lparen()?, cursor)))?
            && parser.peek2::<kw::func>()?
        {
            parse_func_parts(&parser, true)
        } else {
            parse_func_parts(&parser, false)
        }
    }
}

fn check_func_import(
    parser: &Parser,
    module: &str,
    field: &str,
    ty: TypeUse<FunctionType>,
    kind: ImportKind,
) -> Result<ModulePart, Error> {
    if ty.index.is_some() {
        Err(parser.error("type index not supported"))
    } else {
        let ty = ty.inline.unwrap_or_default();
        match check_import(
            module,
            field,
            Some(&HelperImportKind::Func {
                params: &ty.params.iter().map(|(_, _, p)| *p).collect::<Vec<_>>(),
                results: &ty.results,
            }),
        ) {
            None => Ok(ModulePart::Import(kind)),
            Some(error_message) => Err(parser.error(error_message)),
        }
    }
}

fn parse_func_parts(parser: &Parser, import_ok: bool) -> Result<ModulePart, Error> {
    let mut parts = Vec::new();
    let checkpoint = parser.step(|cursor| Ok((cursor, cursor)))?;

    loop {
        if parser.peek::<InlineImport>()? && import_ok {
            parser.step(|_| Ok(((), checkpoint)))?;
            return match parser.parens(|p| p.parse::<Func>()) {
                Ok(Func {
                    kind: FuncKind::Import(InlineImport { module, field }, _),
                    ty,
                    ..
                }) => check_func_import(parser, module, field, ty, ImportKind::InlineFunc),
                Err(e) => Err(e),
                _ => Err(parser.error("unsupported function")),
            };
        }
        parts.push(parser.parse()?);
        if parser.is_empty() && !parser.step(|c| Ok((c.peek_rparen()?, c)))? {
            break;
        }
    }
    Ok(ModulePart::Func(parts))
}

impl Peek for ModulePart {
    fn peek(cursor: Cursor) -> Result<bool, Error> {
        Ok(cursor.peek_lparen()?
            || cursor.peek_rparen()?
            || kw::func::peek(cursor)?
            || Id::peek(cursor)?)
    }

    fn display() -> &'static str {
        "Wasm text other than an instruction"
    }
}

impl<'a> Parse<'a> for LineKind {
    fn parse(parser: Parser<'a>) -> Result<Self, Error> {
        // In the Codillon editor, a line can contain a single instruction
        // or a ModulePart: either a single "module-scope" element
        // (import, export, memory, table, global) or a sequence of parts of
        // an inline (non-imported) function.

        if parser.peek::<ModulePart>()? {
            Ok(LineKind::Other(parser.parse()?))
        } else if parser.is_empty() {
            Ok(LineKind::Empty)
        } else {
            let instr = parser.parse::<Instruction>()?;
            if matches!(instr, Instruction::F32Sqrt | Instruction::F64Sqrt) {
                return Err(parser.error("sqrt instructions not available in CS 10N"));
            }
            Ok(LineKind::Instr(instr.into()))
        }
    }
}

#[derive(Default)]
pub struct CodillonParam<'a>(pub FunctionType<'a>);

impl<'a> Parse<'a> for CodillonParam<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self, Error> {
        // Modified from FunctionType parser
        let mut params = Vec::new();
        parser.parens(|p| {
            p.parse::<kw::param>()?;
            if p.is_empty() {
                return Ok(Self::default());
            }
            let (id, name) = (p.parse::<Option<_>>()?, p.parse::<Option<_>>()?);
            let parse_more = id.is_none() && name.is_none();
            let ty = p.parse()?;
            params.push((id, name, ty));
            while parse_more && !p.is_empty() {
                params.push((None, None, p.parse()?));
            }
            Ok(Self(FunctionType {
                params: params.into(),
                ..Default::default()
            }))
        })
    }
}

#[derive(Default)]
pub struct CodillonResult<'a>(pub FunctionType<'a>);

impl<'a> Parse<'a> for CodillonResult<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self, Error> {
        // Modified from FunctionType parser
        let mut results = Vec::new();
        parser.parens(|p| {
            p.parse::<kw::result>()?;
            while !p.is_empty() {
                results.push(p.parse()?);
            }

            Ok(Self(FunctionType {
                results: results.into(),
                ..Default::default()
            }))
        })
    }
}

impl<'a> Parse<'a> for FuncPart {
    fn parse(parser: Parser<'a>) -> Result<Self, Error> {
        if parser.peek::<kw::func>()? {
            parser.parse::<kw::func>()?;
            Ok(FuncPart::FuncKeyword)
        } else if parser.peek::<Id>()? {
            parser.parse::<Id>()?;
            Ok(FuncPart::Id)
        } else if parser.peek::<InlineExport>()? {
            parser.parse::<InlineExport>()?;
            Ok(FuncPart::InlineExport)
        } else if parser.peek2::<kw::param>()? {
            parser.parse::<CodillonParam>()?;
            Ok(FuncPart::Param)
        } else if parser.peek2::<kw::result>()? {
            parser.parse::<CodillonResult>()?;
            Ok(FuncPart::Result)
        } else if parser.peek2::<kw::local>()? {
            parser.parens(|p| Ok(FuncPart::Local(p.parse::<LocalParser>()?.locals.len())))
        } else if parser.peek::<InlineImport>()? {
            Err(parser.error("imports must be on one line"))
        } else if parser.step(|cursor| match cursor.lparen()? {
            Some(rest) => Ok((true, rest)),
            None => Ok((false, cursor)),
        })? {
            Ok(FuncPart::LParen)
        } else if parser.step(|cursor| match cursor.rparen()? {
            Some(rest) => Ok((true, rest)),
            None => Ok((false, cursor)),
        })? {
            Ok(FuncPart::RParen)
        } else {
            Err(parser.error("unexpected token"))
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct SyntheticWasm {
    pub end_opcodes: usize,
    pub module_field_syntax: Vec<FuncPart>,
    pub module_syntax_first: bool,
}

impl SyntheticWasm {
    pub fn num_strs(&self) -> usize {
        self.module_field_syntax.len() + self.end_opcodes
    }

    pub fn str(&self, idx: usize) -> &str {
        if idx >= self.num_strs() {
            panic!("index out of range");
        }
        if self.module_syntax_first {
            if idx < self.module_field_syntax.len() {
                self.module_field_syntax[idx].into()
            } else {
                "end"
            }
        } else if idx < self.end_opcodes {
            "end"
        } else {
            self.module_field_syntax[idx - self.end_opcodes].into()
        }
    }

    pub fn render_module_field_syntax(&self) -> String {
        self.module_field_syntax
            .iter()
            .map(|&x| -> &str { x.into() })
            .collect()
    }

    pub fn num_ops(&self) -> usize {
        self.end_opcodes
    }

    pub fn op(&self, idx: usize) -> &str {
        if idx < self.num_ops() {
            "end"
        } else {
            panic!("index out of range");
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum SyntaxState {
    Initial,
    AfterLParen,
    AfterFuncHeader(FuncHeader),
    AfterInstruction,
}

#[derive(Default, Copy, Clone, PartialEq, Debug)]
struct FuncHeader {
    /// Fields in a function header follows an order and some extra restrictions.
    /// This structure tracks and transits states, where each state indicates new fields allowed.
    // Lowest possible index for next field in the expected order
    next_field: usize,
}

impl FuncHeader {
    fn transit_state(&mut self, part: FuncPart) -> Result<(), &'static str> {
        // Find position (index) of part in the order list
        let part_pos = match part {
            // Expected order of function fields
            FuncPart::FuncKeyword => 0,  // required
            FuncPart::Id => 1,           // optional
            FuncPart::InlineExport => 2, // optional, repeatable
            FuncPart::Param => 3,        // optional, repeatable
            FuncPart::Result => 4,       // optional, repeatable,
            FuncPart::Local(_) => 5,     // optional, repeatable, on individual lines
            _ => return Err("not a function header field"),
        };

        // Check for order
        // whether required func keyword is skipped
        if self.next_field == 0 && part_pos > 0 {
            return Err("invalid field order");
        }

        let repeatable = matches!(
            part,
            FuncPart::InlineExport | FuncPart::Param | FuncPart::Result | FuncPart::Local(_)
        );
        let order_invalid = if repeatable {
            part_pos + 1 < self.next_field
        } else {
            part_pos < self.next_field
        };
        if order_invalid {
            return Err("invalid field order");
        }

        // Transition state
        self.next_field = part_pos + 1;

        Ok(())
    }
}

impl SyntaxState {
    fn transit_state(
        &mut self,
        info: &LineInfo,
        mut callback: impl FnMut(&SyntaxState),
    ) -> Result<(), &'static str> {
        if info.synthetic_before.module_syntax_first {
            for part in &info.synthetic_before.module_field_syntax {
                self.transit_state_from_func_part(*part)?;
                callback(self);
            }
            if info.synthetic_before.end_opcodes > 0 {
                self.transit_state_from_instruction()?;
                callback(self);
            }
        } else {
            if info.synthetic_before.end_opcodes > 0 {
                self.transit_state_from_instruction()?;
                callback(self);
            }
            for part in &info.synthetic_before.module_field_syntax {
                self.transit_state_from_func_part(*part)?;
                callback(self);
            }
        }

        if !info.is_active() {
            return Ok(());
        }

        let mut has_result = false;
        match &info.kind {
            LineKind::Empty | LineKind::Malformed(_) => {}
            LineKind::Instr(_) => {
                self.transit_state_from_instruction()?;
                callback(self);
            }
            LineKind::Other(module_part) => {
                match module_part {
                    ModulePart::Func(parts) => {
                        for part in parts {
                            // Codillon wants local declarations on their own line.
                            if matches!(part, FuncPart::Local(_)) && parts.len() > 1 {
                                return Err("fields must be on separate lines");
                            }

                            if matches!(part, FuncPart::Result) {
                                has_result = true;
                            }

                            self.transit_state_from_func_part(*part)?;
                            callback(self);

                            if *self == SyntaxState::Initial && has_result {
                                return Err("function with results must end on another line");
                            }
                        }
                    }
                    _ => {
                        if *self != SyntaxState::Initial {
                            return Err("invalid field order");
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn transit_state_from_instruction(&mut self) -> Result<(), &'static str> {
        *self = match self {
            SyntaxState::AfterFuncHeader(_) | SyntaxState::AfterInstruction => {
                SyntaxState::AfterInstruction
            }
            _ => return Err("instruction outside function body"),
        };
        Ok(())
    }

    fn transit_state_from_func_part(&mut self, part: FuncPart) -> Result<(), &'static str> {
        *self = match (&self, part) {
            (SyntaxState::Initial, FuncPart::LParen) => SyntaxState::AfterLParen,
            (SyntaxState::AfterLParen, FuncPart::FuncKeyword) => {
                SyntaxState::AfterFuncHeader(FuncHeader { next_field: 1 })
            }
            (
                &&mut SyntaxState::AfterFuncHeader(mut fh),
                FuncPart::Id
                | FuncPart::InlineExport
                | FuncPart::Param
                | FuncPart::Result
                | FuncPart::Local(_),
            ) => {
                fh.transit_state(part)?;
                SyntaxState::AfterFuncHeader(fh)
            }
            (SyntaxState::AfterFuncHeader(_) | SyntaxState::AfterInstruction, FuncPart::RParen) => {
                SyntaxState::Initial
            }
            _ => return Err("invalid field order"),
        };
        Ok(())
    }
}

/// Fix frames (and missing function beginning and end) by deactivating
/// unmatched ends, appending ends as necessary to close open
/// frames, prepending "(" and "func" and appending ")" as necessary, etc.
pub fn fix_syntax(lines: &mut impl LineInfosMut) {
    use crate::line::Activity::*;
    use SyntaxState::*;
    type DefinedLabel = Option<String>;

    let mut state = Initial;
    let mut frame_stack: Vec<(InstrKind, DefinedLabel)> = Vec::new();
    let mut imports_allowed = true;

    // Structures for symbolic references
    let mut module_symbol_defs = ModuleIdentifiers::default();
    let mut local_symbol_defs: HashSet<String> = HashSet::new();

    assert!(lines.len() > 0);

    // first pass: disable imports appearing after other module fields,
    // and declare module-scope symbolic ids for any surviving lines.

    // There are probably still cases that can produce a "bounce."
    for line_no in 0..lines.len() {
        lines.set_synthetic_before(line_no, SyntheticWasm::default());
        lines.set_active_status(line_no, Active);
        lines.set_invalid(line_no, None);
        lines.set_runtime_error(line_no, None);

        let line_kind = lines.info(line_no).kind.stripped_clone();

        // If line will be rejected in second pass, don't let it define a symbol.
        let orig_state = state;
        match line_kind {
            LineKind::Instr(_) => match state {
                // Fix #2 (simulation from below): prepend "(func" if an instruction appears at module scope
                Initial => {
                    let _ = state.transit_state_from_func_part(FuncPart::LParen);
                    let _ = state.transit_state_from_func_part(FuncPart::FuncKeyword);
                }
                // Fix #3 (simulation from below): prepend "func" if an instruction appears after just "("
                AfterLParen => {
                    let _ = state.transit_state_from_func_part(FuncPart::FuncKeyword);
                }
                _ => {}
            },

            // Enforce no imports after other module fields
            LineKind::Other(module_part) => match module_part {
                ModulePart::Import(_) if imports_allowed => {}
                ModulePart::Import(_) if !imports_allowed => {
                    lines.set_active_status(
                        line_no,
                        Inactive("imports must appear before other module fields"),
                    );
                    continue;
                }
                _ => imports_allowed = false,
            },
            LineKind::Empty | LineKind::Malformed(_) => (),
        }

        if state.transit_state(lines.info(line_no), |_| {}).is_err() {
            state = orig_state;
            continue;
        }

        // Collect module-level symbols
        if let Err(reason) =
            collect_module_symbols(&lines.info(line_no).symbols, &mut module_symbol_defs)
        {
            lines.set_active_status(line_no, Inactive(reason));
            continue;
        }
    }

    // second pass: disable other lines that would make module malformed
    state = Initial;
    for line_no in 0..lines.len() {
        let line_kind = lines.info(line_no).kind.stripped_clone();

        // Enforce correct symbolic reference consumption
        if lines.info(line_no).is_active() {
            // end/else must match the label of the frame being closed, not any enclosing frame
            let label_symbol_defs: Vec<String> = match line_kind {
                LineKind::Instr(InstrKind::End) | LineKind::Instr(InstrKind::Else) => frame_stack
                    .last()
                    .and_then(|(_, label)| label.clone())
                    .into_iter()
                    .collect(),
                _ => frame_stack
                    .iter()
                    .filter_map(|(_, label)| label.clone())
                    .collect(),
            };
            if !symbols_resolved(
                &lines.info(line_no).symbols,
                &module_symbol_defs,
                &local_symbol_defs,
                &label_symbol_defs,
            ) {
                lines.set_active_status(line_no, Inactive("undefined symbolic reference"));
                continue;
            }
        }

        // Fixup instructions that appear where they don't belong
        if matches!(lines.info(line_no).kind, LineKind::Instr(_)) {
            match state {
                // Fix #1: (no longer needed)
                // Fix #2: prepend "(func" if an instruction appears at module scope
                Initial => lines.set_synthetic_before(
                    line_no,
                    SyntheticWasm {
                        module_field_syntax: vec![FuncPart::LParen, FuncPart::FuncKeyword],
                        ..Default::default()
                    },
                ),
                // Fix #3: prepend "func" if an instruction appears after just "("
                AfterLParen => lines.set_synthetic_before(
                    line_no,
                    SyntheticWasm {
                        module_field_syntax: vec![FuncPart::FuncKeyword],
                        ..Default::default()
                    },
                ),
                _ => {}
            }
        }

        // Process the line and transition the syntax state
        let orig_state = state;
        {
            let res = state.transit_state(lines.info(line_no), |_| {});
            match res {
                Ok(()) => {
                    if state == Initial {
                        // Fix #4: at end of function body, synthetically close all open frames
                        lines.set_synthetic_before(
                            line_no,
                            SyntheticWasm {
                                end_opcodes: frame_stack.len(),
                                ..Default::default()
                            },
                        );
                        frame_stack.clear();
                        local_symbol_defs.clear();
                    }
                }
                Err(e) => {
                    // If state transition is unacceptable here, disable line, revert state, and skip to next line
                    lines.set_active_status(line_no, Inactive(e));
                    state = orig_state;
                    continue;
                }
            }
        }

        // Enforce syntax requirements of structured instructions
        if let LineKind::Instr(instr_kind) = line_kind {
            match instr_kind {
                // For a structured instruction that opens a frame, log this.
                InstrKind::If | InstrKind::Loop | InstrKind::OtherStructured => {
                    lines.set_active_status(line_no, Active);
                    frame_stack.push((
                        instr_kind,
                        collect_label_symbol(&lines.info(line_no).symbols),
                    ));
                }

                // Fix #5: if an `else` appears outside an `if` frame, disable it
                InstrKind::Else => {
                    if let Some((InstrKind::If, _)) = frame_stack.last() {
                        lines.set_active_status(line_no, Active);
                        // Carry over labels from If frame
                        let (_, if_label) = frame_stack.pop().unwrap();
                        frame_stack.push((InstrKind::Else, if_label));
                    } else {
                        lines.set_active_status(line_no, Inactive("‘else’ outside ‘if’"));
                    }
                }

                // Fix #6: if an `end` appears outside a frame, disable it
                InstrKind::End => lines.set_active_status(
                    line_no,
                    if frame_stack.pop().is_some() {
                        Active
                    } else {
                        Inactive("nothing to end")
                    },
                ),
                InstrKind::Other => (),
            }
        }

        // Collect defined local symbolic references if there's any.
        // This collection should be after the state transition, because both of them can
        // inactivate a line and need to revert the state and/or remove collected symbols after
        // the inactivation. Reverting state is much simpler than removing symbols.
        if lines.info(line_no).is_active() {
            let collect_result =
                collect_local_symbols(&lines.info(line_no).symbols, &mut local_symbol_defs);
            if let Err(reason) = collect_result {
                // Inactivate line and revert state
                lines.set_active_status(line_no, Inactive(reason));
                state = orig_state;
                continue;
            }
        }
    }

    match state {
        // Fix #7: if ending with "(" state, close with "func)"
        AfterLParen => {
            assert!(frame_stack.is_empty());

            // Make sure there is an empty line that can become a synthetic "func)"
            if !matches!(lines.info(lines.len() - 1).kind, LineKind::Empty) {
                lines.push();
            }

            lines.set_active_status(lines.len() - 1, Active);

            lines.set_synthetic_before(
                lines.len() - 1,
                SyntheticWasm {
                    module_syntax_first: false,
                    end_opcodes: 0,
                    module_field_syntax: vec![FuncPart::FuncKeyword, FuncPart::RParen],
                },
            );
        }

        // Fix #8: if function wasn't ended, close outstanding frames, then close the function
        AfterFuncHeader(_) | AfterInstruction => {
            // Make sure there is an empty line that can become a synthetic ")"
            if !matches!(lines.info(lines.len() - 1).kind, LineKind::Empty) {
                lines.push();
            }
            lines.set_active_status(lines.len() - 1, Active);
            lines.set_synthetic_before(
                lines.len() - 1,
                SyntheticWasm {
                    module_syntax_first: false,
                    end_opcodes: frame_stack.len(),
                    module_field_syntax: vec![FuncPart::RParen],
                },
            );
        }
        _ => {}
    }
}

pub fn find_function_ranges(code: &impl LineInfos) -> Vec<(usize, usize)> {
    use SyntaxState::*;
    let mut ranges = Vec::new();
    let mut state = Initial;
    let mut current_start: Option<usize> = None;

    for line_no in 0..code.len() {
        let mut prev_state = state;
        let on_transition = |new_state: &SyntaxState| {
            match (current_start, prev_state, new_state) {
                (None, _, AfterFuncHeader(_)) => current_start = Some(line_no),

                (
                    Some(start_line),
                    AfterInstruction | AfterFuncHeader(FuncHeader { .. }),
                    Initial,
                ) => {
                    ranges.push((start_line, line_no));
                    current_start = None;
                }

                (Some(_), _, Initial) => current_start = None,

                _ => (),
            }

            prev_state = *new_state;
        };

        state
            .transit_state(code.info(line_no), on_transition)
            .expect("well-formed");
    }
    ranges
}

pub fn parse_line(s: &str) -> LineKind {
    match ParseBuffer::new(s) {
        Ok(buf) => match parser::parse::<LineKind>(&buf) {
            Ok(kind) => kind,
            Err(e) => e.into(),
        },
        Err(e) => e.into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::line::{Activity, LineInfo};
    use wast::parser::parse;

    struct TestLineInfos {
        lines: Vec<LineInfo>,
    }

    impl TestLineInfos {
        fn new<const N: usize>(instrs: [&str; N]) -> Self {
            Self {
                lines: instrs
                    .into_iter()
                    .map(|x| LineInfo {
                        kind: crate::syntax::parse_line(x),
                        active: crate::line::Activity::Active,
                        ..Default::default()
                    })
                    .collect(),
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
    }

    impl LineInfosMut for TestLineInfos {
        fn set_active_status(&mut self, index: usize, new_val: Activity) {
            self.lines[index].active = new_val;
        }

        fn set_synthetic_before(&mut self, index: usize, synth: SyntheticWasm) {
            self.lines[index].synthetic_before = synth;
        }

        fn push(&mut self) {
            self.lines.push(LineInfo::default());
        }

        fn set_invalid(&mut self, index: usize, reason: Option<String>) {
            self.lines[index].invalid = reason;
        }

        fn set_runtime_error(&mut self, _index: usize, _msg: Option<String>) {}
    }

    fn malf(s: &str) -> LineKind {
        LineKind::Malformed(String::from(s))
    }

    #[test]
    fn test_parse_modulepart() -> Result<()> {
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("    (   ")?)?,
            ModulePart::Func(vec![FuncPart::LParen])
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("    )    ")?)?,
            ModulePart::Func(vec![FuncPart::RParen])
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("    func    ")?)?,
            ModulePart::Func(vec![FuncPart::FuncKeyword])
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("    $x    ")?)?,
            ModulePart::Func(vec![FuncPart::Id])
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("  ( export \"main\")    ")?)?,
            ModulePart::Func(vec![FuncPart::InlineExport])
        );
        assert!(
            parse::<ModulePart>(&ParseBuffer::new("  ( import \"foo\" \"bar\" )    ")?).is_err(),
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new(
                r#" ( import "draw" "clear" (func $bar)) "#
            )?)?,
            ModulePart::Import(ImportKind::Import)
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new(r#"  ( export "foo" (func $bar))    "#)?)?,
            ModulePart::Export
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("    (param $x i32)    ")?)?,
            ModulePart::Func(vec![FuncPart::Param])
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("    ( result i32 f32 i64   )    ")?)?,
            ModulePart::Func(vec![FuncPart::Result])
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("    (local $x    f64 )    ")?)?,
            ModulePart::Func(vec![FuncPart::Local(1)])
        );
        assert!(parse::<ModulePart>(&ParseBuffer::new("( param")?).is_err());
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("()")?)?,
            ModulePart::Func(vec![FuncPart::LParen, FuncPart::RParen])
        );
        Ok(())
    }

    #[test]
    fn test_parse_linekind() -> Result<()> {
        assert_eq!(parse::<LineKind>(&ParseBuffer::new("")?)?, LineKind::Empty);

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new("     ")?)?,
            LineKind::Empty
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new("i32.const 4")?)?,
            LineKind::Instr(InstrKind::Other)
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new("   i32.const 4   ")?)?,
            LineKind::Instr(InstrKind::Other)
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new("   if   ")?)?,
            LineKind::Instr(InstrKind::If)
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new("   (   ")?)?,
            LineKind::Other(ModulePart::Func(vec![FuncPart::LParen]))
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new(")")?)?,
            LineKind::Other(ModulePart::Func(vec![FuncPart::RParen]))
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new("()")?)?,
            LineKind::Other(ModulePart::Func(vec![FuncPart::LParen, FuncPart::RParen]))
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new(")func")?)?,
            LineKind::Other(ModulePart::Func(vec![
                FuncPart::RParen,
                FuncPart::FuncKeyword
            ]))
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new(") func")?)?,
            LineKind::Other(ModulePart::Func(vec![
                FuncPart::RParen,
                FuncPart::FuncKeyword
            ]))
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new("   (func)   ")?)?,
            LineKind::Other(ModulePart::Func(vec![
                FuncPart::LParen,
                FuncPart::FuncKeyword,
                FuncPart::RParen
            ]))
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new(
                " ( func $foo ( export \"main\") (param $x i32) (result i32 f64) (local $tmp i64)"
            )?)?,
            LineKind::Other(ModulePart::Func(vec![
                FuncPart::LParen,
                FuncPart::FuncKeyword,
                FuncPart::Id,
                FuncPart::InlineExport,
                FuncPart::Param,
                FuncPart::Result,
                FuncPart::Local(1)
            ]))
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new(
                " ( func $foo ( export \"main\") (import \"draw\" \"point\") (param $x f64) (param $y f64))"
            )?)?,
            LineKind::Other(ModulePart::Import(ImportKind::InlineFunc))
        );

        assert!(
            parse::<LineKind>(&ParseBuffer::new(
                "(import \"draw\" \"clear\" (func $bar)) (func $baz) (export \"foo\" (func $baz))"
            )?)
            .is_err()
        );

        assert!(parse::<LineKind>(&ParseBuffer::new(") func i32.const 7")?).is_err());

        Ok(())
    }

    #[test]
    fn test_func_header() -> Result<()> {
        // valid state transitions
        let mut header_valid = FuncHeader::default();
        let _ = header_valid.transit_state(FuncPart::FuncKeyword);
        assert_eq!(header_valid.next_field, 1);
        let _ = header_valid.transit_state(FuncPart::InlineExport);
        assert_eq!(header_valid.next_field, 3);
        let _ = header_valid.transit_state(FuncPart::InlineExport);
        assert_eq!(header_valid.next_field, 3);
        let _ = header_valid.transit_state(FuncPart::Result);
        assert_eq!(header_valid.next_field, 5);
        let _ = header_valid.transit_state(FuncPart::Local(1));
        assert_eq!(header_valid.next_field, 6);
        let _ = header_valid.transit_state(FuncPart::Local(3));
        assert_eq!(header_valid.next_field, 6);

        // param after local
        assert!(header_valid.transit_state(FuncPart::Param).is_err());

        // no func keyword
        let mut header_no_func_keyword = FuncHeader::default();
        assert!(
            header_no_func_keyword
                .transit_state(FuncPart::Param)
                .is_err()
        );

        // invalid order
        let mut header_invalid_order = FuncHeader::default();
        let _ = header_invalid_order.transit_state(FuncPart::FuncKeyword);
        let _ = header_invalid_order.transit_state(FuncPart::Result);
        assert!(header_invalid_order.transit_state(FuncPart::Id).is_err());

        // repeat non-repeatable field
        let mut header_non_repeatable = FuncHeader::default();
        let _ = header_non_repeatable.transit_state(FuncPart::FuncKeyword);
        assert!(
            header_non_repeatable
                .transit_state(FuncPart::FuncKeyword)
                .is_err()
        );

        Ok(())
    }

    #[test]
    fn test_is_well_formed_instr() -> Result<()> {
        //well-formed instructions
        assert_eq!(parse_line("i32.add"), LineKind::Instr(InstrKind::Other));
        assert_eq!(parse_line("i32.const 5"), LineKind::Instr(InstrKind::Other));
        //not well-formed "instructions"
        assert_eq!(
            parse_line("i32.bogus"),
            malf("unknown operator or unexpected token")
        );
        assert_eq!(parse_line("i32.const"), malf("expected an i32"));
        assert_eq!(parse_line("i32.const x"), malf("expected an i32"));
        //not well-formed "instructions": multiple instructions per line, folded instructions
        assert_eq!(
            parse_line("i32.const 4 i32.const 5"),
            malf("extra tokens remaining after parse")
        );
        assert_eq!(parse_line("(i32.const 4)"), malf("unexpected token"));
        assert_eq!(
            parse_line("(i32.add (i32.const 4) (i32.const 5))"),
            malf("unexpected token")
        );
        //spaces before and after, comments, and empty lines are well-formed
        assert_eq!(
            parse_line("    i32.const 5"),
            LineKind::Instr(InstrKind::Other)
        );
        assert_eq!(
            parse_line("    i32.const 5 ;; hello "),
            LineKind::Instr(InstrKind::Other)
        );
        assert_eq!(
            parse_line("i32.const 5     "),
            LineKind::Instr(InstrKind::Other)
        );
        assert_eq!(parse_line(";;Hello"), LineKind::Empty);
        assert_eq!(parse_line("   ;; Hello "), LineKind::Empty);
        assert_eq!(
            parse_line("i32.const 5   ;;this is a const"),
            LineKind::Instr(InstrKind::Other)
        );
        //        assert_eq!(parse_line(""), LineKind::Instr(InstrKind::Empty);
        //        assert_eq!(parse_line("   "), LineKind::Instr(InstrKind::Empty);
        assert_eq!(parse_line("if"), LineKind::Instr(InstrKind::If));
        assert_eq!(
            parse_line("if (result i32)"),
            LineKind::Instr(InstrKind::If)
        );
        assert_eq!(parse_line("   else   "), LineKind::Instr(InstrKind::Else));
        assert_eq!(parse_line("   end   "), LineKind::Instr(InstrKind::End));
        assert_eq!(
            parse_line("   block   "),
            LineKind::Instr(InstrKind::OtherStructured)
        );
        assert_eq!(parse_line("   loop   "), LineKind::Instr(InstrKind::Loop));
        assert_eq!(
            parse_line("   try_table   "),
            LineKind::Instr(InstrKind::OtherStructured)
        );
        Ok(())
    }

    #[test]
    fn test_fix_syntax() {
        {
            let mut infos1 = TestLineInfos::new(["(func", "block", "i32.const 42", "drop", ")"]);
            fix_syntax(&mut infos1);
            assert!(infos1.lines.iter().all(|x| x.is_active()));
            assert_eq!(infos1.lines[4].synthetic_before.end_opcodes, 1);
        }

        {
            let mut infos2 = TestLineInfos::new([
                "(func", // 0
                "if",    // 1
                "block", // 2
                "else",  // 3
                "end",   // 4
                ")",     // 5
            ]);
            fix_syntax(&mut infos2);
            assert_eq!(infos2.lines[5].synthetic_before.end_opcodes, 1);
            assert_eq!(
                infos2
                    .lines
                    .iter()
                    .map(|x| x.is_active())
                    .collect::<Vec<_>>(),
                [true, true, true, false, true, true]
            );
        }

        {
            let mut infos3 = TestLineInfos::new(["end"]);
            fix_syntax(&mut infos3);
            assert!(!infos3.lines[0].is_active());
        }

        {
            // Test case 4: Simple block
            let mut infos4 =
                TestLineInfos::new(["(func", "block", "i32.const 42", "drop", "end", ")"]);
            fix_syntax(&mut infos4);
        }

        {
            // Test case 5: Complex nested structure
            let mut infos5 = TestLineInfos::new([
                "(func",        // 0
                "block",        // 1
                "i32.const 42", // 2
                "drop",         // 3
                "end",          // 4
                "block",        // 5
                "loop",         // 6
                "if",           // 7
                "i32.const 43", // 8
                "drop",         // 9
                "else",         // 10
                "i32.const 44", // 11
                "drop",         // 12
                "end",          // 13
                "end",          // 14
                "end",          // 15
                ")",            // 16
            ]);
            fix_syntax(&mut infos5);
        }

        {
            // Test case 6: Imports after other module fields
            // (N.B. needs to be an acceptable mod/field or else marked malformed and still "active")
            let mut infos6 = TestLineInfos::new([
                "(global i32 (i32.const 0))",
                "(import \"draw\" \"clear\" (func))",
            ]);
            fix_syntax(&mut infos6);
            assert!(infos6.lines[0].is_active());
            assert!(!infos6.lines[1].is_active());

            infos6 = TestLineInfos::new([
                "(global i32 (i32.const 0))",
                "(func (import \"draw\" \"clear\"))",
            ]);
            fix_syntax(&mut infos6);
            assert!(infos6.lines[0].is_active());
            assert!(!infos6.lines[1].is_active());

            infos6 = TestLineInfos::new(["(func)", "(import \"draw\" \"clear\" (func))"]);
            fix_syntax(&mut infos6);
            assert!(infos6.lines[0].is_active());
            assert!(!infos6.lines[1].is_active());

            infos6 = TestLineInfos::new(["(func)", "(func (import \"draw\" \"clear\"))"]);
            fix_syntax(&mut infos6);
            assert!(infos6.lines[0].is_active());
            assert!(!infos6.lines[1].is_active());
        }

        {
            // Func with params and results on one line
            let mut infos = TestLineInfos::new(["(func $x (param i32) (result f64))"]);
            fix_syntax(&mut infos);
            assert!(!infos.lines[0].is_well_formed());
        }

        {
            // Imported function, then a non-import module field
            let mut infos = TestLineInfos::new([
                "(func $x (import \"draw\" \"set_color\") (param i32 i32 i32))",
                "(memory 1)",
            ]);
            fix_syntax(&mut infos);
            assert!(infos.lines[0].is_well_formed());
            assert!(infos.lines[1].is_well_formed());
        }

        {
            // Attempted multi-line imported function (import on first line)
            let mut infos = TestLineInfos::new([
                "(func $x (import \"draw\" \"set_color\")",
                "(param i32 i32 i32))",
                "(memory 1)",
            ]);
            fix_syntax(&mut infos);
            assert!(!infos.lines[0].is_well_formed());
            assert!(infos.lines[0].is_active());
            assert!(!infos.lines[1].is_well_formed());
            assert!(!infos.lines[1].is_active());
            assert!(infos.lines[2].is_well_formed());
            assert_eq!(
                infos.lines[0].kind,
                LineKind::Malformed("expected `)`".to_string())
            );
            assert_eq!(
                infos.lines[1].active,
                Activity::Inactive("invalid field order")
            );
        }

        {
            // Attempted multi-line imported function (import on second line)
            let mut infos = TestLineInfos::new([
                "(func $x",
                "(import \"draw\" \"set_color\") (param i32 i32 i32))",
                "(memory 1)",
            ]);
            fix_syntax(&mut infos);
            assert!(infos.lines[0].is_well_formed());
            assert_eq!(
                infos.lines[1].kind,
                LineKind::Malformed("imports must be on one line".to_string())
            );
        }

        {
            // Multi-line exported function
            let mut infos = TestLineInfos::new([
                "(func $x",
                "(export \"hello\") (param i32 i32 i32))",
                "(memory 1)",
            ]);
            fix_syntax(&mut infos);
            assert!(infos.lines[0].is_well_formed());
            assert!(infos.lines[1].is_well_formed());
            assert!(infos.lines[2].is_well_formed());
        }

        {
            // Multi-line exported+imported function (inline import after inline export)
            let mut infos = TestLineInfos::new([
                "(func $x",
                "(export \"hello\") (import \"draw\" \"set_color\") (param i32 i32 i32))",
                "(memory 1)",
            ]);
            fix_syntax(&mut infos);
            assert!(infos.lines[0].is_well_formed());
            assert!(!infos.lines[1].is_well_formed());
            assert!(!infos.lines[2].is_well_formed());
            assert_eq!(
                infos.lines[1].kind,
                LineKind::Malformed("imports must be on one line".to_string())
            );
            assert_eq!(
                infos.lines[2].active,
                Activity::Inactive("invalid field order")
            );
        }

        {
            // Imported function, then another import
            let mut infos = TestLineInfos::new([
                "(func $x (import \"draw\" \"set_color\") (param i32 i32 i32))",
                "(memory (import \"listen\" \"listen_memory\") 1)",
            ]);
            fix_syntax(&mut infos);
            assert!(infos.lines[0].is_well_formed());
            assert!(infos.lines[1].is_well_formed());
        }

        {
            // Bad global import
            let mut infos = TestLineInfos::new(["(global (import \"x\" \"y\") i32)"]);
            fix_syntax(&mut infos);
            dbg!(&infos.lines[0]);
            assert!(!infos.lines[0].is_well_formed());
        }

        {
            // Bad memory import
            let mut infos = TestLineInfos::new(["(memory (import \"x\" \"y\") 1)"]);
            fix_syntax(&mut infos);
            dbg!(&infos.lines[0]);
            assert!(!infos.lines[0].is_well_formed());
        }
    }
}
