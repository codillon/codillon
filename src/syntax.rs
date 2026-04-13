// Structures and utilities to support multi-function/multi-module text buffers.

use crate::line::LineInfo;
use crate::symbolic::{
    ModuleIdentifiers, collect_label_symbol, collect_local_symbols, collect_module_symbols,
    symbols_resolved,
};
use anyhow::Result;
use std::collections::HashSet;
use std::ops::Deref;
use wast::{
    Error,
    core::{
        Export, Global, Imports, InlineExport, InlineImport, Instruction, LocalParser, Memory,
        Table, ValType,
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
pub enum ModulePart {
    LParen,
    RParen,
    FuncKeyword,
    Id,
    Export,
    Import,
    InlineExport,
    InlineImport,
    Param,
    Result,
    Local(usize), // number of locals declared
    Global,
    Table,
    Memory,
}

impl From<ModulePart> for &'static str {
    fn from(val: ModulePart) -> &'static str {
        use ModulePart::*;
        match val {
            LParen => "(",
            RParen => ")",
            FuncKeyword => "func",
            _ => panic!("ModulePart cannot be rendered in text"),
        }
    }
}

impl From<kw::func> for ModulePart {
    fn from(_: kw::func) -> Self {
        ModulePart::FuncKeyword
    }
}

impl<'a> From<Id<'a>> for ModulePart {
    fn from(_: Id) -> Self {
        ModulePart::Id
    }
}

impl<'a> From<InlineImport<'a>> for ModulePart {
    fn from(_: InlineImport) -> Self {
        ModulePart::InlineImport
    }
}

impl<'a> From<Imports<'a>> for ModulePart {
    fn from(_: Imports) -> Self {
        ModulePart::Import
    }
}

impl<'a> From<InlineExport<'a>> for ModulePart {
    fn from(_: InlineExport) -> Self {
        ModulePart::InlineExport
    }
}

impl<'a> From<Export<'a>> for ModulePart {
    fn from(_: Export) -> Self {
        ModulePart::Export
    }
}

impl<'a> From<LocalParser<'a>> for ModulePart {
    fn from(parser: LocalParser) -> Self {
        ModulePart::Local(parser.locals.len())
    }
}

impl<'a> From<Memory<'a>> for ModulePart {
    fn from(_: Memory) -> Self {
        ModulePart::Memory
    }
}

impl<'a> From<Global<'a>> for ModulePart {
    fn from(_: Global) -> Self {
        ModulePart::Global
    }
}

impl<'a> From<Table<'a>> for ModulePart {
    fn from(_: Table) -> Self {
        ModulePart::Table
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub enum LineKind {
    #[default]
    Empty,
    Instr(InstrKind),
    Other(Vec<ModulePart>),
    Malformed(String), // explanation
}

pub trait LineInfos {
    fn is_empty(&self) -> bool;
    fn len(&self) -> usize;
    fn info(&self, index: usize) -> impl Deref<Target = crate::line::LineInfo>;
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
    pub indent: usize,
    pub start: usize,
    pub end: usize,
    pub unclosed: bool,
    pub kind: InstrKind,
    pub wide: bool,
}

pub trait FrameInfosMut: LineInfos {
    fn set_indent(&mut self, index: usize, num: usize);
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
        // Prioritize fields of format "(...)" over single "(" token

        if parser.peek::<InlineExport>()? {
            Ok(parser.parse::<InlineExport<'a>>()?.into())
        } else if parser.peek2::<kw::export>()? {
            parser.parens(|p| Ok(p.parse::<Export<'a>>()?.into()))
        } else if parser.peek::<InlineImport>()? {
            Ok(parser.parse::<InlineImport<'a>>()?.into())
        } else if parser.peek2::<kw::import>()? {
            parser.parens(|p| {
                let imports = p.parse::<Imports<'a>>()?;
                match imports {
                    Imports {
                        items:
                            wast::core::ImportItems::Single {
                                sig:
                                    wast::core::ItemSig {
                                        kind:
                                            wast::core::ItemKind::Func { .. }
                                            | wast::core::ItemKind::Global(_)
                                            | wast::core::ItemKind::Memory(_),
                                        ..
                                    },
                                ..
                            },
                        ..
                    } => Ok(imports.into()),
                    _ => Err(parser.error("unsupported import kind")),
                }
            })
        } else if parser.peek2::<kw::param>()? {
            // Modified from FunctionType parser
            parser.parens(|p| {
                p.parse::<kw::param>()?;
                if p.is_empty() {
                    return Ok(ModulePart::Param);
                }

                let id = p.parse::<Option<Id<'a>>>()?;
                let parse_more = id.is_none();
                p.parse::<ValType<'a>>()?;
                while parse_more && !p.is_empty() {
                    p.parse::<ValType<'a>>()?;
                }

                Ok(ModulePart::Param)
            })
        } else if parser.peek2::<kw::result>()? {
            // Modified from FunctionType parser
            parser.parens(|p| {
                p.parse::<kw::result>()?;
                while !p.is_empty() {
                    p.parse::<ValType<'a>>()?;
                }

                Ok(ModulePart::Result)
            })
        } else if parser.peek2::<kw::local>()? {
            // Modified from Local parse_remainder
            parser.parens(|_| Ok(parser.parse::<LocalParser<'a>>()?.into()))
        } else if parser.peek2::<kw::memory>()? {
            parser.parens(|p| match p.parse::<Memory<'a>>()? {
                Memory {
                    kind: wast::core::MemoryKind::Import { .. },
                    ..
                } => Ok(ModulePart::Import),
                _ => Ok(ModulePart::Memory),
            })
        } else if parser.peek2::<kw::table>()? {
            parser.parens(|p| match p.parse::<Table<'a>>()? {
                Table {
                    kind: wast::core::TableKind::Import { .. },
                    ..
                } => Err(parser.error("unsupported import kind")),
                _ => Ok(ModulePart::Table),
            })
        } else if parser.peek2::<kw::global>()? {
            parser.parens(|p| match p.parse::<Global<'a>>()? {
                Global {
                    kind: wast::core::GlobalKind::Import { .. },
                    ..
                } => Ok(ModulePart::Import),
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
        } else if parser.step(|cursor| match cursor.lparen()? {
            Some(rest) => Ok((true, rest)),
            None => Ok((false, cursor)),
        })? {
            Ok(ModulePart::LParen)
        } else if parser.step(|cursor| match cursor.rparen()? {
            Some(rest) => Ok((true, rest)),
            None => Ok((false, cursor)),
        })? {
            Ok(ModulePart::RParen)
        } else if parser.peek::<kw::func>()? {
            Ok(parser.parse::<kw::func>()?.into())
        } else if parser.peek::<Id<'a>>()? {
            Ok(parser.parse::<Id<'a>>()?.into())
        } else {
            Err(parser.error("expected a non-instruction token"))
        }
    }
}

impl Peek for ModulePart {
    fn peek(cursor: Cursor) -> Result<bool, Error> {
        if cursor.peek_lparen()?
            || cursor.peek_rparen()?
            || kw::func::peek(cursor)?
            || Id::peek(cursor)?
        {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn display() -> &'static str {
        "Wasm text other than an instruction"
    }
}

impl<'a> Parse<'a> for LineKind {
    fn parse(parser: Parser<'a>) -> Result<Self, Error> {
        // In the Codillon editor, a line can contain a single instruction
        // or a (possibly empty) sequence of ModuleParts.

        if parser.peek::<ModulePart>()? {
            let mut parts = Vec::new();
            while (!parser.is_empty()) || parser.peek::<ModulePart>()? {
                parts.push(parser.parse()?);
            }
            Ok(LineKind::Other(parts))
        } else if parser.is_empty() {
            Ok(LineKind::Empty)
        } else {
            Ok(LineKind::Instr(parser.parse::<Instruction>()?.into()))
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct SyntheticWasm {
    pub end_opcodes: usize,
    pub module_field_syntax: Vec<ModulePart>,
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
    AfterModuleFieldLParen,
    AfterFuncHeader(FuncHeader),
    AfterInstruction,
}

#[derive(Default, Copy, Clone, PartialEq, Debug)]
struct FuncHeader {
    /// Fields in a function header follows an order and some extra restrictions.
    /// This structure tracks and transits states, where each state indicates new fields allowed.
    // Whether the function has (import ...) field
    is_import: bool,
    // Lowest possible index for next field in the expected order
    next_field: usize,
}

impl FuncHeader {
    fn transit_state(&mut self, part: ModulePart) -> Result<(), &'static str> {
        // Find position (index) of part in the order list
        let part_pos = match part {
            // Expected order of function fields
            ModulePart::FuncKeyword => 0,  // required
            ModulePart::Id => 1,           // optional
            ModulePart::InlineExport => 2, // optional, repeatable
            ModulePart::InlineImport => 3, // optional
            ModulePart::Param => 4,        // optional, repeatable
            ModulePart::Result => 5,       // optional, repeatable,
            ModulePart::Local(_) => 6,     // optional, repeatable, on individual lines
            _ => return Err("not a function header field"),
        };

        // Check for order
        // whether required func keyword is skipped
        if self.next_field == 0 && part_pos > 0 {
            return Err("invalid field order");
        }

        let repeatable = matches!(
            part,
            ModulePart::Export | ModulePart::Param | ModulePart::Result | ModulePart::Local(_)
        );
        let order_invalid = if repeatable {
            part_pos + 1 < self.next_field
        } else {
            part_pos < self.next_field
        };
        if order_invalid {
            return Err("invalid field order");
        }

        // Check for function with (import ...)
        // function import can't have locals & instructions
        if self.is_import && matches!(part, ModulePart::Local(_)) {
            return Err("imported function cannot have locals");
        }

        // Transition state
        self.is_import |= matches!(part, ModulePart::InlineImport);
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
                self.transit_state_from_module_part(*part)?;
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
                self.transit_state_from_module_part(*part)?;
                callback(self);
            }
        }

        if !info.is_active() {
            return Ok(());
        }

        let mut hit_initial = false;
        let mut has_local = false;
        let mut has_result = false;
        match &info.kind {
            LineKind::Empty | LineKind::Malformed(_) => {}
            LineKind::Instr(_) => {
                self.transit_state_from_instruction()?;
                callback(self);
            }
            LineKind::Other(parts) => {
                for part in parts {
                    // Codiillon only wants one module-scope field per line. If this line reached the initial
                    // state and then had more syntax afterward, disable it, revert state, and skip to next.
                    // This rules out lines like "(memory 0) (func)" or "(func) (func)".
                    if hit_initial {
                        return Err("fields must be on separate lines");
                    }

                    // Codillon also only wants one local declaration per line.
                    if has_local {
                        return Err("fields must be on separate lines");
                    }
                    if matches!(part, ModulePart::Local(_)) {
                        has_local = true;
                        if parts.len() > 1 {
                            return Err("fields must be on separate lines");
                        }
                    }
                    if matches!(part, ModulePart::Result) {
                        has_result = true;
                    }

                    self.transit_state_from_module_part(*part)?;
                    callback(self);

                    if *self == SyntaxState::Initial {
                        hit_initial = true;
                        if has_result {
                            return Err("function with results must end on another line");
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

    fn transit_state_from_module_part(&mut self, part: ModulePart) -> Result<(), &'static str> {
        *self = match (&self, part) {
            (SyntaxState::Initial, ModulePart::LParen) => SyntaxState::AfterModuleFieldLParen,
            (
                SyntaxState::Initial,
                ModulePart::Memory
                | ModulePart::Table
                | ModulePart::Global
                | ModulePart::Import
                | ModulePart::Export,
            ) => SyntaxState::Initial,
            (SyntaxState::AfterModuleFieldLParen, ModulePart::FuncKeyword) => {
                SyntaxState::AfterFuncHeader(FuncHeader {
                    is_import: false,
                    next_field: 1,
                })
            }
            (
                &&mut SyntaxState::AfterFuncHeader(mut fh),
                ModulePart::Id
                | ModulePart::InlineExport
                | ModulePart::InlineImport
                | ModulePart::Param
                | ModulePart::Result
                | ModulePart::Local(_),
            ) => {
                fh.transit_state(part)?;
                SyntaxState::AfterFuncHeader(fh)
            }
            (
                SyntaxState::AfterFuncHeader(_) | SyntaxState::AfterInstruction,
                ModulePart::RParen,
            ) => SyntaxState::Initial,
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
    let mut before_imports = true;

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

        // Enforce no imports after other module fields
        if let LineKind::Other(parts) = &line_kind {
            let has_import = parts
                .iter()
                .any(|&p| matches!(p, ModulePart::Import | ModulePart::InlineImport));
            let module_field = parts.iter().any(|&p| {
                matches!(
                    p,
                    ModulePart::Memory
                        | ModulePart::Table
                        | ModulePart::Global
                        | ModulePart::Export
                )
            });
            if has_import && !before_imports {
                lines.set_active_status(
                    line_no,
                    Inactive("imports must appear before other module fields"),
                );
                continue;
            } else if module_field || (parts.contains(&ModulePart::RParen) && !has_import) {
                before_imports = false;
            }

            // Collect module-level symbols
            let collect_result =
                collect_module_symbols(&lines.info(line_no).symbols, &mut module_symbol_defs);
            if let Err(reason) = collect_result {
                lines.set_active_status(line_no, Inactive(reason));
                continue;
            }
        }
    }

    // second pass: disable other lines that would make module malformed
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
                // Fix #1: Disable an instruction that appears in an imported function, skipping to next line
                AfterFuncHeader(FuncHeader {
                    is_import: true, ..
                }) => {
                    lines.set_active_status(
                        line_no,
                        Inactive("imported functions cannot have instructions"),
                    );
                    continue;
                }
                // Fix #2: prepend "(func" if an instruction appears at module scope
                Initial => lines.set_synthetic_before(
                    line_no,
                    SyntheticWasm {
                        module_field_syntax: vec![ModulePart::LParen, ModulePart::FuncKeyword],
                        ..Default::default()
                    },
                ),
                // Fix #3: prepend "func" if an instruction appears after just "("
                AfterModuleFieldLParen => lines.set_synthetic_before(
                    line_no,
                    SyntheticWasm {
                        module_field_syntax: vec![ModulePart::FuncKeyword],
                        ..Default::default()
                    },
                ),
                _ => {}
            }
        }

        // Process the line and transition the syntax state
        let orig_state = state;
        {
            let res = state.transit_state(&lines.info(line_no), |_| {});
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
        AfterModuleFieldLParen => {
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
                    module_field_syntax: vec![ModulePart::FuncKeyword, ModulePart::RParen],
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
                    module_field_syntax: vec![ModulePart::RParen],
                },
            );
        }
        _ => {}
    }
}

pub fn find_import_lines(code: &impl LineInfos) -> Vec<usize> {
    let mut imports = Vec::new();
    for line_no in 0..code.len() {
        if let LineKind::Other(parts) = &code.info(line_no).kind
            && code.info(line_no).is_active()
            && parts
                .iter()
                .any(|&p| matches!(p, ModulePart::Import | ModulePart::InlineImport))
        {
            imports.push(line_no);
        }
    }
    imports
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
                    AfterInstruction
                    | AfterFuncHeader(FuncHeader {
                        is_import: false, ..
                    }),
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
            .transit_state(&code.info(line_no), on_transition)
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
            ModulePart::LParen
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("    )    ")?)?,
            ModulePart::RParen
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("    func    ")?)?,
            ModulePart::FuncKeyword
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("    $x    ")?)?,
            ModulePart::Id
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("  ( export \"main\")    ")?)?,
            ModulePart::InlineExport
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("  ( import \"foo\" \"bar\" )    ")?)?,
            ModulePart::InlineImport
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new(r#" ( import "foo" "bar" (func $bar)) "#)?)?,
            ModulePart::Import
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new(r#"  ( export "foo" (func $bar))    "#)?)?,
            ModulePart::Export
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("    (param $x i32)    ")?)?,
            ModulePart::Param
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("    ( result i32 f32 i64   )    ")?)?,
            ModulePart::Result
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("    (local $x    f64 )    ")?)?,
            ModulePart::Local(1)
        );
        assert!(parse::<ModulePart>(&ParseBuffer::new("( param")?).is_err());
        assert!(parse::<ModulePart>(&ParseBuffer::new("()")?).is_err());
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
            LineKind::Other(vec![ModulePart::LParen])
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new(")")?)?,
            LineKind::Other(vec![ModulePart::RParen])
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new("()")?)?,
            LineKind::Other(vec![ModulePart::LParen, ModulePart::RParen])
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new(")func")?)?,
            LineKind::Other(vec![ModulePart::RParen, ModulePart::FuncKeyword])
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new(") func")?)?,
            LineKind::Other(vec![ModulePart::RParen, ModulePart::FuncKeyword])
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new("   (func)   ")?)?,
            LineKind::Other(vec![
                ModulePart::LParen,
                ModulePart::FuncKeyword,
                ModulePart::RParen
            ])
        );

        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new(
                " ( func $foo ( export \"main\") (import \"modu\" \"bar\") (param $x i32) (result i32 f64) (local $tmp i64)"
            )?)?,
            LineKind::Other(vec![
                ModulePart::LParen,
                ModulePart::FuncKeyword,
                ModulePart::Id,
                ModulePart::InlineExport,
                ModulePart::InlineImport,
                ModulePart::Param,
                ModulePart::Result,
                ModulePart::Local(1)
            ])
        );
        assert_eq!(
            parse::<LineKind>(&ParseBuffer::new(
                "(import \"foo\" \"bar\" (func $bar)) (func $baz) (export \"foo\" (func $baz))"
            )?)?,
            LineKind::Other(vec![
                ModulePart::Import,
                ModulePart::LParen,
                ModulePart::FuncKeyword,
                ModulePart::Id,
                ModulePart::RParen,
                ModulePart::Export
            ])
        );

        assert!(parse::<LineKind>(&ParseBuffer::new(") func i32.const 7")?).is_err());

        Ok(())
    }

    #[test]
    fn test_func_header() -> Result<()> {
        // valid state transitions
        let mut header_valid = FuncHeader::default();
        let _ = header_valid.transit_state(ModulePart::FuncKeyword);
        assert!(!header_valid.is_import);
        assert_eq!(header_valid.next_field, 1);
        let _ = header_valid.transit_state(ModulePart::InlineExport);
        assert!(!header_valid.is_import);
        assert_eq!(header_valid.next_field, 3);
        let _ = header_valid.transit_state(ModulePart::InlineExport);
        assert!(!header_valid.is_import);
        assert_eq!(header_valid.next_field, 3);
        let _ = header_valid.transit_state(ModulePart::InlineImport);
        assert!(header_valid.is_import);
        assert_eq!(header_valid.next_field, 4);
        let _ = header_valid.transit_state(ModulePart::Result);
        assert!(header_valid.is_import);
        assert_eq!(header_valid.next_field, 6);

        // local after import
        assert!(header_valid.transit_state(ModulePart::Local(1)).is_err());

        // no func keyword
        let mut header_no_func_keyword = FuncHeader::default();
        assert!(
            header_no_func_keyword
                .transit_state(ModulePart::Param)
                .is_err()
        );

        // invalid order
        let mut header_invalid_order = FuncHeader::default();
        let _ = header_invalid_order.transit_state(ModulePart::FuncKeyword);
        let _ = header_invalid_order.transit_state(ModulePart::Result);
        assert!(header_invalid_order.transit_state(ModulePart::Id).is_err());

        // repeat non-repeatable field
        let mut header_non_repeatable = FuncHeader::default();
        let _ = header_non_repeatable.transit_state(ModulePart::FuncKeyword);
        assert!(
            header_non_repeatable
                .transit_state(ModulePart::FuncKeyword)
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
        assert_eq!(
            parse_line("(i32.const 4)"),
            malf("expected a non-instruction token")
        );
        assert_eq!(
            parse_line("(i32.add (i32.const 4) (i32.const 5))"),
            malf("expected a non-instruction token")
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
            let mut infos6 =
                TestLineInfos::new(["(global i32 (i32.const 0))", "(import \"\" \"\" (func))"]);
            fix_syntax(&mut infos6);
            assert!(infos6.lines[0].is_active());
            assert!(!infos6.lines[1].is_active());

            infos6 =
                TestLineInfos::new(["(global i32 (i32.const 0))", "(func (import \"\" \"\"))"]);
            fix_syntax(&mut infos6);
            assert!(infos6.lines[0].is_active());
            assert!(!infos6.lines[1].is_active());

            infos6 = TestLineInfos::new(["(func)", "(import \"\" \"\" (func))"]);
            fix_syntax(&mut infos6);
            assert!(infos6.lines[0].is_active());
            assert!(!infos6.lines[1].is_active());

            infos6 = TestLineInfos::new(["(func)", "(func (import \"\" \"\"))"]);
            fix_syntax(&mut infos6);
            assert!(infos6.lines[0].is_active());
            assert!(!infos6.lines[1].is_active());
        }
    }
}
