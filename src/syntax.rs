// Structures and utilities to support multi-function/multi-module text buffers.

use anyhow::Result;
use std::ops::Deref;
use wast::{
    Error,
    core::{InlineImport, Instruction, LocalParser, ValType},
    kw,
    parser::{self, Cursor, Parse, ParseBuffer, Parser, Peek},
    token::Id,
};

use crate::line::{Activity, LineInfo};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum InstrKind {
    If,
    Else,
    End,
    OtherStructured, // block, loop, or try_table
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
    Param,
    Result,
    Local,
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
        ModulePart::Import
    }
}

impl<'a> From<LocalParser<'a>> for ModulePart {
    fn from(_: LocalParser) -> Self {
        ModulePart::Local
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

impl LineKind {
    fn stripped_clone(&self) -> LineKind {
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

impl From<Error> for LineKind {
    fn from(e: Error) -> Self {
        LineKind::Malformed(format!("{e}").lines().next().unwrap_or_default().into())
    }
}

impl<'a> Parse<'a> for ModulePart {
    fn parse(parser: Parser<'a>) -> Result<Self, Error> {
        // Prioritize fields of format "(...)" over single "(" token

        if parser.peek2::<kw::export>()? {
            // Modified from InlineExport parser
            parser.parens(|p| {
                p.parse::<kw::export>()?;
                p.parse::<&str>()?;
                Ok(ModulePart::Export)
            })
        } else if parser.peek2::<kw::import>()? {
            Ok(parser.parse::<InlineImport<'a>>()?.into())
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

#[derive(Default, Clone)]
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
    AfterModuleFieldRParen,
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
    // Expected order of function fields
    const ORDER: &'static [ModulePart] = &[
        ModulePart::FuncKeyword, // 0 - Required
        ModulePart::Id,          // 1 - Optional
        ModulePart::Export,      // 2 - Optional, Repeatable
        ModulePart::Import,      // 3 - Optional
        ModulePart::Param,       // 4 - Optional, Repeatable
        ModulePart::Result,      // 5 - Optional, Repeatable
        ModulePart::Local,       // 6 - Optional, Repeatable
    ];

    fn transit_state(&mut self, part: ModulePart) -> Result<(), &'static str> {
        // Find position (index) of part in the order list
        let part_pos = match Self::ORDER.iter().position(|&p| p == part) {
            Some(pos) => pos,
            None => return Err("not a function header field"),
        };

        // Check for order
        // whether required func keyword is skipped
        if self.next_field == 0 && part_pos > 0 {
            return Err("invalid field order");
        }

        let repeatable = matches!(
            part,
            ModulePart::Export | ModulePart::Param | ModulePart::Result | ModulePart::Local
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
        if self.is_import && matches!(part, ModulePart::Local) {
            return Err("imported function cannot have locals");
        }

        // Transition state
        self.is_import |= matches!(part, ModulePart::Import);
        self.next_field = part_pos + 1;

        Ok(())
    }
}

impl SyntaxState {
    fn transit_state(&mut self, info: &LineInfo) -> Result<(), &'static str> {
        if matches!(info.active, Activity::Inactive(_)) {
            return Ok(());
        }

        if info.synthetic_before.module_syntax_first {
            for part in &info.synthetic_before.module_field_syntax {
                self.transit_state_from_module_part(*part)?;
            }
            if info.synthetic_before.end_opcodes > 0 {
                self.transit_state_from_instruction()?;
            }
        } else {
            if info.synthetic_before.end_opcodes > 0 {
                self.transit_state_from_instruction()?;
            }
            for part in &info.synthetic_before.module_field_syntax {
                self.transit_state_from_module_part(*part)?;
            }
        }

        match &info.kind {
            LineKind::Empty | LineKind::Malformed(_) => {}
            LineKind::Instr(_) => self.transit_state_from_instruction()?,
            LineKind::Other(parts) => {
                for part in parts {
                    self.transit_state_from_module_part(*part)?;
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
            (SyntaxState::AfterModuleFieldLParen, ModulePart::FuncKeyword) => {
                SyntaxState::AfterFuncHeader(FuncHeader {
                    is_import: false,
                    next_field: 1,
                })
            }
            (
                &&mut SyntaxState::AfterFuncHeader(mut fh),
                ModulePart::Id
                | ModulePart::Export
                | ModulePart::Import
                | ModulePart::Param
                | ModulePart::Result
                | ModulePart::Local,
            ) => {
                fh.transit_state(part)?;
                SyntaxState::AfterFuncHeader(fh)
            }
            (
                SyntaxState::AfterFuncHeader(_) | SyntaxState::AfterInstruction,
                ModulePart::RParen,
            ) => SyntaxState::AfterModuleFieldRParen,
            (SyntaxState::AfterModuleFieldRParen, ModulePart::LParen) => {
                SyntaxState::AfterModuleFieldLParen
            }
            (SyntaxState::AfterModuleFieldRParen, _) => {
                return Err("text outside functions");
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
    let mut state = Initial;
    let mut frame_stack: Vec<InstrKind> = Vec::new();

    assert!(lines.len() > 0);

    for line_no in 0..lines.len() {
        lines.set_synthetic_before(line_no, SyntheticWasm::default());
        lines.set_active_status(line_no, Active);
        lines.set_invalid(line_no, None);

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
        {
            let orig_state = state;
            let res = state.transit_state(&lines.info(line_no));
            match res {
                Ok(()) => {
                    if state == AfterModuleFieldRParen {
                        // Fix #4: at end of function body, synthetically close all open frames
                        lines.set_synthetic_before(
                            line_no,
                            SyntheticWasm {
                                end_opcodes: frame_stack.len(),
                                ..Default::default()
                            },
                        );
                        frame_stack.clear();
                        state = Initial;
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
        let instr_kind = lines.info(line_no).kind.stripped_clone();
        if let LineKind::Instr(kind) = instr_kind {
            match kind {
                // For a structured instruction that opens a frame, log this.
                InstrKind::If | InstrKind::OtherStructured => {
                    lines.set_active_status(line_no, Active);
                    frame_stack.push(kind);
                }

                // Fix #5: if an `else` appears outside an `if` frame, disable it
                InstrKind::Else => {
                    if let Some(InstrKind::If) = frame_stack.last() {
                        frame_stack.pop();
                        lines.set_active_status(line_no, Active);
                        frame_stack.push(InstrKind::Else);
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
                _ => (),
            }
        }
    }

    match state {
        // Fix #7: if only contents are a "(", deactivate everything
        AfterModuleFieldLParen => {
            assert!(frame_stack.is_empty());

            for line_no in 0..lines.len() {
                let line_kind = lines.info(line_no).kind.stripped_clone();
                match line_kind {
                    LineKind::Instr(_) | LineKind::Other(_) => {
                        lines.set_active_status(line_no, Inactive(""))
                    }
                    LineKind::Empty | LineKind::Malformed(_) => {}
                }
            }
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

pub fn find_function_ranges(code: &impl LineInfos) -> Option<Vec<(usize, usize)>> {
    if code.len() == 0 {
        return None;
    }
    let mut ranges = Vec::new();
    let mut state = SyntaxState::Initial;
    let mut current_start: Option<usize> = None;

    for line_no in 0..code.len() {
        let orig_state = state;
        match state.transit_state(&code.info(line_no)) {
            Ok(()) => {
                if orig_state == SyntaxState::Initial && state != SyntaxState::Initial {
                    current_start = Some(line_no);
                }

                if state == SyntaxState::AfterModuleFieldRParen {
                    let start = current_start.take().unwrap_or(line_no);
                    ranges.push((start, line_no));
                    state = SyntaxState::Initial;
                }
            }
            Err(_) => {
                state = orig_state;
            }
        }
    }
    Some(ranges)
}

pub fn find_frames(code: &mut impl FrameInfosMut) {
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
        frames: Vec<FrameInfo>,
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
    }

    impl FrameInfosMut for TestLineInfos {
        fn set_indent(&mut self, index: usize, num: usize) {
            self.lines[index].indent = Some(num.try_into().unwrap());
        }

        fn set_frame_info(&mut self, num: usize, frame: FrameInfo) {
            if num >= self.frames.len() {
                self.frames.resize(
                    num + 1,
                    FrameInfo {
                        start: 0,
                        end: 0,
                        indent: 0,
                        unclosed: false,
                        kind: InstrKind::Other,
                    },
                );
            }
            self.frames[num] = frame;
        }

        fn set_frame_count(&mut self, count: usize) {
            self.frames.truncate(count);
        }
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
            ModulePart::Export
        );
        assert_eq!(
            parse::<ModulePart>(&ParseBuffer::new("  ( import \"foo\" \"bar\" )    ")?)?,
            ModulePart::Import
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
            ModulePart::Local
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
                ModulePart::Export,
                ModulePart::Import,
                ModulePart::Param,
                ModulePart::Result,
                ModulePart::Local
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
        let _ = header_valid.transit_state(ModulePart::Export);
        assert!(!header_valid.is_import);
        assert_eq!(header_valid.next_field, 3);
        let _ = header_valid.transit_state(ModulePart::Export);
        assert!(!header_valid.is_import);
        assert_eq!(header_valid.next_field, 3);
        let _ = header_valid.transit_state(ModulePart::Import);
        assert!(header_valid.is_import);
        assert_eq!(header_valid.next_field, 4);
        let _ = header_valid.transit_state(ModulePart::Result);
        assert!(header_valid.is_import);
        assert_eq!(header_valid.next_field, 6);

        // local after import
        assert!(header_valid.transit_state(ModulePart::Local).is_err());

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
        assert_eq!(parse_line("i32.const"), malf("expected a i32"));
        assert_eq!(parse_line("i32.const x"), malf("expected a i32"));
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
        assert_eq!(
            parse_line("   loop   "),
            LineKind::Instr(InstrKind::OtherStructured)
        );
        assert_eq!(
            parse_line("   try_table   "),
            LineKind::Instr(InstrKind::OtherStructured)
        );
        Ok(())
    }

    #[test]
    fn test_fix_syntax_and_find_frames() {
        {
            let mut infos1 = TestLineInfos::new(["(func", "block", "i32.const 42", "drop", ")"]);
            fix_syntax(&mut infos1);
            find_frames(&mut infos1);
            assert!(infos1.lines.iter().all(|x| x.is_active()));
            assert_eq!(
                infos1
                    .lines
                    .iter()
                    .map(|x| x.indent.unwrap())
                    .collect::<Vec<_>>(),
                [0, 1, 2, 2, 0]
            );
            assert_eq!(infos1.lines[4].synthetic_before.end_opcodes, 1);
            assert_eq!(
                infos1.frames,
                [FrameInfo {
                    indent: 1,
                    start: 1,
                    end: 4,
                    unclosed: true,
                    kind: InstrKind::OtherStructured
                }]
            );
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
            find_frames(&mut infos2);
            assert_eq!(infos2.lines[5].synthetic_before.end_opcodes, 1);
            assert_eq!(
                infos2
                    .lines
                    .iter()
                    .map(|x| x.is_active())
                    .collect::<Vec<_>>(),
                [true, true, true, false, true, true]
            );
            assert_eq!(
                infos2
                    .lines
                    .iter()
                    .map(|x| x.indent.unwrap())
                    .collect::<Vec<_>>(),
                [0, 1, 2, 3, 2, 0]
            );
            assert_eq!(
                infos2.frames,
                [
                    FrameInfo {
                        indent: 1,
                        start: 1,
                        end: 5,
                        unclosed: true,
                        kind: InstrKind::If
                    },
                    FrameInfo {
                        indent: 2,
                        start: 2,
                        end: 4,
                        unclosed: false,
                        kind: InstrKind::OtherStructured
                    }
                ]
            );
        }

        {
            let mut infos3 = TestLineInfos::new(["end"]);
            fix_syntax(&mut infos3);
            find_frames(&mut infos3);
            assert!(!infos3.lines[0].is_active());
            assert!(infos3.frames.is_empty());
        }

        {
            // Test case 4: Simple block
            let mut infos4 =
                TestLineInfos::new(["(func", "block", "i32.const 42", "drop", "end", ")"]);
            fix_syntax(&mut infos4);
            find_frames(&mut infos4);
            assert_eq!(
                infos4.frames,
                [FrameInfo {
                    indent: 1,
                    start: 1,
                    end: 4,
                    unclosed: false,
                    kind: InstrKind::OtherStructured
                }]
            );
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
            find_frames(&mut infos5);
            assert_eq!(infos5.frames.len(), 5);
            assert_eq!(
                infos5.frames[0],
                FrameInfo {
                    start: 1,
                    end: 4,
                    unclosed: false,
                    indent: 1,
                    kind: InstrKind::OtherStructured
                }
            );
            assert_eq!(
                infos5.frames[1],
                FrameInfo {
                    start: 5,
                    end: 15,
                    unclosed: false,
                    indent: 1,
                    kind: InstrKind::OtherStructured
                }
            );
            assert_eq!(
                infos5.frames[2],
                FrameInfo {
                    start: 6,
                    end: 14,
                    unclosed: false,
                    indent: 2,
                    kind: InstrKind::OtherStructured
                }
            );
            assert_eq!(
                infos5.frames[3],
                FrameInfo {
                    start: 7,
                    end: 10,
                    unclosed: false,
                    indent: 3,
                    kind: InstrKind::If
                }
            );
            assert_eq!(
                infos5.frames[4],
                FrameInfo {
                    start: 10,
                    end: 13,
                    unclosed: false,
                    indent: 3,
                    kind: InstrKind::Else
                }
            );
        }
    }

    #[test]
    fn test_multiple_functions_in_buffer() {
        let mut infos =
            TestLineInfos::new(["(func", "i32.const 1", ")", "", "(func", "i32.const 2", ")"]);
        fix_syntax(&mut infos);
        find_frames(&mut infos);

        // all non-empty lines should be active
        assert!(infos.lines[0].is_active());
        assert!(infos.lines[1].is_active());
        assert!(infos.lines[2].is_active());
        assert!(infos.lines[4].is_active());
        assert!(infos.lines[5].is_active());
        assert!(infos.lines[6].is_active());

        // there should be two ranges
        assert!(infos.frames.len() == 2);
    }
}
