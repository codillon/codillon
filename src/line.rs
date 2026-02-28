// An individual line of code in the editor. The CodeLine struct
// enforces consistency between the text in the line, the kind and
// "is_instr" status of the line (i.e. whether it's a well-formed
// *and* active instruction), and its CSS presentation.

use crate::{
    dom_struct::DomStruct,
    dom_text::DomText,
    jet::{AccessToken, Component, ElementFactory, NodeRef, WithElement, set_selection_range},
    symbolic::{LineSymbols, parse_line_symbols},
    syntax::{InstrKind, LineKind, ModulePart, SyntheticWasm, parse_line},
    utils::find_comment,
};
use anyhow::{Result, bail};
use web_sys::{HtmlBrElement, HtmlDivElement, HtmlSpanElement};

type DomBr = DomStruct<(), HtmlBrElement>;
type TextSpan = DomStruct<(DomText, ()), HtmlSpanElement>;
type EmptySpan = DomStruct<(), HtmlSpanElement>;
type LinePara = DomStruct<(TextSpan, (TextSpan, (EmptySpan, (DomBr, ())))), HtmlDivElement>;

#[derive(Default, Copy, Clone, PartialEq, Debug)]
pub enum Activity {
    #[default]
    Active,
    Inactive(&'static str), // reason
}

impl PartialEq<bool> for Activity {
    fn eq(&self, val: &bool) -> bool {
        (self == &Activity::Active) == *val
    }
}

#[derive(Default, Clone)]
pub struct LineInfo {
    pub kind: LineKind,
    pub active: Activity,
    pub indent: Option<u16>,
    pub synthetic_before: SyntheticWasm,
    pub invalid: Option<String>,
    pub symbols: LineSymbols,
}

impl LineInfo {
    pub fn new(kind: LineKind) -> Self {
        Self {
            kind,
            ..Default::default()
        }
    }

    pub fn is_active(&self) -> bool {
        self.active == true
    }

    pub fn is_well_formed(&self) -> bool {
        self.is_active() && matches!(self.kind, LineKind::Instr(_) | LineKind::Other(_))
    }

    pub fn is_instr(&self) -> bool {
        self.is_active() && matches!(self.kind, LineKind::Instr(_))
    }

    pub fn is_structured(&self) -> bool {
        self.is_active()
            && matches!(
                self.kind,
                LineKind::Instr(InstrKind::If) | LineKind::Instr(InstrKind::OtherStructured)
            )
    }

    pub fn paren_depths(&self) -> (i32, i32) {
        // indent change before first displayed char, other indent changes
        let mut initial = 0;
        for part in &self.synthetic_before.module_field_syntax {
            match part {
                ModulePart::LParen => initial += 1,
                ModulePart::RParen => initial -= 1,
                _ => {}
            }
        }

        let mut rest = 0;
        match &self.kind {
            LineKind::Other(parts) if self.is_active() => {
                let mut first_letter = true;
                for part in parts {
                    match part {
                        ModulePart::RParen if first_letter => initial -= 1,
                        ModulePart::RParen => rest -= 1,
                        ModulePart::LParen => rest += 1,
                        _ => {}
                    }
                    first_letter = false;
                }
            }
            _ => {}
        }

        (initial, rest)
    }

    // Number of opcodes (synthetic and user-entered) in the line
    pub fn num_ops(&self) -> usize {
        self.synthetic_before.num_ops() + self.is_instr() as usize
    }

    // Number of well-formed strings (synthetic and user-entered) in the line
    pub fn num_well_formed_strs(&self) -> usize {
        self.synthetic_before.num_strs() + self.is_well_formed() as usize
    }

    // Return well-formed (non-comment) Wasm string #n (both instructions and module-scope syntax)
    pub fn well_formed_str<'a>(&'a self, idx: usize, instr_text: &'a str) -> &'a str {
        if idx < self.synthetic_before.num_strs() {
            self.synthetic_before.str(idx)
        } else if self.is_well_formed() && idx == self.synthetic_before.num_strs() {
            instr_text
        } else {
            panic!("index out of range");
        }
    }
}

enum AnimationState {
    Normal,
    Shake(bool),
    Reveal(bool),
    FlyEnd(bool),
}

pub struct CodeLine {
    contents: LinePara,
    info: LineInfo,
    animation_state: AnimationState,
}

impl WithElement for CodeLine {
    type Element = HtmlDivElement;
    fn with_element(&self, f: impl FnMut(&HtmlDivElement), g: AccessToken) {
        self.contents.with_element(f, g)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Position {
    pub in_instr: bool,
    pub offset: usize,
}

impl Position {
    pub fn begin() -> Position {
        Position::instr(0)
    }

    fn instr(offset: usize) -> Position {
        Position {
            in_instr: true,
            offset,
        }
    }

    fn comment(offset: usize) -> Position {
        Position {
            in_instr: false,
            offset,
        }
    }
}

impl Component for CodeLine {
    fn audit(&self) {
        assert_eq!(self.info.kind, parse_line(self.instr().get()));
        assert_eq!(self.contents.get_attribute("class").unwrap(), &self.class());
        let comment = self.contents.get().1.0.get_attribute("data-commentary");
        if let LineKind::Malformed(reason) = &self.info.kind {
            assert_eq!(comment, Some(reason));
        } else if let Activity::Inactive(reason) = &self.info.active
            && !reason.is_empty()
        {
            assert_eq!(comment, Some(reason.to_string()).as_ref());
        } else if let Some(reason) = &self.info.invalid {
            assert_eq!(comment, Some(reason.to_string()).as_ref());
        } else {
            assert_eq!(comment, None);
        }

        self.contents.audit();
    }
}

impl CodeLine {
    pub fn instr(&self) -> &DomText {
        &self.contents.get().0.get().0
    }

    fn instr_mut(&mut self) -> &mut DomText {
        &mut self.contents.get_mut().0.get_mut().0
    }

    pub fn comment(&self) -> &DomText {
        &self.contents.get().1.0.get().0
    }

    fn comment_mut(&mut self) -> &mut DomText {
        &mut self.contents.get_mut().1.0.get_mut().0
    }

    pub fn well_formed_str(&self, idx: usize) -> &str {
        self.info.well_formed_str(idx, self.instr().get())
    }

    pub fn end_position(&self) -> Position {
        if self.comment().is_empty() {
            Position::instr(self.instr().len_utf16())
        } else {
            Position::comment(self.comment().len_utf16())
        }
    }

    pub fn all_whitespace(&self) -> bool {
        self.instr().get().chars().all(|c| c.is_whitespace())
            && self.comment().get().chars().all(|c| c.is_whitespace())
    }

    pub fn get_position(&self, node: NodeRef, offset: usize) -> Result<Position> {
        let instr_end = Position::instr(self.instr().len_utf16());

        // If the position is in the top-level div element, the offset counts nodes
        // within the line.
        if node.is_same_node(&self.contents) {
            return Ok(match offset {
                0 => Position::begin(),
                1 => instr_end,
                _ => self.end_position(),
            });
        }

        // Position in instruction or comment text nodes.
        if node.is_same_node(self.instr()) {
            if offset > self.instr().len_utf16() {
                bail!("invalid offset");
            }
            return Ok(Position::instr(offset));
        } else if node.is_same_node(self.comment()) {
            if offset > self.comment().len_utf16() {
                bail!("invalid offset");
            }
            return Ok(match offset {
                0 => instr_end,
                _ => Position::comment(offset),
            });
        }

        // Position in the instruction or comment spans.
        if node.is_same_node(&self.contents.get().0) {
            return Ok(match offset {
                0 => Position::begin(),
                1 => instr_end,
                _ => bail!("unexpected instr span position {offset}"),
            });
        } else if node.is_same_node(&self.contents.get().1.0) {
            return Ok(match offset {
                0 => instr_end,
                1 => self.end_position(),
                _ => bail!("unexpected comment span position {offset}"),
            });
        } else if node.is_same_node(&self.contents.get().1.1.0) {
            return Ok(match offset {
                0 => self.end_position(),
                _ => bail!("unexpected debug span position {offset}"),
            });
        }

        bail!("position in unknown node");
    }

    fn class(&self) -> String {
        let prefix = if !self.info.is_active() {
            "line malformed-line"
        } else if self.info.invalid.is_some() {
            "line instr-line invalid-line"
        } else {
            match self.info.kind {
                LineKind::Malformed(_) => "line malformed-line",
                LineKind::Empty => "line",
                LineKind::Other(_) => "line meta-line",
                LineKind::Instr(_) => "line instr-line numbered-line",
            }
        }
        .to_string();

        use AnimationState::*;
        fn ab(x: bool) -> &'static str {
            if x { "a" } else { "b" }
        }

        prefix
            + &match self.animation_state {
                Shake(x) => String::from(" shake-") + ab(x),
                Reveal(x) => String::from(" reveal-") + ab(x),
                FlyEnd(x) => String::from(" flyend-") + ab(x),
                Normal => String::new(),
            }
    }

    pub fn shake(&mut self) {
        use AnimationState::*;
        self.animation_state = match self.animation_state {
            Shake(false) => Shake(true),
            _ => Shake(false),
        };
        self.conform();
    }

    pub fn reveal(&mut self) {
        use AnimationState::*;
        self.animation_state = match self.animation_state {
            Reveal(false) => Reveal(true),
            _ => Reveal(false),
        };
        self.conform();
    }

    pub fn fly_end(&mut self) {
        use AnimationState::*;
        self.animation_state = match self.animation_state {
            FlyEnd(false) => FlyEnd(true),
            _ => FlyEnd(false),
        };
        self.conform();
    }

    pub fn new(contents: &str, factory: &ElementFactory) -> Self {
        let mut ret = Self {
            contents: LinePara::new(
                (
                    TextSpan::new((DomText::new(contents), ()), factory.span()),
                    (
                        TextSpan::new((DomText::default(), ()), factory.span()),
                        (
                            DomStruct::new((), factory.span()),
                            (DomBr::new((), factory.br()), ()),
                        ),
                    ),
                ),
                factory.div(),
            ),
            info: LineInfo::default(),
            animation_state: AnimationState::Normal,
        };

        ret.contents.get_mut().0.set_attribute("class", "instr");
        ret.contents.get_mut().1.0.set_attribute("class", "comment");
        ret.contents.get_mut().1.1.0.set_attribute("class", "debug");
        ret.conform_to_text().unwrap();

        ret
    }

    // Make the element presentation (CSS class) match the status.
    // This needs to happen when the active status changes.
    fn conform(&mut self) {
        if let LineKind::Malformed(reason) = &self.info.kind {
            self.contents
                .get_mut()
                .1
                .0
                .set_attribute("data-commentary", reason);
        } else if let Activity::Inactive(reason) = &self.info.active
            && !reason.is_empty()
        {
            self.contents
                .get_mut()
                .1
                .0
                .set_attribute("data-commentary", reason);
        } else if let Some(reason) = &self.info.invalid {
            self.contents
                .get_mut()
                .1
                .0
                .set_attribute("data-commentary", reason);
        } else {
            self.contents
                .get_mut()
                .1
                .0
                .remove_attribute("data-commentary");
        }

        self.contents.set_attribute("class", &self.class());
    }

    // Make the instr/comment split, the kind, and the CSS presentation consistent with the text contents.
    // This needs to happen when the text changes. Returns number of whitespace bytes trimmed from start.
    fn conform_to_text(&mut self) -> Result<usize> {
        // Re-split text nodes at beginning of line comment (if any)
        match find_comment(self.instr().get(), self.comment().get()) {
            None if self.comment().is_empty() => {}
            None => {
                let comment = self.comment_mut().take();
                self.instr_mut().push_str(&comment);
            }
            Some(x) if x == self.instr().len_bytes() => {}
            Some(x) => {
                let concat = self.instr().get().to_owned() + self.comment().get();
                self.instr_mut().set_data(&concat[0..x]);
                self.comment_mut().set_data(&concat[x..]);
            }
        }
        // Trim whitespace at front of instruction
        let ws_bytes = self.instr().len_bytes()
            - self
                .instr()
                .get()
                .trim_start_matches(|x: char| x.is_whitespace() && x != '\n')
                .len();
        if ws_bytes != 0 {
            self.instr_mut().replace_range_bytes(0, ws_bytes, "")?;
        }
        // Update kind, symbols, commentary, and active status
        self.info.kind = parse_line(self.instr().get());
        self.info.symbols = parse_line_symbols(self.instr().get(), self.info.kind.clone());
        self.conform();
        Ok(ws_bytes)
    }

    // Activate or deactivate the line.
    pub fn set_active_status(&mut self, new_val: Activity) {
        if new_val != self.info.active {
            self.info.active = new_val;
            self.conform();
        }
    }

    pub fn set_synthetic_before(&mut self, synth: SyntheticWasm) {
        if !synth.module_field_syntax.is_empty() {
            self.contents
                .get_mut()
                .0
                .set_attribute("data-synthetic-before", &synth.render_module_field_syntax());
        } else {
            self.contents
                .get_mut()
                .0
                .remove_attribute("data-synthetic-before");
        }

        self.info.synthetic_before = synth;
        self.conform();
    }

    pub fn suffix(&self, pos: Position) -> Result<String> {
        Ok(match pos.in_instr {
            true => String::from(self.instr().suffix_utf16(pos.offset)?) + self.comment().get(),
            false => self.comment().suffix_utf16(pos.offset)?.to_string(),
        })
    }

    pub fn first_newline(&self) -> Result<Option<Position>> {
        Ok(if let Some(idx) = self.instr().get().find('\n') {
            Some(Position::instr(self.instr().safe_byte_idx_to_utf16(idx)?))
        } else if let Some(idx) = self.comment().get().find('\n') {
            Some(Position::comment(
                self.comment().safe_byte_idx_to_utf16(idx)?,
            ))
        } else {
            None
        })
    }

    // Modify the contents. This calls replace_range on one of the inner DomTexts (or both), then
    // makes the comment split, kind, and CSS presentation consistent with the new contents.
    pub fn replace_range(
        &mut self,
        mut a: Position,
        b: Position,
        mut string: &str,
    ) -> Result<Position> {
        // Special-case the creation or deletion of the comment separator (";;").
        if a == b && a.in_instr && string == ";" {
            string = ";;"; // type the whole ";;" separator on one ";" keystroke
        } else if a == b
            && b == Position::comment(2)
            && self.comment().get().starts_with(";;")
            && string == ";"
        {
            string = ""; // if user types two ";" characters, give them two total (ignore second one)
        } else if a == Position::comment(1)
            && b == Position::comment(2)
            && string.is_empty()
            && self.comment().get().starts_with(";;")
        {
            a = Position::comment(0); // if user deletes from end of ";;", delete both
        }

        // Do the replacement in the appropriate components (text or comment).
        let mut total_pos = match (a.in_instr, b.in_instr) {
            (true, true) => self.instr_mut().replace_range(a.offset, b.offset, string)?,
            (false, false) => {
                self.instr().len_utf16()
                    + self
                        .comment_mut()
                        .replace_range(a.offset, b.offset, string)?
            }
            (true, false) => {
                self.comment_mut().replace_range(0, b.offset, "")?;
                let instr_len = self.instr().len_utf16();
                self.instr_mut()
                    .replace_range(a.offset, instr_len, string)?
            }
            (false, true) => bail!("unhandled comment -> instr range"),
        };

        total_pos = total_pos.saturating_sub(self.conform_to_text()?);

        Ok(if total_pos <= self.instr().len_utf16() {
            Position::instr(total_pos)
        } else {
            let comment_pos = total_pos - self.instr().len_utf16();
            debug_assert!(comment_pos <= self.comment().len_utf16());
            Position::comment(comment_pos)
        })
    }

    pub fn set_cursor_position(&self, pos: Position) {
        let node = self.position_to_node(pos);
        let offset = pos.offset.try_into().expect("offset -> u32");
        set_selection_range(node, offset, node, offset);
        self.contents.scroll_into_view();
    }

    pub fn info(&self) -> &LineInfo {
        &self.info
    }

    pub fn position_to_node(&self, pos: Position) -> &DomText {
        match pos.in_instr {
            true => self.instr(),
            false => self.comment(),
        }
    }

    // Returns whether the indentation of nonempty text was changed (used to trigger animations)
    pub fn set_indent(&mut self, val: usize) -> bool {
        let old_indent = self.info.indent;
        self.info.indent = Some(val.try_into().unwrap_or(u16::MAX));
        self.contents
            .get_mut()
            .0
            .set_attribute("style", &format!("margin-left: {}px;", val * 25));

        old_indent.is_some() && old_indent != self.info.indent && !self.all_whitespace()
    }

    pub fn unset_indent(&mut self) {
        self.info.indent = None;
    }

    pub fn set_invalid(&mut self, reason: Option<String>) {
        self.info.invalid = reason;
        self.conform();
    }

    pub fn set_type_annotation(&mut self, value: Option<&String>) {
        if let Some(text) = value {
            self.contents.get_mut().0.set_attribute("data-debug", text);
        } else {
            self.contents.get_mut().0.remove_attribute("data-debug");
        }
    }

    pub fn set_debug_annotation(&mut self, value: Option<&String>) {
        if let Some(text) = value {
            self.contents
                .get_mut()
                .1
                .1
                .0
                .set_attribute("data-debug", text);
        } else {
            self.contents.get_mut().1.1.0.remove_attribute("data-debug");
        }
    }

    pub fn set_highlight(&mut self, add_highlight: bool) {
        if add_highlight {
            self.contents.get_mut().0.set_attribute("highlight", "");
        } else {
            self.contents.get_mut().0.remove_attribute("highlight");
        }
    }
}
