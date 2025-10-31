// An individual line of code in the editor. The CodeLine struct
// enforces consistency between the text in the line, the kind and
// "is_instr" status of the line (i.e. whether it's a well-formed
// *and* active instruction), and its CSS presentation.

use crate::{
    dom_struct::DomStruct,
    dom_text::DomText,
    jet::{AccessToken, Component, ElementFactory, NodeRef, WithElement, set_selection_range},
    utils::{InstrKind, LineKind, find_comment, parse_line},
};
use anyhow::{Result, bail};
use web_sys::{HtmlBrElement, HtmlDivElement, HtmlSpanElement};

type DomBr = DomStruct<(), HtmlBrElement>;
type TextSpan = DomStruct<(DomText, ()), HtmlSpanElement>;
type LinePara = DomStruct<(TextSpan, (TextSpan, (DomBr, ()))), HtmlDivElement>;

#[derive(Default, Clone)]
pub struct LineInfo {
    pub kind: LineKind,
    pub active: bool,
    pub indent: Option<u16>,
}

impl LineInfo {
    pub fn new(kind: LineKind, active: bool) -> Self {
        Self {
            kind,
            active,
            indent: None,
        }
    }

    pub fn is_instr(&self) -> bool {
        self.active && matches!(self.kind, LineKind::Instr(_))
    }

    pub fn is_structured(&self) -> bool {
        self.active
            && matches!(
                self.kind,
                LineKind::Instr(InstrKind::If) | LineKind::Instr(InstrKind::OtherStructured)
            )
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
        assert_eq!(
            self.contents.get().1.0.get_attribute("data-commentary"),
            if let LineKind::Malformed(reason) = &self.info.kind {
                Some(reason)
            } else {
                None
            }
        );

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
        }

        bail!("position in unknown node");
    }

    fn class(&self) -> String {
        let prefix = if !self.info.active {
            "inactive-line"
        } else {
            match self.info.kind {
                LineKind::Empty => "empty-line",
                LineKind::Malformed(_) => "malformed-line",
                _ => "good-line",
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
        self.conform_activity();
    }

    pub fn reveal(&mut self) {
        use AnimationState::*;
        self.animation_state = match self.animation_state {
            Reveal(false) => Reveal(true),
            _ => Reveal(false),
        };
        self.conform_activity();
    }

    pub fn fly_end(&mut self) {
        use AnimationState::*;
        self.animation_state = match self.animation_state {
            FlyEnd(false) => FlyEnd(true),
            _ => FlyEnd(false),
        };
        self.conform_activity();
    }

    pub fn new(contents: &str, factory: &ElementFactory) -> Self {
        let mut ret = Self {
            contents: LinePara::new(
                (
                    TextSpan::new((DomText::new(contents), ()), factory.span()),
                    (
                        TextSpan::new((DomText::default(), ()), factory.span()),
                        (DomBr::new((), factory.br()), ()),
                    ),
                ),
                factory.div(),
            ),
            info: LineInfo::default(),
            animation_state: AnimationState::Normal,
        };

        ret.contents.get_mut().0.set_attribute("class", "instr");
        ret.contents.get_mut().1.0.set_attribute("class", "comment");
        ret.conform_to_text().unwrap();

        ret
    }

    // Make the element presentation (CSS class) match the "is_instr" status.
    // This needs to happen when the active status changes.
    fn conform_activity(&mut self) {
        self.contents.set_attribute("class", &self.class());
    }

    fn conform_commentary(&mut self) {
        if let LineKind::Malformed(reason) = &self.info.kind {
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
        // Update kind, commentary, and active status
        self.info.kind = parse_line(self.instr().get());
        self.conform_commentary();
        self.conform_activity();
        Ok(ws_bytes)
    }

    // Activate or deactivate the line.
    pub fn set_active_status(&mut self, is_active: bool) {
        if is_active != self.info.active {
            self.info.active = is_active;
            self.conform_activity();
        }
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
        if a == b && b == Position::instr(self.instr().len_utf16()) && string == ";" {
            string = ";;"; // type the whole ";;" separator on one ";" keystroke
        } else if a == b
            && b == Position::comment(2)
            && self.comment().get() == ";;"
            && string == ";"
        {
            string = ""; // if user types two ";" characters, give them two total (ignore second one)
        } else if a == Position::comment(1)
            && b == Position::comment(2)
            && string.is_empty()
            && self.comment().get() == ";;"
        {
            a = Position::comment(0); // if user deletes from end of ";;" and comment otherwise empty, delete both
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
}
