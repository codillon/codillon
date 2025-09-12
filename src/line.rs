// An individual line of code in the editor. The CodeLine struct
// enforces consistency between the text in the line, the kind and
// "is_instr" status of the line (i.e. whether it's a well-formed
// *and* active instruction), and its CSS presentation.

use crate::{
    dom_struct::DomStruct,
    dom_text::DomText,
    utils::{InstrKind, find_comment, parse_instr},
    web_support::{
        AccessToken, Component, ElementAsNode, ElementFactory, NodeRef, WithElement,
        set_cursor_position,
    },
};
use anyhow::{Result, bail};
use web_sys::{HtmlBrElement, HtmlDivElement, HtmlSpanElement};

type DomBr = DomStruct<(), HtmlBrElement>;
type Comment = DomStruct<(DomText, ()), HtmlSpanElement>;
type LinePara = DomStruct<(DomText, (Comment, (DomBr, ()))), HtmlDivElement>;

pub struct LineInfo {
    pub kind: InstrKind,
    pub active: bool,
}

impl LineInfo {
    pub fn new(kind: InstrKind, active: bool) -> Self {
        Self { kind, active }
    }

    pub fn is_instr(&self) -> bool {
        self.active && !matches!(self.kind, InstrKind::Empty | InstrKind::Malformed(_))
    }
}

pub struct CodeLine {
    contents: LinePara,
    info: LineInfo,
}

impl WithElement for CodeLine {
    type Element = HtmlDivElement;
    fn with_element(&self, f: impl FnMut(&HtmlDivElement), g: AccessToken) {
        self.contents.with_element(f, g)
    }
}

impl ElementAsNode for CodeLine {}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Position {
    Instr(usize),
    Comment(usize),
}

impl Position {
    pub fn begin() -> Position {
        Position::Instr(0)
    }
}

impl Component for CodeLine {
    fn audit(&self) {
        assert_eq!(self.info.kind, parse_instr(self.contents.get().0.get()));
        assert_eq!(self.contents.get_attribute("class").unwrap(), self.class());
        assert_eq!(
            self.contents.get().1.0.get_attribute("data-commentary"),
            if let InstrKind::Malformed(reason) = &self.info.kind {
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
        &self.contents.get().0
    }

    fn instr_mut(&mut self) -> &mut DomText {
        &mut self.contents.get_mut().0
    }

    fn comment(&self) -> &DomText {
        &self.contents.get().1.0.get().0
    }

    fn comment_mut(&mut self) -> &mut DomText {
        &mut self.contents.get_mut().1.0.get_mut().0
    }

    pub fn end_position(&self) -> Position {
        if self.comment().is_empty() {
            Position::Instr(self.instr().len_utf16())
        } else {
            Position::Comment(self.comment().len_utf16())
        }
    }

    pub fn get_position(&self, node: NodeRef, offset: usize) -> Result<Position> {
        use Position::*;

        let instr_end = Instr(self.instr().len_utf16());

        // If the position is in the top-level div element, the offset counts nodes
        // within the line.
        if node.is_same_node(&self.contents) {
            return Ok(match offset {
                0 => Position::begin(),
                1 => instr_end,
                _ => self.end_position(),
            });
        }

        // Position in instruction, comment, or newline text nodes.
        if node.is_same_node(self.instr()) {
            if offset > self.instr().len_utf16() {
                bail!("invalid offset");
            }
            return Ok(Instr(offset));
        } else if node.is_same_node(self.comment()) {
            if offset > self.comment().len_utf16() {
                bail!("invalid offset");
            }
            return Ok(match offset {
                0 => instr_end,
                _ => Comment(offset),
            });
        }

        // Position in the "comment" span.
        if node.is_same_node(&self.contents.get().1.0) {
            return Ok(match offset {
                0 => instr_end,
                1 => self.end_position(),
                _ => bail!("unexpected comment span position {offset}"),
            });
        }

        bail!("position in unknown node");
    }

    fn class(&self) -> &'static str {
        if !self.info.active {
            "inactive-line"
        } else {
            match self.info.kind {
                InstrKind::Empty => "empty-line",
                InstrKind::Malformed(_) => "malformed-line",
                _ => "good-line",
            }
        }
    }

    pub fn new(contents: &str, factory: &ElementFactory) -> Self {
        let mut ret = Self {
            contents: LinePara::new(
                (
                    DomText::new(contents),
                    (
                        Comment::new((DomText::default(), ()), factory.span()),
                        (DomBr::new((), factory.br()), ()),
                    ),
                ),
                factory.div(),
            ),
            info: LineInfo {
                kind: InstrKind::Empty,
                active: true,
            },
        };

        ret.contents.get_mut().1.0.set_attribute("class", "comment");
        ret.conform_to_text().unwrap();

        ret
    }

    // Make the element presentation (CSS class) match the "is_instr" status.
    // This needs to happen when the active status changes.
    fn conform_activity(&mut self) {
        self.contents.set_attribute("class", self.class());
    }

    fn conform_commentary(&mut self) {
        if let InstrKind::Malformed(reason) = &self.info.kind {
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
        self.info.kind = parse_instr(self.instr().get());
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
        use Position::*;

        Ok(match pos {
            Instr(a) => String::from(self.instr().suffix_utf16(a)?) + self.comment().get(),
            Comment(b) => self.comment().suffix_utf16(b)?.to_string(),
        })
    }

    pub fn first_newline(&self) -> Result<Option<Position>> {
        use Position::*;
        Ok(if let Some(idx) = self.instr().get().find('\n') {
            Some(Instr(self.instr().safe_byte_idx_to_utf16(idx)?))
        } else if let Some(idx) = self.comment().get().find('\n') {
            Some(Comment(self.comment().safe_byte_idx_to_utf16(idx)?))
        } else {
            None
        })
    }

    // Modify the contents. This calls replace_range on one of the inner DomTexts (or both), then
    // makes the comment split, kind, and CSS presentation consistent with the new contents.
    pub fn replace_range(
        &mut self,
        start_pos: Position,
        end_pos: Position,
        string: &str,
    ) -> Result<Position> {
        use Position::*;

        let mut total_pos = match (start_pos, end_pos) {
            (Instr(a), Instr(b)) => self.instr_mut().replace_range(a, b, string)?,
            (Comment(a), Comment(b)) => {
                self.instr().len_utf16() + self.comment_mut().replace_range(a, b, string)?
            }
            (Instr(a), Comment(b)) => {
                self.comment_mut().replace_range(0, b, "")?;
                let instr_len = self.instr().len_utf16();
                self.instr_mut().replace_range(a, instr_len, string)?
            }
            (Comment(_), Instr(_)) => bail!("unhandled comment -> instr range"),
        };

        total_pos = total_pos.saturating_sub(self.conform_to_text()?);

        Ok(if total_pos <= self.instr().len_utf16() {
            Instr(total_pos)
        } else {
            let comment_pos = total_pos - self.instr().len_utf16();
            debug_assert!(comment_pos <= self.comment().len_utf16());
            Comment(comment_pos)
        })
    }

    pub fn set_cursor_position(&self, pos: Position) {
        use Position::*;

        match pos {
            Instr(offset) => set_cursor_position(self.instr(), offset),
            Comment(offset) => set_cursor_position(self.comment(), offset),
        }

        self.contents.scroll_into_view();
    }

    pub fn info(&self) -> &LineInfo {
        &self.info
    }
}
