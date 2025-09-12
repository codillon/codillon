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
use web_sys::{HtmlBrElement, HtmlSpanElement, Text};

type DomBr = DomStruct<(), HtmlBrElement>;
type Comment = DomStruct<(DomText, ()), HtmlSpanElement>;
type LineContents = (DomText, (Comment, (DomBr, ())));
type LineSpan = DomStruct<LineContents, HtmlSpanElement>;

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
    contents: LineSpan,
    info: LineInfo,
}

impl WithElement for CodeLine {
    type Element = HtmlSpanElement;
    fn with_element(&self, f: impl FnMut(&HtmlSpanElement), g: AccessToken) {
        self.contents.with_element(f, g)
    }
}

impl ElementAsNode for CodeLine {}

pub enum Position {
    Instr(usize),
    Comment(usize),
}

impl Component for CodeLine {
    fn audit(&self) {
        assert_eq!(
            self.info.kind,
            parse_instr(self.contents.get().0.get_contents())
        );
        assert_eq!(self.contents.get_attribute("class").unwrap(), self.class());
        assert_eq!(
            self.contents.get().1.0.get_attribute("data-commentary"),
            Some(
                if let InstrKind::Malformed(reason) = &self.info.kind {
                    reason
                } else {
                    ""
                }
                .to_string()
            )
            .as_ref()
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

    pub fn begin_position() -> Position {
        Position::Instr(0)
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

        // If the position is in the top-level span element, the offset counts nodes
        // within the span.
        if node.is_same_node(&self.contents) {
            debug_assert!(node.is_a::<HtmlSpanElement>());
            return Ok(match offset {
                0 => Self::begin_position(),
                1 => instr_end,
                _ => self.end_position(),
            });
        }

        // Position in "instruction" or "comment" text nodes.
        if node.is_same_node(self.instr()) {
            debug_assert!(node.is_a::<Text>());
            if offset > self.instr().len_utf16() {
                bail!("invalid offset");
            }
            return Ok(Instr(offset));
        } else if node.is_same_node(self.comment()) {
            debug_assert!(node.is_a::<Text>());
            if offset > self.comment().len_utf16() {
                bail!("invalid offset");
            }
            return match offset {
                0 => Ok(instr_end),
                _ => Ok(Comment(offset)),
            };
        }

        // Position in the "comment" span.
        if node.is_same_node(&self.contents.get().1.0) {
            debug_assert!(node.is_a::<HtmlSpanElement>());
            return Ok(match offset {
                0 => instr_end,
                _ => self.end_position(),
            });
        }

        bail!("position in unknown node");
    }

    fn class(&self) -> &'static str {
        if !self.info.active {
            "line inactive-line"
        } else {
            match self.info.kind {
                InstrKind::Empty => "line empty-line",
                InstrKind::Malformed(_) => "line malformed-line",
                _ => "line good-line",
            }
        }
    }

    pub fn new(contents: &str, factory: &ElementFactory) -> Self {
        let mut ret = Self {
            contents: LineSpan::new(
                (
                    DomText::new(contents),
                    (
                        Comment::new((DomText::default(), ()), factory.span()),
                        (DomBr::new((), factory.br()), ()),
                    ),
                ),
                factory.span(),
            ),
            info: LineInfo {
                kind: InstrKind::Empty,
                active: true,
            },
        };

        ret.contents.get_mut().1.0.set_attribute("class", "comment");
        ret.conform_to_text();

        ret
    }

    // Make the element presentation (CSS class) match the "is_instr" status.
    // This needs to happen when the active status changes.
    fn conform_activity(&mut self) {
        self.contents.set_attribute("class", self.class());
    }

    fn conform_commentary(&mut self) {
        self.contents.get_mut().1.0.set_attribute(
            "data-commentary",
            if let InstrKind::Malformed(reason) = &self.info.kind {
                reason
            } else {
                ""
            },
        )
    }

    // Make the instr/comment split, the kind, and the CSS presentation consistent with the text contents.
    // This needs to happen when the text changes.
    fn conform_to_text(&mut self) {
        match find_comment(self.instr().get_contents(), self.comment().get_contents()) {
            None if self.comment().is_empty() => {}
            None => {
                let comment = self.comment_mut().take();
                self.instr_mut().push_str(&comment);
            }
            Some(x) if x == self.instr().len_bytes() => {}
            Some(x) => {
                let concat = self.instr().get_contents().to_owned() + self.comment().get_contents();
                self.instr_mut().set_data(&concat[0..x]);
                self.comment_mut().set_data(&concat[x..]);
            }
        }
        self.info.kind = parse_instr(self.instr().get_contents());
        self.conform_commentary();
        self.conform_activity();
    }

    // Activate or deactivate the line.
    pub fn set_active_status(&mut self, is_active: bool) {
        if is_active != self.info.active {
            self.info.active = is_active;
            self.conform_activity();
        }
    }

    // Modify the contents. This calls replace_range on the inner DomText, then
    // makes the kind and CSS presentation consistent with the new contents.
    pub fn replace_range(
        &mut self,
        start_pos: Position,
        end_pos: Position,
        string: &str,
    ) -> Result<Position> {
        use Position::*;
        let total_pos = match (start_pos, end_pos) {
            (Instr(a), Instr(b)) => self.instr_mut().replace_range(a, b, string)?,
            (Comment(a), Comment(b)) => {
                self.instr().len_utf16() + self.comment_mut().replace_range(a, b, string)?
            }
            (Instr(a), Comment(b)) | (Comment(b), Instr(a)) => {
                self.comment_mut().replace_range(0, b, "")?;
                let instr_len = self.instr().len_utf16();
                self.instr_mut().replace_range(a, instr_len, string)?
            }
        };

        self.conform_to_text();

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
    }

    pub fn info(&self) -> &LineInfo {
        &self.info
    }
}
