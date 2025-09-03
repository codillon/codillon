// An individual line of code in the editor. The CodeLine struct
// enforces consistency between the text in the line, the kind and
// "is_instr" status of the line (i.e. whether it's a well-formed
// *and* active instruction), and its CSS presentation.

use crate::{
    dom_struct::DomStruct,
    dom_text::DomText,
    utils::{InstrKind, parse_instr},
    web_support::{AccessToken, Component, ElementAsNode, ElementFactory, WithElement},
};
use anyhow::Result;
use web_sys::{HtmlBrElement, HtmlSpanElement};

type DomBr = DomStruct<(), HtmlBrElement>;
type Commentary = DomStruct<(), HtmlSpanElement>;
type LineContents = (DomText, (Commentary, (DomBr, ())));
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
            contents: LineSpan::new(
                (
                    DomText::new(contents),
                    (
                        Commentary::new((), factory.span()),
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

        ret.contents
            .get_mut()
            .1
            .0
            .set_attribute("class", "commentary");
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

    // Make the kind and CSS presentation consistent with the text contents.
    // This needs to happen when the text changes.
    fn conform_to_text(&mut self) {
        self.info.kind = parse_instr(self.contents.get().0.get_contents());
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

    pub fn text(&self) -> &DomText {
        &self.contents.get().0
    }

    // Modify the contents. This calls replace_range on the inner DomText, then
    // makes the kind and CSS presentation consistent with the new contents.
    pub fn replace_range(
        &mut self,
        utf16_start_idx: usize,
        utf16_end_idx: usize,
        string: &str,
    ) -> Result<usize> {
        let pos =
            self.contents
                .get_mut()
                .0
                .replace_range(utf16_start_idx, utf16_end_idx, string)?;
        self.conform_to_text();
        Ok(pos)
    }

    pub fn info(&self) -> &LineInfo {
        &self.info
    }
}
