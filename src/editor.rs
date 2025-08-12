// The Codillon code editor (doesn't do much, but does capture beforeinput and logs to console)

use crate::{
    dom_struct::DomStruct,
    dom_text::DomText,
    dom_vec::DomVec,
    utils::FmtError,
    web_support::{
        AccessToken, Component, ElementFactory, InputEventHandle, NodeRef, WithElement,
        compare_document_position, set_cursor_position,
    },
};
use anyhow::{Context, Result, bail};
use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};
use web_sys::{HtmlBrElement, HtmlDivElement, HtmlSpanElement, Text, console::log_1};

type DomBr = DomStruct<(), HtmlBrElement>;
type LineContents = (DomText, (DomBr, ()));
type EditLine = DomStruct<LineContents, HtmlSpanElement>;

struct _Editor {
    component: DomVec<EditLine, HtmlDivElement>,
}

pub struct Editor(Rc<RefCell<_Editor>>);

impl Editor {
    pub fn new(factory: &ElementFactory) -> Self {
        let mut inner = _Editor {
            component: DomVec::new(factory.div()),
        };

        inner.component.set_attribute("class", "textentry");
        inner.component.set_attribute("contenteditable", "true");
        inner.component.set_attribute("spellcheck", "false");

        let mut ret = Editor(Rc::new(RefCell::new(inner)));

        let editor_ref = Rc::clone(&ret.0);
        ret.component_mut().set_onbeforeinput(move |ev| {
            Editor(editor_ref.clone())
                .handle_input(ev)
                .expect("input handler")
        });

        for i in 0..100 {
            ret.push_line(factory, &format!("This is 🏳️‍⚧️ line {i}."));
        }

        ret
    }

    fn push_line(&mut self, factory: &ElementFactory, string: &str) {
        self.component_mut().push(EditLine::new(
            (DomText::new(string), (DomBr::new((), factory.br()), ())),
            factory.span(),
        ));
    }

    // The input handler. Currently only handles "insertText" events.
    fn handle_input(&mut self, ev: InputEventHandle) -> Result<()> {
        ev.prevent_default();

        let target_range = ev.get_first_target_range()?;

        let the_data = match &ev.input_type() as &str {
            "insertText" => ev.data().context("no data")?,
            "insertFromPaste" => ev
                .data_transfer()
                .context("no data_transfer")?
                .get_data("text/plain")
                .fmt_err()?,
            _ => bail!("unhandled"),
        };

        if !target_range.collapsed() {
            bail!("unhandled non-collapsed selection");
        }

        let (line_index, pos_in_line) = self.find_idx_and_utf16_pos(
            target_range.end_container().fmt_err()?,
            target_range.end_offset().fmt_err()?,
        )?;

        let new_cursor_pos = self
            .line_mut(line_index)?
            .insert_at_utf16_pos(pos_in_line, &the_data)?;

        set_cursor_position(&*self.line(line_index)?, new_cursor_pos);

        #[cfg(debug_assertions)]
        {
            self.audit();
            log_1(&"successful audit".into());
        }

        Ok(())
    }

    // Given a node and offset, find the line index and (UTF-16) position within that line.
    // There are several possibilities for the node (e.g. the div element, the span element,
    // or the text node).
    fn find_idx_and_utf16_pos(&self, node: NodeRef, offset: u32) -> Result<(usize, usize)> {
        let offset = offset as usize;

        // If the position is "in" the div element, make sure the offset matches expectations
        // (either 0 for the very beginning, or #lines for the very end).
        if node.is_a::<HtmlDivElement>() {
            let line_count = self.component().len();
            return Ok(if offset == 0 {
                (0, 0)
            } else if offset == line_count {
                let last_line_idx = line_count.checked_sub(1).context("last line idx")?;
                (last_line_idx, self.line(last_line_idx)?.len_utf16())
            } else {
                bail!("unexpected offset in textentry div")
            });
        }

        // Otherwise, find the line that hosts the position via binary search.
        // (This seems to be sub-millisecond for documents up to 10,000 lines.)
        let line_idx = self
            .component()
            .binary_search_by(|probe| compare_document_position(probe, &node))
            .fmt_err()?;

        // If the position is "in" the span element, make sure the offset matches expectations
        // (either 0 for the beginning of it, or 1 for the end).
        if node.is_a::<HtmlSpanElement>() {
            return Ok(match offset {
                0 => (line_idx, 0),
                1 => (line_idx, self.line(line_idx)?.len_utf16()),
                _ => bail!("unexpected offset when cursor in span"),
            });
        }

        // Otherwise, it must be in the text node. Make sure offset is
        // a sensible UTF-16 position, and return it.
        debug_assert!(node.is_a::<Text>());
        if offset > self.line(line_idx)?.len_utf16() {
            bail!("invalid offset in line {line_idx}");
        }
        Ok((line_idx, offset))
    }

    // Accessors for the component and for a particular line of code
    fn component(&self) -> Ref<'_, DomVec<EditLine, HtmlDivElement>> {
        Ref::map(self.0.borrow(), |c| &c.component)
    }

    fn component_mut(&self) -> RefMut<'_, DomVec<EditLine, HtmlDivElement>> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.component)
    }

    fn line(&self, idx: usize) -> Result<Ref<'_, DomText>> {
        Ok(Ref::map(
            Ref::filter_map(self.component(), |c| c.get(idx))
                .ok()
                .context("line {idx}")?,
            |x| &x.get().0,
        ))
    }

    fn line_mut(&mut self, idx: usize) -> Result<RefMut<'_, DomText>> {
        Ok(RefMut::map(
            RefMut::filter_map(self.component_mut(), |c| c.get_mut(idx))
                .ok()
                .context("line {idx}")?,
            |x| &mut x.get_mut().0,
        ))
    }
}

impl WithElement for Editor {
    type Element = HtmlDivElement;
    fn with_element(&self, f: impl FnMut(&HtmlDivElement), g: AccessToken) {
        self.component().with_element(f, g);
    }
}

impl Component for Editor {
    fn audit(&self) {
        self.component().audit()
    }
}
