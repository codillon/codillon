// The Codillon code editor (doesn't do much, but does capture beforeinput and logs to console)

use crate::{
    dom_struct::DomStruct,
    dom_text::DomText,
    dom_vec::DomVec,
    utils::FmtError,
    web_support::{
        AccessToken, Component, ElementFactory, InputEventHandle, NodeRef, StaticRangeHandle,
        WithElement, compare_document_position, set_cursor_position,
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
    factory: ElementFactory,
}

pub struct Editor(Rc<RefCell<_Editor>>);

impl Editor {
    pub fn new(factory: ElementFactory) -> Self {
        let mut inner = _Editor {
            component: DomVec::new(factory.div()),
            factory,
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
            ret.push_line(&format!("This is 🏳️‍⚧️ line {i}."));
        }

        ret
    }

    fn push_line(&mut self, string: &str) {
        let br = self.0.borrow().factory.br();
        let span = self.0.borrow().factory.span();
        self.component_mut().push(EditLine::new(
            (DomText::new(string), (DomBr::new((), br), ())),
            span,
        ));
    }

    fn insert_line(&mut self, index: usize, string: &str) {
        let br = self.0.borrow().factory.br();
        let span = self.0.borrow().factory.span();
        self.component_mut().insert(
            index,
            EditLine::new((DomText::new(string), (DomBr::new((), br), ())), span),
        );
    }

    fn remove_line(&mut self, index: usize) {
        self.component_mut().remove(index);
    }

    // Replace a given range with new text, multiline enabled
    fn replace_range_with_decorator<F: Fn(&Self, &mut (usize, usize), &mut (usize, usize))>(
        &mut self,
        target_range: StaticRangeHandle,
        new_str: &str,
        range_decorator: F,
    ) -> Result<()> {
        if new_str
            .chars()
            .any(|x| x.is_control() && x != '\r' && x != '\n')
        {
            bail!("unhandled: control char in input");
        }

        let mut start_cursor = self.find_idx_and_utf16_pos(
            target_range.start_container().fmt_err()?,
            target_range.start_offset().fmt_err()?,
        )?;

        let mut end_cursor = self.find_idx_and_utf16_pos(
            target_range.end_container().fmt_err()?,
            target_range.end_offset().fmt_err()?,
        )?;

        range_decorator(self, &mut start_cursor, &mut end_cursor);

        assert!(start_cursor <= end_cursor);

        let new_str_lines = new_str
            .split('\n')
            .map(|line| line.strip_suffix('\r').unwrap_or(line))
            .collect::<Vec<_>>();

        match (end_cursor.0 > start_cursor.0, new_str_lines.len() > 1) {
            (true, true) => {
                let start_line_len = self.line(start_cursor.0)?.len_utf16();
                self.line_mut(start_cursor.0)?.replace_range(
                    start_cursor.1,
                    start_line_len,
                    new_str_lines.first().unwrap(),
                )?;
                self.line_mut(end_cursor.0)?.replace_range(
                    0,
                    end_cursor.1,
                    new_str_lines.last().unwrap(),
                )?;
                end_cursor.1 = str_indices::utf16::count(new_str_lines.last().unwrap());
                for lineno in (start_cursor.0 + new_str_lines.len() - 1..end_cursor.0).rev() {
                    self.remove_line(lineno);
                    end_cursor.0 -= 1;
                }
                for (lineno, new_line) in (1..new_str_lines.len() - 1)
                    .map(|idx| (start_cursor.0 + idx, new_str_lines[idx]))
                {
                    if lineno < end_cursor.0 {
                        self.line_mut(lineno)?.set_data(new_line);
                    } else {
                        self.insert_line(lineno, new_line);
                        end_cursor.0 += 1;
                    }
                }
            }
            (true, false) => {
                let last_line_len = self.line(end_cursor.0)?.len_utf16();
                let last_part = self
                    .line(end_cursor.0)?
                    .slice_utf16(end_cursor.1..last_line_len)?
                    .to_string();
                for line_no in (start_cursor.0 + 1..=end_cursor.0).rev() {
                    self.remove_line(line_no);
                }
                let start_line_len = self.line(start_cursor.0)?.len_utf16();
                self.line_mut(start_cursor.0)?.replace_range(
                    start_cursor.1,
                    start_line_len,
                    &[new_str_lines[0], &last_part].concat(),
                )?;
                end_cursor = (
                    start_cursor.0,
                    start_cursor.1 + str_indices::utf16::count(new_str_lines[0]),
                )
            }
            (false, true) => {
                let the_line_len = self.line(start_cursor.0)?.len_utf16();
                let last_part = self
                    .line(start_cursor.0)?
                    .slice_utf16(end_cursor.1..the_line_len)?
                    .to_string();
                self.line_mut(start_cursor.0)?.replace_range(
                    start_cursor.1,
                    the_line_len,
                    new_str_lines.first().unwrap(),
                )?;
                for (lineno, new_line) in (1..new_str_lines.len() - 1)
                    .map(|idx| (start_cursor.0 + idx, new_str_lines[idx]))
                {
                    self.insert_line(lineno, new_line);
                }
                self.insert_line(
                    start_cursor.0 + new_str_lines.len() - 1,
                    &[*new_str_lines.last().unwrap(), &last_part].concat(),
                );
                end_cursor = (
                    start_cursor.0 + new_str_lines.len() - 1,
                    str_indices::utf16::count(new_str_lines.last().unwrap()),
                )
            }
            (false, false) => {
                self.line_mut(start_cursor.0)?.replace_range(
                    start_cursor.1,
                    end_cursor.1,
                    new_str_lines[0],
                )?;
                end_cursor.1 = start_cursor.1 + str_indices::utf16::count(new_str_lines[0]);
            }
        }

        set_cursor_position(&*self.line(end_cursor.0)?, end_cursor.1);

        Ok(())
    }

    // The input handler. Currently only handles single-line insert/delete events.
    fn handle_input(&mut self, ev: InputEventHandle) -> Result<()> {
        ev.prevent_default();

        let target_range = ev.get_first_target_range()?;

        fn indentity_decorator(
            _editor: &Editor,
            _start_cursor: &mut (usize, usize),
            _end_cursor: &mut (usize, usize),
        ) {
        }

        match &ev.input_type() as &str {
            "insertText" => self.replace_range_with_decorator(
                target_range,
                &ev.data().context("no data")?,
                indentity_decorator,
            ),
            "insertFromPaste" => self.replace_range_with_decorator(
                target_range,
                &ev.data_transfer()
                    .context("no data_transfer")?
                    .get_data("text/plain")
                    .fmt_err()?,
                indentity_decorator,
            ),
            "deleteContentBackward" | "deleteContentForward" => {
                fn delete_decorater(
                    editor: &Editor,
                    start_cursor: &mut (usize, usize),
                    end_cursor: &mut (usize, usize),
                ) {
                    if start_cursor == end_cursor
                        && end_cursor.1 == editor.line(end_cursor.0).expect("get line").len_utf16()
                        && end_cursor.0 + 1 < editor.component().len()
                    {
                        end_cursor.0 += 1;
                        end_cursor.1 = 0;
                    }
                }
                self.replace_range_with_decorator(target_range, "", delete_decorater)
            }
            "insertParagraph" => {
                self.replace_range_with_decorator(target_range, "\n", indentity_decorator)
            }
            _ => bail!(format!(
                "unhandled input type {}, data {:?}",
                ev.input_type(),
                ev.data()
            )),
        }?;

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
                1 | 2 => (line_idx, self.line(line_idx)?.len_utf16()),
                _ => bail!("unexpected offset {offset} when cursor in span"),
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
