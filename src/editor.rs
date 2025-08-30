// The Codillon code editor (doesn't do much, but does capture beforeinput and logs to console)

use crate::{
    dom_sidecar::DomSidecar,
    dom_struct::DomStruct,
    dom_text::DomText,
    dom_vec::DomVec,
    utils::{
        FmtError, InstrKind, LineInfos, LineInfosMut, OkModule, collect_operands, fix_frames,
        parse_instr, str_to_binary,
    },
    web_support::{
        AccessToken, Component, ElementAsNode, ElementFactory, InputEventHandle, NodeRef,
        StaticRangeHandle, WithElement, compare_document_position, set_cursor_position,
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
type Line = DomStruct<LineContents, HtmlSpanElement>;
type LineWithInfo = DomSidecar<Line, LineInfo>;
type ComponentType = DomVec<LineWithInfo, HtmlDivElement>;

#[derive(Copy, Clone)]
pub struct LineInfo {
    pub kind: InstrKind,
    pub active: bool,
}

impl LineInfo {
    pub fn new(kind: InstrKind, active: bool) -> Self {
        Self { kind, active }
    }

    pub fn is_instr(&self) -> bool {
        self.kind != InstrKind::EmptyOrMalformed && self.active
    }
}

struct _Editor {
    module: OkModule,
    synthetic_ends: usize,
    component: ComponentType,
}

pub struct Editor(Rc<RefCell<_Editor>>);

impl Editor {
    pub fn new(factory: &ElementFactory) -> Self {
        let mut inner = _Editor {
            module: OkModule::default(),
            synthetic_ends: 0,
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

        ret.push_line(factory, "i32.const 5");
        ret.push_line(factory, "i32.const 6");
        ret.push_line(factory, "i32.add");
        ret.push_line(factory, "drop");

        ret.on_change();

        ret
    }

    fn push_line(&mut self, factory: &ElementFactory, string: &str) {
        {
            let mut editor = self.0.borrow_mut();
            let component = &mut editor.component;
            component.push(DomSidecar::new(
                Line::new(
                    (DomText::new(string), (DomBr::new((), factory.br()), ())),
                    factory.span(),
                ),
                LineInfo {
                    kind: parse_instr(string),
                    active: true,
                },
            ));
            let line = component.get_mut(component.len() - 1).expect("DomSidecar");
            let class = match line.borrow_sidecar().kind {
                InstrKind::EmptyOrMalformed => "grey-text",
                _ => "black-text",
            };
            line.borrow_component_mut().set_attribute("class", class);
        }
        self.on_change();
    }

    // Replace a given range (currently within a single line) with new text
    fn replace_range(&mut self, target_range: StaticRangeHandle, new_str: &str) -> Result<()> {
        if new_str.chars().any(|x| x.is_control()) {
            bail!("unhandled: control char [e.g. carriage return] in input");
        }

        let (start_line_index, start_pos_in_line) = self.find_idx_and_utf16_pos(
            target_range.start_container().fmt_err()?,
            target_range.start_offset().fmt_err()?,
        )?;

        let (end_line_index, end_pos_in_line) = self.find_idx_and_utf16_pos(
            target_range.end_container().fmt_err()?,
            target_range.end_offset().fmt_err()?,
        )?;

        if start_line_index != end_line_index {
            bail!(
                "unhandled: multi-line target range {start_line_index}/{start_pos_in_line}..{end_line_index}/{end_pos_in_line}"
            );
        }

        let new_cursor_pos = self.line_text_mut(start_line_index).replace_range(
            start_pos_in_line,
            end_pos_in_line,
            new_str,
        )?;

        let new_info = LineInfo {
            kind: parse_instr(self.line_text(start_line_index).get_contents()),
            active: true,
        };

        {
            let mut editor = self.0.borrow_mut();
            let line = &mut editor
                .component
                .get_mut(start_line_index)
                .expect("DomSidecar");

            *line.borrow_sidecar_mut() = new_info;

            let class = match line.borrow_sidecar().kind {
                InstrKind::EmptyOrMalformed => "grey-text",
                _ => "black-text",
            };
            line.borrow_component_mut().set_attribute("class", class);
        }

        set_cursor_position(&*self.line_text(start_line_index), new_cursor_pos);

        Ok(())
    }

    // The input handler. Currently only handles single-line insert/delete events.
    fn handle_input(&mut self, ev: InputEventHandle) -> Result<()> {
        ev.prevent_default();

        let target_range = ev.get_first_target_range()?;

        match &ev.input_type() as &str {
            "insertText" => self.replace_range(target_range, &ev.data().context("no data")?),
            "insertFromPaste" => self.replace_range(
                target_range,
                &ev.data_transfer()
                    .context("no data_transfer")?
                    .get_data("text/plain")
                    .fmt_err()?,
            ),
            "deleteContentBackward" | "deleteContentForward" => {
                self.replace_range(target_range, "")
            }
            _ => bail!(format!(
                "unhandled input type {}, data {:?}",
                ev.input_type(),
                ev.data()
            )),
        }?;

        self.on_change();

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
                (last_line_idx, self.line_text(last_line_idx).len_utf16())
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
        // (either 0 for the beginning of it, or 1 or 2 for the end).
        if node.is_a::<HtmlSpanElement>() {
            return Ok(match offset {
                0 => (line_idx, 0),
                1 | 2 => (line_idx, self.line_text(line_idx).len_utf16()),
                _ => bail!("unexpected offset {offset} when cursor in span"),
            });
        }

        // Otherwise, it must be in the text node. Make sure offset is
        // a sensible UTF-16 position, and return it.
        debug_assert!(node.is_a::<Text>());
        if offset > self.line_text(line_idx).len_utf16() {
            bail!("invalid offset in line {line_idx}");
        }
        Ok((line_idx, offset))
    }

    // Accessors for the component and for a particular line of code
    fn component(&self) -> Ref<'_, ComponentType> {
        Ref::map(self.0.borrow(), |c| &c.component)
    }

    fn component_mut(&mut self) -> RefMut<'_, ComponentType> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.component)
    }

    fn line(&self, idx: usize) -> Ref<'_, LineWithInfo> {
        Ref::map(self.component(), |c| &c[idx])
    }

    fn line_mut(&mut self, idx: usize) -> RefMut<'_, LineWithInfo> {
        RefMut::map(self.component_mut(), |c| &mut c[idx])
    }

    fn line_text(&self, idx: usize) -> Ref<'_, DomText> {
        Ref::map(self.line(idx), |c| &c.borrow_component().get().0)
    }

    fn line_text_mut(&mut self, idx: usize) -> RefMut<'_, DomText> {
        RefMut::map(self.line_mut(idx), |c| {
            &mut c.borrow_component_mut().get_mut().0
        })
    }

    // get the "instructions" (active, well-formed lines) as text
    fn instructions_as_text(&self) -> impl Iterator<Item = Ref<'_, str>> {
        InstructionTextIterator {
            editor: self.component(),
            index: 0,
        }
        .chain(
            std::iter::repeat_with(move || Ref::map(self.0.borrow(), |_| "end"))
                .take(self.0.borrow().synthetic_ends),
        )
    }

    fn on_change(&mut self) {
        // repair syntax
        self.0.borrow_mut().synthetic_ends = fix_frames(self);

        // update module
        let text = self
            .instructions_as_text()
            .fold(String::new(), |acc, elem| acc + "\n" + elem.as_ref());
        let bin = str_to_binary(text).expect("wasm binary");
        self.0.borrow_mut().module = OkModule::build(bin, self).expect("OkModule");

        // log instruction types (TODO: integrate into OkModule)
        web_sys::console::log_1(
            &format!(
                "instruction types: {:?}",
                collect_operands(
                    self.0.borrow().module.borrow_binary(),
                    self.0.borrow().module.borrow_ops()
                ),
            )
            .into(),
        );

        #[cfg(debug_assertions)]
        {
            self.audit();
            log_1(&"successful audit".into());
        }
    }
}

pub struct InstructionTextIterator<'a> {
    editor: Ref<'a, ComponentType>,
    index: usize,
}

impl<'a> Iterator for InstructionTextIterator<'a> {
    type Item = Ref<'a, str>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < self.editor.len() && !self.editor[self.index].borrow_sidecar().is_instr()
        {
            self.index += 1;
        }

        if self.index < self.editor.len() {
            let ret = Some(Ref::map(Ref::clone(&self.editor), |x| {
                x[self.index].borrow_component().get().0.get_contents()
            }));
            self.index += 1;
            ret
        } else {
            None
        }
    }
}

impl LineInfos for Editor {
    fn is_empty(&self) -> bool {
        self.component().is_empty()
    }

    fn len(&self) -> usize {
        self.component().len()
    }

    fn get(&self, index: usize) -> impl std::ops::Deref<Target = LineInfo> {
        Ref::map(self.line(index), |c| c.borrow_sidecar())
    }

    fn synthetic_ends(&self) -> usize {
        self.0.borrow().synthetic_ends
    }
}

impl LineInfosMut for Editor {
    fn get_mut(&mut self, index: usize) -> impl std::ops::DerefMut<Target = LineInfo> {
        RefMut::map(self.line_mut(index), |c| c.borrow_sidecar_mut())
    }

    fn set_attribute(&mut self, index: usize, name: &str, value: &str) {
        self.0
            .borrow_mut()
            .component
            .get_mut(index)
            .expect("DomSidecar")
            .borrow_component_mut()
            .set_attribute(name, value);
    }
}

impl WithElement for Editor {
    type Element = HtmlDivElement;
    fn with_element(&self, f: impl FnMut(&HtmlDivElement), g: AccessToken) {
        self.component().with_element(f, g);
    }
}

impl ElementAsNode for Editor {}

impl Component for Editor {
    fn audit(&self) {
        self.component().audit()
    }
}
