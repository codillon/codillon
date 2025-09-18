// The Codillon code editor

use crate::{
    dom_vec::DomVec,
    line::{CodeLine, LineInfo, Position},
    utils::{
        FmtError, LineInfos, LineInfosMut, OkModule, collect_operands, fix_frames, str_to_binary,
    },
    web_support::{
        AccessToken, Component, ElementAsNode, ElementFactory, InputEventHandle, MouseEventHandle,
        NodeRef, StaticRangeHandle, WithElement, compare_document_position, get_selection,
        set_cursor_range, set_selection,
    },
};
use anyhow::{Context, Result, bail};
use std::{
    cell::{Ref, RefCell, RefMut},
    ops::Deref,
    rc::Rc,
};
use web_sys::{HtmlDivElement, console::log_1};

type ComponentType = DomVec<CodeLine, HtmlDivElement>;

struct _Editor {
    module: MaybeModule,
    synthetic_ends: usize,
    component: ComponentType,
    factory: ElementFactory,
}

pub struct Editor(Rc<RefCell<_Editor>>);

enum MaybeModule {
    Module(OkModule),
    Failure(usize), // culprit line idx
}

impl Editor {
    pub fn new(factory: ElementFactory) -> Self {
        let mut inner = _Editor {
            module: MaybeModule::Module(OkModule::default()),
            synthetic_ends: 0,
            component: DomVec::new(factory.div()),
            factory,
        };

        inner.component.set_attribute("class", "textentry");
        inner.component.set_attribute("contenteditable", "true");
        inner.component.set_attribute("spellcheck", "false");

        let mut ret = Editor(Rc::new(RefCell::new(inner)));

        let editor_ref = Rc::clone(&ret.0);
        ret.component_mut().set_onbeforeinput(move |ev| {
            let _ = Editor(editor_ref.clone())
                .handle_input(ev)
                .map_err(|e| log_1(&format!("{e}").into()));
        });

        let editor_ref = Rc::clone(&ret.0);
        ret.component_mut().set_onkeydown(move |ev| {
            let _ = Editor(editor_ref.clone())
                .handle_keydown(ev)
                .map_err(|e| log_1(&format!("{e}").into()));
        });

        let editor_ref = Rc::clone(&ret.0);
        ret.component_mut().set_onmousedown(move |ev| {
            let _ = Editor(editor_ref.clone())
                .handle_mousedown(ev)
                .map_err(|e| log_1(&format!("{e}").into()));
        });

        // demo contents
        ret.push_line("i32.const 5");
        ret.push_line("i32.const 6");
        ret.push_line("i32.add");
        ret.push_line("drop");

        ret
    }

    fn push_line(&mut self, string: &str) {
        let idx = self.component().len();
        let newline = CodeLine::new(string, &self.0.borrow().factory);
        self.component_mut().push(newline);
        self.on_change(idx);
    }

    // Replace a given range (currently within a single line) with new text
    fn replace_range(&mut self, target_range: StaticRangeHandle, new_str: &str) -> Result<()> {
        if new_str.chars().any(|x| x.is_control() && x != '\n') {
            bail!("unhandled control char in input");
        }

        let (start_line_index, start_pos_in_line) = self.find_idx_and_utf16_pos(
            target_range.start_container().fmt_err()?,
            target_range.start_offset().fmt_err()?,
        )?;

        let (end_line_index, end_pos_in_line) = self.find_idx_and_utf16_pos(
            target_range.end_container().fmt_err()?,
            target_range.end_offset().fmt_err()?,
        )?;

        if let Some(locked_to_line) = self.locked_line()
            && new_str.chars().any(|x| x == '\n')
        {
            bail!("tried to add newline, but editing is locked to {locked_to_line}");
        }

        let mut new_cursor_pos = if start_line_index == end_line_index {
            // Single-line edit
            self.modify_line(start_line_index, |line| {
                line.replace_range(start_pos_in_line, end_pos_in_line, new_str)
            })?
        } else if start_line_index < end_line_index {
            // Multi-line edit. Try to make sure the removal happens, even if the
            // resulting line will break the module.

            // Step 1: Save surviving portion of end line to add to the start line.
            let end_pos = self.line(start_line_index).end_position();
            let s = self.line(end_line_index).suffix(end_pos_in_line)?;

            // Step 2: Remove all lines after the start.
            self.remove_lines(start_line_index + 1, end_line_index + 1)?;

            // Step 3: Add surviving portion of end line to the start line
            self.modify_line(start_line_index, |line| {
                line.replace_range(start_pos_in_line, end_pos, &s)
            })?;

            // Step 3: Insert the new text into the (remaining) start line.
            self.modify_line(start_line_index, |line| {
                line.replace_range(start_pos_in_line, start_pos_in_line, new_str)
            })?
        } else {
            bail!(
                "unhandled reversed target range {start_line_index}@{:?} .. {end_line_index}@{:?}",
                start_pos_in_line,
                end_pos_in_line
            )
        };

        // Split the start line if it contains newline chars.
        let mut fixup_line = start_line_index;
        loop {
            let pos: Option<Position> = self.line(fixup_line).first_newline()?;
            match pos {
                None => break,
                Some(pos) => {
                    let rest = self.line(fixup_line).suffix(pos)?;
                    let end_pos = self.line(fixup_line).end_position();
                    self.modify_line(fixup_line, |line| line.replace_range(pos, end_pos, ""))?;
                    let newline = CodeLine::new(&rest[1..], &self.0.borrow().factory);
                    new_cursor_pos = Position::begin();
                    fixup_line += 1;
                    self.insert_line(fixup_line, newline)?;
                }
            }
        }

        self.line(fixup_line).set_cursor_position(new_cursor_pos);

        Ok(())
    }

    // The input handler.
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
            "insertParagraph" | "insertLineBreak" => self.replace_range(target_range, "\n"),
            _ => bail!(format!(
                "unhandled input type {}, data {:?}",
                ev.input_type(),
                ev.data()
            )),
        }?;

        Ok(())
    }

    // Keydown helpers. Firefox has trouble advancing to the next line if there is an ::after pseudo-element
    // later in the line. It also has trouble deleting if the cursor position is at the end of the surrounding
    // div, so try to prevent this. And it skips lines on ArrowLeft if the previous line is completely empty.
    fn handle_keydown(&mut self, ev: web_sys::KeyboardEvent) -> Result<()> {
        if self.locked_line().is_some() {
            return Ok(());
        }
        match ev.key().as_str() {
            "ArrowRight" => {
                let selection = get_selection();
                if selection.is_collapsed() {
                    let (line_idx, pos) = self.find_idx_and_utf16_pos(
                        selection.focus_node().expect("focus"),
                        selection.focus_offset(),
                    )?;
                    if line_idx + 1 < self.component().len()
                        && pos == self.line(line_idx).end_position()
                    {
                        ev.prevent_default();
                        self.line(line_idx + 1)
                            .set_cursor_position(Position::begin());
                    }
                }
            }

            "ArrowDown" => {
                let selection = get_selection();
                if selection.is_collapsed() {
                    let (line_idx, _) = self.find_idx_and_utf16_pos(
                        selection.focus_node().expect("focus"),
                        selection.focus_offset(),
                    )?;
                    if line_idx + 1 == self.component().len() {
                        ev.prevent_default();
                        self.line(line_idx)
                            .set_cursor_position(self.line(line_idx).end_position());
                    }
                }
            }

            "ArrowLeft" => {
                let selection = get_selection();
                if selection.is_collapsed() {
                    let (line_idx, pos) = self.find_idx_and_utf16_pos(
                        selection.focus_node().expect("focus"),
                        selection.focus_offset(),
                    )?;
                    if line_idx > 0 && pos == Position::begin() {
                        ev.prevent_default();
                        self.line(line_idx - 1)
                            .set_cursor_position(self.line(line_idx - 1).end_position());
                    }
                }
            }

            _ => {}
        }

        Ok(())
    }

    fn handle_mousedown(&mut self, ev: MouseEventHandle) -> Result<()> {
        if let Some(locked_to_line) = self.locked_line() {
            let (line_idx, _) = self.find_idx_and_utf16_pos(ev.target(), 0)?;
            if line_idx != locked_to_line {
                ev.prevent_default();
                self.component_mut()[locked_to_line].shake_culprit();
            }
        }

        Ok(())
    }

    // Given a node and offset, find the line index and (UTF-16) position within that line.
    // There are several possibilities for the node (e.g. the div element, the span element,
    // or the text node).
    fn find_idx_and_utf16_pos(&self, node: NodeRef, offset: u32) -> Result<(usize, Position)> {
        let offset = offset as usize;

        // If the position is "in" the div element, make sure the offset matches expectations
        // (either 0 for the very beginning, or #lines for the very end).
        if node.is_same_node(&*self.component()) {
            let line_count = self.component().len();
            return Ok(if offset == 0 {
                (0, Position::begin())
            } else if offset < line_count {
                (offset, Position::begin())
            } else if offset == line_count {
                let last_line_idx = line_count.checked_sub(1).context("last line idx")?;
                (last_line_idx, self.line(last_line_idx).end_position())
            } else {
                bail!("unexpected offset {offset} in textentry div")
            });
        }

        // Otherwise, find the line that hosts the position via binary search.
        // (This seems to be sub-millisecond for documents up to 10,000 lines.)
        let line_idx = self
            .component()
            .binary_search_by(|probe| compare_document_position(probe, &node))
            .fmt_err()?;

        Ok((line_idx, self.line(line_idx).get_position(node, offset)?))
    }

    // Accessors for the component and for a particular line of code
    fn component(&self) -> Ref<'_, ComponentType> {
        Ref::map(self.0.borrow(), |c| &c.component)
    }

    fn component_mut(&mut self) -> RefMut<'_, ComponentType> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.component)
    }

    fn module(&self) -> Ref<'_, MaybeModule> {
        Ref::map(self.0.borrow(), |c| &c.module)
    }

    fn module_mut(&self) -> RefMut<'_, MaybeModule> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.module)
    }

    fn line(&self, idx: usize) -> Ref<'_, CodeLine> {
        Ref::map(self.component(), |c| &c[idx])
    }

    fn modify_line<T>(&mut self, idx: usize, f: impl Fn(&mut CodeLine) -> Result<T>) -> Result<T> {
        if let Some(locked_to_line) = self.locked_line()
            && idx != locked_to_line
        {
            bail!("tried to modify line {idx} but editing is locked to line {locked_to_line}");
        }

        let function_res = f(&mut self.component_mut()[idx]);
        self.on_change(idx);
        function_res
    }

    fn remove_lines(&mut self, start_idx: usize, end_idx: usize) -> Result<()> {
        if let Some(locked_to_line) = self.locked_line()
            && (start_idx != locked_to_line || end_idx != locked_to_line + 1)
        {
            bail!(
                "tried to remove lines {start_idx}..{end_idx} but editing is locked to line {locked_to_line}"
            );
        }

        for i in (start_idx..end_idx).rev() {
            self.modify_line(i, |line| {
                line.clear();
                Ok(())
            })?;
        }
        self.component_mut().remove_range(start_idx, end_idx);
        Ok(())
    }

    fn insert_line(&mut self, idx: usize, line: CodeLine) -> Result<()> {
        if let Some(locked_to_line) = self.locked_line() {
            bail!("cannot insert line because editing is locked to {locked_to_line}");
        }

        self.component_mut().insert(idx, line);
        self.on_change(idx);

        Ok(())
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

    fn locked_line(&self) -> Option<usize> {
        if let MaybeModule::Failure(locked_to_line) = *self.module() {
            Some(locked_to_line)
        } else {
            None
        }
    }

    fn on_change(&mut self, line_idx: usize) {
        // repair syntax
        fix_frames(self);

        // update module
        let text = self
            .instructions_as_text()
            .fold(String::new(), |acc, elem| acc + "\n" + elem.as_ref());
        match str_to_binary(text) {
            Ok(bin) => {
                if let Some(locked_to_line) = self.locked_line() {
                    let selection = get_selection();
                    self.component_mut()
                        .set_attribute("contenteditable", "true");
                    self.component_mut()[locked_to_line].clear_culprit();
                    // Chrome seems to have a bug in preserving the cursor position in this case,
                    // unless selection is set to something different than where it is.
                    set_cursor_range(&*self.component(), 0, &*self.component(), 0);
                    set_selection(&selection);
                }

                *self.module_mut() =
                    MaybeModule::Module(OkModule::build(bin, self).expect("OkModule"));

                let MaybeModule::Module(ref module) = *self.module() else {
                    panic!("");
                };

                // log instruction types (TODO: integrate into OkModule)
                let _ = collect_operands(module.borrow_binary(), module.borrow_ops());
            }
            Err(e) => {
                let reason = format!("{e}");
                self.component_mut()[line_idx].set_culprit(&reason);
                self.0.borrow_mut().module = MaybeModule::Failure(line_idx);
                self.component_mut()
                    .set_attribute("contenteditable", "false");
            }
        };

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
        while self.index < self.editor.len() && !self.editor[self.index].info().is_instr() {
            self.index += 1;
        }

        if self.index < self.editor.len() {
            let ret = Some(Ref::map(Ref::clone(&self.editor), |x| {
                x[self.index].instr().get()
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

    fn info(&self, index: usize) -> impl Deref<Target = LineInfo> {
        Ref::map(self.line(index), |x| x.info())
    }

    fn synthetic_ends(&self) -> usize {
        self.0.borrow().synthetic_ends
    }
}

impl LineInfosMut for Editor {
    fn set_active_status(&mut self, index: usize, is_active: bool) {
        self.component_mut()[index].set_active_status(is_active)
    }

    fn set_synthetic_ends(&mut self, num: usize) {
        self.0.borrow_mut().synthetic_ends = num;
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
        self.component().audit();
    }
}
