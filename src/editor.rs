// The Codillon code editor

use crate::{
    action_history::{ActionHistory, Edit, Selection},
    debug::{run_binary, with_debug_state},
    dom_canvas::DomCanvas,
    dom_struct::DomStruct,
    dom_vec::DomVec,
    graphics::DomImage,
    jet::{
        AccessToken, Component, ControlHandlers, ElementFactory, InputEventHandle, NodeRef,
        RangeLike, ReactiveComponent, StaticRangeHandle, StorageHandle, WithElement,
        compare_document_position, get_selection, now_ms, set_selection_range,
    },
    line::{Activity, CodeLine, LineInfo, Position},
    slider::Slider,
    syntax::{
        FrameInfo, FrameInfosMut, InstrKind, LineInfos, LineInfosMut, LineKind, SyntheticWasm,
        fix_syntax,
    },
    utils::{FmtError, OperatorType, RawModule, indent_and_frame, str_to_binary},
};
use anyhow::{Context, Result, bail};
use itertools::Itertools;
use std::{
    cell::{Ref, RefCell, RefMut},
    cmp::min,
    collections::HashMap,
    ops::Deref,
    rc::Rc,
};
use wasm_bindgen::closure::Closure;
use web_sys::{HtmlDivElement, console::log_1};

type TextType = DomVec<CodeLine, HtmlDivElement>;
type ComponentType = DomStruct<
    (
        DomImage,
        (
            ReactiveComponent<TextType>,
            (ReactiveComponent<Slider>, (DomCanvas, ())),
        ),
    ),
    HtmlDivElement,
>;

pub const LINE_SPACING: usize = 40;
const STORAGE_ID: &str = "codillon_content";
const SCHEDULE_STORE_MS: i32 = 500;

struct _Editor {
    component: ComponentType,
    factory: ElementFactory,
    action_history: ActionHistory,
    storage: Option<StorageHandle>,
    pending_binary: Option<Vec<u8>>,
    previous_binary_hash: u64,
    worker_running: bool,
    pending_save: Option<Closure<dyn Fn()>>,
}

pub struct Editor(Rc<RefCell<_Editor>>);

impl Editor {
    pub fn new(factory: ElementFactory) -> Self {
        let inner = _Editor {
            component: DomStruct::new(
                (
                    DomImage::new(factory.clone()),
                    (
                        ReactiveComponent::new(DomVec::new(factory.div())),
                        (
                            (ReactiveComponent::new(Slider::new(factory.clone()))),
                            (DomCanvas::new(factory.canvas()), ()),
                        ),
                    ),
                ),
                factory.div(),
            ),
            factory,
            action_history: ActionHistory::default(),
            storage: StorageHandle::new(),
            pending_binary: None,
            previous_binary_hash: 0,
            worker_running: false,
            pending_save: None,
        };

        let mut ret = Editor(Rc::new(RefCell::new(inner)));
        {
            let mut text = ret.textbox_mut();
            text.inner_mut().set_attribute("class", "textentry");
            text.inner_mut().set_attribute(
                "style",
                &format!("--codillon-line-spacing: {}px;", LINE_SPACING),
            );
            text.inner_mut().set_attribute("contenteditable", "true");
            text.inner_mut().set_attribute("spellcheck", "false");

            let editor_ref = Rc::clone(&ret.0);
            text.set_onbeforeinput(move |ev| {
                Editor(editor_ref.clone())
                    .handle_input(ev)
                    .expect("input handler")
            });

            let editor_ref = Rc::clone(&ret.0);
            text.set_onkeydown(move |ev| {
                Editor(editor_ref.clone())
                    .handle_keydown(ev)
                    .expect("keydown handler")
            });
        }
        {
            let editor_ref = Rc::clone(&ret.0);
            ret.slider_mut().set_oninput(move |_| {
                let editor = Editor(editor_ref.clone());
                let _step = editor.slider().inner().value_as_number().round() as usize;
                let step_count = with_debug_state(|state| state.completed_steps.len());
                let current_step = editor.slider().inner().value_as_number() as usize;
                editor.update_slider(step_count, current_step);
                //                editor.build_program_state(step);
            });

            let editor_ref = Rc::clone(&ret.0);
            let beforeunload = Closure::new(move || {
                let editor = Editor(editor_ref.clone());
                if let Some(storage) = &editor_ref.borrow().storage {
                    storage.set_item(STORAGE_ID, &editor.buffer_as_text().join(""));
                }
            });
            StorageHandle::set_onbeforeunload(&beforeunload);
            // Must live for tab lifetime so small memory leak is ok until tab is closed
            beforeunload.forget();
        }

        ret.image_mut().set_attribute("class", "annotations");

        // Restore from localStorage, or use default content
        if let Some(storage) = StorageHandle::new()
            && let Some(content) = storage.get_item(STORAGE_ID)
        {
            for line_str in content.lines() {
                ret.push_line(line_str);
            }
            if ret.on_change().is_err() {
                ret.set_default_contents()
            };
        } else {
            ret.set_default_contents();
        }

        ret.on_change().expect("well-formed initial contents");

        let height = LINE_SPACING * ret.text().len();
        ret.image_mut().set_attribute("height", &height.to_string());

        ret
    }

    fn push_line(&mut self, string: &str) {
        let newline = CodeLine::new(string, &self.0.borrow().factory);
        self.text_mut().push(newline);
    }

    fn set_default_contents(&mut self) {
        let len = self.len();
        self.text_mut().remove_range(0, len);
        self.push_line("(func");
        self.push_line("i32.const 5");
        self.push_line("i32.const 6");
        self.push_line("i32.add");
        self.push_line("drop");
        self.push_line(")");
    }

    fn get_lines_and_positions(
        &self,
        range: &impl RangeLike,
    ) -> Result<(usize, Position, usize, Position)> {
        let (start_line, start_pos) =
            self.find_idx_and_utf16_pos(range.node1().unwrap(), range.offset1())?;

        let (end_line, end_pos) =
            self.find_idx_and_utf16_pos(range.node2().unwrap(), range.offset2())?;

        Ok((start_line, start_pos, end_line, end_pos))
    }

    // Replace a given range (currently within a single line) with new text
    fn replace_range(&mut self, target_range: StaticRangeHandle, new_str: &str) -> Result<()> {
        if new_str.chars().any(|x| x.is_control() && x != '\n') {
            bail!("unhandled control char in input");
        }

        let saved_selection = self.get_lines_and_positions(&get_selection())?; // in case we need to revert

        let (start_line, start_pos, end_line, end_pos) =
            self.get_lines_and_positions(&target_range)?;

        let mut backup = Vec::new();
        for i in start_line..end_line + 1 {
            backup.push(self.line(i).suffix(Position::begin())?);
        }

        // disable animations; will be re-enabled if any indentation changes
        self.0.borrow_mut().component.remove_attribute("class");

        let mut new_cursor_pos = if start_line == end_line {
            // Single-line edit.
            let was_structured = self.line(start_line).info().is_structured();
            let old_instr = self.line(start_line).instr().get().to_string();
            // Do the replacement.
            let new_pos = self
                .line_mut(start_line)
                .replace_range(start_pos, end_pos, new_str)?;

            // Should we add a "courtesy" `end` (because of a newly added structured instruction)?
            let is_structured = self.line(start_line).info().is_structured();
            let is_if = self.line(start_line).info().kind == LineKind::Instr(InstrKind::If);

            if !was_structured
                && is_structured
                && !old_instr.starts_with(self.line(start_line).instr().get())
            {
                // search for existing deactivated matching `end` (or `else` if the new instr is `if`)
                let mut need_end = true;
                for i in start_line + 1..self.text().len() {
                    if self.line(i).info().kind == LineKind::Instr(InstrKind::End)
                        && !self.line(i).info().is_active()
                    {
                        need_end = false;
                    }
                    if is_if
                        && self.line(i).info().kind == LineKind::Instr(InstrKind::Else)
                        && !self.line(i).info().is_active()
                    {
                        need_end = false;
                    }
                }
                if need_end {
                    let matching_end = CodeLine::new("end", &self.0.borrow().factory);
                    self.text_mut().insert(start_line + 1, matching_end);
                    self.line_mut(start_line + 1).reveal();
                }
            } else if was_structured && !is_structured {
                // Should we delete an unnecessary subsequent `end` (because of a removed structured instr)?
                if self.len() > start_line + 1
                    && self.line(start_line + 1).info().is_active()
                    && self.line(start_line + 1).info().kind == LineKind::Instr(InstrKind::End)
                    && self.line(start_line + 1).comment().is_empty()
                {
                    self.text_mut().remove(start_line + 1);
                    self.line_mut(start_line).fly_end();
                }
            }

            // Return new cursor position
            new_pos
        } else if start_line < end_line {
            // Multi-line edit.

            if self.line(start_line).all_whitespace() {
                self.line_mut(start_line).unset_indent();
            }

            // Step 1: Add surviving portion of end line to the start line.
            let end_pos_in_line = self.line(start_line).end_position();
            let s = self.line(end_line).suffix(end_pos)?;

            // Save absolute offset for use later, because changing the line can
            // move text between instr and comment and will invalidate existing Position.
            let start_pos_absolute = self
                .line(start_line)
                .position_to_absolute_utf16_offset(start_pos)?;

            self.line_mut(start_line)
                .replace_range(start_pos, end_pos_in_line, &s)?;

            // Recompute start_pos based on new line contents
            let start_pos = self
                .line(start_line)
                .absolute_utf16_offset_to_position(start_pos_absolute)?;

            // Step 2: Remove all lines after the start.
            self.text_mut().remove_range(start_line + 1, end_line + 1);

            // Step 3: Insert the new text into the (remaining) start line.
            self.line_mut(start_line)
                .replace_range(start_pos, start_pos, new_str)?
        } else {
            bail!(
                "unhandled reversed target range {start_line}@{:?} .. {end_line}@{:?}",
                start_pos,
                end_pos
            )
        };

        // Split the start line if it contains newline chars.
        let mut fixup_line = start_line;
        loop {
            let pos: Option<Position> = self.line(fixup_line).first_newline()?;
            match pos {
                None => break,
                Some(pos) => {
                    let rest = self.line(fixup_line).suffix(pos)?;
                    let end_pos = self.line(fixup_line).end_position();
                    self.line_mut(fixup_line).replace_range(pos, end_pos, "")?;
                    let newline = CodeLine::new(&rest[1..], &self.0.borrow().factory);
                    new_cursor_pos = Position::begin();
                    fixup_line += 1;
                    self.text_mut().insert(fixup_line, newline);
                }
            }
        }

        // Is the new module well-formed? Otherwise, revert this entire change.
        match self.on_change() {
            Ok(()) => {
                self.line(fixup_line).set_cursor_position(new_cursor_pos);
                let mut new_lines = Vec::new();
                for i in start_line..=fixup_line {
                    new_lines.push(self.line(i).suffix(Position::begin())?);
                }
                // Store new edit
                {
                    self.0.borrow_mut().action_history.store_edit(Edit {
                        start_line,
                        old_lines: backup,
                        new_lines,
                        selection_before: Selection {
                            start_line: saved_selection.0,
                            start_pos: saved_selection.1,
                            end_line: saved_selection.2,
                            end_pos: saved_selection.3,
                        },
                        selection_after: Selection {
                            start_line: fixup_line,
                            start_pos: new_cursor_pos,
                            end_line: fixup_line,
                            end_pos: new_cursor_pos,
                        },
                        time_ms: now_ms(),
                    });
                }
            }
            Err(e) => {
                log_1(&format!("reverting after {e}").into());

                // restore backup
                self.text_mut().remove_range(start_line, fixup_line + 1);
                for (i, contents) in backup.iter().enumerate() {
                    let mut line = CodeLine::new(contents, &self.0.borrow().factory);
                    line.shake();
                    self.text_mut().insert(start_line + i, line);
                }

                self.on_change().expect("well-formed after restore");

                // restore selection
                let (start_line, start_pos, end_line, end_pos) = saved_selection;

                let start_line = self.line(start_line);
                let new_start_node = start_line.position_to_node(start_pos);
                let end_line = self.line(end_line);
                let new_end_node = end_line.position_to_node(end_pos);

                set_selection_range(
                    new_start_node,
                    start_pos.offset.try_into().expect("offset -> u32"),
                    new_end_node,
                    end_pos.offset.try_into().expect("offset -> u32"),
                );
            }
        }

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
            "deleteContentBackward" | "deleteContentForward" | "deleteByCut" => {
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
        match ev.key().as_str() {
            "ArrowRight" => {
                let selection = get_selection();
                if selection.is_collapsed() {
                    let (line_idx, pos) = self.find_idx_and_utf16_pos(
                        selection.focus_node().context("focus")?,
                        selection.focus_offset(),
                    )?;
                    if line_idx + 1 < self.text().len() && pos == self.line(line_idx).end_position()
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
                        selection.focus_node().context("focus")?,
                        selection.focus_offset(),
                    )?;
                    if line_idx + 1 == self.text().len() {
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
                        selection.focus_node().context("focus")?,
                        selection.focus_offset(),
                    )?;
                    if line_idx > 0 && pos == Position::begin() {
                        ev.prevent_default();
                        self.line(line_idx - 1)
                            .set_cursor_position(self.line(line_idx - 1).end_position());
                    }
                }
            }

            "z" | "Z" => {
                if ev.ctrl_key() || ev.meta_key() {
                    ev.prevent_default();
                    if ev.shift_key() {
                        self.redo()?;
                    } else {
                        self.undo()?;
                    }
                }
            }

            _ => {}
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
        if node.is_same_node(&*self.text()) {
            let line_count = self.text().len();
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
            .text()
            .binary_search_by(|probe| compare_document_position(probe, &node))
            .fmt_err()?;

        Ok((line_idx, self.line(line_idx).get_position(node, offset)?))
    }

    // Accessors
    fn textbox(&self) -> Ref<'_, ReactiveComponent<TextType>> {
        Ref::map(self.0.borrow(), |c| &c.component.get().1.0)
    }

    fn textbox_mut(&self) -> RefMut<'_, ReactiveComponent<TextType>> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.component.get_mut().1.0)
    }

    fn image_mut(&self) -> RefMut<'_, DomImage> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.component.get_mut().0)
    }

    fn text(&self) -> Ref<'_, TextType> {
        Ref::map(self.textbox(), |c| c.inner())
    }

    fn text_mut(&mut self) -> RefMut<'_, TextType> {
        RefMut::map(self.textbox_mut(), |c| c.inner_mut())
    }

    fn line(&self, idx: usize) -> Ref<'_, CodeLine> {
        Ref::map(self.text(), |c| &c[idx])
    }

    fn line_mut(&mut self, idx: usize) -> RefMut<'_, CodeLine> {
        RefMut::map(self.text_mut(), |c| &mut c[idx])
    }

    fn slider(&self) -> Ref<'_, ReactiveComponent<Slider>> {
        Ref::map(self.0.borrow(), |c| &c.component.get().1.1.0)
    }

    fn slider_mut(&self) -> RefMut<'_, ReactiveComponent<Slider>> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.component.get_mut().1.1.0)
    }

    // get the user-entered text buffer (doesn't include synthetic Wasm)
    fn buffer_as_text(&self) -> impl Iterator<Item = Ref<'_, str>> {
        UserTextIterator {
            editor: self.text(),
            line_field_idx: 0,
        }
    }

    // get the active, well-formed line contents
    fn active_as_text(&self) -> impl Iterator<Item = Ref<'_, str>> {
        ActiveTextIterator {
            editor: self.text(),
            line_idx: 0,
            active_str_idx: 0,
        }
    }

    fn on_change(&mut self) -> Result<()> {
        // repair syntax
        fix_syntax(self);

        let wasm_bin = str_to_binary(self.active_as_text().join(" "))?;
        let binary_hash = Self::hash_binary(&wasm_bin);
        let raw_module = RawModule::new(self, &wasm_bin)?;
        let validized = raw_module.fix_validity(self, &wasm_bin)?;
        let types = validized.to_types_table(&wasm_bin)?;

        // indent operators and find frames
        indent_and_frame(self, &validized, &types);

        let mut last_line_no = 0;
        for i in 0..validized.functions.len() {
            for (op, OperatorType { inputs, outputs }) in
                std::iter::zip(&validized.functions[i].operators, &types.funcs[i].ops)
            {
                let mut type_str = String::new();

                for t in inputs {
                    match t {
                        Some(ty) => type_str
                            .push_str(&(types.slots[ty.0].0.to_string() + ":" + &ty.0.to_string())),
                        None => type_str.push('?'),
                    }
                    type_str.push(' ');
                }
                if inputs.is_empty() {
                    type_str.push_str("𝜖 ");
                }

                type_str.push('→');
                for t in outputs {
                    type_str.push(' ');
                    type_str.push_str(&(types.slots[t.0].0.to_string() + ":" + &t.0.to_string()));
                }
                if outputs.is_empty() {
                    type_str.push_str(" 𝜖");
                }

                while last_line_no < op.line_idx {
                    self.line_mut(last_line_no).set_debug_annotation(None);
                    last_line_no += 1;
                }
                last_line_no += 1;

                self.line_mut(op.line_idx)
                    .set_debug_annotation(Some(&type_str));
            }
        }

        for i in last_line_no..self.len() {
            self.line_mut(i).set_debug_annotation(None);
        }

        let binary_changed = self.0.borrow().previous_binary_hash != binary_hash;
        // instrumentation
        self.execute(
            &validized.build_instrumented_binary(&types)?,
            binary_changed,
        );
        self.0.borrow_mut().previous_binary_hash = binary_hash;
        self.schedule_save(); // schedule save to local storage

        #[cfg(debug_assertions)]
        self.audit();

        Ok(())
    }

    fn schedule_save(&self) {
        if self.0.borrow().pending_save.is_some() {
            return;
        }
        let editor_ref = Rc::clone(&self.0);
        let closure = Closure::new(move || {
            let editor_content = Editor(editor_ref.clone()).buffer_as_text().join("");
            let mut inner = editor_ref.borrow_mut();
            if let Some(storage) = &inner.storage {
                storage.set_item(STORAGE_ID, &editor_content);
                inner.pending_save = None;
            }
        });
        StorageHandle::set_timeout_with_callback(&closure, SCHEDULE_STORE_MS);
        self.0.borrow_mut().pending_save = Some(closure);
    }

    fn execute(&self, binary: &[u8], binary_changed: bool) {
        let inner = Rc::clone(&self.0);
        inner.borrow_mut().pending_binary = Some(binary.to_vec());
        if inner.borrow().worker_running {
            return;
        }
        inner.borrow_mut().worker_running = true;
        wasm_bindgen_futures::spawn_local(async move {
            loop {
                let Some(binary) = inner.borrow_mut().pending_binary.take() else {
                    break;
                };
                run_binary(&binary)
                    .await
                    .unwrap_or_else(|e| log_1(&format!("Codillon runtime error: {e}").into()));
                let editor_handle = Editor(Rc::clone(&inner));
                // print out steps for debugging
                // XXX: render in UI

                with_debug_state(|state| {
                    use crate::debug::TerminationType::*;
                    log_1(
                        &format!(
                            "terminated after {} steps with status {:?}",
                            state.completed_steps.len(),
                            state.termination
                        )
                        .into(),
                    );

                    let step_count = state.completed_steps.len();
                    if step_count > 1 {
                        editor_handle.update_slider(step_count, step_count - 1);
                        // Move slider to the end if binary changed
                        if binary_changed {
                            editor_handle
                                .slider_mut()
                                .inner_mut()
                                .set_value_as_number(step_count as f64 - 1.0);
                        }
                    } else {
                        editor_handle.update_slider(step_count, 0);
                    }

                    match &state.termination {
                        Running => log_1(
                            &"abnormal exit (still running, maybe from an unhandled return value)"
                                .into(),
                        ),
                        TooManySteps | Success => {}
                        HitInvalid => log_1(
                            &format!(
                                "hit invalid @ line #{}",
                                state.completed_steps.last().unwrap().line_num
                            )
                            .into(),
                        ),
                        HitBadImport => log_1(
                            &format!(
                                "hit bad import @ line #{}",
                                state.completed_steps.iter().rev().nth(1).unwrap().line_num
                            )
                            .into(),
                        ),
                        Error(reason) => log_1(
                            &format!(
                                "hit error {} @ line #{}",
                                reason,
                                state.completed_steps.last().unwrap().line_num
                            )
                            .into(),
                        ),
                    }

                    /*
                            for (i, step) in state.completed_steps.iter().enumerate() {
                                log_1(&format!("step #{i}: {:?}", step).into());
                        }
                    */
                });
            }
            inner.borrow_mut().worker_running = false;
        });
    }

    fn hash_binary(binary: &[u8]) -> u64 {
        use std::hash::{DefaultHasher, Hash, Hasher};
        let mut hasher = DefaultHasher::new();
        binary.hash(&mut hasher);
        hasher.finish()
    }

    fn update_slider(&self, step_count: usize, current_step: usize) {
        if step_count > 1 {
            self.slider_mut().inner_mut().show();
            self.slider_mut()
                .inner_mut()
                .build_ticks(step_count - 1, current_step);
        } else {
            self.slider_mut().inner_mut().hide();
        }
    }

    fn apply_selection(
        &mut self,
        start_line: usize,
        remove_len: usize,
        insert_lines: &[String],
        selection_after: &Selection,
    ) -> Result<()> {
        let mut document_length = self.len();
        if start_line > document_length {
            bail!("start_line {start_line} out of range");
        }
        // Removing ending
        let end = min(
            start_line
                .checked_add(remove_len)
                .unwrap_or(document_length),
            self.len(),
        );
        if end > start_line {
            self.text_mut().remove_range(start_line, end);
        }
        // Add new lines
        for (index, value) in insert_lines.iter().enumerate() {
            let newline = CodeLine::new(value, &self.0.borrow().factory);
            self.text_mut().insert(start_line + index, newline);
        }
        self.on_change()?;
        document_length = self.len().saturating_sub(1);
        let start_index = min(selection_after.start_line, document_length);
        let end_index = min(selection_after.end_line, document_length);
        // Restore caret position
        set_selection_range(
            self.line(start_index)
                .position_to_node(selection_after.start_pos),
            selection_after
                .start_pos
                .offset
                .try_into()
                .expect("offset -> u32"),
            self.line(end_index)
                .position_to_node(selection_after.end_pos),
            selection_after
                .end_pos
                .offset
                .try_into()
                .expect("offset -> u32"),
        );
        Ok(())
    }

    // Undo the most recent edit.
    pub fn undo(&mut self) -> Result<()> {
        let mut inner = self.0.borrow_mut();
        if let Some(edit) = inner.action_history.undo_stack.pop() {
            drop(inner);
            self.apply_selection(
                edit.start_line,
                edit.new_lines.len(),
                &edit.old_lines,
                &edit.selection_before,
            )?;
            let mut inner = self.0.borrow_mut();
            inner.action_history.redo_stack.push(edit);
            inner.action_history.last_time_ms = 0.0;
        }
        Ok(())
    }

    // Re-apply most recently undone edit
    pub fn redo(&mut self) -> Result<()> {
        let mut inner = self.0.borrow_mut();
        if let Some(edit) = inner.action_history.redo_stack.pop() {
            drop(inner);
            self.apply_selection(
                edit.start_line,
                edit.old_lines.len(),
                &edit.new_lines,
                &edit.selection_after,
            )?;
            let mut inner = self.0.borrow_mut();
            inner.action_history.undo_stack.push(edit);
            inner.action_history.last_time_ms = 0.0;
        }
        Ok(())
    }
}

pub struct UserTextIterator<'a> {
    editor: Ref<'a, TextType>,
    line_field_idx: usize,
}

impl<'a> Iterator for UserTextIterator<'a> {
    type Item = Ref<'a, str>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.line_field_idx == self.editor.len() * 3 {
            return None;
        }

        let (line_idx, field_idx) = (self.line_field_idx / 3, self.line_field_idx % 3);
        self.line_field_idx += 1;

        let line = Ref::map(Ref::clone(&self.editor), |x| &x[line_idx]);
        Some(Ref::map(line, |x| match field_idx {
            0 => x.instr().get(),
            1 => x.comment().get(),
            _ => "\n",
        }))
    }
}

pub struct ActiveTextIterator<'a> {
    editor: Ref<'a, TextType>,
    line_idx: usize,
    active_str_idx: usize,
}

impl<'a> Iterator for ActiveTextIterator<'a> {
    type Item = Ref<'a, str>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.line_idx < self.editor.len()
            && self.active_str_idx >= self.editor[self.line_idx].info().num_well_formed_strs()
        {
            self.line_idx += 1;
            self.active_str_idx = 0;
        }

        if self.line_idx == self.editor.len() {
            return None;
        }

        let ret = Some(Ref::map(Ref::clone(&self.editor), |x| {
            x[self.line_idx].well_formed_str(self.active_str_idx)
        }));

        self.active_str_idx += 1;

        ret
    }
}

impl LineInfos for Editor {
    fn is_empty(&self) -> bool {
        self.text().is_empty()
    }

    fn len(&self) -> usize {
        self.text().len()
    }

    fn info(&self, index: usize) -> impl Deref<Target = LineInfo> {
        Ref::map(self.line(index), |x| x.info())
    }
}

impl LineInfosMut for Editor {
    fn set_active_status(&mut self, index: usize, new_val: Activity) {
        self.line_mut(index).set_active_status(new_val)
    }

    fn set_synthetic_before(&mut self, index: usize, synth: SyntheticWasm) {
        self.line_mut(index).set_synthetic_before(synth);
        // self.image_mut().set_synthetic_before(index, synth);
    }

    fn push(&mut self) {
        self.push_line("");
    }

    fn set_invalid(&mut self, index: usize, reason: Option<String>) {
        self.line_mut(index).set_invalid(reason)
    }
}

impl FrameInfosMut for Editor {
    fn set_indent(&mut self, index: usize, num: usize) {
        if self.line_mut(index).set_indent(num) {
            self.0
                .borrow_mut()
                .component
                .set_attribute("class", "animated");
        }
    }

    fn set_frames(&mut self, frames: Vec<FrameInfo>) {
        let mut tagged_frames = HashMap::with_capacity(frames.len());
        for frame in frames {
            let id = self.line(frame.start).id();
            tagged_frames.insert(id, frame);
        }
        self.image_mut().set_frames(tagged_frames);
    }
}

impl WithElement for Editor {
    type Element = HtmlDivElement;
    fn with_element(&self, f: impl FnMut(&HtmlDivElement), g: AccessToken) {
        self.0.borrow().component.with_element(f, g);
    }
}

impl Component for Editor {
    fn audit(&self) {
        self.0.borrow().component.audit()
    }
}
