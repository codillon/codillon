// The Codillon code editor

use crate::{
    action_history::{ActionHistory, Edit, Selection},
    autocomplete::Autocomplete,
    debug::{ExecutionState, TerminationType, run_binary, step_count, termination_type},
    dom_canvas::DomCanvas,
    dom_struct::DomStruct,
    dom_vec::DomVec,
    graphics::DomImage,
    jet::{
        AccessToken, Component, ControlHandlers, ElementFactory, InputEventHandle, NodeRef,
        RangeLike, ReactiveComponent, StorageHandle, WithElement, compare_document_position,
        get_selection, now_ms, set_selection_range,
    },
    line::{Activity, CodeLine, LineInfo, Position},
    save_status::SaveStatus,
    slider::Slider,
    syntax::{
        FrameInfo, FrameInfosMut, InstrKind, LineInfos, LineInfosMut, LineKind, SyntheticWasm,
        fix_syntax,
    },
    utils::{
        AnnotatedOperatorType, FmtError, OperatorType, RawModule, SlotConnections, SlotInfo,
        SlotUse, TypedModule, ValidModule, find_connections, indent_and_frame, str_to_binary,
    },
};
use anyhow::{Context, Result, bail};
use itertools::{Itertools, zip_eq};
use std::{
    cell::{Ref, RefCell, RefMut},
    cmp::min,
    collections::HashMap,
    ops::Deref,
    rc::Rc,
};
use wasm_bindgen::closure::Closure;
use web_sys::{BeforeUnloadEvent, HtmlDivElement, console::log_1};

type TextType = DomVec<CodeLine, HtmlDivElement>;
type ComponentType = DomStruct<
    (
        ReactiveComponent<Slider>,
        (
            DomImage,
            (
                ReactiveComponent<TextType>,
                (DomCanvas, (Autocomplete, (SaveStatus, ()))),
            ),
        ),
    ),
    HtmlDivElement,
>;

pub const LINE_SPACING: usize = 45;
const STORAGE_ID: &str = "codillon_content";
const SCHEDULE_STORE_MS: i32 = 500;
const RETRY_STORE_MS: i32 = 2000;

struct _Editor {
    component: ComponentType,
    factory: ElementFactory,
    action_history: ActionHistory,
    storage: Option<StorageHandle>,
    pending_binary: Option<(Vec<u8>, usize)>,
    previous_instrumented_binary_hash: u64,
    worker_running: bool,
    pending_save: Option<Closure<dyn Fn()>>,
    version: usize,
    slot_connections: SlotConnections,
    execution_state: ExecutionState,
}

pub struct Editor(Rc<RefCell<_Editor>>);

impl Editor {
    pub fn new(factory: ElementFactory) -> Self {
        let inner = _Editor {
            component: DomStruct::new(
                (
                    ReactiveComponent::new(Slider::new(factory.clone())),
                    (
                        DomImage::new(factory.clone()),
                        (
                            ReactiveComponent::new(DomVec::new(factory.div())),
                            (
                                DomCanvas::new(factory.canvas()),
                                (Autocomplete::new(&factory), (SaveStatus::new(&factory), ())),
                            ),
                        ),
                    ),
                ),
                factory.div(),
            ),
            factory,
            action_history: ActionHistory::default(),
            storage: StorageHandle::new(),
            pending_binary: None,
            previous_instrumented_binary_hash: 0,
            worker_running: false,
            pending_save: None,
            version: 0,
            slot_connections: SlotConnections::default(),
            execution_state: ExecutionState::default(),
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

            let editor_ref = Rc::clone(&ret.0);
            text.set_onmousedown(move |_| {
                Editor(editor_ref.clone()).autocomplete_mut().update("");
            });
        }
        {
            let editor_ref = Rc::clone(&ret.0);
            ret.autocomplete_mut().set_handler(move |accepted| {
                Editor(Rc::clone(&editor_ref))
                    .accept_autocomplete(accepted)
                    .expect("autocomplete handler")
            });

            let editor_ref = Rc::clone(&ret.0);
            ret.slider_mut().set_oninput(move |_| {
                let editor = Editor(editor_ref.clone());
                editor.update_live_info(step_count(), editor.current_step());
            });

            let editor = Editor(Rc::clone(&ret.0));
            let beforeunload = Closure::new(move |ev: BeforeUnloadEvent| {
                if editor.save_status().is_dirty() {
                    ev.prevent_default();
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
    fn replace_range(&mut self, target_range: &impl RangeLike, new_str: &str) -> Result<()> {
        if new_str.chars().any(|x| x.is_control() && x != '\n') {
            bail!("unhandled control char in input");
        }

        let saved_selection = self.get_lines_and_positions(&get_selection())?; // in case we need to revert

        let (start_line, start_pos, end_line, end_pos) =
            self.get_lines_and_positions(target_range)?;

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

            // Special case: if end line contributes 100% of the text, preserve its ID
            if end_pos == Position::begin() && start_pos == Position::begin() {
                let start_id = self.line(start_line).id();
                let end_id = self.line(end_line).id();
                self.line_mut(start_line).set_id(end_id);
                self.line_mut(end_line).set_id(start_id);
            }

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
                    let mut newline = CodeLine::new(&rest[1..], &self.0.borrow().factory);

                    // preserve identity in one special case
                    if pos == Position::begin() {
                        let old_id = self.line(fixup_line).id();
                        let new_id = newline.id();
                        self.line_mut(fixup_line).set_id(new_id);
                        newline.set_id(old_id);
                    }

                    new_cursor_pos = Position::begin();
                    fixup_line += 1;
                    self.text_mut().insert(fixup_line, newline);
                }
            }
        }

        // Is the new module well-formed and "validizable"? Otherwise, revert this entire change.
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
            "insertText" => self.replace_range(&target_range, &ev.data().context("no data")?),
            "insertFromPaste" => self.replace_range(
                &target_range,
                &ev.data_transfer()
                    .context("no data_transfer")?
                    .get_data("text/plain")
                    .fmt_err()?,
            ),
            "deleteContentBackward" | "deleteContentForward" | "deleteByCut" => {
                self.replace_range(&target_range, "")
            }
            "insertParagraph" | "insertLineBreak" => self.replace_range(&target_range, "\n"),
            _ => bail!(format!(
                "unhandled input type {}, data {:?}",
                ev.input_type(),
                ev.data()
            )),
        }?;

        self.show_autocomplete()?;

        Ok(())
    }

    // Keydown helpers. Firefox has trouble advancing to the next line if there is an ::after pseudo-element
    // later in the line. It also has trouble deleting if the cursor position is at the end of the surrounding
    // div, so try to prevent this. And it skips lines on ArrowLeft if the previous line is completely empty.
    fn handle_keydown(&mut self, ev: web_sys::KeyboardEvent) -> Result<()> {
        if ev.key().starts_with("Arrow") || ev.key() == "Escape" {
            self.autocomplete_mut().update("");
        }

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

            "z" | "Z" if ev.ctrl_key() || ev.meta_key() => {
                ev.prevent_default();
                if ev.shift_key() {
                    self.redo()?;
                } else {
                    self.undo()?;
                }
            }

            "Tab" => {
                let accepted = self.autocomplete().first_suggestion().map(String::from);
                if let Some(accepted) = accepted {
                    self.accept_autocomplete(&accepted)?;
                }
                ev.prevent_default();
            }
            _ => {}
        }

        Ok(())
    }

    fn accept_autocomplete(&mut self, accepted: &str) -> Result<()> {
        let selection = get_selection();
        if selection.is_collapsed() {
            let (_, pos) = self.find_idx_and_utf16_pos(
                selection.focus_node().context("focus")?,
                selection.focus_offset(),
            )?;

            if pos.in_instr && pos.offset < accepted.len() {
                Editor(Rc::clone(&self.0)).replace_range(&selection, &accepted[pos.offset..])?;
            }
        }
        self.autocomplete_mut().update("");
        Ok(())
    }

    fn show_autocomplete(&self) -> Result<()> {
        let selection = get_selection();
        if !selection.is_collapsed() {
            self.autocomplete_mut().update("");
            return Ok(());
        }

        let (line_idx, pos) = self.find_idx_and_utf16_pos(
            selection.focus_node().context("focus")?,
            selection.focus_offset(),
        )?;
        if !pos.in_instr {
            self.autocomplete_mut().update("");
            return Ok(());
        }

        let prefix = self.line(line_idx).instr().get()[..pos.offset].to_string();
        self.autocomplete_mut().update(&prefix);
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
        Ref::map(self.0.borrow(), |c| &c.component.get().1.1.0)
    }

    fn textbox_mut(&self) -> RefMut<'_, ReactiveComponent<TextType>> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.component.get_mut().1.1.0)
    }

    fn image_mut(&self) -> RefMut<'_, DomImage> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.component.get_mut().1.0)
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
        Ref::map(self.0.borrow(), |c| &c.component.get().0)
    }

    fn slider_mut(&self) -> RefMut<'_, ReactiveComponent<Slider>> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.component.get_mut().0)
    }

    fn autocomplete(&self) -> Ref<'_, Autocomplete> {
        Ref::map(self.0.borrow(), |c| &c.component.get().1.1.1.1.0)
    }

    fn autocomplete_mut(&self) -> RefMut<'_, Autocomplete> {
        RefMut::map(self.0.borrow_mut(), |c| {
            &mut c.component.get_mut().1.1.1.1.0
        })
    }

    fn save_status(&self) -> Ref<'_, SaveStatus> {
        Ref::map(self.0.borrow(), |c| &c.component.get().1.1.1.1.1.0)
    }

    fn save_status_mut(&self) -> RefMut<'_, SaveStatus> {
        RefMut::map(self.0.borrow_mut(), |c| {
            &mut c.component.get_mut().1.1.1.1.1.0
        })
    }

    fn slot_connections(&self) -> Ref<'_, SlotConnections> {
        Ref::map(self.0.borrow(), |c| &c.slot_connections)
    }

    fn slot_connections_mut(&mut self) -> RefMut<'_, SlotConnections> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.slot_connections)
    }

    fn execution_state_mut(&self) -> RefMut<'_, ExecutionState> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.execution_state)
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
        let raw_module = RawModule::new(self, &wasm_bin)?;
        let validized = raw_module.fix_validity(self, &wasm_bin)?;
        let types = validized.to_types_table(&wasm_bin)?;

        // indent operators and find frames
        indent_and_frame(self, &validized, &types);

        // update visual types of params, locals, and operators
        *self.slot_connections_mut() = find_connections(&validized, &types);
        self.update_displayed_types(&validized, &types)?;

        // instrumentation
        let instrumented_binary = &validized.build_instrumented_binary(&types)?;
        let instrumented_binary_hash = Self::hash_binary(instrumented_binary);
        if self.0.borrow().previous_instrumented_binary_hash != instrumented_binary_hash {
            self.0.borrow_mut().version += 1;
            let version = self.0.borrow().version;
            self.execute(instrumented_binary, version);
            self.0.borrow_mut().previous_instrumented_binary_hash = instrumented_binary_hash;
        } else {
            if step_count() > 0 {
                self.update_live_info(step_count(), self.current_step());
            } else {
                self.image_mut().set_arrow_location(None);
            }
        }

        self.schedule_save(); // schedule save to local storage

        #[cfg(debug_assertions)]
        self.audit();

        Ok(())
    }

    fn update_displayed_types<'a>(
        &mut self,
        validized: &ValidModule<'a>,
        types: &TypedModule,
    ) -> Result<()> {
        // set types
        let line_count = self.len();
        self.image_mut().set_type_count(line_count);
        let mut last_line_no = 0;
        // set types of globals
        for (global, global_type) in zip_eq(&validized.globals, &types.globals) {
            while last_line_no < global.line_idx {
                self.image_mut().clear_type(last_line_no);
                last_line_no += 1;
            }

            self.image_mut().set_type(
                global.line_idx,
                0,
                AnnotatedOperatorType {
                    inputs: vec![],
                    outputs: vec![SlotInfo {
                        slot: types.slots[global_type.0].clone(),
                        used: true,
                    }],
                },
            );
            last_line_no = global.line_idx + 1;
        }

        for (func, func_types) in zip_eq(&validized.functions, &types.funcs) {
            // set types of params
            let start_line = func.lines.0;
            while last_line_no < start_line {
                self.image_mut().clear_type(last_line_no);
                last_line_no += 1;
            }

            if !func_types.params.is_empty() {
                let param_types: Vec<Option<SlotInfo>> = func_types
                    .params
                    .iter()
                    .map(|SlotUse(x)| {
                        Some(SlotInfo {
                            slot: types.slots[*x].clone(),
                            used: true,
                        })
                    })
                    .collect();
                self.image_mut().set_type(
                    start_line,
                    0,
                    AnnotatedOperatorType {
                        inputs: param_types,
                        outputs: vec![],
                    },
                );
                last_line_no = start_line + 1;
            }

            // set types of locals
            {
                let mut local_line_idx: Option<usize> = None;
                let mut local_slots = vec![];
                for (local, SlotUse(slot_idx)) in zip_eq(&func.locals, &func_types.locals) {
                    match local_line_idx {
                        Some(line_idx) if line_idx == local.line_idx => local_slots.push(*slot_idx),
                        None => {
                            local_line_idx = Some(local.line_idx);
                            debug_assert!(local_slots.is_empty());
                            local_slots.push(*slot_idx);
                        }
                        Some(line_idx) => {
                            // flush
                            while last_line_no < line_idx {
                                self.image_mut().clear_type(last_line_no);
                                last_line_no += 1;
                            }

                            let indent = self.info(line_idx).indent.unwrap_or(0);
                            self.image_mut().set_type(
                                line_idx,
                                indent,
                                AnnotatedOperatorType {
                                    inputs: vec![],
                                    outputs: local_slots
                                        .iter()
                                        .map(|x| SlotInfo {
                                            slot: types.slots[*x].clone(),
                                            used: true,
                                        })
                                        .collect(),
                                },
                            );

                            local_line_idx = Some(local.line_idx);
                            local_slots.truncate(0);
                            local_slots.push(*slot_idx);

                            last_line_no = line_idx + 1;
                        }
                    }
                }
                // flush
                if let Some(local_line_idx) = local_line_idx {
                    while last_line_no < local_line_idx {
                        self.image_mut().clear_type(last_line_no);
                        last_line_no += 1;
                    }
                    let indent = self.info(local_line_idx).indent.unwrap_or(0);
                    self.image_mut().set_type(
                        local_line_idx,
                        indent,
                        AnnotatedOperatorType {
                            inputs: vec![],
                            outputs: local_slots
                                .iter()
                                .map(|x| SlotInfo {
                                    slot: types.slots[*x].clone(),
                                    used: true,
                                })
                                .collect(),
                        },
                    );
                    last_line_no = local_line_idx + 1;
                }
            }

            // set types of ops
            for (i, (op, OperatorType { inputs, outputs })) in
                zip_eq(&func.operators, &func_types.ops).enumerate()
            {
                while last_line_no < op.line_idx {
                    self.image_mut().clear_type(last_line_no);
                    last_line_no += 1;
                }
                let inputs: Vec<Option<SlotInfo>> = inputs
                    .iter()
                    .map(|x| {
                        x.as_ref().map(|SlotUse(y)| SlotInfo {
                            slot: types.slots[*y].clone(),
                            used: self.slot_connections().written[*y].is_some(),
                        })
                    })
                    .collect();
                let mut all_used = true;
                let outputs: Vec<SlotInfo> = outputs
                    .iter()
                    .map(|SlotUse(x)| SlotInfo {
                        slot: types.slots[*x].clone(),
                        used: {
                            let is_used = i + 1 == func.operators.len()
                                || self.slot_connections().read[*x].is_some();
                            all_used &= is_used;
                            is_used
                        },
                    })
                    .collect();

                if !all_used && self.info(op.line_idx).invalid.is_none() {
                    self.line_mut(op.line_idx).set_invalid(Some(String::new()));
                }

                let indent = self.info(op.line_idx).indent.unwrap_or(0);

                if !func_types.params.is_empty() && op.line_idx == start_line {
                    // special case: for an empty func, don't override the param types with the function end type
                    if !inputs.is_empty() {
                        bail!("unexpected non-empty end op on same line as function start");
                    }
                } else {
                    self.image_mut().set_type(
                        op.line_idx,
                        indent,
                        AnnotatedOperatorType { inputs, outputs },
                    );
                }

                last_line_no = op.line_idx + 1;
            }
        }
        for i in last_line_no..line_count {
            self.image_mut().clear_type(i);
        }

        let editor_ref = &mut *self.0.borrow_mut();
        editor_ref
            .component
            .get_mut()
            .1
            .0
            .set_connections(&editor_ref.slot_connections);
        Ok(())
    }

    fn schedule_save(&mut self) {
        self.save_status_mut().mark_dirty();
        if self.0.borrow().pending_save.is_some() {
            // A save or retry already in flight
            return;
        }
        self.attempt_save(SCHEDULE_STORE_MS);
    }

    fn attempt_save(&self, delay_ms: i32) {
        let editor_ref = Rc::clone(&self.0);
        let closure = Closure::new(move || {
            let mut editor = Editor(editor_ref.clone());
            let saved_version = editor.save_status().get_version();
            let content = editor.buffer_as_text().join("");
            let success = {
                let mut inner = editor_ref.borrow_mut();
                inner.pending_save = None;
                // Currently synchronous and atomic but could be asynchronous in the future
                inner
                    .storage
                    .as_ref()
                    .map(|s| s.try_set_item(STORAGE_ID, &content))
                    .unwrap_or(false)
            };
            editor
                .save_status_mut()
                .notify_save_result(success, saved_version);

            if success {
                // Schedule save again if version number was updated during save
                if editor.save_status().is_dirty() {
                    editor.schedule_save();
                }
            } else {
                // Retry if unsuccessful
                editor.attempt_save(RETRY_STORE_MS);
            }
        });
        StorageHandle::set_timeout_with_callback(&closure, delay_ms);
        self.0.borrow_mut().pending_save = Some(closure);
    }

    fn execute(&self, binary: &[u8], version: usize) {
        let inner = Rc::clone(&self.0);
        inner.borrow_mut().pending_binary = Some((binary.to_vec(), version));
        if inner.borrow().worker_running {
            return;
        }
        inner.borrow_mut().worker_running = true;
        wasm_bindgen_futures::spawn_local(async move {
            loop {
                let Some((binary, version)) = inner.borrow_mut().pending_binary.take() else {
                    break;
                };
                run_binary(&binary)
                    .await
                    .unwrap_or_else(|e| log_1(&format!("Codillon runtime error: {e}").into()));

                let editor_handle = Editor(Rc::clone(&inner));

                if version != editor_handle.0.borrow().version {
                    continue;
                }

                let mut slider_step = editor_handle.current_step();
                // Move slider to the end if binary changed and it's in the middle
                if step_count() == 0 {
                    editor_handle.slider_mut().inner_mut().hide();
                    editor_handle.image_mut().set_arrow_location(None);
                    return;
                }

                if editor_handle.slider().inner().is_visible() {
                    if slider_step != 0 {
                        slider_step = step_count() - 1;
                    }
                } else {
                    slider_step = step_count() - 1;
                }

                let slot_count = editor_handle.slot_connections().written.len();
                editor_handle.execution_state_mut().reset(slot_count);
                editor_handle.update_live_info(step_count(), slider_step);
                editor_handle.move_slider(slider_step);
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

    fn current_step(&self) -> usize {
        self.slider().inner().value_as_number().round() as usize
    }

    fn move_slider(&self, current_step: usize) {
        self.slider_mut()
            .inner_mut()
            .set_value_as_number(current_step as f64);
    }

    fn update_live_info(&self, step_count: usize, current_step: usize) {
        assert!(step_count > 0);
        if step_count > 1 {
            self.slider_mut().inner_mut().show();
            self.slider_mut().inner_mut().build_ticks(
                step_count - 1,
                current_step,
                &termination_type(),
            )
        } else {
            self.slider_mut().inner_mut().hide();
        }

        fn find_error(state: &ExecutionState) -> Option<(usize, &str)> {
            Some(match &state.status {
                Some((line_no, TerminationType::Error, Some(msg))) => (*line_no, msg),
                Some((line_no, TerminationType::HitBadImport, _)) => {
                    (*line_no, "called unknown import")
                }
                _ => return None,
            })
        }

        let editor_ref = &mut *self.0.borrow_mut();

        // clear runtime error
        if let Some((line_no, _)) = find_error(&editor_ref.execution_state) {
            editor_ref
                .component
                .get_mut()
                .1
                .1
                .0
                .inner_mut()
                .get_mut(line_no)
                .unwrap()
                .set_runtime_error(None);
        };

        // compute the state for the desired step
        editor_ref.execution_state.goto_step(current_step);
        Self::update_slots(editor_ref);
        if step_count > 1
            && let Some((line_no, _, _)) = &editor_ref.execution_state.status
            && let Some(indent) = &editor_ref.component.get().1.1.0.inner()[*line_no]
                .info()
                .indent
                .clone()
        {
            editor_ref
                .component
                .get_mut()
                .1
                .0
                .set_arrow_location(Some((*line_no, *indent as usize)));
        } else {
            editor_ref.component.get_mut().1.0.set_arrow_location(None);
        }

        // display runtime error (and HitBadImport)
        if let Some((line_no, msg)) = find_error(&editor_ref.execution_state) {
            editor_ref
                .component
                .get_mut()
                .1
                .1
                .0
                .inner_mut()
                .get_mut(line_no)
                .unwrap()
                .set_runtime_error(Some(msg.to_string()));
        }
    }

    fn update_slots(editor_ref: &mut _Editor) {
        for (slot_idx, value) in editor_ref.execution_state.slots.iter().enumerate() {
            let where_written = &editor_ref.slot_connections.written[slot_idx];
            if let Some(coord) = where_written {
                editor_ref
                    .component
                    .get_mut()
                    .1
                    .0
                    .set_slot_value(coord, false, value);
            }

            let where_read = &editor_ref.slot_connections.read[slot_idx];
            if let Some(coord) = where_read {
                editor_ref
                    .component
                    .get_mut()
                    .1
                    .0
                    .set_slot_value(coord, true, value);
            }
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

    fn set_runtime_error(&mut self, index: usize, msg: Option<String>) {
        self.line_mut(index).set_runtime_error(msg)
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
        let animated = self.0.borrow().component.get_attribute("class") == Some("animated");
        self.image_mut().set_frames(tagged_frames, animated);
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
