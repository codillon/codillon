// The Codillon code editor

use crate::{
    action_history::{ActionHistory, Edit},
    autocomplete::Autocomplete,
    debug::{ExecutionState, TerminationType, run_binary, step_count, termination_type},
    dom_canvas::DomCanvas,
    dom_struct::DomStruct,
    dom_vec::DomVec,
    graphics::DomImage,
    jet::{
        AccessToken, Component, ControlHandlers, ElementFactory, InputEventHandle, NodeRef,
        RangeLike, ReactiveComponent, StorageHandle, WindowHandle, WithElement,
        compare_document_position, get_selection, now_ms, set_selection_range,
    },
    line::{Activity, CodeLine, LineInfo, Position, PositionRange},
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
use itertools::zip_eq;
use regex::Regex;
use std::{
    cell::{Ref, RefCell, RefMut},
    cmp::min,
    collections::HashMap,
    rc::{Rc, Weak},
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

macro_rules! get {
    ($comp:expr,$field:ident) => {
        &field!($field $comp.get())
    };
}

macro_rules! get_mut {
    ($comp:expr,$field:ident) => {
        &mut field!($field $comp.get_mut())
    };
}

macro_rules! field {
    (slider $comp:expr) => {
        $comp.0
    };
    (image $comp:expr) => {
        $comp.1.0
    };
    (textbox $comp:expr) => {
        $comp.1.1.0
    };
    (canvas $comp:expr) => {
        $comp.1.1.1.0
    };
    (autocomplete $comp:expr) => {
        $comp.1.1.1.1.0
    };
    (save_status $comp:expr) => {
        $comp.1.1.1.1.1.0
    };
}

pub const LINE_SPACING: usize = 45;
const STORAGE_ID: &str = "codillon_content";
const SCHEDULE_STORE_MS: i32 = 500;
const RETRY_STORE_MS: i32 = 2000;

struct Editor {
    component: ComponentType,
    factory: ElementFactory,
    action_history: ActionHistory,
    storage: Option<StorageHandle>,
    pending_binary: Option<(Vec<u8>, usize)>,
    previous_instrumented_binary_hash: u64,
    worker_running: bool,
    pending_save: Option<Closure<dyn Fn()>>,
    go_button_line: Option<usize>,
    version: usize,
    slot_connections: SlotConnections,
    execution_state: ExecutionState,
    scroll_on_next_input: bool,
    window: WindowHandle,
    holder: Weak<RefCell<Self>>,
    input_filter: Regex,
}

pub struct EditorHolder(Rc<RefCell<Editor>>);

impl EditorHolder {
    pub fn new(factory: ElementFactory) -> Result<Self> {
        let ret = Self(Rc::new(RefCell::new(Editor::new(factory)?)));
        ret.borrow_mut().init(Rc::downgrade(&ret.0));
        Ok(ret)
    }

    fn borrow(&self) -> Ref<'_, Editor> {
        self.0.borrow()
    }

    fn borrow_mut(&self) -> RefMut<'_, Editor> {
        self.0.borrow_mut()
    }
}

impl LineInfos for Editor {
    fn is_empty(&self) -> bool {
        self.text().is_empty()
    }

    fn len(&self) -> usize {
        self.text().len()
    }

    fn info(&self, index: usize) -> &LineInfo {
        self.line(index).info()
    }
}

impl LineInfosMut for Editor {
    fn set_active_status(&mut self, index: usize, new_val: Activity) {
        self.line_mut(index).set_active_status(new_val)
    }

    fn set_synthetic_before(&mut self, index: usize, synth: SyntheticWasm) {
        self.line_mut(index).set_synthetic_before(synth);
    }

    fn push(&mut self) {
        let newline = CodeLine::new("", &self.factory);
        self.text_mut().push(newline);
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
            self.component.set_attribute("class", "animated");
        }
    }

    fn set_frames(&mut self, frames: Vec<FrameInfo>) {
        let mut tagged_frames = HashMap::with_capacity(frames.len());
        for frame in frames {
            let id = self.line(frame.start).id();
            tagged_frames.insert(id, frame);
        }
        let animated = self.component.get_attribute("class") == Some("animated");
        self.image_mut().set_frames(tagged_frames, animated);
    }
}

impl WithElement for EditorHolder {
    type Element = HtmlDivElement;
    fn with_element(&self, f: impl FnMut(&HtmlDivElement), g: AccessToken) {
        self.0.borrow().component.with_element(f, g);
    }
}

impl Component for EditorHolder {
    #[cfg(debug_assertions)]
    fn audit(&self) {
        self.0.borrow().audit()
    }
}

impl Editor {
    #[cfg(debug_assertions)]
    fn audit(&self) {
        self.component.audit();
        self.window.audit();

        if self.component.elem().is_connected() {
            self.component.elem().assert_is_entire_body();
        }
    }

    fn textbox(&self) -> &ReactiveComponent<TextType> {
        get!(self.component, textbox)
    }

    fn textbox_mut(&mut self) -> &mut ReactiveComponent<TextType> {
        get_mut!(self.component, textbox)
    }

    fn image_mut(&mut self) -> &mut DomImage {
        get_mut!(self.component, image)
    }

    fn text(&self) -> &TextType {
        self.textbox().inner()
    }

    fn text_mut(&mut self) -> &mut TextType {
        self.textbox_mut().inner_mut()
    }

    fn line(&self, idx: usize) -> &CodeLine {
        &self.text()[idx]
    }

    fn line_mut(&mut self, idx: usize) -> &mut CodeLine {
        &mut self.text_mut()[idx]
    }

    fn slider(&self) -> &ReactiveComponent<Slider> {
        get!(self.component, slider)
    }

    fn slider_mut(&mut self) -> &mut ReactiveComponent<Slider> {
        get_mut!(self.component, slider)
    }

    fn autocomplete(&self) -> &Autocomplete {
        get!(self.component, autocomplete)
    }

    fn autocomplete_mut(&mut self) -> &mut Autocomplete {
        get_mut!(self.component, autocomplete)
    }

    fn save_status(&self) -> &SaveStatus {
        get!(self.component, save_status)
    }

    fn save_status_mut(&mut self) -> &mut SaveStatus {
        get_mut!(self.component, save_status)
    }

    fn slot_connections(&self) -> &SlotConnections {
        &self.slot_connections
    }

    fn slot_connections_mut(&mut self) -> &mut SlotConnections {
        &mut self.slot_connections
    }

    fn execution_state_mut(&mut self) -> &mut ExecutionState {
        &mut self.execution_state
    }

    fn update_live_info(
        &mut self,
        step_count: usize,
        current_step: Option<usize>,
        source_changed: bool,
    ) {
        assert!(step_count > 0);
        let current_step = current_step.unwrap_or(self.current_step());
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

        // clear runtime error
        if let Some((line_no, _)) = find_error(&self.execution_state) {
            self.line_mut(line_no).set_runtime_error(None);
        };

        // compute the state for the desired step
        self.execution_state
            .goto_step(current_step, get_mut!(self.component, canvas));

        for (slot_idx, value) in self.execution_state.slots.iter().enumerate() {
            let where_written = &self.slot_connections.written[slot_idx];
            if let Some(coord) = where_written {
                get_mut!(self.component, image).set_slot_value(coord, false, value);
            }

            let where_read = &self.slot_connections.read[slot_idx];
            if let Some(coord) = where_read {
                get_mut!(self.component, image).set_slot_value(coord, true, value);
            }
        }

        if step_count > 1
            && let Some((line_no, _, _)) = &self.execution_state.status
            && let Some(indent) = &self.text()[*line_no].info().indent.clone()
        {
            get_mut!(self.component, image)
                .set_arrow_location(!source_changed, Some((*line_no, *indent as usize)));
            if self.scroll_on_next_input {
                get_mut!(self.component, image).scroll_to_arrow();
                self.scroll_on_next_input = false;
            }
        } else {
            get_mut!(self.component, image).set_arrow_location(false, None);
        }

        // display runtime error (and HitBadImport)
        let Self {
            execution_state,
            component,
            ..
        } = self;
        if let Some((line_no, msg)) = find_error(execution_state) {
            get_mut!(component, textbox).inner_mut()[line_no]
                .set_runtime_error(Some(msg.to_string()));
        }
    }

    fn current_step(&self) -> usize {
        self.slider().inner().value_as_number().round() as usize
    }

    fn execute(&mut self, binary: &[u8], version: usize) {
        self.pending_binary = Some((binary.to_vec(), version));
        if self.worker_running {
            return;
        }
        self.worker_running = true;

        let holder = self.holder();

        wasm_bindgen_futures::spawn_local(async move {
            loop {
                let Some((binary, version)) = holder.borrow_mut().pending_binary.take() else {
                    break;
                };
                run_binary(&binary)
                    .await
                    .unwrap_or_else(|e| log_1(&format!("Codillon runtime error: {e}").into()));

                let mut ed = holder.borrow_mut();
                if version != ed.version {
                    continue;
                }

                let mut slider_step = ed.current_step();
                // Move slider to the end if binary changed and it's in the middle
                if step_count() == 0 {
                    ed.slider_mut().inner_mut().hide();
                    ed.image_mut().set_arrow_location(false, None);
                    break;
                }

                if ed.slider().inner().is_visible() {
                    if slider_step != 0 {
                        slider_step = step_count() - 1;
                    }
                } else {
                    slider_step = step_count() - 1;
                }

                let slot_count = ed.slot_connections().written.len();
                ed.execution_state_mut().reset(slot_count);
                ed.update_live_info(step_count(), Some(slider_step), true);
                ed.slider_mut()
                    .inner_mut()
                    .set_value_as_number(slider_step as f64);
            }
            holder.borrow_mut().worker_running = false;
        });
    }

    fn get_lines_and_positions(&self, range: &impl RangeLike) -> Result<PositionRange> {
        let (start_line, start_pos) =
            self.find_idx_and_utf16_pos(range.node1().unwrap(), range.offset1())?;

        let (end_line, end_pos) =
            self.find_idx_and_utf16_pos(range.node2().unwrap(), range.offset2())?;

        Ok(PositionRange {
            start_line,
            start_pos,
            end_line,
            end_pos,
        })
    }

    // Replace a given range (currently within a single line) with new text
    fn replace_range(&mut self, target_range: &impl RangeLike, new_str: &str) -> Result<()> {
        if new_str.chars().any(|x| x.is_control() && x != '\n') {
            bail!("unhandled control char in input");
        }

        let saved_selection = self.get_lines_and_positions(&get_selection())?; // in case we need to revert

        let PositionRange {
            start_line,
            start_pos,
            end_line,
            end_pos,
        } = self.get_lines_and_positions(target_range)?;

        let mut backup = Vec::new();
        for i in start_line..end_line + 1 {
            backup.push(self.line(i).suffix(Position::begin())?);
        }

        // disable animations; will be re-enabled if any indentation changes
        self.component.remove_attribute("class");

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
                    let matching_end = CodeLine::new("end", &self.factory);
                    self.text_mut().insert(start_line + 1, matching_end);
                    self.line_mut(start_line + 1).reveal();
                }
            } else if was_structured && !is_structured {
                // Should we delete an unnecessary subsequent `end` (because of a removed structured instr)?
                if self.text().len() > start_line + 1
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
                    let mut newline = CodeLine::new(&rest[1..], &self.factory);

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
                    self.action_history.store_edit(Edit {
                        start_line,
                        old_lines: backup,
                        new_lines,
                        selection_before: saved_selection,
                        selection_after: PositionRange {
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
                    let mut line = CodeLine::new(contents, &self.factory);
                    line.shake();
                    self.text_mut().insert(start_line + i, line);
                }

                self.on_change().expect("well-formed after restore");

                // restore selection
                let start_line = self.line(saved_selection.start_line);
                let new_start_node = start_line.position_to_node(saved_selection.start_pos);
                let end_line = self.line(saved_selection.end_line);
                let new_end_node = end_line.position_to_node(saved_selection.end_pos);

                set_selection_range(
                    new_start_node,
                    saved_selection.start_pos.offset.try_into().unwrap(),
                    new_end_node,
                    saved_selection.end_pos.offset.try_into().unwrap(),
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
                &self.input_filter.replace_all(
                    &ev.data_transfer()
                        .context("no data transfer")?
                        .get_data("text/plain")
                        .fmt_err()?,
                    "",
                ),
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

    // Given a node and offset, find the line index and (UTF-16) position within that line.
    // There are several possibilities for the node (e.g. the div element, the span element,
    // or the text node).
    fn find_idx_and_utf16_pos(&self, node: NodeRef, offset: u32) -> Result<(usize, Position)> {
        let offset = offset as usize;

        // If the position is "in" the div element, make sure the offset matches expectations
        // (either 0 for the very beginning, or #lines for the very end).
        if node.is_same_node(self.text()) {
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

    fn on_change(&mut self) -> Result<()> {
        // repair syntax
        fix_syntax(self);

        let wasm_bin = str_to_binary(self.active_as_text())?;
        let raw_module = RawModule::new(self, &wasm_bin)?;
        let (validized, go_button_line_idx) = raw_module.fix_validity(self, &wasm_bin)?;
        let types = validized.to_types_table(&wasm_bin)?;

        // set run button for "go" handler function
        self.update_go_button(go_button_line_idx);

        // indent operators and find frames
        indent_and_frame(self, &validized, &types);

        // update visual types of params, locals, and operators
        self.scroll_on_next_input = false; // edit to text buffer -> don't scroll to arrow
        *self.slot_connections_mut() = find_connections(&validized, &types);
        self.update_displayed_types(&validized, &types)?;

        // instrumentation
        let instrumented_binary = &validized.build_instrumented_binary(&types)?;
        let instrumented_binary_hash = Self::hash_binary(instrumented_binary);
        if self.previous_instrumented_binary_hash != instrumented_binary_hash {
            self.version += 1;
            self.execute(instrumented_binary, self.version);
            self.previous_instrumented_binary_hash = instrumented_binary_hash;
        } else {
            if step_count() > 0 {
                self.update_live_info(step_count(), None, false);
            } else {
                self.image_mut().set_arrow_location(false, None);
            }
        }

        self.schedule_save(); // schedule save to local storage

        #[cfg(debug_assertions)]
        self.audit();

        Ok(())
    }

    // get the user-entered text buffer (doesn't include synthetic Wasm)
    fn buffer_as_text(&self) -> String {
        let mut ret = String::new();
        for line in self.text().iter() {
            ret.push_str(line.instr().get());
            ret.push_str(line.comment().get());
            ret.push('\n');
        }
        ret
    }

    // get the active, well-formed line contents
    fn active_as_text(&self) -> String {
        let mut ret = String::new();
        for line in self.text().iter() {
            for i in 0..line.info().num_well_formed_strs() {
                ret.push_str(line.well_formed_str(i));
                ret.push(' ');
            }
        }
        ret
    }

    fn update_displayed_types<'a>(
        &mut self,
        validized: &ValidModule<'a>,
        types: &TypedModule,
    ) -> Result<()> {
        // set types
        let line_count = self.text().len();
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

                            let indent = self.line(line_idx).info().indent.unwrap_or(0);
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
                    let indent = self.line(local_line_idx).info().indent.unwrap_or(0);
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

                if !all_used && self.line(op.line_idx).info().invalid.is_none() {
                    self.line_mut(op.line_idx).set_invalid(Some(String::new()));
                }

                let indent = self.line(op.line_idx).info().indent.unwrap_or(0);

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

        get_mut!(self.component, image).set_connections(&self.slot_connections);
        Ok(())
    }

    fn update_go_button(&mut self, new_button_line: Option<usize>) {
        let prev_button_line = self.go_button_line;
        if prev_button_line == new_button_line {
            return;
        }
        if let Some(idx) = prev_button_line {
            self.line_mut(idx).remove_run_button_closure();
        }
        if let Some(idx) = new_button_line {
            self.line_mut(idx).set_run_button_closure();
        }
        self.go_button_line = new_button_line;
    }

    fn schedule_save(&mut self) {
        self.save_status_mut().mark_dirty();
        if self.pending_save.is_some() {
            // A save or retry already in flight
            return;
        }
        self.attempt_save(SCHEDULE_STORE_MS);
    }

    fn attempt_save(&mut self, delay_ms: i32) {
        let holder = Rc::clone(&self.holder.upgrade().unwrap());
        let closure = Closure::new(move || {
            let mut editor = holder.borrow_mut();
            let saved_version = editor.save_status().get_version();
            let content = editor.buffer_as_text();
            let success = {
                editor.pending_save = None;
                // Currently synchronous and atomic but could be asynchronous in the future
                editor
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
        self.window.set_timeout_with_callback(&closure, delay_ms);
        self.pending_save = Some(closure);
    }

    fn hash_binary(binary: &[u8]) -> u64 {
        use std::hash::{DefaultHasher, Hash, Hasher};
        let mut hasher = DefaultHasher::new();
        binary.hash(&mut hasher);
        hasher.finish()
    }

    fn apply_selection(
        &mut self,
        start_line: usize,
        remove_len: usize,
        insert_lines: &[String],
        selection_after: &PositionRange,
    ) -> Result<()> {
        let mut document_length = self.text().len();
        if start_line > document_length {
            bail!("start_line {start_line} out of range");
        }
        // Removing ending
        let end = min(
            start_line
                .checked_add(remove_len)
                .unwrap_or(document_length),
            self.text().len(),
        );
        if end > start_line {
            self.text_mut().remove_range(start_line, end);
        }
        // Add new lines
        for (index, value) in insert_lines.iter().enumerate() {
            let newline = CodeLine::new(value, &self.factory);
            self.text_mut().insert(start_line + index, newline);
        }
        self.on_change()?;
        document_length = self.text().len().saturating_sub(1);
        let start_index = min(selection_after.start_line, document_length);
        let end_index = min(selection_after.end_line, document_length);
        // Restore caret position
        set_selection_range(
            self.line(start_index)
                .position_to_node(selection_after.start_pos),
            selection_after.start_pos.offset.try_into().unwrap(),
            self.line(end_index)
                .position_to_node(selection_after.end_pos),
            selection_after.end_pos.offset.try_into().unwrap(),
        );
        Ok(())
    }

    // Undo the most recent edit.
    pub fn undo(&mut self) -> Result<()> {
        if let Some(edit) = self.action_history.undo_stack.pop() {
            self.apply_selection(
                edit.start_line,
                edit.new_lines.len(),
                &edit.old_lines,
                &edit.selection_before,
            )?;
            self.action_history.redo_stack.push(edit);
            self.action_history.last_time_ms = 0.0;
        }
        Ok(())
    }

    // Re-apply most recently undone edit
    pub fn redo(&mut self) -> Result<()> {
        if let Some(edit) = self.action_history.redo_stack.pop() {
            self.apply_selection(
                edit.start_line,
                edit.old_lines.len(),
                &edit.new_lines,
                &edit.selection_after,
            )?;
            self.action_history.undo_stack.push(edit);
            self.action_history.last_time_ms = 0.0;
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
                self.replace_range(&selection, &accepted[pos.offset..])?;
            }
        }
        self.autocomplete_mut().update("");
        Ok(())
    }

    fn show_autocomplete(&mut self) -> Result<()> {
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

    fn new(factory: ElementFactory) -> Result<Self> {
        let mut ret = Self {
            component: DomStruct::new(
                (
                    ReactiveComponent::new(Slider::new(factory.clone())),
                    (
                        DomImage::new(factory.clone()),
                        (
                            ReactiveComponent::new(DomVec::new(factory.div())),
                            (
                                DomCanvas::new(factory.canvas())?,
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
            go_button_line: None,
            version: 0,
            slot_connections: SlotConnections::default(),
            execution_state: ExecutionState::default(),
            scroll_on_next_input: false,
            window: WindowHandle::default(),
            holder: Weak::new(),
            input_filter: Regex::new("\r")?,
        };

        let text = ret.textbox_mut();
        text.inner_mut().set_attribute("class", "textentry");
        text.inner_mut().set_attribute(
            "style",
            &format!("--codillon-line-spacing: {}px;", LINE_SPACING),
        );
        text.inner_mut().set_attribute("contenteditable", "true");
        text.inner_mut().set_attribute("spellcheck", "false");

        ret.image_mut().set_attribute("class", "annotations");

        Ok(ret)
    }

    fn holder(&self) -> EditorHolder {
        EditorHolder(self.holder.upgrade().unwrap())
    }

    fn init(&mut self, holder: Weak<RefCell<Editor>>) {
        self.holder = holder;

        let e = self.holder();
        self.textbox_mut()
            .set_onbeforeinput(move |ev| e.borrow_mut().handle_input(ev).unwrap());

        let e = self.holder();
        self.textbox_mut()
            .set_onkeydown(move |ev| e.borrow_mut().handle_keydown(ev).unwrap());

        let e = self.holder();
        self.textbox_mut()
            .set_onmousedown(move |_| e.borrow_mut().autocomplete_mut().update(""));

        let e = self.holder();
        self.autocomplete_mut()
            .set_handler(move |accepted| e.borrow_mut().accept_autocomplete(accepted).unwrap());

        let e = self.holder();
        self.slider_mut()
            .set_oninput(move |_| e.borrow_mut().update_live_info(step_count(), None, false));

        let e = self.holder();
        self.slider_mut()
            .set_onkeydown(move |_| e.borrow_mut().scroll_on_next_input = true);

        let e = self.holder();
        self.window
            .set_onbeforeunload(move |ev: BeforeUnloadEvent| {
                if e.borrow().save_status().is_dirty() {
                    ev.prevent_default();
                }
            });

        // Restore from localStorage, or use default content
        if let Some(storage) = StorageHandle::new()
            && let Some(content) = storage.get_item(STORAGE_ID)
        {
            for line_str in content.lines() {
                self.push_line(line_str);
            }
            if self.on_change().is_err() {
                self.set_default_contents()
            };
        } else {
            self.set_default_contents();
        }

        self.on_change().expect("well-formed initial contents");

        let height = LINE_SPACING * self.text().len();
        self.image_mut()
            .set_attribute("height", &height.to_string());
    }

    fn push_line(&mut self, string: &str) {
        let newline = CodeLine::new(string, &self.factory);
        self.text_mut().push(newline);
    }

    fn set_default_contents(&mut self) {
        let len = self.text().len();
        self.text_mut().remove_range(0, len);
        self.push_line("(func");
        self.push_line("i32.const 5");
        self.push_line("i32.const 6");
        self.push_line("i32.add");
        self.push_line("drop");
        self.push_line(")");
    }
}
