// The Codillon code editor

use crate::{
    autocomplete::Autocomplete,
    debug::{WebAssemblyTypes, last_step, make_imports, program_state_to_js, with_changes},
    dom_struct::DomStruct,
    dom_vec::DomVec,
    graphics::DomImage,
    jet::{
        AccessToken, Component, ControlHandlers, ElementFactory, ElementHandle, InputEventHandle,
        NodeRef, RangeLike, ReactiveComponent, StaticRangeHandle, WithElement,
        compare_document_position, get_selection, now_ms, set_selection_range,
    },
    line::{Activity, CodeLine, LineInfo, Position},
    syntax::{
        FrameInfo, FrameInfosMut, InstrKind, LineInfos, LineInfosMut, LineKind, SyntheticWasm,
        find_frames, find_function_end, fix_syntax,
    },
    utils::{Aligned, CodillonType, FmtError, RawFunction, RawModule, str_to_binary},
};
use anyhow::{Context, Result, bail};
use std::{
    cell::{Ref, RefCell, RefMut},
    cmp::min,
    ops::Deref,
    rc::{Rc, Weak},
};
use wasm_bindgen::{JsCast, JsValue};
use wasmparser::{Parser, Payload};
use web_sys::{HtmlDivElement, HtmlInputElement, console::log_1};

type TextType = DomVec<CodeLine, HtmlDivElement>;
type ComponentType = DomStruct<
    (
        DomImage,
        (
            ReactiveComponent<TextType>,
            (ElementHandle<HtmlInputElement>, (Autocomplete, ())),
        ),
    ),
    HtmlDivElement,
>;

pub const LINE_SPACING: usize = 40;
// Chrome uses 1 second and VSCode uses 300 - 500 ms, but for comparatively short
// WebAssembly lines, 150 ms felt more natural.
pub const GROUP_INTERVAL_MS: f64 = 150.0;
// 10 minutes - arbitrary
pub const KEEP_DURATION_MS: f64 = 10.0 * 60.0 * 1000.0;

#[derive(Clone)]
struct Selection {
    start_line: usize,
    start_pos: Position,
    end_line: usize,
    end_pos: Position,
}

#[derive(Clone)]
struct Edit {
    start_line: usize,
    old_lines: Vec<String>,
    // new lines
    new_lines: Vec<String>,
    selection_before: Selection,
    selection_after: Selection,
    time_ms: f64,
}

#[derive(Clone)]
pub struct ProgramState {
    pub step_number: usize,
    pub line_number: i32,
    pub stack_state: Vec<WebAssemblyTypes>,
    pub locals_state: Vec<WebAssemblyTypes>,
    pub globals_state: Vec<WebAssemblyTypes>,
    pub memory_state: Vec<WebAssemblyTypes>,
}

pub fn new_program_state() -> ProgramState {
    ProgramState {
        step_number: 0,
        line_number: 0,
        stack_state: Vec::new(),
        locals_state: Vec::new(),
        globals_state: Vec::new(),
        memory_state: Vec::new(),
    }
}

struct _Editor {
    component: ComponentType,
    factory: ElementFactory,

    // Storing changes
    undo_stack: Vec<Edit>,
    redo_stack: Vec<Edit>,
    last_time_ms: f64,

    program_state: ProgramState,
    saved_states: Vec<Option<ProgramState>>,
}

pub struct Editor(Rc<RefCell<_Editor>>);

impl Clone for Editor {
    fn clone(&self) -> Self {
        Editor(Rc::clone(&self.0))
    }
}

impl Editor {
    pub fn new(factory: ElementFactory) -> Self {
        let inner = _Editor {
            component: DomStruct::new(
                (
                    DomImage::new(factory.clone()),
                    (
                        ReactiveComponent::new(DomVec::new(factory.div())),
                        (factory.input(), (Autocomplete::new(factory.clone()), ())),
                    ),
                ),
                factory.div(),
            ),
            factory,
            undo_stack: Vec::new(),
            redo_stack: Vec::new(),
            last_time_ms: 0.0,
            program_state: new_program_state(),
            saved_states: vec![Some(new_program_state())],
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
            let mut binding = ret.0.borrow_mut();
            let slider = &mut binding.component.get_mut().1.1.0;
            ret.setup_slider(Rc::downgrade(&ret.0), slider);
        }
        ret.image_mut().set_attribute("class", "annotations");

        {
            let mut binding = ret.0.borrow_mut();
            let autocomplete = &mut binding.component.get_mut().1.1.1.0;
            let editor_weak = Rc::downgrade(&ret.0);
            autocomplete.set_on_select(move |s| {
                if let Some(editor_rc) = editor_weak.upgrade() {
                    let mut editor = Editor(editor_rc);
                    editor.apply_completion(s).expect("completion");
                }
            });
        }

        ret.push_line("(func");
        ret.push_line("i32.const 5");
        ret.push_line("i32.const 6");
        ret.push_line("i32.add");
        ret.push_line("drop");
        ret.push_line(")");

        ret.on_change().expect("well-formed initial contents");

        let height = LINE_SPACING * ret.text().len();
        ret.image_mut().set_attribute("height", &height.to_string());

        ret
    }

    fn push_line(&mut self, string: &str) {
        let newline = CodeLine::new(string, &self.0.borrow().factory);
        self.text_mut().push(newline);
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
        let (start_line, start_pos, end_line, end_pos) =
            self.get_lines_and_positions(&target_range)?;
        self.replace_range_impl(start_line, start_pos, end_line, end_pos, new_str)
    }

    fn replace_range_impl(
        &mut self,
        start_line: usize,
        start_pos: Position,
        end_line: usize,
        end_pos: Position,
        new_str: &str,
    ) -> Result<()> {
        if new_str.chars().any(|x| x.is_control() && x != '\n') {
            bail!("unhandled control char in input");
        }

        let saved_selection = self.get_lines_and_positions(&get_selection())?; // in case we need to revert

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

            // Bounce on attempts to add whitespace at the prefix of a line
            if start_pos == Position::begin() && new_str == " " {
                self.line_mut(start_line).shake();
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
            self.line_mut(start_line)
                .replace_range(start_pos, end_pos_in_line, &s)?;

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
                let time_ms = now_ms();
                self.store_edit(
                    Edit {
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
                        time_ms,
                    },
                    time_ms,
                );
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

        self.update_autocomplete()?;

        Ok(())
    }

    fn update_autocomplete(&mut self) -> Result<()> {
        let selection = get_selection();
        if !selection.is_collapsed() {
            self.autocomplete_mut().hide();
            return Ok(());
        }

        let (line_idx, pos, _, _) = match self.get_lines_and_positions(&selection) {
            Ok(x) => x,
            Err(_) => {
                self.autocomplete_mut().hide();
                return Ok(());
            }
        };

        if !pos.in_instr {
            self.autocomplete_mut().hide();
            return Ok(());
        }

        // Extract current_word as owned String before calling autocomplete
        let current_word: String = {
            let line = self.line(line_idx);
            let text = line.instr().get();

            // Calculate byte offset from utf16 offset
            let mut current_utf16 = 0;
            let mut byte_idx = 0;
            for c in text.chars() {
                if current_utf16 >= pos.offset {
                    break;
                }
                current_utf16 += c.len_utf16();
                byte_idx += c.len_utf8();
            }

            let prefix_area = &text[..byte_idx];
            let last_word_start = prefix_area
                .rfind(|c: char| !c.is_alphanumeric() && c != '.' && c != '_')
                .map(|i| i + 1)
                .unwrap_or(0);
            prefix_area[last_word_start..].to_string()
        }; // line borrow ends here

        if current_word.is_empty() {
            self.autocomplete_mut().hide();
        } else {
            // Calculate approximate position based on line index
            // Use fixed left margin and LINE_SPACING for vertical position
            let left = 120.0; // Left margin where instructions start
            let top = ((line_idx + 1) * LINE_SPACING) as f64 + 10.0; // Below current line

            let factory = self.0.borrow().factory.clone();
            self.autocomplete_mut()
                .filter(&current_word, &factory, left, top);
        }

        Ok(())
    }

    fn apply_completion(&mut self, completion: String) -> Result<()> {
        // Get position.
        let selection = get_selection();
        let (line_idx, pos, _, _) = self.get_lines_and_positions(&selection)?;

        let start_utf16 = {
            let line = self.line(line_idx);
            let text = line.instr().get();

            let mut current_utf16 = 0;
            let mut byte_idx = 0;
            for c in text.chars() {
                if current_utf16 >= pos.offset {
                    break;
                }
                current_utf16 += c.len_utf16();
                byte_idx += c.len_utf8();
            }
            let prefix_area = &text[..byte_idx];
            let last_word_start_byte = prefix_area
                .rfind(|c: char| !c.is_alphanumeric() && c != '.' && c != '_')
                .map(|i| i + 1)
                .unwrap_or(0);

            // Convert start byte to utf16 offset for replacement
            let start_chars = text[..last_word_start_byte].chars();
            start_chars.map(|c| c.len_utf16()).sum()
        };

        let range_start = Position {
            in_instr: true,
            offset: start_utf16,
        };
        let range_end = pos; // current cursor

        // Perform replacement
        self.replace_range_impl(line_idx, range_start, line_idx, range_end, &completion)?;
        self.autocomplete_mut().hide();
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
                if self.autocomplete().is_visible() {
                    ev.prevent_default();
                    self.autocomplete_mut().select_next();
                    return Ok(());
                }
                let selection = get_selection();
                if selection.is_collapsed() {
                    let (line_idx, _) = self.find_idx_and_utf16_pos(
                        selection.focus_node().context("focus")?,
                        selection.focus_offset(),
                    )?;
                    if line_idx + 1 == self.text().len() {
                        self.line(line_idx)
                            .set_cursor_position(self.line(line_idx).end_position());
                    }
                }
            }

            "ArrowUp" => {
                if self.autocomplete().is_visible() {
                    ev.prevent_default();
                    self.autocomplete_mut().select_prev();
                    return Ok(());
                }
            }

            "Tab" | "Enter" if self.autocomplete().is_visible() => {
                ev.prevent_default();
                let completion_opt = self.autocomplete().get_selected();
                if let Some(completion) = completion_opt {
                    self.apply_completion(completion)?;
                }
            }

            "Escape" => {
                if self.autocomplete().is_visible() {
                    ev.prevent_default();
                    self.autocomplete_mut().hide();
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

        if self.autocomplete().is_visible()
            && ev.key() != "ArrowDown"
            && ev.key() != "ArrowUp"
            && (ev.key() == "ArrowLeft" || ev.key() == "ArrowRight")
        {
            self.autocomplete_mut().hide();
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

    // Accessors for the component and for a particular line of code
    fn textbox(&self) -> Ref<'_, ReactiveComponent<TextType>> {
        Ref::map(self.0.borrow(), |c| &c.component.get().1.0)
    }

    fn textbox_mut(&self) -> RefMut<'_, ReactiveComponent<TextType>> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.component.get_mut().1.0)
    }

    fn image_mut(&self) -> RefMut<'_, DomImage> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.component.get_mut().0)
    }

    fn autocomplete(&self) -> Ref<'_, Autocomplete> {
        Ref::map(self.0.borrow(), |c| &c.component.get().1.1.1.0)
    }

    fn autocomplete_mut(&self) -> RefMut<'_, Autocomplete> {
        RefMut::map(self.0.borrow_mut(), |c| &mut c.component.get_mut().1.1.1.0)
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

    // get the "instructions" (active, well-formed lines) as text
    fn buffer_as_text(&self) -> impl Iterator<Item = Ref<'_, str>> {
        InstructionTextIterator {
            editor: self.text(),
            line_idx: 0,
            active_str_idx: 0,
        }
    }

    fn on_change(&mut self) -> Result<()> {
        // repair syntax
        fix_syntax(self);

        // find frames in the function
        find_frames(self);

        // find the end of the function, if there is one
        if let Some(function_end) = find_function_end(self) {
            let wasm_bin = str_to_binary(
                self.buffer_as_text()
                    .fold(String::new(), |acc, elem| acc + "\n" + elem.as_ref()),
            )?;

            let raw_module = self.to_raw_module(&wasm_bin, function_end)?;
            let validized = raw_module.fix_validity(&wasm_bin, self)?;
            let types = validized.to_types_table(&wasm_bin)?;

            let mut last_line_no = 0;

            for (
                op,
                CodillonType {
                    inputs,
                    outputs,
                    input_arity,
                },
            ) in std::iter::zip(&validized.functions[0].operators, &types.functions[0].types)
            {
                let mut type_str = String::new();

                if let Some(params) = input_arity {
                    type_str.push_str(&"? ".repeat(*params as usize));
                    if *params == 0u8 {
                        type_str.push_str("𝜖 ");
                    }
                } else {
                    for t in inputs {
                        type_str.push_str(&t.instr_type.to_string());
                        type_str.push(' ');
                    }
                    if inputs.is_empty() {
                        type_str.push_str("𝜖 ");
                    }
                }

                type_str.push('→');
                for t in outputs {
                    type_str.push(' ');
                    type_str.push_str(&t.to_string());
                }
                if outputs.is_empty() {
                    type_str.push_str(" 𝜖");
                }

                while last_line_no < op.line_idx {
                    self.line_mut(last_line_no).set_type_annotation(None);
                    last_line_no += 1;
                }
                last_line_no += 1;

                self.line_mut(op.line_idx)
                    .set_type_annotation(Some(&type_str));
            }

            for i in last_line_no..self.len() {
                self.line_mut(i).set_type_annotation(None);
            }

            // instrumentation
            self.execute(&validized.build_executable_binary(&types)?);
        }

        #[cfg(debug_assertions)]
        self.audit();

        Ok(())
    }

    /// note that the FuncEnd will also get a line in the Instruction Table for a given function
    fn to_raw_module<'a>(&self, wasm_bin: &'a [u8], function_end: usize) -> Result<RawModule<'a>> {
        let parser = Parser::new(0);
        let mut locals = Vec::new();
        let mut ops = Vec::new();

        for payload in parser.parse_all(wasm_bin) {
            if let Payload::CodeSectionEntry(body) = payload? {
                let mut local_reader = body.get_locals_reader()?.into_iter();
                for local in local_reader.by_ref() {
                    let entry = local?;
                    locals.push((entry.0, entry.1));
                }
                let mut op_reader = local_reader.into_operators_reader();
                while !op_reader.eof() {
                    ops.push(op_reader.read()?);
                }
                op_reader.finish()?;
            }
        }

        //include the function's end opcode

        //match each operator with its idx in the editor
        let mut aligned_ops = Vec::new();
        let mut ops_iter = ops.into_iter();

        for line_idx in 0..self.len() {
            for _ in 0..self.line(line_idx).num_ops() {
                let op = ops_iter.next().context("not enough operators")?;
                aligned_ops.push(Aligned { op, line_idx });
            }
        }

        match ops_iter.next() {
            Some(end @ wasmparser::Operator::End) => {
                aligned_ops.push(Aligned {
                    op: end,
                    line_idx: function_end,
                });
            }
            Some(_) => {
                bail!("not enough instructions");
            }
            None => {
                bail!("not enough operators");
            }
        }

        if ops_iter.next().is_some() {
            bail!("not enough instructions");
        }

        Ok(RawModule {
            functions: vec![RawFunction {
                locals,
                operators: aligned_ops,
            }],
        })
    }

    fn execute(&self, binary: &[u8]) {
        async fn run_binary(binary: &[u8]) -> Result<String> {
            use js_sys::{Function, Reflect};
            // Build import objects for the instrumented module.
            let imports = make_imports().fmt_err()?;
            let promise = js_sys::WebAssembly::instantiate_buffer(binary, &imports);
            let js_value = wasm_bindgen_futures::JsFuture::from(promise)
                .await
                .fmt_err()?;
            let instance = Reflect::get(&js_value, &JsValue::from_str("instance")).fmt_err()?;
            let exports = Reflect::get(&instance, &JsValue::from_str("exports")).fmt_err()?;
            let main = Reflect::get(&exports, &JsValue::from_str("main")).fmt_err()?;
            let main = wasm_bindgen::JsCast::dyn_ref::<Function>(&main)
                .context("main is not an exported function")?;
            let res = main.apply(&JsValue::null(), &js_sys::Array::new());
            res.map(|x| format!("{:?}", x)).fmt_err()
        }

        let binary = binary.to_vec();
        let editor_handle = Editor(Rc::clone(&self.0));
        wasm_bindgen_futures::spawn_local(async move {
            let _ = run_binary(&binary).await;
            // Update slider
            let step = {
                let mut inner = editor_handle.0.borrow_mut();
                inner
                    .component
                    .get_mut()
                    .1
                    .1
                    .0
                    .set_attribute("max", &(last_step() + 1).to_string());
                let step = inner.program_state.step_number;
                inner.program_state = new_program_state();
                inner.saved_states = vec![Some(new_program_state())];
                step
            };
            editor_handle.build_program_state(0, step);
            editor_handle.update_debug_panel(None);
        });
    }

    fn update_debug_panel(&self, error: Option<String>) {
        let inner = self.0.borrow_mut();
        let step = inner.program_state.step_number;
        let line_num = inner.program_state.line_number as usize;
        let saved_states = inner.saved_states.clone();
        let mut textentry: RefMut<ReactiveComponent<TextType>> =
            RefMut::map(inner, |comp| &mut comp.component.get_mut().1.0);
        let lines: &mut TextType = textentry.inner_mut();
        if let Some(message) = error {
            lines[0].set_debug_annotation(Some(&message));
            for i in 1..lines.len() {
                lines[i].set_debug_annotation(None);
                lines[i].set_highlight(false);
            }
            return;
        }
        for i in 0..lines.len() {
            lines[i].set_highlight(false);
            if let Some(program_state) = &saved_states[i]
                && program_state.step_number <= step
            {
                lines[i].set_debug_annotation(Some(&program_state_to_js(program_state)));
            } else {
                lines[i].set_debug_annotation(None);
            }
        }

        lines[line_num].set_highlight(true);
    }

    fn setup_slider(
        &self,
        editor: Weak<RefCell<_Editor>>,
        slider: &mut ElementHandle<HtmlInputElement>,
    ) {
        slider.set_attribute("type", "range");
        slider.set_attribute("min", "0");
        slider.set_attribute("value", "0");
        slider.set_attribute("class", "step-slider");

        // Slider closure for updating program state.
        slider.set_oninput(move |event: web_sys::Event| {
            if let Some(input) = event
                .target()
                .and_then(|t| t.dyn_into::<web_sys::HtmlInputElement>().ok())
            {
                let value = input.value().parse::<usize>().unwrap_or(0);
                if let Some(rc) = editor.upgrade() {
                    Editor(rc).slider_change(value);
                }
            }
        });
    }

    fn slider_change(&self, slider_step: usize) {
        let program_step = {
            let mut inner = self.0.borrow_mut();
            if inner.program_state.step_number == slider_step {
                return;
            }
            // For backwards steps, reset program state
            if slider_step < inner.program_state.step_number {
                inner.program_state = new_program_state();
            }
            inner.program_state.step_number
        };
        // Reconstruct up to slider_step
        self.build_program_state(program_step, slider_step);
        self.update_debug_panel(None);
    }

    fn build_program_state(&self, start: usize, stop: usize) {
        let mut inner = self.0.borrow_mut();
        let line_count = inner.component.get().1.0.inner().len();
        inner.saved_states.resize_with(line_count, || None);
        with_changes(|changes| {
            for (i, change) in changes.enumerate().skip(start).take(stop - start) {
                inner.program_state.step_number = i + 1;
                inner.program_state.line_number = change.line_number;
                let new_length = inner
                    .program_state
                    .stack_state
                    .len()
                    .saturating_sub(change.num_pops as usize);
                inner.program_state.stack_state.truncate(new_length);
                for push in &change.stack_pushes {
                    inner.program_state.stack_state.push(push.clone());
                }
                if let Some((idx, val)) = &change.locals_change {
                    let idx_usize = *idx as usize;
                    let locals = &mut inner.program_state.locals_state;
                    if locals.len() <= idx_usize {
                        locals.resize(idx_usize + 1, WebAssemblyTypes::I32(0));
                    }
                    locals[idx_usize] = val.clone();
                }
                // Resizing won't be necessary when number of globals are known
                if let Some((idx, val)) = &change.globals_change {
                    let idx_usize = *idx as usize;
                    let globals = &mut inner.program_state.globals_state;
                    if globals.len() <= idx_usize {
                        globals.resize(idx_usize + 1, WebAssemblyTypes::I32(0));
                    }
                    globals[idx_usize] = val.clone();
                }
                if let Some((idx, val)) = &change.memory_change {
                    let idx_usize = *idx as usize;
                    let memory = &mut inner.program_state.memory_state;
                    if memory.len() <= idx_usize {
                        memory.resize(idx_usize + 1, WebAssemblyTypes::I32(0));
                    }
                    memory[idx_usize] = val.clone();
                }
                inner.saved_states[change.line_number as usize] = Some(inner.program_state.clone());
            }
        });
    }

    fn store_edit(&mut self, edit: Edit, now_ms: f64) {
        let mut inner = self.0.borrow_mut();
        inner.redo_stack.clear();
        // Combine edit if close together
        if now_ms - inner.last_time_ms <= GROUP_INTERVAL_MS
            && let Some(last_edit) = inner.undo_stack.last_mut()
            && last_edit.start_line == edit.start_line
            && last_edit.new_lines.len() == 1
            && edit.new_lines.len() == 1
        {
            last_edit.new_lines = edit.new_lines;
            last_edit.selection_after = edit.selection_after;
            last_edit.time_ms = now_ms;
        } else {
            inner.undo_stack.push(edit);
        }
        // Remove old edits
        let num_discard = inner
            .undo_stack
            .iter()
            .take_while(|ed| (now_ms - ed.time_ms) > KEEP_DURATION_MS)
            .count();
        if num_discard > 0 {
            inner.undo_stack.drain(0..num_discard);
        }
        inner.last_time_ms = now_ms;
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
        if let Some(edit) = inner.undo_stack.pop() {
            drop(inner);
            self.apply_selection(
                edit.start_line,
                edit.new_lines.len(),
                &edit.old_lines,
                &edit.selection_before,
            )?;
            let mut inner = self.0.borrow_mut();
            inner.redo_stack.push(edit);
            inner.last_time_ms = 0.0;
        }
        Ok(())
    }

    // Re-apply most recently undone edit
    pub fn redo(&mut self) -> Result<()> {
        let mut inner = self.0.borrow_mut();
        if let Some(edit) = inner.redo_stack.pop() {
            drop(inner);
            self.apply_selection(
                edit.start_line,
                edit.old_lines.len(),
                &edit.new_lines,
                &edit.selection_after,
            )?;
            let mut inner = self.0.borrow_mut();
            inner.undo_stack.push(edit);
            inner.last_time_ms = 0.0;
        }
        Ok(())
    }
}

pub struct InstructionTextIterator<'a> {
    editor: Ref<'a, TextType>,
    line_idx: usize,
    active_str_idx: usize,
}

impl<'a> Iterator for InstructionTextIterator<'a> {
    type Item = Ref<'a, str>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.line_idx < self.editor.len()
            && self.active_str_idx >= self.editor[self.line_idx].num_well_formed_strs()
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

    fn set_frame_count(&mut self, count: usize) {
        self.image_mut().set_frame_count(count)
    }

    fn set_frame_info(&mut self, num: usize, frame: FrameInfo) {
        self.image_mut().set_frame(num, frame)
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
