// The Codillon code editor

use crate::{
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
        find_frames, find_function_ranges, fix_syntax,
    },
    utils::{
        CodillonType, FmtError, RawModule, ValidModule, content_to_lines, lines_to_content,
        str_to_binary,
    },
};
use anyhow::{Context, Result, bail};
use std::{
    cell::{Ref, RefCell, RefMut},
    cmp::min,
    ops::Deref,
    rc::{Rc, Weak},
};
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{HtmlDivElement, HtmlInputElement, console::log_1};

type TextType = DomVec<CodeLine, HtmlDivElement>;
type Toolbar = DomStruct<
    (
        ElementHandle<HtmlInputElement>,
        (
            ElementHandle<HtmlInputElement>,
            (ElementHandle<HtmlInputElement>, ()),
        ),
    ),
    HtmlDivElement,
>;
type ComponentType = DomStruct<
    (
        Toolbar,
        (
            DomImage,
            (
                ReactiveComponent<TextType>,
                (ElementHandle<HtmlInputElement>, ()),
            ),
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

#[derive(Clone, Default)]
pub struct ProgramState {
    pub step_number: usize,
    pub line_number: i32,
    pub stack_state: Vec<WebAssemblyTypes>,
    pub locals_state: Vec<WebAssemblyTypes>,
    pub globals_state: Vec<WebAssemblyTypes>,
    pub memory_state: Vec<WebAssemblyTypes>,
}

struct _Editor {
    component: ComponentType,
    factory: ElementFactory,

    // Storing changes
    undo_stack: Vec<Edit>,
    redo_stack: Vec<Edit>,
    last_time_ms: f64,

    program_state: ProgramState,
    function_locals: Vec<Vec<WebAssemblyTypes>>,
    globals: Vec<WebAssemblyTypes>,
    saved_states: Vec<Option<ProgramState>>,
    function_ranges: Vec<(usize, usize)>,
}

pub struct Editor(Rc<RefCell<_Editor>>);

impl Clone for Editor {
    fn clone(&self) -> Self {
        Editor(Rc::clone(&self.0))
    }
}

impl Editor {
    pub fn new(factory: ElementFactory) -> Self {
        // Build toolbar elements
        let mut export_btn = factory.input();
        export_btn.set_attribute("type", "button");
        export_btn.set_attribute("value", "Export .wat");
        export_btn.set_attribute("class", "toolbar-button");

        let mut import_btn = factory.input();
        import_btn.set_attribute("type", "button");
        import_btn.set_attribute("value", "Import .wat");
        import_btn.set_attribute("class", "toolbar-button");

        let mut file_input = factory.input();
        file_input.set_attribute("type", "file");
        file_input.set_attribute("accept", ".wat,.wast,.txt");
        file_input.set_attribute("style", "display:none");

        let mut toolbar_div = factory.div();
        toolbar_div.set_attribute("class", "toolbar");

        let toolbar: Toolbar =
            DomStruct::new((export_btn, (import_btn, (file_input, ()))), toolbar_div);

        let inner = _Editor {
            component: DomStruct::new(
                (
                    toolbar,
                    (
                        DomImage::new(factory.clone()),
                        (
                            ReactiveComponent::new(DomVec::new(factory.div())),
                            ((factory.input()), ()),
                        ),
                    ),
                ),
                factory.div(),
            ),
            factory,
            undo_stack: Vec::new(),
            redo_stack: Vec::new(),
            last_time_ms: 0.0,
            program_state: ProgramState::default(),
            function_locals: Vec::new(),
            globals: Vec::new(),
            saved_states: Vec::new(),
            function_ranges: Vec::new(),
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
            let slider = &mut binding.component.get_mut().1.1.1.0;
            ret.setup_slider(Rc::downgrade(&ret.0), slider);
        }
        ret.image_mut().set_attribute("class", "annotations");

        // Wire toolbar handlers
        {
            let editor_ref = Rc::clone(&ret.0);
            let mut binding = ret.0.borrow_mut();
            let component = binding.component.get_mut();
            let toolbar = component.0.get_mut();

            let export_btn = &mut toolbar.0;
            let import_btn = &mut toolbar.1.0;
            let file_input = &mut toolbar.1.1.0;

            // Export button
            {
                let editor_ref = Rc::clone(&editor_ref);
                export_btn.set_onclick(move |_| {
                    Editor(editor_ref.clone()).export_wat();
                });
            }
            // Import button
            {
                let editor_ref = Rc::clone(&editor_ref);
                import_btn.set_onclick(move |_| {
                    let binding = editor_ref.borrow();
                    binding.component.get().0.get().1.1.0.click();
                });
            }
            // Import - handle file change
            {
                let editor_ref = Rc::clone(&editor_ref);
                file_input.set_onchange(move |event: web_sys::Event| {
                    handle_file_import(Rc::clone(&editor_ref), event);
                });
            }
        }

        // Restore from localStorage, or use default content
        let mut restored_ok = false;
        if let Some(content) = restore_from_local_storage()
            && !content.is_empty()
        {
            for line_str in content.lines() {
                ret.push_line(line_str);
            }
            if ret.on_change().is_ok() {
                restored_ok = true;
            } else {
                // Clear malformed restored content and fall back to default
                let len = ret.text().len();
                if len > 0 {
                    ret.text_mut().remove_range(0, len);
                }
            }
        }
        if !restored_ok {
            ret.push_default_lines();
            ret.on_change().expect("well-formed initial contents");
        }

        let height = LINE_SPACING * ret.text().len();
        ret.image_mut().set_attribute("height", &height.to_string());

        ret
    }

    fn push_line(&mut self, string: &str) {
        let newline = CodeLine::new(string, &self.0.borrow().factory);
        self.text_mut().push(newline);
    }

    fn push_default_lines(&mut self) {
        self.push_line("(func");
        self.push_line("i32.const 5");
        self.push_line("i32.const 6");
        self.push_line("i32.add");
        self.push_line("drop");
        self.push_line(")");
    }

    fn get_content(&self) -> String {
        let text = self.text();
        let len = text.len();
        let mut lines = Vec::with_capacity(len);
        for i in 0..len {
            lines.push(text[i].suffix(Position::begin()).unwrap_or_default());
        }
        lines_to_content(&lines)
    }

    fn load_content(&mut self, content: &str) {
        let len = self.text().len();
        if len > 0 {
            self.text_mut().remove_range(0, len);
        }
        {
            let mut inner = self.0.borrow_mut();
            inner.undo_stack.clear();
            inner.redo_stack.clear();
        }
        for line_str in content_to_lines(content) {
            self.push_line(line_str);
        }
        let _ = self.on_change();
        let height = LINE_SPACING * self.text().len();
        self.image_mut()
            .set_attribute("height", &height.to_string());
    }

    fn export_wat(&self) {
        let content = self.get_content();
        let arr = js_sys::Array::new();
        arr.push(&JsValue::from_str(&content));
        let opts = web_sys::BlobPropertyBag::new();
        opts.set_type("text/plain");
        let blob =
            web_sys::Blob::new_with_str_sequence_and_options(&arr, &opts).expect("Blob::new");
        let url = web_sys::Url::create_object_url_with_blob(&blob).expect("createObjectURL");

        let window = web_sys::window().expect("window");
        let document = window.document().expect("document");
        let a: web_sys::HtmlAnchorElement = document
            .create_element("a")
            .expect("create_element")
            .unchecked_into();
        a.set_href(&url);
        a.set_download("program.wat");
        let body = document.body().expect("body");
        body.append_child(&a).expect("append_child");
        a.click();
        body.remove_child(&a).expect("remove_child");
        web_sys::Url::revoke_object_url(&url).expect("revokeObjectURL");
    }

    fn save_to_local_storage(&self) {
        let content = self.get_content();
        let _ = web_sys::window()
            .and_then(|w| w.local_storage().ok().flatten())
            .map(|storage| storage.set_item("codillon_content", &content));
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

    // Accessors for the component and for a particular line of code
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

        // Get function ranges
        self.0.borrow_mut().function_ranges = find_function_ranges(self);

        let wasm_bin = str_to_binary(
            self.buffer_as_text()
                .fold(String::new(), |acc, elem| acc + "\n" + elem.as_ref()),
        )?;

        let raw_module = RawModule::new(self, &wasm_bin, &self.0.borrow().function_ranges)?;
        let validized = raw_module.fix_validity(&wasm_bin, self)?;
        let types = validized.to_types_table(&wasm_bin)?;

        let mut last_line_no = 0;
        for i in 0..validized.functions.len() {
            for (
                op,
                CodillonType {
                    inputs,
                    outputs,
                    input_arity,
                },
            ) in std::iter::zip(&validized.functions[i].operators, &types.functions[i].types)
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
        }

        for i in last_line_no..self.len() {
            self.line_mut(i).set_type_annotation(None);
        }

        // instrumentation
        self.initialize_globals(&validized);
        self.initialize_locals(&validized);
        self.execute(&validized.build_executable_binary(&types)?);

        self.save_to_local_storage();

        #[cfg(debug_assertions)]
        self.audit();

        Ok(())
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
                    .1
                    .0
                    .set_attribute("max", &(last_step() + 1).to_string());
                let step = inner.program_state.step_number;
                inner.program_state = ProgramState::default();
                inner.saved_states = Vec::new();
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
            RefMut::map(inner, |comp| &mut comp.component.get_mut().1.1.0);
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
            if let Some(cur_state) = saved_states.get(i)
                && let Some(program_state) = &cur_state
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
                inner.program_state = ProgramState::default();
            }
            inner.program_state.step_number
        };
        // Reconstruct up to slider_step
        self.build_program_state(program_step, slider_step);
        self.update_debug_panel(None);
    }

    fn valtype_default_editor(ty: &wasmparser::ValType) -> WebAssemblyTypes {
        match ty {
            wasmparser::ValType::I32 => WebAssemblyTypes::I32(0),
            wasmparser::ValType::I64 => WebAssemblyTypes::I64(0),
            wasmparser::ValType::F32 => WebAssemblyTypes::F32(0.0),
            wasmparser::ValType::F64 => WebAssemblyTypes::F64(0.0),
            wasmparser::ValType::V128 => WebAssemblyTypes::V128(0u128),
            _ => panic!("unsupported valtype for locals"),
        }
    }

    fn initialize_globals(&self, validized: &ValidModule) {
        use wasmparser::Operator::*;
        let mut globals: Vec<WebAssemblyTypes> = Vec::with_capacity(validized.globals.len());
        for global in &validized.globals {
            let mut init_reader = global.init_expr.get_operators_reader();
            if let Ok(op) = init_reader.read() {
                globals.push(match op {
                    I32Const { value } => WebAssemblyTypes::I32(value),
                    F32Const { value } => WebAssemblyTypes::F32(value.into()),
                    I64Const { value } => WebAssemblyTypes::I64(value),
                    F64Const { value } => WebAssemblyTypes::F64(value.into()),
                    V128Const { value } => WebAssemblyTypes::V128(value.into()),
                    _ => WebAssemblyTypes::I32(0),
                });
            }
        }
        self.0.borrow_mut().globals = globals;
    }

    fn initialize_locals(&self, validized: &ValidModule) {
        let mut function_locals: Vec<Vec<WebAssemblyTypes>> =
            Vec::with_capacity(validized.functions.len());
        for (func_idx, func) in validized.functions.iter().enumerate() {
            let mut locals: Vec<WebAssemblyTypes> =
                Vec::with_capacity(validized.get_func_type(func_idx).params().len());
            for param in validized.get_func_type(func_idx).params() {
                locals.push(Self::valtype_default_editor(param));
            }
            for (count, local_type) in &func.locals {
                for _ in 0..*count {
                    locals.push(Self::valtype_default_editor(local_type));
                }
            }
            function_locals.push(locals);
        }
        self.0.borrow_mut().function_locals = function_locals;
    }

    fn build_program_state(&self, start: usize, stop: usize) {
        let mut inner = self.0.borrow_mut();
        let line_count = inner.component.get().1.1.0.inner().len();
        inner.saved_states.resize_with(line_count, || None);
        if start == 0 {
            inner.program_state.globals_state = inner.globals.clone();
            if let Some((first_start, _first_end)) = inner.function_ranges.first().cloned() {
                inner.saved_states[first_start] = Some(inner.program_state.clone());
            }
        }
        with_changes(|changes| {
            for (i, change) in changes.enumerate().skip(start).take(stop - start) {
                inner.program_state.step_number = i + 1;
                inner.program_state.line_number = change.line_number;
                let line_num = change.line_number as usize;
                let func_idx = inner
                    .function_ranges
                    .iter()
                    .position(|(s, e)| line_num >= *s && line_num <= *e)
                    .expect("line inside function");
                let new_length = inner
                    .program_state
                    .stack_state
                    .len()
                    .saturating_sub(change.num_pops as usize);
                inner.program_state.stack_state.truncate(new_length);
                for push in &change.stack_pushes {
                    inner.program_state.stack_state.push(*push);
                }
                if let Some(updates) = &change.locals_change {
                    for (idx, val) in updates {
                        inner.function_locals[func_idx][*idx] = *val;
                    }
                }
                inner.program_state.locals_state = inner.function_locals[func_idx].clone();
                if let Some((idx, val)) = &change.globals_change {
                    inner.program_state.globals_state[*idx as usize] = *val;
                }
                if let Some((idx, val)) = &change.memory_change {
                    let idx_usize = *idx as usize;
                    let memory = &mut inner.program_state.memory_state;
                    if memory.len() <= idx_usize {
                        memory.resize(idx_usize + 1, WebAssemblyTypes::I32(0));
                    }
                    memory[idx_usize] = *val;
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

fn restore_from_local_storage() -> Option<String> {
    web_sys::window()?
        .local_storage()
        .ok()
        .flatten()?
        .get_item("codillon_content")
        .ok()
        .flatten()
}

fn handle_file_import(editor_ref: Rc<RefCell<_Editor>>, event: web_sys::Event) {
    let target = match event.target() {
        Some(t) => t,
        None => return,
    };
    let input: web_sys::HtmlInputElement = match target.dyn_into() {
        Ok(i) => i,
        Err(_) => return,
    };
    let files = match input.files() {
        Some(f) => f,
        None => return,
    };
    let file = match files.get(0) {
        Some(f) => f,
        None => return,
    };
    let reader = match web_sys::FileReader::new() {
        Ok(r) => r,
        Err(_) => return,
    };
    let _ = reader.read_as_text(&file);

    let editor_clone = Rc::clone(&editor_ref);
    let reader_clone = reader.clone();
    let onload = wasm_bindgen::closure::Closure::wrap(Box::new(move |_: web_sys::Event| {
        if let Ok(result) = reader_clone.result()
            && let Some(text) = result.as_string()
        {
            Editor(Rc::clone(&editor_clone)).load_content(&text);
        }
    }) as Box<dyn FnMut(web_sys::Event)>);
    reader.set_onload(Some(onload.as_ref().unchecked_ref()));
    onload.forget();

    // Reset input so the same file can be re-imported
    input.set_value("");
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
