// The Codillon code editor

use crate::{
    dom_struct::DomStruct,
    dom_vec::DomVec,
    graphics::DomImage,
    jet::{
        AccessToken, Component, ControlHandlers, ElementFactory, InputEventHandle, NodeRef,
        RangeLike, ReactiveComponent, StaticRangeHandle, WithElement, compare_document_position,
        get_selection, set_selection_range,
    },
    line::{CodeLine, LineInfo, Position},
    syntax::{InstrKind, LineKind, SyntheticWasm, find_frames, fix_syntax},
    utils::{
        CodillonInstruction, FmtError, FrameInfo, FrameInfosMut, InstructionTable, LineInfos,
        LineInfosMut, str_to_binary,
    },
};
use anyhow::{Context, Result, bail};
use std::{
    cell::{Ref, RefCell, RefMut},
    ops::Deref,
    rc::Rc,
};
use wasmparser::{Parser, Payload};
use web_sys::{HtmlDivElement, console::log_1};

type TextType = DomVec<CodeLine, HtmlDivElement>;
type ComponentType = DomStruct<(DomImage, (ReactiveComponent<TextType>, ())), HtmlDivElement>;

pub const LINE_SPACING: usize = 40;

struct _Editor {
    component: ComponentType,
    factory: ElementFactory,
}

pub struct Editor(Rc<RefCell<_Editor>>);

impl Editor {
    pub fn new(factory: ElementFactory) -> Self {
        let inner = _Editor {
            component: DomStruct::new(
                (
                    DomImage::new(factory.clone()),
                    (ReactiveComponent::new(DomVec::new(factory.div())), ()),
                ),
                factory.div(),
            ),
            factory,
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

        ret.image_mut().set_attribute("class", "annotations");

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
                        && !self.line(i).info().active
                    {
                        need_end = false;
                    }
                    if is_if
                        && self.line(i).info().kind == LineKind::Instr(InstrKind::Else)
                        && !self.line(i).info().active
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
                    && self.line(start_line + 1).info().active
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
            Ok(()) => self.line(fixup_line).set_cursor_position(new_cursor_pos),
            Err(_) => {
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

        // create binary
        let text = self
            .buffer_as_text()
            .fold(String::new(), |acc, elem| acc + "\n" + elem.as_ref());
        let wasm_bin = str_to_binary(text)?;

        //create Instruction Table
        let instruction_table = self.to_instruction_table(&wasm_bin)?;

        //annotation and instrumentation
        // For now, don't return Err even if `to_types_table` fails.
        // Otherwise, keystrokes that create well-formed-but-invalid
        // modules will be rejected and cause the "shake" animation.
        let _types_table = instruction_table.to_types_table(&wasm_bin);
        if let Err(e) = _types_table {
            log_1(&format!("Generating types table failed: {e}").into());
        }
        Self::execute(&instruction_table.build_executable_binary()?);

        // find frames in the function
        find_frames(self);

        #[cfg(debug_assertions)]
        {
            self.audit();
            log_1(&"successful audit".into());
        }

        Ok(())
    }

    /// TODO: support multi-function by returning a "Function Table" (a vector of instruction tables)
    /// note that the FuncEnd will also get a line in the Instruction Table for a given function
    fn to_instruction_table<'a>(&self, wasm_bin: &'a [u8]) -> Result<InstructionTable<'a>> {
        let parser = Parser::new(0);
        let mut ops = Vec::new();

        for payload in parser.parse_all(wasm_bin) {
            if let Payload::CodeSectionEntry(body) = payload? {
                for op in body.get_operators_reader()?.into_iter() {
                    ops.push(op?);
                }
            }
        }

        //pop the function's end operator off the Vec<Operators>
        ops.pop();

        //match each operator with its idx in the editor
        let mut instruction_table = Vec::new();
        let mut ops_iter = ops.into_iter();

        for line_idx in 0..self.len() {
            for _ in 0..self.line(line_idx).num_ops() {
                let op = ops_iter.next().context("not enough operators")?;
                instruction_table.push(CodillonInstruction { op, line_idx });
            }
        }

        if ops_iter.next().is_some() {
            bail!("not enough instructions");
        }

        Ok(InstructionTable {
            table: instruction_table,
        })
    }

    fn execute(binary: &[u8]) {
        async fn run_binary(binary: &[u8]) -> Result<String, wasm_bindgen::JsValue> {
            use js_sys::{Function, Reflect};
            use wasm_bindgen::JsValue;
            let promise = js_sys::WebAssembly::instantiate_buffer(binary, &js_sys::Object::new());
            let js_value = wasm_bindgen_futures::JsFuture::from(promise).await?;
            let instance = Reflect::get(&js_value, &JsValue::from_str("instance"))
                .map_err(|_| "failed to get instance")?;
            let exports = Reflect::get(&instance, &JsValue::from_str("exports"))
                .map_err(|_| "failed to get exports")?;
            let main = Reflect::get(&exports, &JsValue::from_str("main"))
                .map_err(|_| "failed to get main function")?;
            let main = wasm_bindgen::JsCast::dyn_ref::<Function>(&main)
                .ok_or("main is not an exported function")?;
            let res = main.apply(&JsValue::null(), &js_sys::Array::new())?;
            let string = js_sys::JSON::stringify(&res)?;
            let string = string
                .as_string()
                .ok_or(JsValue::from("stringify did not return string"))?;

            Ok(string)
        }

        let binary = binary.to_vec();
        wasm_bindgen_futures::spawn_local(async move {
            match run_binary(&binary).await {
                Ok(v) => web_sys::console::log_1(&format!("Ran successfully: {}", v).into()),
                Err(e) => web_sys::console::log_1(&e),
            }
        });
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
    fn set_active_status(&mut self, index: usize, is_active: bool) {
        self.line_mut(index).set_active_status(is_active)
    }

    fn set_synthetic_before(&mut self, index: usize, synth: SyntheticWasm) {
        self.line_mut(index).set_synthetic_before(synth);
        // self.image_mut().set_synthetic_before(index, synth);
    }

    fn push(&mut self) {
        self.push_line("");
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
