// The Codillon code editor

use crate::{
    dom_vec::DomVec,
    jet::{
        AccessToken, Component, ElementFactory, InputEventHandle, NodeRef, RangeLike,
        StaticRangeHandle, WithElement, compare_document_position, get_selection,
        set_selection_range,
    },
    line::{CodeLine, LineInfo, Position},
    utils::{
        FmtError, LineInfos, LineInfosMut, OkModule, collect_operands, fix_frames, str_to_binary,
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
    module: OkModule,
    synthetic_ends: usize,
    component: ComponentType,
    factory: ElementFactory,
}

pub struct Editor(Rc<RefCell<_Editor>>);

impl Editor {
    pub fn new(factory: ElementFactory) -> Self {
        let mut inner = _Editor {
            module: OkModule::default(),
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
            Editor(editor_ref.clone())
                .handle_input(ev)
                .expect("input handler")
        });

        let editor_ref = Rc::clone(&ret.0);
        ret.component_mut().set_onkeydown(move |ev| {
            Editor(editor_ref.clone())
                .handle_keydown(ev)
                .expect("keydown handler")
        });

        ret.push_line("i32.const 5");
        ret.push_line("i32.const 6");
        ret.push_line("i32.add");
        ret.push_line("drop");

        ret
    }

    fn push_line(&mut self, string: &str) {
        let newline = CodeLine::new(string, &self.0.borrow().factory);
        self.component_mut().push(newline);
        self.on_change().expect("well-formed after push_line");
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

        let mut new_cursor_pos = if start_line == end_line {
            // Single-line edit
            self.line_mut(start_line)
                .replace_range(start_pos, end_pos, new_str)?
        } else if start_line < end_line {
            // Multi-line edit.

            // Step 1: Add surviving portion of end line to the start line.
            let end_pos_in_line = self.line(start_line).end_position();
            let s = self.line(end_line).suffix(end_pos)?;
            self.line_mut(start_line)
                .replace_range(start_pos, end_pos_in_line, &s)?;

            // Step 2: Remove all lines after the start.
            self.component_mut()
                .remove_range(start_line + 1, end_line + 1);

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
                    self.component_mut().insert(fixup_line, newline);
                }
            }
        }

        // Is the new module well-formed? Otherwise, revert this entire change.
        match self.on_change() {
            Ok(()) => self.line(fixup_line).set_cursor_position(new_cursor_pos),
            Err(_) => {
                // restore backup
                self.component_mut()
                    .remove_range(start_line, fixup_line + 1);
                for (i, contents) in backup.iter().enumerate() {
                    let mut line = CodeLine::new(contents, &self.0.borrow().factory);
                    line.strobe();
                    self.component_mut().insert(start_line + i, line);
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
                        selection.focus_node().context("focus")?,
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

    fn line(&self, idx: usize) -> Ref<'_, CodeLine> {
        Ref::map(self.component(), |c| &c[idx])
    }

    fn line_mut(&mut self, idx: usize) -> RefMut<'_, CodeLine> {
        RefMut::map(self.component_mut(), |c| &mut c[idx])
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

    fn on_change(&mut self) -> Result<()> {
        // repair syntax
        fix_frames(self);

        // update module
        let text = self
            .instructions_as_text()
            .fold(String::new(), |acc, elem| acc + "\n" + elem.as_ref());
        let wasm_bin = str_to_binary(text)?;
        Self::execute(&wasm_bin);
        self.0.borrow_mut().module = OkModule::build(wasm_bin, self)?;

        // log instruction types (TODO: integrate into OkModule)
        let _ = collect_operands(
            self.0.borrow().module.borrow_binary(),
            self.0.borrow().module.borrow_ops(),
        );

        #[cfg(debug_assertions)]
        {
            self.audit();
            log_1(&"successful audit".into());
        }

        Ok(())
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
        self.line_mut(index).set_active_status(is_active)
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

impl Component for Editor {
    fn audit(&self) {
        self.component().audit()
    }
}
