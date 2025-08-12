// The Codillon code editor (doesn't do much, but does capture beforeinput and logs to console)

use crate::{
    dom_sidecar::DomSidecar,
    dom_struct::DomStruct,
    dom_text::DomText,
    dom_vec::DomVec,
    utils::*,
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
type EditLine = DomStruct<LineContents, HtmlSpanElement>;

pub struct EditlineSidecar {
    id: usize,
    info: InstrInfo,
    activated: bool,
}

impl EditlineSidecar {
    pub fn new(id: usize, info: InstrInfo, activated: bool) -> Self {
        Self {
            id,
            info,
            activated,
        }
    }

    pub fn can_have_op(&self) -> bool {
        self.info != InstrInfo::EmptyorMalformed && self.activated
    }
}

struct _Editor {
    _next_id: usize,
    _id_map: HashMap<usize, usize>,
    _module: OkModule,
    _synthetic_ends: usize,
    component: DomVec<DomSidecar<EditLine, EditlineSidecar>, HtmlDivElement>,
}

pub struct Editor(Rc<RefCell<_Editor>>);

impl Editor {
    pub fn new(factory: &ElementFactory) -> Self {
        let mut inner = _Editor {
            _next_id: 0,
            _id_map: HashMap::default(),
            _module: OkModule::build(str_to_binary("").expect("wasm binary"), Vec::new())
                .expect("OkModule"),
            _synthetic_ends: 0,
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

        ret.push_line(factory, "i32.const 1");
        ret.push_line(factory, "drop");

        web_sys::console::log_1(
            &format!(
                "operands: {:?}",
                collect_operands(
                    ret.0.borrow()._module.borrow_binary(),
                    ret.0.borrow()._module.borrow_ops().as_ref().unwrap()
                ),
            )
            .into(),
        );

        ret
    }

    fn push_line(&mut self, factory: &ElementFactory, string: &str) {
        {
            let mut editor = self.0.borrow_mut();
            let id = editor._next_id;
            let component = &mut editor.component;
            component.push(DomSidecar::new(
                EditLine::new(
                    (DomText::new(string), (DomBr::new((), factory.br()), ())),
                    factory.span(),
                ),
                EditlineSidecar {
                    id,
                    info: parse_instr(string),
                    activated: true,
                },
            ));
            editor._next_id += 1;
        }
        self.fix_frames();
        self.update_module();
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

        let new_cursor_pos = self.line_mut(start_line_index)?.replace_range(
            start_pos_in_line,
            end_pos_in_line,
            new_str,
        )?;

        set_cursor_position(&*self.line(start_line_index)?, new_cursor_pos);

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

    //get activated, non-empty, well-formed lines as '\n' delimited string
    fn text(&self) -> String {
        let editor = self.0.borrow();
        let mut lines = String::new();
        for i in 0..editor.component.len() {
            let line = editor.component.get(i).expect("DomSidecar");
            let text = line.borrow_component().get().0.get_contents();
            let sidecar = line.borrow_sidecar();
            if sidecar.info != InstrInfo::EmptyorMalformed && sidecar.activated {
                lines.push_str(&format!("{text}\n"));
            }
        }
        lines
    }

    //get vector of each line's InstrInfo
    fn instrs(&self) -> Vec<InstrInfo> {
        let editor = self.0.borrow();
        let mut lines = Vec::new();
        for i in 0..editor.component.len() {
            let line = editor.component.get(i).expect("DomSidecar");
            let info = line.borrow_sidecar().info;
            lines.push(info)
        }
        lines
    }

    fn update_module(&mut self) {
        let lines = self.text();
        let mut editor = self.0.borrow_mut();
        web_sys::console::log_1(&format!("lines: {lines}").into());
        let bin = str_to_binary(&lines).expect("wasm binary");
        let mut sidecars = Vec::new();
        for i in 0..editor.component.len() {
            let sidecar = editor
                .component
                .get(i)
                .expect("DomSidecar")
                .borrow_sidecar();
            sidecars.push(sidecar);
        }
        editor._module = OkModule::build(bin, sidecars).expect("OkModule");
    }

    fn fix_frames(&mut self) {
        let instrs = self.instrs();
        let mut editor = self.0.borrow_mut();
        let (deactivate_indices, num_synthetic_end) = fix_frames(&instrs);
        for i in 0..editor.component.len() {
            let sidecar = editor
                .component
                .get_mut(i)
                .expect("DomSidecar")
                .borrow_sidecar_mut();
            sidecar.activated = !deactivate_indices.contains(&i);
        }
        editor._synthetic_ends = num_synthetic_end;
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
