use std::ops::{Deref, DerefMut};

use crate::{
    utils::{InstrInfo, parse_instr},
    view::*,
};
use anyhow::{Ok, Result};
use leptos::prelude::*;
use web_sys::{InputEvent, wasm_bindgen::JsCast};

// The `EditLine` reflects the state of an individual line.
// TODO: WebAssembly syntax checking
#[derive(Debug)]
pub struct EditLine {
    id: usize,
    // Text invariant:
    // Empty logical text => physical text is EXACTLY the cosmetic space.
    // Nonempty logical text => physical text contains NO cosmetic space.
    logical_text: String,
    div_ref: DivRef,
    instr: Result<Option<InstrInfo>>,
}

pub struct TextGuard<'a> {
    inner: &'a mut EditLine,
}

impl Deref for TextGuard<'_> {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.inner.logical_text
    }
}

impl DerefMut for TextGuard<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner.logical_text
    }
}

impl Drop for TextGuard<'_> {
    fn drop(&mut self) {
        self.inner.update_instr();
    }
}

impl EditLine {
    pub fn new(id: usize, start_text: String) -> Self {
        Self {
            id,
            logical_text: start_text,
            div_ref: DivRef::new(),
            instr: Ok(None),
        }
    }

    pub fn id(&self) -> &usize {
        &self.id
    }

    pub fn logical_text(&self) -> &str {
        &self.logical_text
    }

    pub fn display_text(&self) -> &str {
        const COSMETIC_SPACE: &str = "\u{FEFF}";

        if self.logical_text().is_empty() {
            COSMETIC_SPACE
        } else {
            self.logical_text()
        }
    }

    pub fn char_count(&self) -> usize {
        self.logical_text().chars().count() // TODO: memoize
    }

    pub fn display_char_count(&self) -> usize {
        self.display_text().chars().count() // TODO: memoize
    }

    pub fn logic_text_mut(&mut self) -> TextGuard {
        TextGuard { inner: self }
    }

    pub fn div_ref(&self) -> DivRef {
        self.div_ref
    }

    // Splits the current line at POS, returning what is removed (RHS).
    pub fn split_self(&mut self, pos: usize) -> String {
        let byte_pos = self.char_to_byte(pos);
        self.logic_text_mut().split_off(byte_pos)
    }

    pub fn instr(&self) -> &Result<Option<InstrInfo>> {
        &self.instr
    }

    fn update_instr(&mut self) {
        self.instr = parse_instr(self.logical_text());
    }

    fn char_to_byte(&self, char_pos: usize) -> usize {
        if char_pos >= self.char_count() {
            return self.logical_text().len();
        }

        self.logical_text()
            .char_indices()
            .nth(char_pos)
            .unwrap_or_else(|| panic!("char pos {char_pos}"))
            .0
    }

    // Handle leading spaces and return the selection range.
    pub fn preprocess_input(&mut self, ev: &InputEvent) -> (usize, usize) {
        let range = ev
            .get_target_ranges()
            .get(0)
            .clone()
            .unchecked_into::<DomRange>();

        let text_node = self.text_node();

        if range.start_container().expect("container") != text_node
            || range.end_container().expect("container") != text_node
        {
            panic!("InputEvent targets a range outside the text node for this EditLine")
        }

        let start_char_pos = range.start_offset().expect("offset") as usize;
        let end_char_pos = range.end_offset().expect("offset") as usize;

        let start_byte_pos = self.char_to_byte(start_char_pos);
        let end_byte_pos = self.char_to_byte(end_char_pos);

        if start_byte_pos > end_byte_pos {
            (end_byte_pos, start_byte_pos)
        } else {
            (start_byte_pos, end_byte_pos)
        }
    }

    // Handle insert and delete events for this line.
    pub fn handle_input(&mut self, ev: &InputEvent) -> usize {
        let (start_pos, end_pos) = self.preprocess_input(ev);
        let mut cursor_pos = start_pos;

        match ev.input_type().as_str() {
            "insertText" => {
                let new_text = &ev.data().unwrap_or_default();
                self.logic_text_mut()
                    .replace_range(start_pos..end_pos, new_text);
                cursor_pos = start_pos + new_text.chars().count();
            }
            "deleteContentBackward" => {
                if start_pos == end_pos && start_pos > 0 {
                    self.logic_text_mut()
                        .replace_range(start_pos - 1..start_pos, "");
                    cursor_pos = start_pos - 1;
                } else {
                    self.logic_text_mut().replace_range(start_pos..end_pos, "");
                }
            }
            "deleteContentForward" => {
                if start_pos == end_pos && start_pos < self.logical_text().chars().count() {
                    self.logic_text_mut()
                        .replace_range(start_pos..start_pos + 1, "");
                } else {
                    self.logic_text_mut().replace_range(start_pos..end_pos, "");
                }
            }
            other => leptos::leptos_dom::log!("unhandled: {other}"),
        }
        cursor_pos
    }

    pub fn text_node(&self) -> DomNode {
        let node = self
            .div_ref
            .get_untracked()
            .expect("div")
            .first_child()
            .expect("text");
        if node.node_type() != DomNode::TEXT_NODE {
            panic!("non-text node found");
        }
        node
    }
}
