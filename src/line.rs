use crate::view::*;
use leptos::{prelude::*, *};
use web_sys::{InputEvent, wasm_bindgen::JsCast};

// The `EditLine` reflects the state of an individual line.
// TODO: WebAssembly syntax checking
#[derive(Default)]
pub struct EditLine {
    id: usize,
    text: String,
    div_ref: DivRef,
}

impl EditLine {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            text: String::from("Hello, world"),
            div_ref: DivRef::new(),
        }
    }

    pub fn id(&self) -> &usize {
        &self.id
    }
    pub fn text(&self) -> &String {
        &self.text
    }
    pub fn div_ref(&self) -> DivRef {
        self.div_ref
    }

    pub fn as_str(&self) -> &str {
        &self.text
    }

    const COSMETIC_SPACE: char = '\u{FEFF}';

    fn rationalize(&mut self, cursor_pos: &mut usize) {
        // Adjust the line so the cursor still shows up even if the text is empty,
        // by replacing empty strings with a zero-width space character and removing it
        // later -- adjusting cursor position to match.

        self.text.retain(|c| c != Self::COSMETIC_SPACE);
        *cursor_pos = (*cursor_pos).min(self.text.len());
        let no_initial_ws = self.text.trim_start();
        *cursor_pos = (*cursor_pos).saturating_sub(self.text.len() - no_initial_ws.len());
        self.text = no_initial_ws.to_string();

        if self.text.is_empty() {
            self.text.push(Self::COSMETIC_SPACE);
        }
    }

    // Handle insert and delete events for this line.
    pub fn handle_input(&mut self, ev: InputEvent) -> usize {
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

        let mut start_pos = range.start_offset().expect("offset") as usize;
        let mut end_pos = range.end_offset().expect("offset") as usize;

        if self.text.starts_with(Self::COSMETIC_SPACE) {
            self.text.clear();
        }

        start_pos = start_pos.min(self.text.len());
        end_pos = end_pos.min(self.text.len());

        if start_pos > end_pos {
            (start_pos, end_pos) = (end_pos, start_pos);
        }

        let mut cursor_pos = start_pos;

        match ev.input_type().as_str() {
            "insertText" => {
                let new_text = &ev.data().unwrap_or_default();
                self.text.replace_range(start_pos..end_pos, new_text);
                cursor_pos = start_pos + new_text.len();
            }
            "deleteContentBackward" => {
                if start_pos == end_pos && start_pos > 0 {
                    self.text.replace_range(start_pos - 1..start_pos, "");
                    cursor_pos = start_pos - 1;
                } else {
                    self.text.replace_range(start_pos..end_pos, "");
                }
            }
            "deleteContentForward" => {
                if start_pos == end_pos && start_pos < self.text.len() {
                    self.text.replace_range(start_pos..start_pos + 1, "");
                } else {
                    self.text.replace_range(start_pos..end_pos, "");
                }
            }
            other => leptos_dom::log!("unhandled: {other}"),
        }

        self.rationalize(&mut cursor_pos);
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
