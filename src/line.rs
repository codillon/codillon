use crate::view::*;
use leptos::{prelude::*, *};
use reactive_stores::Store;
use web_sys::{InputEvent, wasm_bindgen::JsCast};

// The `EditLine` reflects the state of an individual line.
// TODO: WebAssembly syntax checking
#[derive(Store, Debug, Clone, Default)]
pub struct EditLine {
    id: usize,
    // Text invariant:
    // Empty logical text => physical text is EXACTLY the cosmetic space.
    // Nonempty logical text => physical text contains NO cosmetic space.
    logical_text: String,
    div_ref: DivRef,
}

impl EditLine {
    pub fn new(id: usize, start_text: String) -> Self {
        Self {
            id,
            logical_text: start_text,
            div_ref: DivRef::new(),
        }
    }

    pub fn id(&self) -> &usize {
        &self.id
    }

    pub fn get_logical_text(&self) -> &str {
        &self.logical_text
    }

    pub fn div_ref(&self) -> DivRef {
        self.div_ref
    }

    // Splits the current line at POS, returning what is removed (RHS).
    pub fn split_self(&mut self, pos: usize) -> String {
        if self.get_logical_text().is_empty() {
            return String::from("");
        }

        let remainder = self.logical_text.split_off(pos);

        // If we slice the line at position 0, the physical text is empty.
        if self.get_logical_text().is_empty() {
            self.logical_text = String::from("");
        }

        if remainder.is_empty() {
            String::from("")
        } else {
            remainder
        }
    }

    // Handle leading spaces and return the selection range.
    pub fn preprocess_input(&mut self, ev: InputEvent) -> (usize, usize) {
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

        // Remove the cosmetic space on a new line UNLESS we're simply
        // making another new line.
        if self.get_logical_text().is_empty()
            && ev.input_type().as_str() != "insertParagraph"
            && ev.input_type().as_str() != "insertLineBreak"
        {
            self.logical_text.clear();
        }

        start_pos = start_pos.min(self.get_logical_text().len());
        end_pos = end_pos.min(self.get_logical_text().len());

        if start_pos > end_pos {
            (end_pos, start_pos)
        } else {
            (start_pos, end_pos)
        }
    }

    // Handle insert and delete events for this line.
    pub fn handle_input(&mut self, ev: InputEvent) -> usize {
        let (start_pos, end_pos) = self.preprocess_input(ev.clone());
        let mut cursor_pos = start_pos;

        match ev.input_type().as_str() {
            "insertText" => {
                let new_text = &ev.data().unwrap_or_default();
                self.logical_text
                    .replace_range(start_pos..end_pos, new_text);
                cursor_pos = start_pos + new_text.len();
            }
            "deleteContentBackward" => {
                if start_pos == end_pos && start_pos > 0 {
                    self.logical_text
                        .replace_range(start_pos - 1..start_pos, "");
                    cursor_pos = start_pos - 1;
                } else {
                    self.logical_text.replace_range(start_pos..end_pos, "");
                }
            }
            "deleteContentForward" => {
                if start_pos == end_pos && start_pos < self.get_logical_text().len() {
                    self.logical_text
                        .replace_range(start_pos..start_pos + 1, "");
                } else {
                    self.logical_text.replace_range(start_pos..end_pos, "");
                }
            }
            other => leptos_dom::log!("unhandled: {other}"),
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
