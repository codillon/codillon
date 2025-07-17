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
    text: String,
    div_ref: DivRef,
}

impl EditLine {
    pub fn new(id: usize, start_text: String) -> Self {
        Self {
            id,
            text: start_text,
            div_ref: DivRef::new(),
        }
    }

    const COSMETIC_SPACE: char = '\u{FEFF}';

    pub fn id(&self) -> &usize {
        &self.id
    }

    // To uphold the invariant, the logical text is
    // empty iff the only contents of the text is the cosmetic space.
    pub fn logical_text(&self) -> &str {
        if self.text == String::from(Self::COSMETIC_SPACE) {
            ""
        } else {
            &self.text
        }
    }

    pub fn div_ref(&self) -> DivRef {
        self.div_ref
    }

    fn rationalize(&mut self, cursor_pos: &mut usize) {
        // Adjust the line so the cursor still shows up even if the text is empty,
        // by replacing empty strings with a zero-width space character and removing it
        // later -- adjusting cursor position to match.

        self.text.retain(|c| c != Self::COSMETIC_SPACE);
        *cursor_pos = (*cursor_pos).min(self.text.len());
        let no_initial_ws = self.text.trim_start();
        *cursor_pos = (*cursor_pos).saturating_sub(self.logical_text().len() - no_initial_ws.len());
        self.text = no_initial_ws.to_string();

        if self.logical_text().is_empty() {
            self.text.push(Self::COSMETIC_SPACE);
        }
    }

    // Splits the current line at POS, returning what is removed (RHS).
    pub fn split_self(&mut self, pos: usize) -> String {
        if self.logical_text().is_empty() {
            return String::from(Self::COSMETIC_SPACE);
        }

        let remainder = self.text.split_off(pos);

        // If we slice the line at position 0, the physical text is empty.
        if self.logical_text().is_empty() {
            self.text = String::from(Self::COSMETIC_SPACE);
        }

        if remainder.is_empty() {
            String::from(Self::COSMETIC_SPACE)
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
        if self.logical_text().is_empty()
            && ev.input_type().as_str() != "insertParagraph"
            && ev.input_type().as_str() != "insertLineBreak"
        {
            self.text.clear();
        }

        start_pos = start_pos.min(self.logical_text().len());
        end_pos = end_pos.min(self.logical_text().len());

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
                if start_pos == end_pos && start_pos < self.logical_text().len() {
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
