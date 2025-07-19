use crate::view::*;
use leptos::prelude::*;
use web_sys::{InputEvent, wasm_bindgen::JsCast};

// The `EditLine` reflects the state of an individual line.
// TODO: WebAssembly syntax checking
pub struct EditLine {
    id: usize,
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

    pub fn id(&self) -> &usize {
        &self.id
    }

    pub fn logical_text(&self) -> &str {
        &self.text
    }

    pub fn display_text(&self) -> &str {
        const COSMETIC_SPACE: &str = "\u{FEFF}";

        if self.text.is_empty() {
            COSMETIC_SPACE
        } else {
            &self.text
        }
    }

    pub fn char_count(&self) -> usize {
        self.text.chars().count() // TODO: memoize
    }

    pub fn display_char_count(&self) -> usize {
        self.display_text().chars().count() // TODO: memoize
    }

    pub fn div_ref(&self) -> DivRef {
        self.div_ref
    }

    // Splits the current line at POS, returning what is removed (RHS).
    pub fn split_self(&mut self, pos: usize) -> String {
        self.text.split_off(self.char_to_byte(pos))
    }

    fn char_to_byte(&self, char_pos: usize) -> usize {
        if char_pos >= self.char_count() {
            return self.text.len();
        }

        self.text
            .char_indices()
            .nth(char_pos)
            .unwrap_or_else(|| panic!("char pos {char_pos}"))
            .0
    }

    // Gets the indices of the current inline selection.
    // Panics if the selection spans multiple lines.
    pub fn get_inline_range(&mut self, ev: &InputEvent) -> (usize, usize) {
        let range = ev
            .get_target_ranges()
            .get(0)
            .clone()
            .unchecked_into::<DomRange>();

        let text_node = self.text_node();

        // TODO: Reevaluate why we might need this.
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
        let (start_pos, end_pos) = self.get_inline_range(ev);
        let mut cursor_pos = start_pos;

        match ev.input_type().as_str() {
            "insertText" => {
                let new_text = &ev.data().unwrap_or_default();
                self.text.replace_range(start_pos..end_pos, new_text);
                cursor_pos = start_pos + new_text.chars().count();
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
                if start_pos == end_pos && start_pos < self.text.chars().count() {
                    self.text.replace_range(start_pos..start_pos + 1, "");
                } else {
                    self.text.replace_range(start_pos..end_pos, "");
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
