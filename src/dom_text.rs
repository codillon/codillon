// A Codillon Text Component. This represents a string;
// the interface allows assignment, appending, and inserting into the
// string, and enforces that the DOM contents will match the Rust contents.

use crate::web_support::{AccessToken, Component, TextHandle, WithNode};

#[derive(Default)]
pub struct DomText {
    contents: String,
    text_node: TextHandle,
}

impl DomText {
    pub fn new(string: &str) -> Self {
        let mut ret = Self::default();
        ret.set_data(string);
        ret
    }

    pub fn replace_data(&mut self, utf16_idx_range: std::ops::Range<usize>, string: &str) {
        let begin_byte_idx = str_indices::utf16::to_byte_idx(&self.contents, utf16_idx_range.start);
        let end_byte_idx = str_indices::utf16::to_byte_idx(&self.contents, utf16_idx_range.end);

        self.contents
            .replace_range(begin_byte_idx..end_byte_idx, string);
        self.text_node.replace_data(
            utf16_idx_range.start as u32,
            (utf16_idx_range.end - utf16_idx_range.start) as u32,
            string,
        );
    }

    pub fn set_data(&mut self, string: &str) {
        self.contents = string.to_string();
        self.text_node.set_data(string);
    }

    pub fn text(&self) -> &String {
        &self.contents
    }
}

impl WithNode for DomText {
    fn with_node(&self, f: impl FnMut(&web_sys::Node), g: AccessToken) {
        self.text_node.with_node(f, g);
    }
}

impl Component for DomText {
    fn audit(&self) {
        assert_eq!(self.contents, self.text_node.data());
    }
}
