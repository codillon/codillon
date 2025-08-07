// A Codillon Text Component. This represents a string;
// the interface allows assignment, appending, and inserting into the
// string, and enforces that the DOM contents will match the Rust contents.

use std::ops::{Deref, DerefMut};

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

    pub fn replace_data(&mut self, char_idx_range: std::ops::Range<usize>, string: &str) {
        let begin_byte_idx = str_indices::chars::to_byte_idx(&self.contents, char_idx_range.start);
        let begin_utf16_idx = str_indices::utf16::from_byte_idx(&self.contents, begin_byte_idx);
        let end_byte_idx = str_indices::chars::to_byte_idx(&self.contents, char_idx_range.end);
        let end_utf16_idx = str_indices::utf16::from_byte_idx(&self.contents, char_idx_range.end);
        self.contents
            .replace_range(begin_byte_idx..end_byte_idx, string);
        self.text_node.replace_data(
            begin_utf16_idx as u32,
            (end_utf16_idx - begin_utf16_idx) as u32,
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

    pub fn text_mut(&mut self) -> TextGuard {
        TextGuard(self)
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

pub struct TextGuard<'a>(&'a mut DomText);

impl<'a> Deref for TextGuard<'a> {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.0.contents
    }
}

impl<'a> DerefMut for TextGuard<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0.contents
    }
}

impl<'a> Drop for TextGuard<'a> {
    fn drop(&mut self) {
        self.0.text_node.set_data(&self.0.contents);
    }
}
