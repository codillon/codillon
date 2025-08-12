// A Codillon Text Component. This represents a string;
// the interface allows assignment, appending, and inserting into the
// string, and enforces that the DOM contents will match the Rust contents.

use crate::web_support::{AccessToken, Component, TextHandle, WithNode};
use anyhow::{Result, bail};

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

    pub fn push_str(&mut self, string: &str) {
        self.contents.push_str(string);
        self.text_node.append_data(string);
    }

    pub fn set_data(&mut self, string: &str) {
        self.contents = string.to_string();
        self.text_node.set_data(string);
    }

    pub fn insert_at_char(&mut self, char_idx: usize, string: &str) -> Result<usize> {
        let byte_idx = str_indices::chars::to_byte_idx(&self.contents, char_idx);
        let utf16_idx = str_indices::utf16::from_byte_idx(&self.contents, byte_idx);
        self.contents.insert_str(byte_idx, string);
        self.text_node.insert_data(utf16_idx.try_into()?, string);
        let utf16_inserted = str_indices::utf16::count(string);
        Ok(utf16_idx + utf16_inserted)
    }

    pub fn insert_at_utf16_pos(&mut self, utf16_idx: usize, string: &str) -> Result<usize> {
        let byte_idx = str_indices::utf16::to_byte_idx(&self.contents, utf16_idx);

        if utf16_idx != str_indices::utf16::from_byte_idx(&self.contents, byte_idx) {
            bail!("invalid UTF-16 position (not at char boundary)");
        }

        self.contents.insert_str(byte_idx, string);
        self.text_node.insert_data(utf16_idx.try_into()?, string);
        let utf16_inserted = str_indices::utf16::count(string);
        Ok(utf16_idx + utf16_inserted)
    }

    pub fn len_utf16(&self) -> usize {
        str_indices::utf16::count(&self.contents)
    }

    pub fn len_bytes(&self) -> usize {
        self.contents.len()
    }

    pub fn is_empty(&self) -> bool {
        self.contents.is_empty()
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
