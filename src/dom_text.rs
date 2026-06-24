// A Codillon Text Component. This represents a string;
// the interface allows assignment, appending, and inserting into the
// string, and enforces that the DOM contents will match the Rust contents.

use crate::jet::{AccessToken, Component, TextHandle, WebOffset, WithNode};
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

    pub fn take(&mut self) -> String {
        self.text_node.set_data("");
        std::mem::take(&mut self.contents)
    }

    pub fn utf16_to_byte_idx(&self, utf16_idx: WebOffset) -> Result<usize> {
        let byte_idx = str_indices::utf16::to_byte_idx(&self.contents, utf16_idx.usize());

        #[cfg(debug_assertions)]
        {
            if utf16_idx.usize() != str_indices::utf16::from_byte_idx(&self.contents, byte_idx) {
                bail!("invalid UTF-16 position (not at char boundary)");
            }
        }

        Ok(byte_idx)
    }

    pub fn byte_idx_to_utf16(&self, byte_idx: usize) -> Result<WebOffset> {
        let utf16_idx =
            WebOffset::new_from_usize(str_indices::utf16::from_byte_idx(&self.contents, byte_idx));

        #[cfg(debug_assertions)]
        if byte_idx != str_indices::utf16::to_byte_idx(&self.contents, utf16_idx.usize()) {
            bail!("invalid byte position (not at UTF-16 boundary)");
        }

        Ok(utf16_idx)
    }

    pub fn replace_range(
        &mut self,
        byte_start_idx: usize,
        byte_end_idx: usize,
        string: &str,
    ) -> Result<usize> {
        let utf16_start_idx = self.byte_idx_to_utf16(byte_start_idx)?;
        let utf16_end_idx = self.byte_idx_to_utf16(byte_end_idx)?;
        if utf16_start_idx > utf16_end_idx || byte_start_idx > byte_end_idx {
            bail!("invalid range");
        }
        let utf16_count = utf16_end_idx - utf16_start_idx;
        self.contents
            .replace_range(byte_start_idx..byte_end_idx, string);
        self.text_node
            .replace_data(utf16_start_idx, utf16_count, string)?;
        Ok(byte_start_idx + string.len())
    }

    pub fn len(&self) -> usize {
        self.contents.len()
    }

    pub fn is_empty(&self) -> bool {
        self.contents.is_empty()
    }

    pub fn get(&self) -> &str {
        &self.contents
    }

    pub fn suffix(&self, byte_start_idx: usize) -> &str {
        &self.contents[byte_start_idx..]
    }
}

impl WithNode for DomText {
    fn with_node<T, F: FnMut(&web_sys::Node) -> T>(&self, f: F, g: AccessToken) -> T {
        self.text_node.with_node(f, g)
    }
}

impl WithNode for &DomText {
    fn with_node<T, F: FnMut(&web_sys::Node) -> T>(&self, f: F, g: AccessToken) -> T {
        self.text_node.with_node(f, g)
    }
}

impl Component for DomText {
    #[cfg(debug_assertions)]
    fn audit(&self) {
        assert_eq!(self.contents, self.text_node.data());
    }
}
