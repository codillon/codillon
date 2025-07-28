use std::ops::{Deref, DerefMut};

use anyhow::Ok;
use delegate::delegate;
use wasm_bindgen::JsCast;

use super::common::*;

/// Should be used to hold a snnipet of text.
#[derive(Debug)]
pub struct SafeSpan {
    node: web_sys::HtmlSpanElement,
    text: String,
}

impl UnsafeNode for SafeSpan {
    fn default() -> Self {
        super::SafeSpan {
            node: document()
                .create_element("span")
                .unwrap()
                .dyn_into()
                .unwrap(),
            text: String::new(),
        }
    }
    fn node(&self) -> &web_sys::HtmlElement {
        &self.node
    }
}

impl SafeNode for SafeSpan {
    fn audit(&self) -> anyhow::Result<()> {
        if let Some(text) = self.text_content()
            && *self.text() == text
        {
            Ok(())
        } else {
            Err(anyhow::anyhow!(
                "SafeText audit failed: data not exist or not match"
            ))
        }
    }
}

impl SafeSpan {
    delegate! {
        to self.node
        {
            fn set_text_content(&self, value: Option<&str>);
            fn text_content(&self) -> Option<String>;
        }
    }
    pub fn text(&self) -> &String {
        &self.text
    }

    pub fn text_mut<'a>(&'a mut self) -> SpanTextGuard<'a> {
        SpanTextGuard::new(self)
    }
}

/// As a mut reference guard for compenent, will update the dom when the mut ref is being dropped
#[derive(Debug)]
pub struct SpanTextGuard<'a> {
    inner: &'a mut SafeSpan,
}

impl<'a> SpanTextGuard<'a> {
    pub fn new(inner: &'a mut SafeSpan) -> Self {
        SpanTextGuard { inner }
    }
}

impl Deref for SpanTextGuard<'_> {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        self.inner.text()
    }
}

impl DerefMut for SpanTextGuard<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner.text
    }
}

impl Drop for SpanTextGuard<'_> {
    fn drop(&mut self) {
        self.inner.set_text_content(Some(self.inner.text()));
    }
}
