use std::ops::{Deref, DerefMut};

use wasm_bindgen::JsCast;
use web_sys::{Document, HtmlDivElement};

pub struct TextGuard<'a> {
    inner: &'a mut EditLine,
}

impl Deref for TextGuard<'_> {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.inner.text
    }
}

impl DerefMut for TextGuard<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner.text
    }
}

impl Drop for TextGuard<'_> {
    fn drop(&mut self) {
        self.inner.sync_dom();
    }
}

#[derive(Debug)]
pub struct EditLine {
    id: usize,
    node: HtmlDivElement,
    text: String,
}

impl EditLine {
    pub const ID_ATTRIBUTE: &str = "data-codillon-line-id";
    pub const COSMETIC_SPACE: &str = "\u{FEFF}";

    pub fn node(&self) -> &HtmlDivElement {
        &self.node
    }

    pub fn text_node(&self) -> web_sys::Node {
        self.node.first_child().expect("Read textnode")
    }

    pub fn new(id: usize, document: &Document) -> EditLine {
        let div_ref = document
            .create_element("div")
            .expect("Create Editline Failed")
            .dyn_into::<HtmlDivElement>()
            .unwrap();
        div_ref
            .set_attribute(Self::ID_ATTRIBUTE, &id.to_string())
            .unwrap();
        let editline = EditLine {
            id,
            node: div_ref,
            text: String::new(),
        };
        editline.sync_dom();
        editline
    }

    pub fn sync_dom(&self) {
        self.node.set_text_content(Some(if self.text.is_empty() {
            EditLine::COSMETIC_SPACE
        } else {
            &self.text
        }));
    }

    pub fn id(&self) -> &usize {
        &self.id
    }

    pub fn text(&self) -> &String {
        &self.text
    }

    pub fn text_mut(&mut self) -> TextGuard {
        TextGuard { inner: self }
    }
}
