use crate::common::*;
use wasm_bindgen::JsCast;
use web_sys::{HtmlDivElement, Node};

#[derive(Debug)]
pub struct EditLine {
    id: usize,
    node: HtmlDivElement,
    text: String,
}

impl Component for EditLine {
    fn node_ref(&self) -> impl AsRef<web_sys::Element> {
        &self.node
    }
    fn update_dom(&self) {
        self.node_ref()
            .as_ref()
            .set_text_content(Some(if self.text().is_empty() {
                EditLine::COSMETIC_SPACE
            } else {
                self.text()
            }));
    }
}

impl Drop for EditLine {
    fn drop(&mut self) {
        self.drop_dom();
    }
}

impl EditLine {
    pub const ID_ATTRIBUTE: &str = "data-codillon-line-id";
    pub const COSMETIC_SPACE: &str = "\u{FEFF}";

    pub fn new(id: usize) -> EditLine {
        let div_ref = document()
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
        editline.update_dom();
        editline
    }

    pub fn id(&self) -> &usize {
        &self.id
    }

    pub fn text(&self) -> &String {
        &self.text
    }

    pub fn text_mut(&mut self) -> &mut String {
        &mut self.text
    }

    pub fn text_node(&self) -> Node {
        self.node_ref().as_ref().first_child().expect("Textnode")
    }
}
