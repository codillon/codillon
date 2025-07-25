use crate::common::*;
use wasm_bindgen::JsCast;
use web_sys::{Element, HtmlDivElement, Node};

#[derive(Debug)]
pub struct EditLine {
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
            node: div_ref,
            text: String::new(),
        };
        editline.update_dom();
        editline
    }

    pub fn text(&self) -> &String {
        &self.text
    }

    pub fn text_mut(&mut self) -> &mut String {
        &mut self.text
    }

    /// Be used by selection. To be disposed later.
    pub fn text_node(&self) -> Node {
        self.node_ref().as_ref().first_child().expect("Textnode")
    }

    // Given an HTML Node, finds the Codillion-assigned unique ID of the EditLine.
    // These are stored in HTML attributes of the line-by-line Div elements.
    pub fn find_id_from_node(orig_node: &Node) -> Option<usize> {
        let mut node = orig_node.clone();
        loop {
            if let Ok(elem) = node.clone().dyn_into::<Element>()
                && let Some(id_str) = elem.get_attribute(EditLine::ID_ATTRIBUTE)
            {
                return id_str.parse::<usize>().ok();
            }
            match node.parent_node() {
                Some(n) => node = n,
                None => return None,
            }
        }
    }
}
