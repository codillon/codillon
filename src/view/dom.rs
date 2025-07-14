// View-layer abstractions that facilitate cross-layer communication
// while separating our core from direct DOM-handling.
use leptos::prelude::*;
use leptos::*;

pub type DivRef = NodeRef<leptos::html::Div>;
pub type DomNode = Node;
pub type DomRange = Range;
pub type DomSelection = Selection;

pub fn get_current_selection() -> DomSelection {
    web_sys::window()
        .expect("window not found")
        .get_selection()
        .expect("selection error")
        .expect("selection not found")
}

// Given an HTML Node, finds the Codillion-assigned unique ID of the EditLine.
// These are stored in HTML attributes of the line-by-line Div elements.
pub fn find_id_from_node(orig_node: &Node) -> Option<usize> {
    let mut node = orig_node.clone();
    loop {
        if let Ok(elem) = node.clone().dyn_into::<Element>()
            && let Some(id_str) = elem.get_attribute("data-codillon-line-id")
        {
            return id_str.parse::<usize>().ok();
        }
        match node.parent_node() {
            Some(n) => node = n,
            None => return None,
        }
    }
}

pub use leptos::prelude::GetUntracked;
pub use leptos::wasm_bindgen::JsCast;
pub use web_sys::{Element, InputEvent, KeyboardEvent, MouseEvent, Node, Range, Selection};
