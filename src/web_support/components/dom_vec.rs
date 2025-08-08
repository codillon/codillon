// A Codillon DOM "vector": a variable-length collection of Components of the same type

use crate::web_support::{AccessToken, AnyElement, Component, ElementHandle, WithElement};
use delegate::delegate;

pub struct DomVec<Child: Component, Element: AnyElement> {
    contents: Vec<Child>,
    elem: ElementHandle<Element>,
}

impl<Child: Component, Element: AnyElement> DomVec<Child, Element> {
    pub fn new(elem: ElementHandle<Element>) -> Self {
        Self {
            contents: Vec::new(),
            elem,
        }
    }

    pub fn push(&mut self, elem: Child) {
        self.contents.push(elem);
        self.elem.append_node(self.contents.last().unwrap());
    }

    pub fn remove(&mut self, index: usize) -> Child {
        self.contents.remove(index)
    }

    pub fn set_contents(&mut self, elem: Child) {
        self.contents = vec![elem];
        self.elem.attach_node(self.contents.last().unwrap());
    }

    delegate! {
    to self.contents {
        pub fn get(&self, index: usize) -> Option<&Child>;
        pub fn get_mut(&mut self, index: usize) -> Option<&mut Child>;
    }
    to self.elem {
        pub fn set_attribute(&mut self, name: &str, value: &str);
    pub fn set_onbeforeinput<F: Fn(web_sys::InputEvent) + 'static>(&mut self, handler: F);
    }
    }
}

// To audit, audit the parent element itself, then for each child component,
// audit it, and also verify that the child's opinion of its node matches the
// actual child node of the DomVec's parent element.
impl<Child: Component, Element: AnyElement> Component for DomVec<Child, Element> {
    fn audit(&self) {
        self.elem.audit();
        let dom_children = self.elem.get_child_node_list();
        assert_eq!(dom_children.length(), self.contents.len());
        for (index, elem) in self.contents.iter().enumerate() {
            elem.audit();
            dom_children.audit_node(index, elem);
        }
    }
}

// Accessors for the parent element (only usable by the web_support module).
impl<Child: Component, Element: AnyElement> WithElement for DomVec<Child, Element> {
    type Element = Element;
    fn with_element(&self, f: impl FnMut(&Element), g: AccessToken) {
        self.elem.with_element(f, g);
    }
}
