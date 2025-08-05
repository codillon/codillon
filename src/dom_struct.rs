// A Codillon DOM "struct" (product type): a (possibly empty) collection
// of heterogeneous Components of (possibly) different types.

use crate::web_support::{
    AccessToken, AnyElement, ArrayHandle, Component, ElementHandle, NodeListHandle, WithElement,
    WithNode,
};
use delegate::delegate;

pub struct DomStruct<Child: Structure, Element: AnyElement> {
    contents: Child,
    elem: ElementHandle<Element>,
}

// A "structure" is basically a static linked list of Components. It has a way
// to install itself (and the subsequent Components) into an ArrayHandle (not the actual DOM -- just
// an array of Nodes), and to audit that it (and the subsequent Components) match the DOM.
pub trait Structure {
    const LEN: usize;
    fn install(&self, nodes: &mut ArrayHandle, index: usize);
    fn audit(&self, node_list: &NodeListHandle, index: usize);
}

// Base case: an empty structure (aka the end of every structure).
impl Structure for () {
    const LEN: usize = 0;
    fn install(&self, nodes: &mut ArrayHandle, index: usize) {
        assert_eq!(index, nodes.length());
    }
    fn audit(&self, node_list: &NodeListHandle, index: usize) {
        assert_eq!(index, node_list.length());
    }
}

// Recursive case: a non-empty structure (aka any part of a structure other than the end).
impl<First: Component, Rest: Structure> Structure for (First, Rest) {
    const LEN: usize = Rest::LEN + 1;
    fn install(&self, nodes: &mut ArrayHandle, index: usize) {
        assert_eq!(index + Self::LEN, nodes.length());
        nodes.set(index, &self.0);
        self.1.install(nodes, index + 1);
    }
    fn audit(&self, node_list: &NodeListHandle, index: usize) {
        assert_eq!(index + Self::LEN, node_list.length());
        node_list.audit_node(index, &self.0);
        self.0.audit();
        self.1.audit(node_list, index + 1);
    }
}

// The DomStruct itself.
impl<Child: Structure, Element: AnyElement> DomStruct<Child, Element> {
    // Create the DomStruct, installing each member of the structure
    // into an ArrayHandle, and then attaching that ArrayHandle to the DOM.
    pub fn new(contents: Child, elem: ElementHandle<Element>) -> Self {
        let mut child_nodes = ArrayHandle::new_with_length(Child::LEN);
        contents.install(&mut child_nodes, 0);
        elem.attach_nodes(child_nodes);
        Self { contents, elem }
    }

    pub fn get(&self) -> &Child {
        &self.contents
    }

    pub fn get_mut(&mut self) -> &mut Child {
        &mut self.contents
    }

    pub fn set_contents(&mut self, new_contents: Child) {
        self.contents = new_contents;
    }

    delegate! {
    to self.elem {
    pub fn set_attribute(&mut self, name: &str, value: &str);
        pub fn set_onbeforeinput<F: Fn(web_sys::InputEvent) + 'static>(&mut self, handler: F);
    }
    }
}

// To audit, audit the parent element itself, then audit the structure members.
impl<Child: Structure, Element: AnyElement> Component for DomStruct<Child, Element> {
    fn audit(&self) {
        self.elem.audit();
        let dom_children = self.elem.get_child_node_list();
        assert_eq!(dom_children.length(), Child::LEN);
        self.contents.audit(&dom_children, 0);
    }
}

// Accessors for the parent element (only usable by the web_support module).
impl<Child: Structure, Element: AnyElement> WithNode for DomStruct<Child, Element> {
    fn with_node(&self, f: impl FnMut(&web_sys::Node), g: AccessToken) {
        self.elem.with_node(f, g);
    }
}

impl<Child: Structure, Element: AnyElement> WithElement<Element> for DomStruct<Child, Element> {
    fn with_element(&self, f: impl FnMut(&Element), g: AccessToken) {
        self.elem.with_element(f, g);
    }
}
