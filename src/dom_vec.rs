// A Codillon DOM "vector": a variable-length collection of Components of the same type

use crate::jet::{AccessToken, AnyElement, Component, ElementHandle, WithElement};
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

    pub fn insert(&mut self, index: usize, elem: Child) {
        if index == self.contents.len() {
            self.push(elem)
        } else if index < self.contents.len() {
            self.contents.insert(index, elem);
            self.elem.insert_node(index, &self.contents[index]);
        } else {
            panic!("index > len");
        }
    }

    pub fn remove_range(&mut self, begin: usize, end: usize) {
        self.contents.drain(begin..end);
    }

    pub fn clear(&mut self) {
        self.contents.clear();
    }

    pub fn set_contents(&mut self, elem: Child) {
        self.contents = vec![elem];
        self.elem.attach_node(self.contents.last().unwrap());
    }

    delegate! {
    to self.contents {
        pub fn truncate(&mut self, len: usize);
        pub fn remove(&mut self, index: usize) -> Child;
        pub fn len(&self) -> usize;
        pub fn is_empty(&self) -> bool;
        pub fn get(&self, index: usize) -> Option<&Child>;
        pub fn get_mut(&mut self, index: usize) -> Option<&mut Child>;
        pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Child>;
    pub fn binary_search_by<'a, F>(&'a self, f: F) -> Result<usize, usize>
    where
            F: FnMut(&'a Child) -> std::cmp::Ordering;
    }
    to self.elem {
    pub fn set_attribute(&mut self, name: &str, value: &str);
    pub fn remove_attribute(&mut self, name: &str);
    pub fn get_attribute(&self, name: &str) -> Option<&String>;
    pub fn scroll_into_view(&self);
    }
    }
}

impl<Child: Component, Element: AnyElement> std::ops::Index<usize> for DomVec<Child, Element> {
    type Output = Child;

    fn index(&self, index: usize) -> &Child {
        &self.contents[index]
    }
}

impl<Child: Component, Element: AnyElement> std::ops::IndexMut<usize> for DomVec<Child, Element> {
    fn index_mut(&mut self, index: usize) -> &mut Child {
        &mut self.contents[index]
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

// Accessors for the parent element (only usable by the jet (web support) module).
impl<Child: Component, Element: AnyElement> WithElement for DomVec<Child, Element> {
    type Element = Element;
    fn with_element(&self, f: impl FnMut(&Element), g: AccessToken) {
        self.elem.with_element(f, g);
    }
}
