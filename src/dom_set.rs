// A Codillon DOM "set": an unordered collection of Components of the same type, indexed by a u32

use crate::{
    dom_vec::DomVec,
    jet::{AccessToken, AnyElement, Component, ElementHandle, WithElement},
};
use std::{
    collections::{HashMap, VecDeque},
    ops::{Index, IndexMut},
};

pub struct DomSet<Child: Component, Element: AnyElement> {
    contents: DomVec<Child, Element>,
    mapping: HashMap<u32, usize>,
    free_list: VecDeque<usize>,
}

impl<Child: Component, Element: AnyElement> DomSet<Child, Element> {
    pub fn new(elem: ElementHandle<Element>) -> Self {
        Self {
            contents: DomVec::new(elem),
            mapping: Default::default(),
            free_list: Default::default(),
        }
    }

    pub fn insert(&mut self, id: u32, val: Child) {
        if let Some(idx) = self.free_list.pop_front() {
            self.mapping.insert(id, idx);
            self.contents[idx].assign(val);
        } else {
            self.mapping.insert(id, self.contents.len());
            self.contents.push(val);
        }
    }

    pub fn remove(&mut self, id: u32, default: Child) {
        let idx = self.mapping.remove(&id).unwrap();
        self.contents[idx].assign(default);
        self.free_list.push_back(idx);
    }

    pub fn len(&self) -> usize {
        self.mapping.len()
    }

    pub fn is_empty(&self) -> bool {
        self.mapping.is_empty()
    }

    pub fn get(&self, id: u32) -> Option<&Child> {
        self.mapping
            .get(&id)
            .and_then(|idx| self.contents.get(*idx))
    }

    pub fn get_mut(&mut self, id: u32) -> Option<&mut Child> {
        self.mapping
            .get(&id)
            .and_then(|idx| self.contents.get_mut(*idx))
    }

    pub fn ids(&self) -> impl Iterator<Item = &u32> {
        self.mapping.keys()
    }

    pub fn for_each_mut(&mut self, mut f: impl FnMut(u32, &mut Child)) {
        for (id, idx) in self.mapping.iter() {
            f(*id, &mut self.contents[*idx])
        }
    }
}

impl<Child: Component, Element: AnyElement> Index<u32> for DomSet<Child, Element> {
    type Output = Child;

    fn index(&self, id: u32) -> &Child {
        &self.contents[self.mapping[&id]]
    }
}

impl<Child: Component, Element: AnyElement> IndexMut<u32> for DomSet<Child, Element> {
    fn index_mut(&mut self, id: u32) -> &mut Child {
        &mut self.contents[self.mapping[&id]]
    }
}

impl<Child: Component, Element: AnyElement> Component for DomSet<Child, Element> {
    #[cfg(debug_assertions)]
    fn audit(&self) {
        self.contents.audit();
    }
}

impl<Child: Component, Element: AnyElement> WithElement for DomSet<Child, Element> {
    type Element = Element;
    fn with_element<T, F: FnMut(&Element) -> T>(&self, f: F, g: AccessToken) -> T {
        self.contents.with_element(f, g)
    }
}
