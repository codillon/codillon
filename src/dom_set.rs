// A Codillon DOM "set": an unordered collection of Components of the same type, indexed by an arbitrary key

use crate::{
    dom_vec::DomVec,
    jet::{AccessToken, AnyElement, Component, ElementHandle, WithElement},
};
use std::{
    collections::{HashMap, VecDeque},
    hash::Hash,
    ops::{Index, IndexMut},
};

pub struct DomSet<Child: Component, Element: AnyElement, Key> {
    contents: DomVec<Child, Element>,
    mapping: HashMap<Key, usize>,
    free_list: VecDeque<usize>,
}

impl<Child: Component, Element: AnyElement, Key: Eq + Hash> DomSet<Child, Element, Key> {
    pub fn new(elem: ElementHandle<Element>) -> Self {
        Self {
            contents: DomVec::new(elem),
            mapping: Default::default(),
            free_list: Default::default(),
        }
    }

    pub fn insert(&mut self, id: Key, val: Child) {
        if let Some(idx) = self.mapping.get(&id) {
            self.contents[*idx].assign(val);
        } else if let Some(idx) = self.free_list.pop_front() {
            self.mapping.insert(id, idx);
            self.contents[idx].assign(val);
        } else {
            self.mapping.insert(id, self.contents.len());
            self.contents.push(val);
        }
    }

    pub fn remove(&mut self, id: Key, default: Child) {
        let idx = self.mapping.remove(&id).unwrap();
        self.contents[idx].assign(default);
        self.free_list.push_back(idx);
    }

    pub fn rehome(&mut self, cur_id: &Key, new_id: Key) -> &mut Child {
        assert!(!self.mapping.contains_key(&new_id));
        let idx = self.mapping.remove(cur_id).unwrap();
        self.mapping.insert(new_id, idx);
        &mut self.contents[idx]
    }

    pub fn len(&self) -> usize {
        self.mapping.len()
    }

    pub fn is_empty(&self) -> bool {
        self.mapping.is_empty()
    }

    pub fn get(&self, id: &Key) -> Option<&Child> {
        self.mapping.get(id).and_then(|idx| self.contents.get(*idx))
    }

    pub fn get_mut(&mut self, id: &Key) -> Option<&mut Child> {
        self.mapping
            .get(id)
            .and_then(|idx| self.contents.get_mut(*idx))
    }

    pub fn ids(&self) -> impl Iterator<Item = &Key> {
        self.mapping.keys()
    }

    pub fn for_each(&self, mut f: impl FnMut(&Key, &Child)) {
        for (id, idx) in self.mapping.iter() {
            f(id, &self.contents[*idx])
        }
    }

    pub fn for_each_mut(&mut self, mut f: impl FnMut(&Key, &mut Child)) {
        for (id, idx) in self.mapping.iter() {
            f(id, &mut self.contents[*idx])
        }
    }
}

impl<Child: Component, Element: AnyElement, Key: Eq + Hash> Index<Key>
    for DomSet<Child, Element, Key>
{
    type Output = Child;

    fn index(&self, id: Key) -> &Child {
        &self.contents[self.mapping[&id]]
    }
}

impl<Child: Component, Element: AnyElement, Key: Eq + Hash> IndexMut<Key>
    for DomSet<Child, Element, Key>
{
    fn index_mut(&mut self, id: Key) -> &mut Child {
        &mut self.contents[self.mapping[&id]]
    }
}

impl<Child: Component, Element: AnyElement, Key> Component for DomSet<Child, Element, Key> {
    #[cfg(debug_assertions)]
    fn audit(&self) {
        self.contents.audit();
    }
}

impl<Child: Component, Element: AnyElement, Key> WithElement for DomSet<Child, Element, Key> {
    type Element = Element;
    fn with_element<T, F: FnMut(&Element) -> T>(&self, f: F, g: AccessToken) -> T {
        self.contents.with_element(f, g)
    }
}
