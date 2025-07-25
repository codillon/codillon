use std::ops::{Deref, DerefMut};
use web_sys::{self, Document, window};

/// As a wraper for `Element` in web_sys
pub trait Component {
    fn node_ref(&self) -> impl AsRef<web_sys::Element>;

    fn drop_dom(&self) {
        self.node_ref().as_ref().remove();
    }

    /// Will not essentially update all children's dom. Responsible for its own part.
    fn update_dom(&self) {}

    /// Insert as the index-th child of the this component.
    /// If the index is too big, we will just insert it at the end of the component
    fn dom_insert_child(&self, index: usize, sub_node: &impl Component) {
        let self_node = self.node_ref();
        let self_node_ref = self_node.as_ref();
        let child_node = self_node_ref.child_nodes().item(index as u32);
        self_node_ref
            .insert_before(sub_node.node_ref().as_ref(), child_node.as_ref())
            .expect("Insert Child to htmlElement");
    }
}

/// As a mut reference guard for compenent, will update the dom when the mut ref is being dropped
#[derive(Debug)]
pub struct ComponentGuard<'a, T: Component> {
    inner: &'a mut T,
}

impl<'a, T: Component> ComponentGuard<'a, T> {
    pub fn new(component: &'a mut T) -> Self {
        ComponentGuard { inner: component }
    }
}

impl<'a, T: Component> Deref for ComponentGuard<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.inner
    }
}

impl<'a, T: Component> DerefMut for ComponentGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner
    }
}

impl<'a, T: Component> Drop for ComponentGuard<'a, T> {
    fn drop(&mut self) {
        self.inner.update_dom();
    }
}

#[derive(Debug)]
pub struct ComponentHolder<T: Component> {
    inner: T,
}

/// This holder forces outer code to use ComponentGuard
impl<T: Component> ComponentHolder<T> {
    pub fn new(component: T) -> Self {
        ComponentHolder { inner: component }
    }

    pub fn ref_mut(&mut self) -> ComponentGuard<T> {
        ComponentGuard::new(&mut self.inner)
    }
}

impl<T: Component> AsRef<T> for ComponentHolder<T> {
    fn as_ref(&self) -> &T {
        &self.inner
    }
}

impl<T: Component> From<T> for ComponentHolder<T> {
    fn from(component: T) -> Self {
        ComponentHolder::new(component)
    }
}

pub fn document() -> Document {
    window().unwrap().document().unwrap()
}

pub fn get_dom_selection() -> web_sys::Selection {
    web_sys::window()
        .unwrap()
        .get_selection()
        .expect("Cannot get dom selection")
        .expect("Dom selection not found")
}
