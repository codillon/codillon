// A Codillon "switchable" component -- contains an inner ElementComponent that is conditionally displayed.

use crate::jet::{
    AccessToken, AnyElement, Component, ElementComponent, ElementHandle, WithElement,
};

pub struct DomSwitch<T: ElementComponent, Element: AnyElement> {
    active: bool,
    inner: T,
    elem: ElementHandle<Element>,
}

impl<T: ElementComponent, Element: AnyElement> DomSwitch<T, Element> {
    pub fn new(val: T, elem: ElementHandle<Element>) -> Self {
        Self {
            active: false,
            inner: val,
            elem,
        }
    }

    pub fn activate(&mut self) {
        if !self.active {
            self.elem.attach_node(&self.inner);
            self.active = true;
        }
    }

    pub fn deactivate(&mut self) {
        if self.active {
            self.inner.remove();
            self.active = false;
        }
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

impl<T: ElementComponent, Element: AnyElement> WithElement for DomSwitch<T, Element> {
    type Element = Element;
    fn with_element(&self, f: impl FnMut(&Self::Element), g: AccessToken) {
        self.elem.with_element(f, g)
    }
}

impl<T: ElementComponent, Element: AnyElement> Component for DomSwitch<T, Element> {
    fn audit(&self) {
        self.elem.audit();
        let dom_children = self.elem.get_child_node_list();
        if self.active {
            assert_eq!(dom_children.length(), 1);
            dom_children.audit_node(0, &self.inner);
        } else {
            assert_eq!(dom_children.length(), 0);
        }
    }
}
