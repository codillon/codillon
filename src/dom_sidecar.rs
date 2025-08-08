// A Codillon DOM component + a "sidecar" of anything else.

use crate::web_support::{AccessToken, Component, ElementComponent, WithElement, WithNode};

pub struct DomSidecar<InnerComponent: Component, T> {
    component: InnerComponent,
    sidecar: T,
}

impl<InnerComponent: Component + Default, T: Default> Default for DomSidecar<InnerComponent, T> {
    fn default() -> Self {
        Self {
            component: InnerComponent::default(),
            sidecar: T::default(),
        }
    }
}

impl<InnerComponent: Component, T> DomSidecar<InnerComponent, T> {
    pub fn new(component: InnerComponent, sidecar: T) -> Self {
        Self { component, sidecar }
    }

    pub fn borrow_component(&self) -> &InnerComponent {
        &self.component
    }

    pub fn borrow_sidecar(&self) -> &T {
        &self.sidecar
    }

    pub fn borrow_component_mut(&mut self) -> &mut InnerComponent {
        &mut self.component
    }

    pub fn borrow_sidecar_mut(&mut self) -> &mut T {
        &mut self.sidecar
    }
}

impl<InnerComponent: Component, T> WithNode for DomSidecar<InnerComponent, T> {
    fn with_node(&self, f: impl FnMut(&web_sys::Node), g: AccessToken) {
        self.component.with_node(f, g)
    }
}

impl<InnerComponent: Component, T> Component for DomSidecar<InnerComponent, T> {
    fn audit(&self) {
        self.component.audit()
    }
}

impl<InnerComponent: ElementComponent, T> WithElement for DomSidecar<InnerComponent, T> {
    type Element = InnerComponent::Element;
    fn with_element(&self, f: impl FnMut(&Self::Element), g: AccessToken) {
        self.component.with_element(f, g)
    }
}
