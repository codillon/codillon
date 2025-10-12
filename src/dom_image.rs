// A Codillon image (SVG) component

use crate::jet::{AccessToken, Component, ElementHandle, WithElement};
use web_sys::SvgElement;

pub struct DomImage {
    elem: ElementHandle<SvgElement>,
}

impl DomImage {
    pub fn new(elem: ElementHandle<SvgElement>) -> Self {
        Self { elem }
    }
}

impl WithElement for DomImage {
    type Element = SvgElement;
    fn with_element(&self, f: impl FnMut(&SvgElement), g: AccessToken) {
        self.elem.with_element(f, g)
    }
}

impl Component for DomImage {
    fn audit(&self) {
        self.elem.audit();
    }
}
