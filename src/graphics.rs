// A Codillon SVG component

use crate::jet::{AccessToken, Component, ElementFactory, ElementHandle, WithElement};
use delegate::delegate;
use web_sys::{SvgElement, SvgLineElement};

pub struct DomImage {
    elem: ElementHandle<SvgElement>,
    lines: Vec<ElementHandle<SvgLineElement>>,
}

impl DomImage {
    pub fn new(factory: &ElementFactory) -> Self {
        let mut ret = Self {
            elem: factory.svg(),
            lines: Vec::new(),
        };

        let mut line = factory.svg_line();
        line.set_attribute("x1", "50");
        line.set_attribute("x2", "200");
        line.set_attribute("y1", "50");
        line.set_attribute("y2", "200");
        line.set_attribute("stroke", "purple");
        line.set_attribute("stroke-width", "15");
        line.set_attribute("stroke-opacity", "20%");
        ret.lines.push(line);

        ret.elem.attach_node(&ret.lines[0]);

        ret
    }

    delegate! {
    to self.elem {
    pub fn set_attribute(&mut self, name: &str, value: &str);
    pub fn remove_attribute(&mut self, name: &str);
    pub fn get_attribute(&self, name: &str) -> Option<&String>;
    }
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
