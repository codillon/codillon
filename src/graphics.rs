// A Codillon SVG component

use crate::{
    dom_vec::DomVec,
    jet::{AccessToken, Component, ElementFactory, ElementHandle, WithElement},
};
use delegate::delegate;
use web_sys::{SvgElement, SvgLineElement};

struct FrameLine {
    start_idx: usize,
    end_idx: usize,
    indent: usize,
    line: ElementHandle<SvgLineElement>,
}

impl FrameLine {
    fn new(factory: &ElementFactory, start_idx: usize, end_idx: usize, indent: usize) -> Self {
        let mut ret = Self {
            start_idx,
            end_idx,
            indent,
            line: factory.svg_line(),
        };

        ret.line.set_attribute("stroke", "darkgray");
        ret.line.set_attribute("stroke-width", "3px");
        ret.reconcile();

        ret
    }

    fn reconcile(&mut self) {
        let x_pos = 95 + self.indent * 25;

        self.line.set_attribute("x1", &format!("{x_pos}px"));
        self.line.set_attribute("x2", &format!("{x_pos}px"));
        self.line
            .set_attribute("y1", &format!("{}px", (self.start_idx + 1) * 40));
        self.line
            .set_attribute("y2", &format!("{}px", (self.start_idx + 2) * 40));

        self.line.set_attribute(
            "style",
            &format!(
                "transform: scaleY({});",
                (self.end_idx - self.start_idx - 1)
            ),
        );
    }
}

impl WithElement for FrameLine {
    type Element = SvgLineElement;
    fn with_element(&self, f: impl FnMut(&Self::Element), g: AccessToken) {
        self.line.with_element(f, g)
    }
}

impl Component for FrameLine {
    fn audit(&self) {
        self.line.audit();
    }
}

pub struct DomImage {
    contents: DomVec<FrameLine, SvgElement>,
    height: usize,
    factory: ElementFactory,
}

impl WithElement for DomImage {
    type Element = SvgElement;
    fn with_element(&self, f: impl FnMut(&Self::Element), g: AccessToken) {
        self.contents.with_element(f, g)
    }
}

impl Component for DomImage {
    fn audit(&self) {
        self.contents.audit();
    }
}

impl DomImage {
    pub fn new(factory: ElementFactory) -> Self {
        Self {
            contents: DomVec::new(factory.svg()),
            height: 0,
            factory,
        }
    }

    fn make_height_at_least(&mut self, height: usize) {
        if height > self.height {
            self.height = height;
            self.contents
                .set_attribute("height", &format!("{}px", height * 40));
        }
    }

    pub fn set_frame(&mut self, frame_num: usize, indentation: usize, start: usize, end: usize) {
        self.make_height_at_least(end + 1);
        match self.contents.get_mut(frame_num) {
            Some(FrameLine {
                start_idx,
                end_idx,
                indent,
                ..
            }) if start == *start_idx && end == *end_idx && indentation == *indent => {}
            Some(frame_line) => {
                frame_line.start_idx = start;
                frame_line.end_idx = end;
                frame_line.indent = indentation;
                frame_line.reconcile();
            }
            None => {
                while self.contents.len() <= frame_num {
                    self.contents.push(FrameLine::new(&self.factory, 0, 1, 0));
                }
                let f = &mut self.contents.get_mut(frame_num).unwrap();
                f.start_idx = start;
                f.end_idx = end;
                f.indent = indentation;
                f.reconcile();
            }
        }
    }

    pub fn set_frame_count(&mut self, count: usize) {
        self.contents.truncate(count);
    }

    delegate! {
    to self.contents {
    pub fn set_attribute(&mut self, name: &str, value: &str);
    pub fn remove_attribute(&mut self, name: &str);
    pub fn get_attribute(&self, name: &str) -> Option<&String>;
    }
    }
}
