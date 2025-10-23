// A Codillon SVG component

use crate::{
    dom_struct::DomStruct,
    dom_switch::DomSwitch,
    dom_vec::DomVec,
    jet::{AccessToken, Component, ElementFactory, ElementHandle, WithElement},
    utils::FrameInfo,
};
use delegate::delegate;
use web_sys::{SvgCircleElement, SvgElement, SvgLineElement, SvggElement};

struct FrameLine {
    start_idx: usize,
    end_idx: usize,
    indent: usize,
    unclosed: bool,
    display: DomStruct<
        (
            ElementHandle<SvgLineElement>,
            (DomSwitch<ElementHandle<SvgCircleElement>, SvggElement>, ()),
        ),
        SvggElement,
    >,
}

impl FrameLine {
    fn new(factory: &ElementFactory, start_idx: usize, end_idx: usize, indent: usize) -> Self {
        let mut ret = Self {
            start_idx,
            end_idx,
            indent,
            unclosed: false,
            display: DomStruct::new(
                (
                    factory.svg_line(),
                    (DomSwitch::new(factory.svg_circle(), factory.svg_g()), ()),
                ),
                factory.svg_g(),
            ),
        };

        let circle = &mut ret.display.get_mut().1.0.inner_mut();
        circle.set_attribute("cx", "0");
        circle.set_attribute("cy", "0");
        circle.set_attribute("r", "5");
        circle.set_attribute("fill", "white");
        circle.set_attribute("stroke", "darkred");
        circle.set_attribute("stroke-width", "2");

        let line = &mut ret.display.get_mut().0;
        line.set_attribute("stroke", "darkgray");
        line.set_attribute("stroke-width", "3px");
        line.set_attribute("x1", "0px");
        line.set_attribute("x2", "0px");
        line.set_attribute("y1", "0px");
        line.set_attribute("y2", "100px");

        ret.reconcile();

        ret
    }

    fn update(&mut self, info: &FrameInfo) {
        self.start_idx = info.start;
        self.end_idx = info.end;
        self.indent = info.indent;
        self.unclosed = info.unclosed;
        self.reconcile();
    }

    fn reconcile(&mut self) {
        let mut total_len = 40.0 * (self.end_idx - self.start_idx - 1) as f64;
        if total_len > 0.0 {
            total_len -= 12.0;
        }
        if self.unclosed {
            total_len += 8.0;
        }
        let current_len = 100.0;

        self.display.get_mut().0.set_attribute(
            "style",
            &format!(
                "transform: translateX({}px) translateY({}px) scaleY({});",
                92 + self.indent * 25,
                40 + self.start_idx * 40 + 12,
                total_len / current_len
            ),
        );

        if self.unclosed {
            self.display.get_mut().1.0.inner_mut().set_attribute(
                "style",
                &format!(
                    "transform: translateX({}px) translateY({}px);",
                    92 + self.indent * 25,
                    self.end_idx * 40 + 12,
                ),
            );
            self.display.get_mut().1.0.activate();
        } else {
            self.display.get_mut().1.0.deactivate();
        }
    }
}

impl WithElement for FrameLine {
    type Element = SvggElement;
    fn with_element(&self, f: impl FnMut(&Self::Element), g: AccessToken) {
        self.display.with_element(f, g)
    }
}

impl Component for FrameLine {
    fn audit(&self) {
        self.display.audit();
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

    pub fn set_frame(&mut self, frame: &FrameInfo) {
        self.make_height_at_least(frame.end + 1);
        match self.contents.get_mut(frame.num) {
            Some(FrameLine {
                start_idx,
                end_idx,
                indent,
                unclosed,
                ..
            }) if frame.start == *start_idx
                && frame.end == *end_idx
                && frame.indent == *indent
                && frame.unclosed == *unclosed => {}
            Some(frame_line) => frame_line.update(frame),
            None => {
                while self.contents.len() <= frame.num {
                    self.contents.push(FrameLine::new(&self.factory, 0, 1, 0));
                }
                self.contents.get_mut(frame.num).unwrap().update(frame)
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
