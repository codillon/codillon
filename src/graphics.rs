// The "graphics" component of the Codillon editor. This displays frame boundaries and will eventually
// include the dataflow.

use crate::{
    dom_struct::DomStruct,
    dom_vec::DomVec,
    editor::LINE_SPACING,
    jet::{AccessToken, Component, ElementFactory, ElementHandle, WithElement},
    syntax::InstrKind,
    utils::FrameInfo,
};
use delegate::delegate;
use std::cmp::max;
use web_sys::{
    SvgDefsElement, SvgElement, SvgLineElement, SvgPathElement, SvgUseElement, SvggElement,
};

// One "line" representing a Wasm frame boundary.
type DomLine = DomStruct<
    (
        ElementHandle<SvgLineElement>,      // the line itself
        (ElementHandle<SvgUseElement>, ()), // an optional symbol for "unclosed" frames
    ),
    SvggElement,
>;

// Store the FrameInfo alongside each line so that it can skip updates if there is no change.
struct FrameLine {
    info: Option<FrameInfo>,
    elem: DomLine,
}

impl FrameLine {
    fn new(factory: &ElementFactory) -> Self {
        let mut ret = Self {
            info: None,
            elem: DomStruct::new(
                (factory.svg_line(), (factory.svg_use(), ())),
                factory.svg_g(),
            ),
        };

        // Place the "unclosed" symbol and line at standardized coordinates (0,0),
        // so we can use a (possibly animated) CSS transform to move them to their real locations.
        let symbol = &mut ret.elem.get_mut().1.0;
        symbol.set_attribute("x", "0");
        symbol.set_attribute("y", "0");
        symbol.set_attribute("class", "annotation");

        let line = &mut ret.elem.get_mut().0;
        line.set_attribute("x1", "0px");
        line.set_attribute("x2", "0px");
        line.set_attribute("y1", "0px");
        line.set_attribute("y2", "100px");
        line.set_attribute("stroke-width", "3px");
        line.set_attribute("class", "annotation");

        ret
    }

    // Make the DOM SVG element reflect the new Wasm FrameInfo that it represents.
    // Store the "info" in the FrameLine so that we can short-circuit future updates if there is no change.
    fn update(&mut self, info: FrameInfo) {
        if let Some(existing_info) = &self.info
            && *existing_info == info
        {
            return;
        }

        let mut total_len = LINE_SPACING as f64 * (max(1, info.end - info.start) - 1) as f64;
        if total_len > 0.0 {
            total_len -= 12.0;
            if info.unclosed {
                total_len += 8.0;
            }
        }
        let current_len = 100.0;

        self.elem.get_mut().0.set_attribute(
            "style",
            &format!(
                "transform: translateX({}px) translateY({}px) scaleY({});",
                92 + info.indent * 25,
                (info.start + 1) * LINE_SPACING + 12,
                total_len / current_len
            ),
        );

        if info.unclosed {
            self.elem.get_mut().1.0.set_attribute(
                "style",
                &format!(
                    "transform: translateX({}px) translateY({}px);",
                    92 + info.indent * 25,
                    info.end * LINE_SPACING + 12,
                ),
            );
            self.elem.get_mut().1.0.set_attribute("href", "#unclosed");
        } else {
            self.elem.get_mut().1.0.remove_attribute("href");
        }

        self.elem.get_mut().0.set_attribute(
            "stroke",
            match info.kind {
                InstrKind::If => "green",
                InstrKind::Else => "blue",
                _ => "darkgray",
            },
        );
    }
}

impl WithElement for FrameLine {
    type Element = SvggElement;
    fn with_element(&self, f: impl FnMut(&Self::Element), g: AccessToken) {
        self.elem.with_element(f, g)
    }
}

impl Component for FrameLine {
    fn audit(&self) {
        self.elem.audit();
    }
}

type SVGDefs = DomVec<ElementHandle<SvgPathElement>, SvgDefsElement>; // definitions in SVG header
type CodillonBlocks = DomVec<FrameLine, SvggElement>; // the lines themselves
type CodillonSVG = DomStruct<(SVGDefs, (CodillonBlocks, ())), SvgElement>;

pub struct DomImage {
    contents: CodillonSVG,
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
    fn blocks(&self) -> &CodillonBlocks {
        &self.contents.get().1.0
    }

    fn blocks_mut(&mut self) -> &mut CodillonBlocks {
        &mut self.contents.get_mut().1.0
    }

    pub fn new(factory: ElementFactory) -> Self {
        let mut icon = factory.svg_path();

        // The "unclosed" symbol looks like a ⊘ (Circled Division Slash)
        // character, or like an "End of All Prohibitions"
        // European road sign.

        // Put it in the defs so it can be referenced repeatedly when needed.
        icon.set_attribute("d", "M 5.1970835,0 C 5.1970835,2.87027 2.87027,5.1970835 0,5.1970835 -2.87027,5.1970835 -5.1970835,2.87027 -5.1970835,0 -5.1970835,-2.87027 -2.87027,-5.1970835 0,-5.1970835 2.87027,-5.1970835 5.1970835,-2.87027 5.1970835,0 Z M 3.6812636,-3.6812727 -3.681272,3.6812676");
        icon.set_attribute("fill", "white");
        icon.set_attribute("stroke", "darkred");
        icon.set_attribute("stroke-width", "2");
        icon.set_attribute("id", "unclosed");

        let mut ret = Self {
            contents: CodillonSVG::new(
                (
                    SVGDefs::new(factory.svg_defs()),
                    (CodillonBlocks::new(factory.svg_g()), ()),
                ),
                factory.svg(),
            ),
            height: 0,
            factory,
        };

        ret.contents.get_mut().0.push(icon);

        ret
    }

    fn make_height_at_least(&mut self, height: usize) {
        if height > self.height {
            self.height = height;
            self.contents
                .set_attribute("height", &format!("{}px", height * LINE_SPACING));
        }
    }

    pub fn set_frame(&mut self, num: usize, frame: FrameInfo) {
        self.make_height_at_least(frame.end + 1);
        match self.blocks_mut().get_mut(num) {
            Some(frame_line) => frame_line.update(frame),
            None => {
                while self.blocks().len() <= num {
                    let blocks = &mut self.contents.get_mut().1.0;
                    blocks.push(FrameLine::new(&self.factory));
                }
                self.blocks_mut().get_mut(num).unwrap().update(frame)
            }
        }
    }

    pub fn set_frame_count(&mut self, count: usize) {
        self.blocks_mut().truncate(count);
    }

    delegate! {
    to self.contents {
    pub fn set_attribute(&mut self, name: &str, value: &str);
    pub fn remove_attribute(&mut self, name: &str);
    pub fn get_attribute(&self, name: &str) -> Option<&String>;
    }
    }
}
