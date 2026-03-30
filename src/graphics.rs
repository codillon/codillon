// The "graphics" component of the Codillon editor. This displays frame boundaries and will eventually
// include the dataflow.

use crate::{
    dom_struct::DomStruct,
    dom_vec::DomVec,
    editor::LINE_SPACING,
    jet::{AccessToken, Component, ElementFactory, ElementHandle, WithElement, now_ms},
    line::INDENT_PX,
    syntax::{FrameInfo, InstrKind},
    utils::BLOCK_BOUNDARY_INDENT,
};
use anyhow::Result;
use delegate::delegate;
use std::collections::HashMap;
use web_sys::{
    SvgAnimateElement, SvgDefsElement, SvgElement, SvgLineElement, SvgPathElement, SvgUseElement,
    SvggElement,
};

type AnimLine = DomStruct<
    (
        ElementHandle<SvgAnimateElement>,
        (
            ElementHandle<SvgAnimateElement>,
            (
                ElementHandle<SvgAnimateElement>,
                (
                    ElementHandle<SvgAnimateElement>,
                    (ElementHandle<SvgAnimateElement>, ()),
                ),
            ),
        ),
    ),
    SvgLineElement,
>;

// One "line" representing a Wasm frame boundary.
type DomLine = DomStruct<
    (
        AnimLine,
        (
            AnimLine,
            (
                AnimLine,
                (SymbolUse, ()), // an optional symbol for "unclosed" frames
            ),
        ),
    ),
    SvggElement,
>;

type SymbolUse = DomStruct<
    (
        ElementHandle<SvgAnimateElement>,
        (
            ElementHandle<SvgAnimateElement>,
            (ElementHandle<SvgAnimateElement>, ()),
        ),
    ),
    SvgUseElement,
>;

impl SymbolUse {
    fn anim_x(&mut self) -> &mut ElementHandle<SvgAnimateElement> {
        &mut self.get_mut().0
    }

    fn anim_y(&mut self) -> &mut ElementHandle<SvgAnimateElement> {
        &mut self.get_mut().1.0
    }

    fn anim_opacity(&mut self) -> &mut ElementHandle<SvgAnimateElement> {
        &mut self.get_mut().1.1.0
    }

    fn new_symbol(factory: &ElementFactory) -> Self {
        let mut ret = DomStruct::new(
            (
                factory.svg_animate(),
                (factory.svg_animate(), (factory.svg_animate(), ())),
            ),
            factory.svg_use(),
        );

        ret.set_attribute("href", "#unclosed");
        ret.set_attribute("opacity", "0");

        setup_anim(ret.anim_x(), "x");
        setup_anim(ret.anim_y(), "y");
        setup_anim(ret.anim_opacity(), "opacity");

        ret
    }

    fn snapshot(&mut self) {
        let x = self.elem().x().anim_val().value().unwrap().to_string();
        let y = self.elem().y().anim_val().value().unwrap().to_string();
        self.anim_x().set_attribute("from", &x);
        self.anim_y().set_attribute("from", &y);
        let old_opacity = self
            .anim_opacity()
            .get_attribute("to")
            .unwrap_or("0")
            .to_string();

        self.anim_opacity().set_attribute("from", &old_opacity);
    }

    fn set_visibility(&mut self, smooth: bool, visible: bool) {
        if smooth {
            self.snapshot();
            self.anim_opacity()
                .set_attribute("to", if visible { "1" } else { "0" });
            let _ = self.anim_opacity().begin_element();
        } else {
            self.set_attribute("opacity", if visible { "1" } else { "0" });
        }
    }

    fn goto(&mut self, smooth: bool, x: usize, y: usize) {
        if smooth {
            self.snapshot();
            self.anim_x().set_attribute("to", &x.to_string());
            self.anim_y().set_attribute("to", &y.to_string());
            let _ = self.anim_x().begin_element();
            let _ = self.anim_y().begin_element();
        } else {
            self.anim_x().remove_attribute("to");
            self.anim_y().remove_attribute("to");
        }

        self.set_attribute("x", &x.to_string());
        self.set_attribute("y", &y.to_string());
    }
}
// Store the FrameInfo alongside each line so that it can skip updates if there is no change.
struct FrameLine {
    info: Option<FrameInfo>,
    elem: DomLine,
}

const X_OFFSET_PX: usize = 81;
const LINE_OFFSET_PX: usize = 8;
const WIDTH: usize = 4;
const MARGIN: usize = 8;

struct FrameLimits {
    x_left: usize,
    x_right: usize,
    y_top: usize,
    y_bot: usize,
}

impl FrameLimits {
    fn new(info: &FrameInfo) -> Self {
        Self {
            x_left: X_OFFSET_PX + INDENT_PX * info.indent,
            x_right: X_OFFSET_PX + INDENT_PX * (info.indent + BLOCK_BOUNDARY_INDENT) - MARGIN,
            y_top: info.start * LINE_SPACING + LINE_SPACING / 2 + LINE_OFFSET_PX,
            y_bot: info.end * LINE_SPACING + LINE_SPACING / 2 + LINE_OFFSET_PX,
        }
    }
}

fn setup_anim(anim: &mut ElementHandle<SvgAnimateElement>, attr: &str) {
    anim.set_attribute("attributeName", attr);
    anim.set_attribute("dur", "200ms");
    anim.set_attribute("fill", "freeze");
}

impl AnimLine {
    fn new_line(factory: &ElementFactory) -> Self {
        let mut ret = DomStruct::new(
            (
                factory.svg_animate(),
                (
                    factory.svg_animate(),
                    (
                        factory.svg_animate(),
                        (factory.svg_animate(), (factory.svg_animate(), ())),
                    ),
                ),
            ),
            factory.svg_line(),
        );
        ret.set_attribute("stroke-width", &format!("{WIDTH}px"));
        ret.set_attribute("stroke-opacity", "0");

        setup_anim(ret.anim_x1(), "x1");
        setup_anim(ret.anim_x2(), "x2");
        setup_anim(ret.anim_y1(), "y1");
        setup_anim(ret.anim_y2(), "y2");
        setup_anim(ret.anim_opacity(), "stroke-opacity");

        ret
    }

    fn anim_x1(&mut self) -> &mut ElementHandle<SvgAnimateElement> {
        &mut self.get_mut().0
    }

    fn anim_x2(&mut self) -> &mut ElementHandle<SvgAnimateElement> {
        &mut self.get_mut().1.0
    }

    fn anim_y1(&mut self) -> &mut ElementHandle<SvgAnimateElement> {
        &mut self.get_mut().1.1.0
    }

    fn anim_y2(&mut self) -> &mut ElementHandle<SvgAnimateElement> {
        &mut self.get_mut().1.1.1.0
    }

    fn anim_opacity(&mut self) -> &mut ElementHandle<SvgAnimateElement> {
        &mut self.get_mut().1.1.1.1.0
    }

    fn set(&mut self, name: &str, val: usize) {
        self.set_attribute(name, &val.to_string());
    }

    fn snapshot(&mut self) {
        let x1 = self.elem().x1().anim_val().value().unwrap().to_string();
        let x2 = self.elem().x2().anim_val().value().unwrap().to_string();
        let y1 = self.elem().y1().anim_val().value().unwrap().to_string();
        let y2 = self.elem().y2().anim_val().value().unwrap().to_string();
        self.anim_x1().set_attribute("from", &x1);
        self.anim_x2().set_attribute("from", &x2);
        self.anim_y1().set_attribute("from", &y1);
        self.anim_y2().set_attribute("from", &y2);

        let old_opacity = self
            .anim_opacity()
            .get_attribute("to")
            .unwrap_or("0")
            .to_string();

        self.anim_opacity().set_attribute("from", &old_opacity);
    }

    fn vert(&mut self, smooth: bool, x: usize, y1: usize, y2: usize) {
        if smooth {
            self.snapshot();
            self.anim_x1().set_attribute("to", &x.to_string());
            self.anim_x2().set_attribute("to", &x.to_string());
            self.anim_y1().set_attribute("to", &y1.to_string());
            self.anim_y2().set_attribute("to", &y2.to_string());
            let _ = self.anim_x1().begin_element();
            let _ = self.anim_x2().begin_element();
            let _ = self.anim_y1().begin_element();
            let _ = self.anim_y2().begin_element();
        } else {
            self.anim_x1().remove_attribute("to");
            self.anim_x2().remove_attribute("to");
            self.anim_y1().remove_attribute("to");
            self.anim_y2().remove_attribute("to");
        }

        self.set("x1", x);
        self.set("x2", x);
        self.set("y1", y1);
        self.set("y2", y2);
    }

    fn horiz(&mut self, smooth: bool, x1: usize, x2: usize, y: usize) {
        if smooth {
            self.snapshot();
            self.anim_x1().set_attribute("to", &x1.to_string());
            self.anim_x2().set_attribute("to", &x2.to_string());
            self.anim_y1().set_attribute("to", &y.to_string());
            self.anim_y2().set_attribute("to", &y.to_string());
            let _ = self.anim_x1().begin_element();
            let _ = self.anim_x2().begin_element();
            let _ = self.anim_y1().begin_element();
            let _ = self.anim_y2().begin_element();
        } else {
            self.anim_x1().remove_attribute("to");
            self.anim_x2().remove_attribute("to");
            self.anim_y1().remove_attribute("to");
            self.anim_y2().remove_attribute("to");
        }

        self.set("x1", x1);
        self.set("x2", x2);
        self.set("y1", y);
        self.set("y2", y);
    }

    fn set_visibility(&mut self, smooth: bool, visible: bool) {
        if smooth {
            self.snapshot();
            self.anim_opacity()
                .set_attribute("to", if visible { "1" } else { "0" });
            let _ = self.anim_opacity().begin_element();
        } else {
            self.set_attribute("stroke-opacity", if visible { "1" } else { "0" });
        }
    }
}

impl FrameLine {
    fn line1(&mut self) -> &mut AnimLine {
        &mut self.elem.get_mut().0
    }

    fn line2(&mut self) -> &mut AnimLine {
        &mut self.elem.get_mut().1.0
    }

    fn line3(&mut self) -> &mut AnimLine {
        &mut self.elem.get_mut().1.1.0
    }

    fn symbol(&mut self) -> &mut SymbolUse {
        &mut self.elem.get_mut().1.1.1.0
    }

    fn new(factory: &ElementFactory) -> Self {
        Self {
            info: None,
            elem: DomStruct::new(
                (
                    AnimLine::new_line(factory),
                    (
                        AnimLine::new_line(factory),
                        (
                            AnimLine::new_line(factory),
                            (SymbolUse::new_symbol(factory), ()),
                        ),
                    ),
                ),
                factory.svg_g(),
            ),
        }
    }

    // Make the DOM SVG element reflect the new Wasm FrameInfo that it represents.
    // Store the "info" in the FrameLine so that we can short-circuit future updates if there is no change.
    fn update(&mut self, info: FrameInfo, mut smooth: bool) -> Result<()> {
        if let Some(current_info) = &self.info {
            if info == *current_info {
                return Ok(());
            }

            if info.kind == current_info.kind
                && info.indent == current_info.indent
                && info.unclosed == current_info.unclosed
                && info.end - info.start == current_info.end - current_info.start
            {
                smooth = false;
            }
        }

        let FrameLimits {
            x_left,
            x_right,
            y_top,
            y_bot,
        } = FrameLimits::new(&info);

        if info.kind == InstrKind::Else {
            self.line1()
                .horiz(smooth, x_left - WIDTH / 2, x_left - WIDTH / 2, y_top);
        } else {
            self.line1()
                .horiz(smooth, x_right, x_left - WIDTH / 2, y_top);
        }
        self.line2().vert(smooth, x_left, y_top, y_bot);
        self.symbol().goto(smooth, x_left, y_bot);

        if info.unclosed {
            self.line3()
                .horiz(smooth, x_left - WIDTH / 2, x_left - WIDTH / 2, y_bot);
        } else {
            self.line3()
                .horiz(smooth, x_left - WIDTH / 2, x_right, y_bot);
        }

        self.set_color(&info);
        self.info = Some(info);

        Ok(())
    }

    fn set_color(&mut self, info: &FrameInfo) {
        match info.kind {
            InstrKind::OtherStructured => {
                self.line1().set_attribute("stroke", "darkgray");
                self.line2().set_attribute("stroke", "darkgray");
                self.line3().set_attribute("stroke", "darkgray");
            }
            InstrKind::If => {
                self.line1().set_attribute("stroke", "green");
                self.line2().set_attribute("stroke", "green");
                self.line3().set_attribute("stroke", "purple");
            }
            InstrKind::Else => {
                self.line1().set_attribute("stroke", "purple");
                self.line2().set_attribute("stroke", "blue");
                self.line3().set_attribute("stroke", "blue");
            }
            InstrKind::Loop => {
                self.line1().set_attribute("stroke", "darkred");
                self.line2().set_attribute("stroke", "darkred");
                self.line3().set_attribute("stroke", "darkred");
            }
            InstrKind::Other | InstrKind::End => panic!("unexpected frame kind"),
        }
    }

    fn set_visibility(&mut self, smooth: bool, visible: bool, unclosed: bool) {
        self.line1().set_visibility(smooth, visible);
        self.line2().set_visibility(smooth, visible);
        self.line3().set_visibility(smooth, visible);
        self.symbol().set_visibility(smooth, unclosed);
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
    width: usize,
    factory: ElementFactory,
    frame_map: HashMap<u32, usize>,    // frame identity -> block idx
    pending_delete: HashMap<u32, f64>, // frame identity -> timestamp to delete
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
            width: 0,
            factory,
            frame_map: Default::default(),
            pending_delete: Default::default(),
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

    fn make_width_at_least(&mut self, width: usize) {
        if width > self.width {
            self.width = width;
            self.contents.set_attribute(
                "width",
                &format!(
                    "{}px",
                    X_OFFSET_PX + (width + BLOCK_BOUNDARY_INDENT) * INDENT_PX
                ),
            );
        }
    }

    pub fn set_frames(&mut self, frames: HashMap<u32, FrameInfo>, mut smooth: bool) {
        let now = now_ms();

        /* should we force a smooth transition? */
        if frames.len() != self.blocks().len() - self.pending_delete.len() {
            smooth = true;
        }
        for (id, info) in &frames {
            if let Some(idx) = self.frame_map.get(id) {
                let old_info = &self.blocks().get(*idx).unwrap().info;
                if let Some(old_info) = old_info
                    && (old_info.indent != info.indent
                        || old_info.unclosed != info.unclosed
                        || old_info.kind != info.kind)
                {
                    smooth = true;
                }
            } else {
                smooth = true;
            }
        }

        /* delete frames whose vanishing animations ought to have finished */
        for (id, _ts) in self
            .pending_delete
            .extract_if(|id, ts| *ts > now || frames.contains_key(id))
        {
            let idx = self.frame_map[&id];
            self.contents.get_mut().1.0.remove(idx);
            self.frame_map.remove(&id);
            for other_idx in self.frame_map.values_mut() {
                if *other_idx >= idx {
                    *other_idx -= 1;
                }
            }
        }

        /* smoothly vanish frames that no longer exist */
        {
            let mut to_vanish: Vec<(u32, usize)> = vec![];
            for (id, idx) in &self.frame_map {
                if !frames.contains_key(id) {
                    to_vanish.push((*id, *idx));
                }
            }
            for (id, idx) in to_vanish {
                self.blocks_mut()
                    .get_mut(idx)
                    .unwrap()
                    .set_visibility(true, false, false);
                self.pending_delete.insert(id, now + 1000.0);
            }
        }

        /* update existing frames and add new ones */
        for frame in frames {
            self.make_height_at_least(frame.1.end + 1);
            let index = self.frame_map.get(&frame.0).copied();

            if let Some(idx) = index {
                self.make_width_at_least(frame.1.indent);
                // update existing
                let last_block = self.blocks_mut().get_mut(idx).unwrap();
                let unclosed = frame.1.unclosed;
                last_block.set_visibility(true, true, unclosed);
                last_block.update(frame.1, smooth).unwrap();
            } else {
                // add new frame
                let idx = self.blocks().len();
                self.make_width_at_least(frame.1.indent);
                let blocks = &mut self.contents.get_mut().1.0;
                blocks.push(FrameLine::new(&self.factory));
                let last_block = blocks.last_mut().unwrap();
                let unclosed = frame.1.unclosed;
                last_block.set_visibility(false, false, false);
                last_block.update(frame.1, false).unwrap();
                last_block.set_visibility(true, true, unclosed);
                self.frame_map.insert(frame.0, idx);
            }
        }
    }

    delegate! {
    to self.contents {
    pub fn set_attribute(&mut self, name: &str, value: &str);
    pub fn remove_attribute(&mut self, name: &str);
    pub fn get_attribute(&self, name: &str) -> Option<&str>;
    }
    }
}
