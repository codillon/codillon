// The "graphics" component of the Codillon editor. This displays frame boundaries and will eventually
// include the dataflow.

use crate::{
    dom_struct::DomStruct,
    dom_vec::DomVec,
    editor::LINE_SPACING,
    jet::{AccessToken, Component, ElementFactory, ElementHandle, WithElement, now_ms},
    line::INDENT_PX,
    syntax::{FrameInfo, InstrKind},
    utils::{BLOCK_BOUNDARY_INDENT, OperandConnections, SlotInfo},
};
use anyhow::Result;
use delegate::delegate;
use itertools::zip_eq;
use palette::{Mix, Srgb};
use std::{collections::HashMap, str::FromStr};
use wasmparser::ValType;
use web_sys::{
    SvgAnimateElement, SvgDefsElement, SvgElement, SvgLineElement, SvgPathElement, SvgUseElement,
    SvggElement,
};

const SYM_HW: f32 = 15.0;

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

    fn snapshot_xy(&mut self) {
        let x = self.elem().x().anim_val().value().unwrap().to_string();
        let y = self.elem().y().anim_val().value().unwrap().to_string();
        self.anim_x().set_attribute("from", &x);
        self.anim_y().set_attribute("from", &y);
    }

    fn snapshot_vis(&mut self) {
        let old_opacity = self
            .anim_opacity()
            .get_attribute("to")
            .unwrap_or("0.5")
            .to_string();
        self.anim_opacity().set_attribute("from", &old_opacity);
    }

    fn set_visibility(&mut self, smooth: bool, visible: bool) {
        if smooth {
            self.snapshot_vis();
        } else {
            self.anim_opacity()
                .set_attribute("from", if visible { "1" } else { "0" });
        }

        self.anim_opacity()
            .set_attribute("to", if visible { "1" } else { "0" });

        let _ = self.anim_opacity().begin_element();
    }

    fn goto(&mut self, smooth: bool, x: usize, y: usize) {
        if smooth {
            self.snapshot_xy();
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

const X_OFFSET_PX: usize = 81 + 4 * INDENT_PX;
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
        let x_offset = if info.indent > 0 {
            X_OFFSET_PX
        } else {
            X_OFFSET_PX - INDENT_PX * BLOCK_BOUNDARY_INDENT
        };
        let top_offset = if info.wide { 0 } else { LINE_SPACING / 2 };
        Self {
            x_left: x_offset + INDENT_PX * info.indent,
            x_right: x_offset + INDENT_PX * (info.indent + BLOCK_BOUNDARY_INDENT) - MARGIN,
            y_top: info.start * LINE_SPACING + top_offset + LINE_OFFSET_PX,
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
                self.line1().set_attribute("stroke", "pink");
                self.line2().set_attribute("stroke", "pink");
                self.line3().set_attribute("stroke", "pink");
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
type Fractions = DomVec<OperatorFraction, SvggElement>;
type Connections = DomVec<ElementHandle<SvgPathElement>, SvggElement>;
type CodillonSVG =
    DomStruct<(SVGDefs, (Connections, (Fractions, (CodillonBlocks, ())))), SvgElement>;

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

fn ty_to_color(ty: &Option<&ValType>) -> &'static str {
    use ValType::*;
    match ty {
        Some(I32) => "#6A5ACD",
        Some(I64) => "#483D8B",
        Some(F32) => "#228B22",
        Some(F64) => "#006400",
        None => "#000000",
        _ => "#000000",
    }
}

fn ty_to_muted(ty: &Option<&ValType>) -> String {
    let orig = ty_to_color(ty);
    let bg = "#FFFFF0";
    let orig_linear = Srgb::from_str(orig).unwrap().into_linear::<f32>();
    let bg_linear = Srgb::from_str(bg).unwrap().into_linear::<f32>();
    let muted = orig_linear.mix(bg_linear, 0.7);
    let final_color: Srgb<u8> = Srgb::from_linear(muted);
    format!("#{final_color:x}")
}

impl DomImage {
    fn blocks(&self) -> &CodillonBlocks {
        &self.contents.get().1.1.1.0
    }

    fn blocks_mut(&mut self) -> &mut CodillonBlocks {
        &mut self.contents.get_mut().1.1.1.0
    }

    fn connections_mut(&mut self) -> &mut Connections {
        &mut self.contents.get_mut().1.0
    }

    fn fractions(&self) -> &Fractions {
        &self.contents.get().1.1.0
    }

    fn fractions_mut(&mut self) -> &mut Fractions {
        &mut self.contents.get_mut().1.1.0
    }

    fn defs_mut(&mut self) -> &mut SVGDefs {
        &mut self.contents.get_mut().0
    }

    fn make_icon(
        factory: &ElementFactory,
        id: &str,
        d: &str,
        fill: &str,
        stroke: &str,
        stroke_width: &str,
    ) -> ElementHandle<SvgPathElement> {
        let mut ret = factory.svg_path();

        ret.set_attribute("id", id);
        ret.set_attribute("d", d);
        ret.set_attribute("fill", fill);
        ret.set_attribute("stroke", stroke);
        ret.set_attribute("stroke-width", stroke_width);
        ret
    }

    fn make_stranded(
        factory: &ElementFactory,
        id: &str,
        d: &str,
        ty: &Option<&ValType>,
    ) -> ElementHandle<SvgPathElement> {
        let mut ret = factory.svg_path();

        ret.set_attribute("id", id);
        ret.set_attribute("d", d);
        ret.set_attribute("fill", "none");
        ret.set_attribute("stroke", &ty_to_muted(ty));
        ret.set_attribute("stroke-width", "8");
        ret.set_attribute("paint-order", "stroke");
        ret
    }

    pub fn new(factory: ElementFactory) -> Self {
        use ValType::*;
        let mut ret = Self {
            contents: CodillonSVG::new(
                (
                    SVGDefs::new(factory.svg_defs()),
                    (
                        Connections::new(factory.svg_g()),
                        (
                            DomVec::new(factory.svg_g()),
                            (CodillonBlocks::new(factory.svg_g()), ()),
                        ),
                    ),
                ),
                factory.svg(),
            ),
            height: 0,
            width: 0,
            factory: factory.clone(),
            frame_map: Default::default(),
            pending_delete: Default::default(),
        };

        // The "unclosed" symbol looks like a ⊘ (Circled Division Slash)
        // character, or like an "End of All Prohibitions"
        // European road sign.
        ret.defs_mut().push(Self::make_icon(
            &factory,
            "unclosed",
            "M 5.1970835,0 C 5.1970835,2.87027 2.87027,5.1970835 0,5.1970835 -2.87027,5.1970835 -5.1970835,2.87027 -5.1970835,0 -5.1970835,-2.87027 -2.87027,-5.1970835 0,-5.1970835 2.87027,-5.1970835 5.1970835,-2.87027 5.1970835,0 Z M 3.6812636,-3.6812727 -3.681272,3.6812676",
            "#fffff0",
            "darkred",
            "2",
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "mystery_in_empty",
	    "M -0,-20
C 0.742,-20 2.506,-13.686 3.106,-13.249
c 0.6,0.437 7.136,0.168 7.365,0.875 0.229,0.707 -5.216,4.341 -5.445,5.048 -0.229,0.707 2.045,6.855 1.445,7.292
C 5.871,0.404 0.742,-3.664 -0,-3.664 -0.742,-3.664 -5.871,0.404 -6.471,-0.033 -7.071,-0.47 -4.797,-6.618 -5.026,-7.325
c -0.229,-0.707 -5.674,-4.341 -5.445,-5.048 0.229,-0.707 6.765,-0.438 7.365,-0.875
C -2.506,-13.686 -0.742,-20 -0,-20
Z",
            "#fffff0",
            "darkred",
            "2",
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "mystery_in",
	    "M -0,-20
C 0.742,-20 2.506,-13.686 3.106,-13.249
c 0.6,0.437 7.136,0.168 7.365,0.875 0.229,0.707 -5.216,4.341 -5.445,5.048 -0.229,0.707 2.045,6.855 1.445,7.292
C 5.871,0.404 0.742,-3.664 -0,-3.664 -0.742,-3.664 -5.871,0.404 -6.471,-0.033 -7.071,-0.47 -4.797,-6.618 -5.026,-7.325
c -0.229,-0.707 -5.674,-4.341 -5.445,-5.048 0.229,-0.707 6.765,-0.438 7.365,-0.875
C -2.506,-13.686 -0.742,-20 -0,-20
Z",
	    &ty_to_muted(&None),
	    ty_to_color(&None),
            "1",
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "mystery_out",
"m 0,0
c 0.742,0 2.506,6.314 3.106,6.751 0.6,0.437 7.136,0.168 7.365,0.875 0.229,0.707 -5.216,4.341 -5.445,5.048 -0.229,0.707 2.045,6.855 1.445,7.292 -0.6,0.438 -5.729,-3.63 -6.471,-3.63 -0.742,0 -5.871,4.068 -6.471,3.631 -0.6,-0.437 1.674,-6.585 1.445,-7.292
C -5.255,11.968 -10.7,8.334 -10.471,7.627 -10.242,6.92 -3.706,7.189 -3.106,6.752 -2.506,6.314 -0.742,0 0,0
Z",
            ty_to_color(&None),
            "white",
            "0.5",
        ));

        ret.defs_mut().push(Self::make_stranded(
            &factory,
            "mystery_out_stranded",
"m 0,0
c 0.742,0 2.506,6.314 3.106,6.751 0.6,0.437 7.136,0.168 7.365,0.875 0.229,0.707 -5.216,4.341 -5.445,5.048 -0.229,0.707 2.045,6.855 1.445,7.292 -0.6,0.438 -5.729,-3.63 -6.471,-3.63 -0.742,0 -5.871,4.068 -6.471,3.631 -0.6,-0.437 1.674,-6.585 1.445,-7.292
C -5.255,11.968 -10.7,8.334 -10.471,7.627 -10.242,6.92 -3.706,7.189 -3.106,6.752 -2.506,6.314 -0.742,0 0,0
Z",
	    &None,
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "i32_in_empty",
            "M -4.9,-12 H -15 V 0 H 15 V -12 H 4.9",
            "#fffff0",
            ty_to_color(&Some(&I32)),
            "1.5",
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "i32_in",
            "M -4.9,-12 H -15 V 0 H 15 V -12 H 4.9",
            &ty_to_muted(&Some(&I32)),
            ty_to_color(&Some(&I32)),
            "1.5",
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "i32_out",
            "M -15,0 V 12 H 15 V 0 Z",
            ty_to_color(&Some(&I32)),
            "white",
            "0.25",
        ));

        ret.defs_mut().push(Self::make_stranded(
            &factory,
            "i32_out_stranded",
            "M -15,0 V 12 H 15 V 0 Z",
            &Some(&I32),
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "i64_in_empty",
            "M -4.9,-20 H -15 V 0 H 15 V -20 H 4.9",
            "#fffff0",
            ty_to_color(&Some(&I64)),
            "2",
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "i64_in",
            "M -4.9,-20 H -15 V 0 H 15 V -20 H 4.9",
            &ty_to_muted(&Some(&I64)),
            ty_to_color(&Some(&I64)),
            "2",
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "i64_out",
            "M -15,0 V 20 H 15 V 0 Z",
            ty_to_color(&Some(&I64)),
            "white",
            "0.25",
        ));

        ret.defs_mut().push(Self::make_stranded(
            &factory,
            "i64_out_stranded",
            "M -15,0 V 20 H 15 V 0 Z",
            &Some(&I64),
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "f32_out",
            "M 15,7.5
A 15,7.5 0 0 1 0,15 15,7.5 0 0 1 -15,7.5 15,7.5 0 0 1 0,0 15,7.5 0 0 1 15,7.5
Z",
            ty_to_color(&Some(&F32)),
            "white",
            "0.25",
        ));

        ret.defs_mut().push(Self::make_stranded(
            &factory,
            "f32_out_stranded",
            "M 15,7.5
A 15,7.5 0 0 1 0,15 15,7.5 0 0 1 -15,7.5 15,7.5 0 0 1 0,0 15,7.5 0 0 1 15,7.5
Z",
            &Some(&F32),
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "f64_out",
            "M 15,10 A 15,10 0 0 1 0,20 15,10 0 0 1 -15,10 15,10 0 0 1 0,0 15,10 0 0 1 15,10 Z",
            ty_to_color(&Some(&F64)),
            "white",
            "0.25",
        ));

        ret.defs_mut().push(Self::make_stranded(
            &factory,
            "f64_out_stranded",
            "M 15,10 A 15,10 0 0 1 0,20 15,10 0 0 1 -15,10 15,10 0 0 1 0,0 15,10 0 0 1 15,10 Z",
            &Some(&F64),
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "f32_in",
            "M 4.499,-11.724
A 15,6 0 0 1 14.826,-5.09 15,6 0 0 1 0,-0 15,6 0 0 1 -14.826,-5.09 15,6 0 0 1 -4.5,-11.724",
            &ty_to_muted(&Some(&F32)),
            ty_to_color(&Some(&F32)),
            "1.5",
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "f32_in_empty",
            "M 4.499,-11.724
A 15,6 0 0 1 14.826,-5.09 15,6 0 0 1 0,-0 15,6 0 0 1 -14.826,-5.09 15,6 0 0 1 -4.5,-11.724",
            "#fffff0",
            ty_to_color(&Some(&F32)),
            "1.5",
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "f64_in",
"M 4.498,-19.54
A 15,10 0 0 1 14.827,-8.484 15,10 0 0 1 0.003,-0 15,10 0 0 1 -14.826,-8.481 15,10 0 0 1 -4.503,-19.539",
            &ty_to_muted(&Some(&F64)),
            ty_to_color(&Some(&F64)),
            "2",
        ));

        ret.defs_mut().push(Self::make_icon(
            &factory,
            "f64_in_empty",
"M 4.498,-19.54
A 15,10 0 0 1 14.827,-8.484 15,10 0 0 1 0.003,-0 15,10 0 0 1 -14.826,-8.481 15,10 0 0 1 -4.503,-19.539",
            "#fffff0",
            ty_to_color(&Some(&F64)),
            "2",
        ));

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
            self.contents.get_mut().1.1.1.0.remove(idx);
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
            self.make_height_at_least(frame.1.end + 2);
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
                let blocks = &mut self.contents.get_mut().1.1.1.0;
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

    pub fn clear_type(&mut self, line_no: usize) {
        if let Some(frac) = self.fractions_mut().get_mut(line_no) {
            frac.clear()
        }
    }

    pub fn set_type(
        &mut self,
        line_no: usize,
        indent: u16,
        input_types: Vec<Option<SlotInfo>>,
        output_types: Vec<SlotInfo>,
    ) {
        self.make_height_at_least(line_no + 2);
        self.make_width_at_least(indent as usize);

        self.contents
            .get_mut()
            .1
            .1
            .0
            .get_mut(line_no)
            .unwrap()
            .draw(&self.factory, line_no, indent, input_types, output_types);
    }

    pub fn set_type_count(&mut self, count: usize) {
        self.fractions_mut().truncate(count);
        while self.contents.get().1.1.0.len() < count {
            self.contents
                .get_mut()
                .1
                .1
                .0
                .push(OperatorFraction::new_empty(&self.factory));
        }
    }

    pub fn set_connections(&mut self, connections: OperandConnections) {
        use ValType::*;
        let mut connection_idx = 0;
        self.connections_mut().truncate(0);
        for (src, dst) in zip_eq(connections.written, connections.read) {
            let (Some(src), Some(dst)) = (src, dst) else {
                continue;
            };
            let write_base = self.fractions().get(src.line_idx).unwrap().target.unwrap();
            let read_base = self.fractions().get(dst.line_idx).unwrap().target.unwrap();
            let (x, ty) = self
                .fractions()
                .get(src.line_idx)
                .unwrap()
                .output_locations_and_types[src.operand_num];
            let reader_offset = match ty {
                Some(I32) => 11,
                Some(I64) => 19,
                Some(F32) => 11,
                Some(F64) => 19,
                _ => 10,
            };
            let write_x = write_base.0 + x;
            let (relative_x, read_scale) = self
                .fractions()
                .get(dst.line_idx)
                .unwrap()
                .input_locations_and_scales[dst.operand_num];
            let read_x = read_base.0 + relative_x;
            let write_y = write_base.1 + 10.0;
            let read_y = read_base.1 - reader_offset as f32;
            // let y_distance = read_y - write_y;
            let first_control_height = write_y + 1.0;
            let second_control_height = write_y;

            while connection_idx >= self.connections_mut().len() {
                let newpath = self.factory.svg_path();
                self.connections_mut().push(newpath);
            }
            let cx = &mut self.connections_mut()[connection_idx];
            cx.set_attribute("stroke", &ty_to_muted(&ty.as_ref()));
            cx.set_attr_num("stroke-width", 10.0 * read_scale);
            cx.set_attribute("fill", "none");
            cx.set_attribute(
                "d",
                &format!(
                    "M {write_x} {write_y}
C {write_x},{first_control_height} {read_x},{second_control_height}, {read_x},{read_y}"
                ),
            );
            connection_idx += 1;
        }
        //        self.connections_mut().truncate(connection_idx);
    }

    delegate! {
    to self.contents {
    pub fn set_attribute(&mut self, name: &str, value: &str);
    pub fn remove_attribute(&mut self, name: &str);
    pub fn get_attribute(&self, name: &str) -> Option<&str>;
    }
    }
}

type FractionVec = DomVec<ElementHandle<SvgUseElement>, SvggElement>;
type SymbolsType = DomStruct<(FractionVec, (FractionVec, ())), SvggElement>;

struct FractionCache {
    input_types: Vec<Option<SlotInfo>>,
    output_types: Vec<SlotInfo>,
}

struct OperatorFraction {
    symbols: SymbolsType,
    cache: Option<FractionCache>,
    target: Option<(f32, f32)>,
    input_locations_and_scales: Vec<(f32, f32)>,
    output_locations_and_types: Vec<(f32, Option<ValType>)>,
}

impl OperatorFraction {
    fn new_empty(factory: &ElementFactory) -> Self {
        let mut ret = Self {
            symbols: DomStruct::new(
                (
                    DomVec::new(factory.svg_g()),
                    (DomVec::new(factory.svg_g()), ()),
                ),
                factory.svg_g(),
            ),
            cache: None,
            target: None,
            input_locations_and_scales: vec![],
            output_locations_and_types: vec![],
        };
        ret.symbols.set_attribute("class", "fraction");

        ret
    }

    fn outputs(&mut self) -> &mut FractionVec {
        &mut self.symbols.get_mut().0
    }

    fn inputs(&mut self) -> &mut FractionVec {
        &mut self.symbols.get_mut().1.0
    }

    fn clear(&mut self) {
        self.inputs().truncate(0);
        self.outputs().truncate(0);
        self.cache = None;
        self.target = None;
        self.input_locations_and_scales.clear();
        self.output_locations_and_types.clear();
    }

    fn draw(
        &mut self,
        factory: &ElementFactory,
        line_no: usize,
        indent: u16,
        input_types: Vec<Option<SlotInfo>>,
        output_types: Vec<SlotInfo>,
    ) {
        let target_x = X_OFFSET_PX + INDENT_PX * indent as usize
            - INDENT_PX * BLOCK_BOUNDARY_INDENT / 2
            - MARGIN / 2;
        let target_y = line_no * LINE_SPACING + LINE_SPACING / 2 + LINE_OFFSET_PX;
        self.symbols.set_attribute(
            "transform",
            &format!("translate({} {})", target_x, target_y),
        );
        self.target = Some((target_x as f32, target_y as f32));

        if let Some(cache) = &self.cache
            && cache.input_types == input_types
            && cache.output_types == output_types
        {
            return;
        }

        let in_len = input_types.len() as i32;
        let out_len = output_types.len() as i32;
        self.inputs().truncate(input_types.len());
        while self.inputs().len() < input_types.len() {
            self.inputs().push(factory.svg_use());
        }

        let num_stranded: usize = output_types.iter().map(|ty| !ty.used as usize).sum();

        self.outputs().truncate(output_types.len() + num_stranded);
        while self.outputs().len() < output_types.len() + num_stranded {
            self.outputs().push(factory.svg_use());
        }
        self.input_locations_and_scales.clear();
        self.output_locations_and_types.clear();

        fn scale(len: i32) -> f32 {
            let max_width = (BLOCK_BOUNDARY_INDENT * INDENT_PX - 2 * MARGIN + 1) as f32;
            if len as f32 * SYM_HW * 2.0 > max_width {
                max_width / (len as f32 * SYM_HW * 2.0)
            } else {
                1.0
            }
        }

        let in_scale = scale(in_len);
        let out_scale = scale(out_len);

        if in_scale != 1.0 {
            self.inputs()
                .set_attribute("transform", &format!("scale({in_scale} 1)"));
        } else {
            self.inputs().remove_attribute("transform");
        }
        if out_scale != 1.0 {
            self.outputs()
                .set_attribute("transform", &format!("scale({out_scale} 1)"));
        } else {
            self.outputs().remove_attribute("transform");
        }

        let left_edge_in = -SYM_HW * (in_len - 1) as f32;
        let left_edge_out = -SYM_HW * (out_len - 1) as f32;

        fn render(maybe_slot: Option<&SlotInfo>, is_input: bool) -> String {
            let (slot, used) = match maybe_slot {
                Some(s) => (
                    match s.slot.ty {
                        Some(ty) => ty.to_string(),
                        None => "mystery".to_string(),
                    },
                    s.used,
                ),
                None => ("mystery".to_string(), false),
            };
            format!(
                "#{}_{}{}",
                slot,
                if is_input { "in" } else { "out" },
                if !used && is_input { "_empty" } else { "" }
            )
        }

        for (i, ty) in input_types.iter().enumerate() {
            let sym = &mut self.inputs().get_mut(i).unwrap();
            let x = left_edge_in + 2.0 * SYM_HW * i as f32;
            sym.set_attr_num("x", x);
            sym.set_attribute("href", &render(ty.as_ref(), true));
            self.input_locations_and_scales
                .push((in_scale * x, in_scale));
        }

        let num_stranded = output_types
            .iter()
            .map(|ty| if ty.used { 0 } else { 1 })
            .sum();

        for (i, ty) in output_types.iter().enumerate().take(num_stranded) {
            let sym = &mut self.outputs().get_mut(i).unwrap();
            let x = left_edge_out + 2.0 * SYM_HW * i as f32;
            sym.set_attr_num("x", x);
            sym.set_attribute("href", &(render(Some(ty), false) + "_stranded"));
        }

        for (i, ty) in output_types.iter().enumerate() {
            let sym = &mut self.outputs().get_mut(num_stranded + i).unwrap();
            let x = left_edge_out + 2.0 * SYM_HW * i as f32;
            sym.set_attr_num("x", x);
            sym.set_attribute("href", &render(Some(ty), false));

            self.output_locations_and_types
                .push((out_scale * x, ty.slot.ty));
        }

        self.cache = Some(FractionCache {
            input_types,
            output_types,
        });
    }
}

impl WithElement for OperatorFraction {
    type Element = SvggElement;
    fn with_element(&self, f: impl FnMut(&Self::Element), g: AccessToken) {
        self.symbols.with_element(f, g)
    }
}

impl Component for OperatorFraction {
    fn audit(&self) {
        self.symbols.audit();
    }
}
