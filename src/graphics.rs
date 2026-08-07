// The "graphics" component of the Codillon editor. This displays frame boundaries and will eventually
// include the dataflow.

use crate::{
    debug::SlotContents,
    delegate_element_component,
    dom_set::DomSet,
    dom_struct::DomStruct,
    dom_text::DomText,
    dom_vec::DomVec,
    editor::LINE_SPACING,
    jet::{AccessToken, Component, ElementFactory, ElementHandle, WithElement},
    line::INDENT_PX,
    syntax::{FrameInfo, InstrKind},
    utils::{
        AnimationRequest, AnnotatedOperatorType, BLOCK_BOUNDARY_INDENT, Coordinate, SlotConnection,
        SlotInfo, Tween,
    },
};
use delegate::delegate;
use palette::{Mix, Srgb};
use std::{
    cell::Cell,
    collections::{HashMap, HashSet},
    str::FromStr,
};
use thousands::Separable;
use wasmparser::ValType;
use web_sys::{
    SvgDefsElement, SvgElement, SvgLinearGradientElement, SvgMaskElement, SvgPathElement,
    SvgStopElement, SvgTextElement, SvgUseElement, SvggElement, console::log_1,
};

const SYM_HW: f32 = 15.0;

#[derive(Debug, PartialEq)]
pub struct FractionInfo {
    pub line_no: usize,
    pub indent: u16,
    pub ty: AnnotatedOperatorType,
}

// One "line" representing a Wasm frame boundary.
type DomLine = DomStruct<
    (
        ElementHandle<SvgPathElement>,
        (SymbolUse, ()), // an optional symbol for "unclosed" frames
    ),
    SvggElement,
>;

struct SymbolUse {
    elem: ElementHandle<SvgUseElement>,
    opacity: Tween,
}
delegate_element_component!(SymbolUse, elem, SvgUseElement);

impl SymbolUse {
    fn new_symbol(factory: &ElementFactory) -> Self {
        let mut ret = Self {
            elem: factory.svg_use(),
            opacity: Tween::Post(0.0),
        };

        ret.elem.set_attribute("href", "#unclosed");
        ret.elem
            .set_attr_num("opacity", ret.opacity.value().unwrap());

        ret
    }

    fn set_visibility(&mut self, smooth: bool, visible: bool) -> AnimationRequest {
        let ret = self.opacity.goto(smooth, visible as usize as f64);
        self.elem
            .set_attr_num("opacity", self.opacity.value().unwrap());
        ret
    }

    fn has_pending_animation(&self) -> AnimationRequest {
        self.opacity.is_pending()
    }

    fn animate(&mut self, t: f64) {
        self.opacity.animate(t);
        self.elem
            .set_attr_num("opacity", self.opacity.value().unwrap());
    }

    fn snap_to(&mut self, x: f64, y: f64) {
        self.elem.set_attr_num("x", x);
        self.elem.set_attr_num("y", y);
    }
}
struct FrameLine {
    info: Option<FrameInfo>,
    loop_was_taken: bool,
    animated_indent: Option<f64>,
    elem: DomLine,
    opacity: Tween,

    x_left: Tween,
    y_top: Tween,
    bwidth_factor: Tween,
    hheight: Tween,
    w3: Tween,
}

const BASE_X_OFFSET_PX: usize = 101;
const X_OFFSET_PX: usize = BASE_X_OFFSET_PX + 4 * INDENT_PX;
const LINE_OFFSET_PX: usize = 8;
const WIDTH: usize = 2;
const MARGIN: usize = 8;

pub fn indent_px(indent: u16) -> usize {
    (indent as usize) * INDENT_PX
}

impl FrameLine {
    fn line(&mut self) -> &mut ElementHandle<SvgPathElement> {
        &mut self.elem.get_mut().0
    }

    fn symbol(&self) -> &SymbolUse {
        &self.elem.get().1.0
    }

    fn symbol_mut(&mut self) -> &mut SymbolUse {
        &mut self.elem.get_mut().1.0
    }

    fn new(factory: &ElementFactory) -> Self {
        let mut ret = Self {
            info: None,
            loop_was_taken: false,
            animated_indent: None,
            elem: DomStruct::new(
                (factory.svg_path(), (SymbolUse::new_symbol(factory), ())),
                factory.svg_g(),
            ),
            opacity: Tween::Pre,
            x_left: Tween::Pre,
            y_top: Tween::Pre,
            bwidth_factor: Tween::Pre,
            hheight: Tween::Pre,
            w3: Tween::Pre,
        };

        ret.line().set_attr_num("stroke-width", WIDTH);
        ret.line().set_attribute("fill", "none");

        ret
    }

    fn hide(&mut self, smooth: bool) -> AnimationRequest {
        // "else" should should move down to the bottom of an existing if block
        let mut ret = AnimationRequest(false);
        if let Some(info) = &self.info
            && info.kind == InstrKind::Else
        {
            ret |= self.y_top.goto(
                smooth,
                (info.end * LINE_SPACING + LINE_SPACING / 2 + LINE_OFFSET_PX) as f64,
            );
            ret |= self.hheight.goto(smooth, 0.0);
        }

        ret | self.opacity.goto(smooth, 0.0) | self.symbol_mut().set_visibility(smooth, false)
    }

    const BACKUP: f64 = 30.0; // controls arm of frame vertical

    // Make the DOM SVG element reflect the new Wasm FrameInfo that it represents.
    fn update(
        &mut self,
        info: FrameInfo,
        animated_indent: Option<f64>,
        smooth: bool,
    ) -> AnimationRequest {
        let x_offset = if info.indent > 0 {
            X_OFFSET_PX
        } else {
            X_OFFSET_PX - indent_px(BLOCK_BOUNDARY_INDENT)
        } as f64;
        let top_offset = if info.wide { 0 } else { LINE_SPACING / 2 };
        let x_left = x_offset + indent_px(info.indent) as f64;
        let y_top = (info.start * LINE_SPACING + top_offset + LINE_OFFSET_PX) as f64;
        let y_bot = (info.end * LINE_SPACING + LINE_SPACING / 2 + LINE_OFFSET_PX) as f64;

        let height = y_bot - y_top;
        let hheight = if info.impairment.is_unclosed() {
            if info.indent > 0 {
                0.5 * height - 0.29 * LINE_SPACING as f64
            } else {
                0.5 * height + 0.2 * LINE_SPACING as f64
            }
        } else {
            0.5 * height
        };
        let bwidth_factor = if info.impairment.is_unclosed() {
            0.0
        } else {
            1.0
        };

        // "else" should move up from the bottom of an existing if block
        if self.info.is_none() && info.kind == InstrKind::Else {
            self.y_top.snap(y_bot);
            self.hheight.snap(0.0);
        }

        let mut ret = self.opacity.goto(smooth, 1.0);
        ret |= self
            .symbol_mut()
            .set_visibility(smooth, info.impairment.is_unclosed());

        ret |= self.y_top.goto(smooth, y_top);
        ret |= self.hheight.goto(smooth, hheight);
        ret |= self.x_left.goto(smooth, x_left);
        ret |= self.bwidth_factor.goto(smooth, bwidth_factor);
        ret |= self.w3.goto(
            smooth,
            if info.impairment.is_unclosed() {
                0.5 * Self::BACKUP
            } else {
                Self::BACKUP
            },
        );

        if info.impairment.is_bad() {
            self.line().set_attribute("stroke", "darkred");
        } else {
            match info.kind {
                InstrKind::OtherStructured => {
                    self.line().set_attribute("stroke", "darkgray");
                }
                InstrKind::If => {
                    self.line().set_attribute("stroke", "green");
                }
                InstrKind::Else => {
                    self.line().set_attribute("stroke", "purple");
                }
                InstrKind::Loop => {
                    self.line().set_attribute("stroke", "pink");
                }
                InstrKind::Other | InstrKind::End => panic!("unexpected frame kind"),
            };
        }

        self.info = Some(info);
        self.animated_indent = animated_indent;

        self.draw() | ret
    }

    fn draw(&mut self) -> AnimationRequest {
        let Self {
            info: Some(info),
            loop_was_taken,
            animated_indent,
            elem,
            opacity,
            x_left,
            y_top,
            bwidth_factor,
            hheight,
            w3,
        } = self
        else {
            panic!();
        };

        let (opacity, x_left, y_top, bwidth_factor, hheight, w3) = (
            opacity.value().unwrap(),
            x_left.value().unwrap(),
            y_top.value().unwrap(),
            bwidth_factor.value().unwrap(),
            hheight.value().unwrap(),
            w3.value().unwrap(),
        );

        let x_offset = if info.indent > 0 {
            X_OFFSET_PX
        } else {
            X_OFFSET_PX - indent_px(BLOCK_BOUNDARY_INDENT)
        } as f64;

        let x_max = x_offset + (indent_px(info.indent + BLOCK_BOUNDARY_INDENT) - MARGIN) as f64;
        let total_dist = (x_max - x_left).max(0.0);

        let x_right = if let Some(actual_indent) = animated_indent
            && info.indent > 0
        {
            let line_max = *actual_indent + BASE_X_OFFSET_PX as f64;
            let distance_to_end = x_max - line_max;
            let weight = (distance_to_end / total_dist).clamp(0.0, 1.0);
            let weight = weight * weight;
            line_max - (MARGIN as f64) * (1.0 - weight)
        } else {
            x_max
        };

        let width = x_right - x_left;
        let bwidth = bwidth_factor * width;
        let dist1 = (hheight * 0.95).min(LINE_SPACING as f64 * 0.5 * 0.95);
        let dist2 = hheight - dist1;

        let backup = Self::BACKUP;

        let l1 = (hheight * 0.5).min(LINE_SPACING as f64 * 0.5);
        let la = 10.0;
        let l2 = 15.0;
        let xfac = 0.6f64;
        let yfac = (1.0 - xfac * xfac).sqrt();

        let slope = (l1 * xfac) / (hheight - l1 * yfac);
        let slope2 = (1.0 - slope * slope).sqrt();

        // this shape is... way too complicated. But it looks nice!
        let d = if *loop_was_taken {
            &format!(
                "M {x_right},{y_top} h -{width} c {},{} {},{} {},{} {},{} {},{} {},{} {},{} {},{} {},{} {},{} {},{} {},{} h {bwidth}",
                -backup,
                0,
                -backup,
                l1 * yfac,
                -backup - l1 * xfac,
                hheight, /* x */
                -la * slope,
                la * slope2,
                -l2,
                la,
                -l2,
                0, /* y */
                0,
                -la,
                l2 - la * slope,
                -la * slope2,
                l2,
                0, /* z */
                l1 * xfac,
                hheight - l1 * yfac,
                l1 * xfac,
                hheight,
                backup + (l1 * xfac) * bwidth_factor,
                hheight, /* omega */
            )
        } else {
            &format!(
                "M {x_right},{y_top} h -{width} c -{backup},0 -{backup},{dist1} -{backup},{hheight} 0,{dist2} 0,{hheight} {w3},{hheight} h {bwidth}"
            )
        };

        elem.get_mut().0.set_attribute("d", d);
        elem.get_mut().0.set_attr_num("opacity", opacity);

        self.symbol_mut().snap_to(
            x_right - width - backup + w3 + bwidth,
            y_top + 2.0 * hheight,
        );

        self.has_pending_animation()
    }

    fn has_pending_animation(&self) -> AnimationRequest {
        self.opacity.is_pending()
            | self.x_left.is_pending()
            | self.y_top.is_pending()
            | self.bwidth_factor.is_pending()
            | self.hheight.is_pending()
            | self.w3.is_pending()
            | self.symbol().has_pending_animation()
    }

    pub fn animate(&mut self, t: f64, animated_indent: Option<f64>) -> AnimationRequest {
        self.animated_indent = animated_indent;

        self.symbol_mut().animate(t);
        self.opacity.animate(t);
        self.x_left.animate(t);
        self.y_top.animate(t);
        self.bwidth_factor.animate(t);
        self.hheight.animate(t);
        self.w3.animate(t);
        self.draw()
    }
}

delegate_element_component!(FrameLine, elem, <DomLine as WithElement>::Element);

type ArrowElem = DomStruct<
    (
        ElementHandle<SvgUseElement>,
        (ElementHandle<SvgUseElement>, ()),
    ),
    SvggElement,
>;

struct Arrow {
    elem: ArrowElem,
    x: Tween,
    y: Tween,

    line_idx: Option<usize>,
}
delegate_element_component!(Arrow, elem, SvggElement);

impl Arrow {
    fn new(factory: &ElementFactory) -> Self {
        let mut ret = Self {
            elem: DomStruct::new(
                (factory.svg_use(), (factory.svg_use(), ())),
                factory.svg_g(),
            ),
            x: Default::default(),
            y: Default::default(),
            line_idx: None,
        };
        ret.target_elem().set_attribute("class", "arrow-target");
        ret
    }

    fn arrow_elem(&mut self) -> &mut ElementHandle<SvgUseElement> {
        &mut self.elem.get_mut().0
    }

    fn target_elem(&mut self) -> &mut ElementHandle<SvgUseElement> {
        &mut self.elem.get_mut().1.0
    }

    fn goto(&mut self, smooth: bool, loc: Option<(usize, bool, usize)>) -> AnimationRequest {
        let Some((line_idx, below_line, indent)) = loc else {
            self.arrow_elem().remove_attribute("href");
            self.line_idx = None;
            return AnimationRequest(false);
        };

        self.arrow_elem().set_attribute("href", "#arrow");
        self.line_idx = Some(line_idx);

        let target_x = X_OFFSET_PX + indent * INDENT_PX - 4;
        let target_y = line_idx * LINE_SPACING
            + LINE_SPACING / 2
            + LINE_OFFSET_PX
            + if below_line { LINE_SPACING / 2 } else { 0 };

        if smooth {
            self.x.approach(target_x as f64);
            self.y.approach(target_y as f64);
            return AnimationRequest(true);
        }

        // otherwise, snap change
        self.arrow_elem().set_attr_num("x", target_x);
        self.arrow_elem().set_attr_num("y", target_y);
        self.x.snap(target_x as f64);
        self.y.snap(target_y as f64);
        AnimationRequest(false)
    }

    fn animate(&mut self, t: f64, animated_indents: &HashMap<usize, f64>) -> AnimationRequest {
        let animated_indent: Option<f64> = self
            .line_idx
            .and_then(|idx| animated_indents.get(&idx).copied());
        if let Some(actual_indent) = animated_indent {
            let target_x = BASE_X_OFFSET_PX as f64 + actual_indent - 4.0;
            self.x.retarget(target_x);
        }
        if self.x.is_pending().0 {
            self.x.animate(t);
            self.y.animate(t);
        }
        let (Some(x), Some(y)) = (self.x.value(), self.y.value()) else {
            return AnimationRequest(false);
        };
        self.arrow_elem().set_attr_num("x", x);
        self.arrow_elem().set_attr_num("y", y);
        debug_assert_eq!(self.x.is_pending(), self.y.is_pending());
        self.x.is_pending()
    }

    fn scroll_to_target(&mut self) {
        if let Some(target_x) = self.x.target()
            && let Some(target_y) = self.y.target()
        {
            self.elem.get_mut().1.0.set_attr_num("x", target_x);
            self.elem.get_mut().1.0.set_attr_num("y", target_y);
            self.elem.get().1.0.scroll_into_view();
        }
    }
}

// definitions in SVG header
type SVGDefs = DomStruct<
    (
        DomVec<ElementHandle<SvgPathElement>, SvggElement>,
        (
            DomVec<ElementHandle<SvgStopElement>, SvgLinearGradientElement>,
            (),
        ),
    ),
    SvgDefsElement,
>;
type CodillonBlocks = DomSet<FrameLine, SvggElement, u32>; // the lines themselves
type Fractions = DomSet<OperatorFraction, SvggElement, u32>;
type Connections = DomSet<RenderedConnection, SvggElement, Coordinate>;
type CodillonSVG = DomStruct<
    (
        SVGDefs,
        (Connections, (Fractions, (CodillonBlocks, (Arrow, ())))),
    ),
    SvgElement,
>;

type UnfurlMask = DomStruct<(ElementHandle<SvgPathElement>, ()), SvgMaskElement>;
type PathUnfurl = DomStruct<(ElementHandle<SvgPathElement>, (UnfurlMask, ())), SvggElement>;

struct RenderedConnection {
    connection: SlotConnection,
    path: PathUnfurl,
    src_targets: FractionTargets,
    dst_targets: FractionTargets,

    reveal: Tween,
    targeting_info: ConnectionTargetingInfo,
}

delegate_element_component!(RenderedConnection, path, SvggElement);

#[derive(Default)]
struct PendingAnimations {
    frames: HashSet<u32>,
    connections: HashSet<Coordinate>,
}

impl PendingAnimations {
    fn has_pending(&self) -> bool {
        !self.frames.is_empty() || !self.connections.is_empty()
    }
}

pub struct DomImage {
    contents: CodillonSVG,
    height: usize,
    width: u16,
    factory: ElementFactory,
    connection_dst2src: HashMap<Coordinate /* dst */, Coordinate /* src */>,
    connection_interest: HashMap<u32 /* pos */, Vec<Coordinate> /* conn's attached */>,
    animations: PendingAnimations,
    last_line_count_for_frames: usize,
    last_line_count_for_connections: usize,
}

impl WithElement for DomImage {
    type Element = SvgElement;
    fn with_element<T, F: FnMut(&Self::Element) -> T>(&self, f: F, g: AccessToken) -> T {
        self.contents.with_element(f, g)
    }
}

impl Component for DomImage {
    #[cfg(debug_assertions)]
    fn audit(&self) {
        self.contents.audit();
        let mut src_hit = HashSet::new();
        let mut dst_hit = HashSet::new();
        for (src, cx) in self.connections().iter() {
            if cx.is_hiding() {
                continue;
            }
            let (the_src, the_dst) = cx.connection.is_connected().unwrap();
            assert_eq!(src, the_src);
            assert!(src_hit.insert(the_src.clone()));
            assert!(dst_hit.insert(the_dst.clone()));
            assert_eq!(self.connection_dst2src[the_dst], src.clone());
            assert!(self.connection_interest[&the_src.position_id].contains(the_src));
            assert!(self.connection_interest[&the_dst.position_id].contains(the_src));
        }
        for other_dst in self.connection_dst2src.keys() {
            assert!(dst_hit.contains(other_dst));
        }
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

fn icon_height(ty: Option<ValType>) -> f32 {
    use ValType::*;
    match ty {
        Some(I32) => 11.0,
        Some(I64) => 19.0,
        Some(F32) => 11.0,
        Some(F64) => 19.0,
        _ => 10.0,
    }
}

macro_rules! get {
    ($comp:expr,$field:ident) => {
        &field!($field $comp.get())
    };
}

macro_rules! get_mut {
    ($comp:expr,$field:ident) => {
        &mut field!($field $comp.get_mut())
    };
}

macro_rules! field {
    (defs $comp:expr) => {
        $comp.0
    };
    (connections $comp:expr) => {
        $comp.1.0
    };
    (fractions $comp:expr) => {
        $comp.1.1.0
    };
    (blocks $comp:expr) => {
        $comp.1.1.1.0
    };
    (arrow $comp:expr) => {
        $comp.1.1.1.1.0
    };
}

impl DomImage {
    fn defs_mut(&mut self) -> &mut SVGDefs {
        get_mut!(self.contents, defs)
    }

    fn connections(&self) -> &Connections {
        get!(self.contents, connections)
    }

    fn connections_mut(&mut self) -> &mut Connections {
        get_mut!(self.contents, connections)
    }

    fn fractions(&self) -> &Fractions {
        get!(self.contents, fractions)
    }

    fn fractions_mut(&mut self) -> &mut Fractions {
        get_mut!(self.contents, fractions)
    }

    fn blocks(&self) -> &CodillonBlocks {
        get!(self.contents, blocks)
    }

    fn arrow_mut(&mut self) -> &mut Arrow {
        get_mut!(self.contents, arrow)
    }

    fn make_empty(mut ret: ElementHandle<SvgPathElement>) -> ElementHandle<SvgPathElement> {
        ret.set_attribute("fill", "url(#fade-up)");
        ret
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
                    SVGDefs::new(
                        (
                            DomVec::new(factory.svg_g()),
                            (DomVec::new(factory.svg_linear_gradient()), ()),
                        ),
                        factory.svg_defs(),
                    ),
                    (
                        Connections::new(factory.svg_g()),
                        (
                            DomSet::new(factory.svg_g()),
                            (
                                CodillonBlocks::new(factory.svg_g()),
                                (Arrow::new(&factory), ()),
                            ),
                        ),
                    ),
                ),
                factory.svg(),
            ),
            height: 0,
            width: 0,
            factory: factory.clone(),
            connection_dst2src: Default::default(),
            connection_interest: Default::default(),
            animations: Default::default(),
            last_line_count_for_frames: Default::default(),
            last_line_count_for_connections: Default::default(),
        };

        // The "unclosed" symbol looks like a ⊘ (Circled Division Slash)
        // character, or like an "End of All Prohibitions"
        // European road sign.
        ret.defs_mut().get_mut().0.push(Self::make_icon(
            &factory,
            "unclosed",
            "M 5.1970835,0 C 5.1970835,2.87027 2.87027,5.1970835 0,5.1970835 -2.87027,5.1970835 -5.1970835,2.87027 -5.1970835,0 -5.1970835,-2.87027 -2.87027,-5.1970835 0,-5.1970835 2.87027,-5.1970835 5.1970835,-2.87027 5.1970835,0 Z M 3.6812636,-3.6812727 -3.681272,3.6812676",
            "#fffff0",
            "darkred",
            "2",
        ));

        {
            let mut arrow_icon = Self::make_icon(
                &factory,
                "arrow",
                "m -95.773,0.938 -3.75,5.625 h 1.406 l 3.75,-5.625 z  m -2.812,0 -3.75,5.625 h 1.406 l 3.75,-5.625 z  m -2.813,0 -3.75,5.625 h 1.406 l 3.75,-5.625 z  m -2.812,0 -3.75,5.625 h 1.406 l 3.75,-5.625 z  m -1.406,0 v -1.875 L -110,-7.5 h 12.656 l 4.383,5.563 H -6.344 V -5.453 L -0.001,0 -6.344,5.453 V 1.938 H -92.96 l -4.383,5.562 h -12.656 z  m 9.844,-1.875 h 1.406 l -3.75,-5.625 h -1.406 z  m -2.812,0 h 1.406 l -3.75,-5.625 h -1.406 z  m -2.813,0 h 1.406 l -3.75,-5.625 h -1.406 z  m -2.812,0 h 1.406 l -3.75,-5.625 h -1.406 z",
                "#000080",
                "#fffff0",
                "1",
            );
            arrow_icon.set_attribute("paint-order", "stroke");

            ret.defs_mut().get_mut().0.push(arrow_icon);
        }

        ret.defs_mut().get_mut().0.push(Self::make_icon(
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

        ret.defs_mut().get_mut().0.push(Self::make_icon(
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

        ret.defs_mut().get_mut().0.push(Self::make_icon(
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

        ret.defs_mut().get_mut().0.push(Self::make_stranded(
            &factory,
            "mystery_out_stranded",
"m 0,0
c 0.742,0 2.506,6.314 3.106,6.751 0.6,0.437 7.136,0.168 7.365,0.875 0.229,0.707 -5.216,4.341 -5.445,5.048 -0.229,0.707 2.045,6.855 1.445,7.292 -0.6,0.438 -5.729,-3.63 -6.471,-3.63 -0.742,0 -5.871,4.068 -6.471,3.631 -0.6,-0.437 1.674,-6.585 1.445,-7.292
C -5.255,11.968 -10.7,8.334 -10.471,7.627 -10.242,6.92 -3.706,7.189 -3.106,6.752 -2.506,6.314 -0.742,0 0,0
Z",
	    &None,
        ));

        ret.defs_mut()
            .get_mut()
            .0
            .push(Self::make_empty(Self::make_icon(
                &factory,
                "i32_in_empty",
                "M -4.9,-12 H -15 V 0 H 15 V -12 H 4.9",
                "#fffff0",
                ty_to_color(&Some(&I32)),
                "1.5",
            )));

        ret.defs_mut().get_mut().0.push(Self::make_icon(
            &factory,
            "i32_in",
            "M -4.9,-12 H -15 V 0 H 15 V -12 H 4.9",
            &ty_to_muted(&Some(&I32)),
            ty_to_color(&Some(&I32)),
            "1.5",
        ));

        ret.defs_mut().get_mut().0.push(Self::make_icon(
            &factory,
            "i32_out",
            "M -15,0 V 12 H 15 V 0 Z",
            ty_to_color(&Some(&I32)),
            "white",
            "0.25",
        ));

        ret.defs_mut().get_mut().0.push(Self::make_stranded(
            &factory,
            "i32_out_stranded",
            "M -15,0 V 12 H 15 V 0 Z",
            &Some(&I32),
        ));

        ret.defs_mut()
            .get_mut()
            .0
            .push(Self::make_empty(Self::make_icon(
                &factory,
                "i64_in_empty",
                "M -4.9,-20 H -15 V 0 H 15 V -20 H 4.9",
                "#fffff0",
                ty_to_color(&Some(&I64)),
                "2",
            )));

        ret.defs_mut().get_mut().0.push(Self::make_icon(
            &factory,
            "i64_in",
            "M -4.9,-20 H -15 V 0 H 15 V -20 H 4.9",
            &ty_to_muted(&Some(&I64)),
            ty_to_color(&Some(&I64)),
            "2",
        ));

        ret.defs_mut().get_mut().0.push(Self::make_icon(
            &factory,
            "i64_out",
            "M -15,0 V 20 H 15 V 0 Z",
            ty_to_color(&Some(&I64)),
            "white",
            "0.25",
        ));

        ret.defs_mut().get_mut().0.push(Self::make_stranded(
            &factory,
            "i64_out_stranded",
            "M -15,0 V 20 H 15 V 0 Z",
            &Some(&I64),
        ));

        ret.defs_mut().get_mut().0.push(Self::make_icon(
            &factory,
            "f32_out",
            "M 15,7.5
A 15,7.5 0 0 1 0,15 15,7.5 0 0 1 -15,7.5 15,7.5 0 0 1 0,0 15,7.5 0 0 1 15,7.5
Z",
            ty_to_color(&Some(&F32)),
            "white",
            "0.25",
        ));

        ret.defs_mut().get_mut().0.push(Self::make_stranded(
            &factory,
            "f32_out_stranded",
            "M 15,7.5
A 15,7.5 0 0 1 0,15 15,7.5 0 0 1 -15,7.5 15,7.5 0 0 1 0,0 15,7.5 0 0 1 15,7.5
Z",
            &Some(&F32),
        ));

        ret.defs_mut().get_mut().0.push(Self::make_icon(
            &factory,
            "f64_out",
            "M 15,10 A 15,10 0 0 1 0,20 15,10 0 0 1 -15,10 15,10 0 0 1 0,0 15,10 0 0 1 15,10 Z",
            ty_to_color(&Some(&F64)),
            "white",
            "0.25",
        ));

        ret.defs_mut().get_mut().0.push(Self::make_stranded(
            &factory,
            "f64_out_stranded",
            "M 15,10 A 15,10 0 0 1 0,20 15,10 0 0 1 -15,10 15,10 0 0 1 0,0 15,10 0 0 1 15,10 Z",
            &Some(&F64),
        ));

        ret.defs_mut().get_mut().0.push(Self::make_icon(
            &factory,
            "f32_in",
            "M 4.499,-11.724
A 15,6 0 0 1 14.826,-5.09 15,6 0 0 1 0,-0 15,6 0 0 1 -14.826,-5.09 15,6 0 0 1 -4.5,-11.724",
            &ty_to_muted(&Some(&F32)),
            ty_to_color(&Some(&F32)),
            "1.5",
        ));

        ret.defs_mut()
            .get_mut()
            .0
            .push(Self::make_empty(Self::make_icon(
                &factory,
                "f32_in_empty",
                "M 4.499,-11.724
A 15,6 0 0 1 14.826,-5.09 15,6 0 0 1 0,-0 15,6 0 0 1 -14.826,-5.09 15,6 0 0 1 -4.5,-11.724",
                "#fffff0",
                ty_to_color(&Some(&F32)),
                "1.5",
            )));

        ret.defs_mut().get_mut().0.push(Self::make_icon(
            &factory,
            "f64_in",
"M 4.498,-19.54
A 15,10 0 0 1 14.827,-8.484 15,10 0 0 1 0.003,-0 15,10 0 0 1 -14.826,-8.481 15,10 0 0 1 -4.503,-19.539",
            &ty_to_muted(&Some(&F64)),
            ty_to_color(&Some(&F64)),
            "2",
        ));

        ret.defs_mut().get_mut().0.push(Self::make_empty(Self::make_icon(
            &factory,
            "f64_in_empty",
"M 4.498,-19.54
A 15,10 0 0 1 14.827,-8.484 15,10 0 0 1 0.003,-0 15,10 0 0 1 -14.826,-8.481 15,10 0 0 1 -4.503,-19.539",
            "#fffff0",
            ty_to_color(&Some(&F64)),
            "2",
        )));

        ret.defs_mut().get_mut().1.0.set_attribute("id", "fade-up");
        ret.defs_mut().get_mut().1.0.set_attribute("x1", "0%");
        ret.defs_mut().get_mut().1.0.set_attribute("x2", "0%");
        ret.defs_mut().get_mut().1.0.set_attribute("y1", "100%");
        ret.defs_mut().get_mut().1.0.set_attribute("y2", "0%");

        let mut add_stop = |offset, color| {
            let mut stop = factory.svg_stop();
            stop.set_attribute("offset", offset);
            stop.set_attribute("stop-color", color);
            ret.defs_mut().get_mut().1.0.push(stop);
        };
        add_stop("0%", "#fffff0ff");
        add_stop("60%", "#fffff0ff");
        add_stop("100%", "#fffff000");

        ret
    }

    fn make_height_at_least(&mut self, height: usize) {
        if height > self.height {
            self.height = height;
            self.contents
                .set_attribute("height", &format!("{}px", height * LINE_SPACING));
        }
    }

    fn make_width_at_least(&mut self, width: u16) {
        if width > self.width {
            self.width = width;
            self.contents.set_attribute(
                "width",
                &format!(
                    "{}px",
                    X_OFFSET_PX + indent_px(width + BLOCK_BOUNDARY_INDENT)
                ),
            );
        }
    }

    pub fn set_loops_taken(&mut self, loops_taken: &HashSet<u32>) {
        get_mut!(self.contents, blocks).for_each_mut(|id, block| {
            let taken = loops_taken.contains(id);
            if taken != block.loop_was_taken {
                block.loop_was_taken = taken;
                let _ = block.draw();
            }
        });
    }

    pub fn set_frames(
        &mut self,
        frames: HashMap<u32, FrameInfo>,
        animated_indents: HashMap<usize, f64>,
        line_count: usize,
    ) {
        let smooth = line_count == self.last_line_count_for_frames;
        self.last_line_count_for_frames = line_count;

        /* vanish frames that no longer exist */
        get_mut!(self.contents, blocks).for_each_mut(|id, block| {
            if !frames.contains_key(id) && block.hide(smooth).0 {
                self.animations.frames.insert(*id);
            }
        });

        /* delete frames whose vanishing animations have finished */
        {
            let mut to_delete = vec![];
            for (id, bl) in self.blocks().iter() {
                if bl.opacity.value() == Some(0.0) {
                    to_delete.push(*id);
                }
            }
            for id in to_delete {
                get_mut!(self.contents, blocks).remove(&id, FrameLine::new(&self.factory));
            }
        }

        /* update existing frames and add new ones */
        for (id, info) in frames {
            self.make_height_at_least(info.end + 2);
            self.make_width_at_least(info.indent);

            if self.blocks().get(&id).is_none() {
                get_mut!(self.contents, blocks).insert(id, FrameLine::new(&self.factory));
            }

            let bl = &mut get_mut!(self.contents, blocks)[&id];
            let animated_indent = animated_indents.get(&info.start).copied();
            if bl.update(info, animated_indent, smooth).0 {
                self.animations.frames.insert(id);
            }
        }
    }

    pub fn animate_frames(&mut self, t: f64, animated_indents: &HashMap<usize, f64>) {
        get_mut!(self.contents, blocks).for_each_mut(|id, bl| {
            let animation_pending = if let Some(info) = &bl.info
                && let Some(animated_indent) = animated_indents.get(&info.start)
            {
                bl.animate(t, Some(*animated_indent))
            } else {
                bl.animate(t, None) // make sure to finish every animation
            };

            if animation_pending.0 {
                self.animations.frames.insert(*id);
            } else {
                self.animations.frames.remove(id);
            }
        });
    }

    pub fn animate_types(&mut self, t: f64, animated_indents: &HashMap<usize, f64>) {
        let mut dirty_connection_ids: HashSet<Coordinate> = Default::default();
        get_mut!(self.contents, fractions).for_each_mut(|id, fr| {
            let info = fr.info.as_ref().unwrap();
            if let Some(animated_indent) = animated_indents.get(&info.line_no) {
                fr.animated_indent = Some(*animated_indent);
            } else {
                fr.animated_indent = None;
            };

            fr.draw();
            if let Some(vec) = self.connection_interest.get(id) {
                for src in vec {
                    dirty_connection_ids.insert(src.clone());
                }
            }
        });

        let conns = get_mut!(self.contents, connections);
        for src in dirty_connection_ids {
            let _ = conns[&src].animate(t);
            conns[&src].draw();
        }
    }

    pub fn animate_connections(&mut self, t: f64) {
        self.animations.connections.retain(|cx_src| {
            let Some(cx) = get_mut!(self.contents, connections).get_mut(cx_src) else {
                return false;
            };
            cx.animate(t).0
        });
    }

    pub fn has_pending_animation(&self) -> bool {
        self.animations.has_pending()
    }

    pub fn set_types(
        &mut self,
        types: HashMap<u32, FractionInfo>,
        animated_indents: HashMap<usize, f64>,
    ) {
        /* delete types that no longer exist */
        {
            let mut to_vanish: Vec<u32> = vec![];
            for id in self.fractions().ids() {
                if !types.contains_key(id) {
                    to_vanish.push(*id);
                }
            }
            for id in to_vanish {
                get_mut!(self.contents, fractions)
                    .remove(&id, OperatorFraction::new(&self.factory));
            }
        }

        /* update existing types and add new ones */
        for (id, info) in types {
            self.make_height_at_least(info.line_no + 2);
            self.make_width_at_least(info.indent);

            if self.fractions().get(&id).is_none() {
                get_mut!(self.contents, fractions).insert(id, OperatorFraction::new(&self.factory));
            }

            let frac = &mut get_mut!(self.contents, fractions)[&id];
            let animated_indent = animated_indents.get(&info.line_no).copied();

            frac.update(&self.factory, info, animated_indent);
        }
    }

    pub fn set_connections(&mut self, connections: &Vec<SlotConnection>, line_count: usize) {
        let smooth = line_count == self.last_line_count_for_connections;
        self.last_line_count_for_connections = line_count;

        /* rehome connections when possible */
        let cx_sources: HashSet<Coordinate> = connections
            .iter()
            .filter_map(|c| c.read.as_ref().and_then(|_| c.written.source().cloned()))
            .collect();

        for new_conn in connections {
            if let Some(src) = new_conn.written.source()
                && let Some(dst) = &new_conn.read
                && self.connections().get(src).is_none()
                && let Some(cur_src) = self.connection_dst2src.get(dst)
                && !cx_sources.contains(cur_src)
            {
                get_mut!(self.contents, connections)
                    .rehome(cur_src, src.clone())
                    .connection
                    .written = new_conn.written.clone();
                self.connection_dst2src.remove(dst).unwrap();
            }
        }

        /* vanish connections that no longer exist */
        let (_, (conns, (fracs, _))) = self.contents.get_mut();
        conns.for_each_mut(|src, cx| {
            if !cx_sources.contains(src) && cx.hide(smooth, fracs).0 {
                self.animations.connections.insert(src.clone());
            }
        });

        /* delete connections whose vanishing animations have finished */
        {
            let mut to_delete = vec![];
            for (src, cx) in self.connections().iter() {
                if cx.reveal.value() == Some(0.0) {
                    to_delete.push(src.clone());
                }
            }

            for src in to_delete {
                get_mut!(self.contents, connections)
                    .remove(&src, RenderedConnection::new(&self.factory));
            }
        }

        /* add connections from given SlotConnections */
        for new_conn in connections {
            let Some((src, _dst)) = new_conn.is_connected() else {
                continue;
            };

            /* retarget current connections when possible (rehoming/re-sourcing happened above)*/
            let (_, (conns, (fracs, _))) = self.contents.get_mut();

            if let Some(cur_conn) = conns.get_mut(src) {
                if cur_conn.update(new_conn.clone(), smooth, fracs).0 {
                    self.animations.connections.insert(src.clone());
                }
            } else {
                let mut cx = RenderedConnection::new(&self.factory);
                if cx.update(new_conn.clone(), smooth, self.fractions()).0 {
                    self.animations.connections.insert(src.clone());
                }
                self.connections_mut().insert(src.clone(), cx);
            }
        }

        /* recompute linkage */
        self.connection_dst2src.clear();
        self.connection_interest.clear();
        for (_src, cx) in get!(self.contents, connections).iter() {
            let Some((src, dst)) = cx.connection.is_connected() else {
                continue;
            };
            debug_assert_eq!(_src, src);
            if !cx.is_hiding() {
                self.connection_dst2src.insert(dst.clone(), src.clone());
            }
            self.connection_interest
                .entry(src.position_id)
                .or_default()
                .push(src.clone());
            self.connection_interest
                .entry(dst.position_id)
                .or_default()
                .push(src.clone());
        }
    }

    pub fn set_slot_value(
        &mut self,
        location: &Coordinate,
        is_input: bool,
        value: &Option<SlotContents>,
    ) {
        let fraction = &mut self.fractions_mut()[&location.position_id];
        let slot = if is_input {
            &mut fraction.inputs()[location.operand_num]
        } else {
            let num_outputs = fraction.targets.output_locations_scales_and_types.len();
            let num_stranded = fraction.outputs().len() - num_outputs;
            &mut fraction.outputs()[num_stranded + location.operand_num]
        };
        if let Some(value) = value {
            slot.get_mut().1.0.set_val(&value.val);
            if value.old {
                slot.get_mut().0.set_attribute("filter", "grayscale(0.75)");
            } else {
                slot.get_mut().0.remove_attribute("filter");
            }
        } else {
            slot.get_mut().0.remove_attribute("filter");
            slot.get_mut().1.0.clear();
        }
    }

    pub fn set_arrow_location(
        &mut self,
        smooth: bool,
        loc: Option<(usize, bool, usize)>,
    ) -> AnimationRequest {
        self.arrow_mut().goto(smooth, loc)
    }

    pub fn animate_arrow(
        &mut self,
        t: f64,
        animated_indents: &HashMap<usize, f64>,
    ) -> AnimationRequest {
        self.arrow_mut().animate(t, animated_indents)
    }

    pub fn scroll_to_arrow(&mut self) {
        self.arrow_mut().scroll_to_target()
    }

    delegate! {
    to self.contents {
    pub fn set_attribute(&mut self, name: &str, value: &str);
    pub fn remove_attribute(&mut self, name: &str);
    pub fn get_attribute(&self, name: &str) -> Option<&str>;
    }
    }

    #[cfg(all(test, target_arch = "wasm32"))]
    pub fn is_fraction_at_pos(&self, id: u32) -> bool {
        self.fractions().get(&id).is_some()
    }
}

struct AutoSizedNumber {
    elem: DomStruct<(DomText, ()), SvgTextElement>,
    expected_width: f32,
}

type Slot = DomStruct<(ElementHandle<SvgUseElement>, (AutoSizedNumber, ())), SvggElement>;
type FractionVec = DomVec<Slot, SvggElement>;
type SymbolsType = DomStruct<(FractionVec, (FractionVec, ())), SvggElement>;

#[derive(Default, Clone)]
struct FractionTargets {
    target: Option<(f32, f32)>,
    final_target: Option<(f32, f32)>,
    input_locations_scales_and_types: Vec<(f32, f32, Option<ValType>)>,
    output_locations_scales_and_types: Vec<(f32, f32, Option<ValType>)>,
}

struct OperatorFraction {
    info: Option<FractionInfo>,
    animated_indent: Option<f64>,
    symbols: SymbolsType,

    targets: FractionTargets,
}

impl AutoSizedNumber {
    fn new(factory: &ElementFactory) -> Self {
        let mut ret = Self {
            elem: DomStruct::new((DomText::new(""), ()), factory.svg_text()),
            expected_width: 0.0,
        };
        ret.elem.set_attribute("class", "slot-value");
        ret
    }

    pub fn clear(&mut self) {
        self.expected_width = 0.0;
        self.elem.get_mut().0.set_data("");
    }

    pub fn set_val(&mut self, val: &impl Separable) {
        let s = val.separate_with_commas();
        // Firefox and Chrome seem to differ slightly in computation of widths
        // for MLMSansDemiCond10-Regular @ 8 pt, so need some slop in the eventual comparison.
        self.expected_width = s
            .chars()
            .map(|ch| match ch {
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => 5.06667,
                '.' | ',' => 2.8,
                '-' => 3.377777,
                _ => {
                    log_1(&format!("unhandled character \"{ch}\" in number").into());
                    4.0 // XXX need to handle inf and NaN
                }
            })
            .sum();
        if self.expected_width > 20.0 {
            let scale_factor = 20.0 / self.expected_width;
            self.elem.set_attribute(
                "transform",
                &format!("scale({scale_factor},{scale_factor})"),
            );
        } else {
            self.elem.remove_attribute("transform");
        }
        self.elem.get_mut().0.set_data(&s);
    }

    pub fn set_pos(&mut self, x: f32, y: f32) {
        self.elem.set_attr_num("x", x);
        self.elem.set_attr_num("y", y);
    }

    pub fn set_fill(&mut self, color: &str) {
        self.elem.set_attribute("fill", color);
    }
}

impl WithElement for AutoSizedNumber {
    type Element = SvgTextElement;
    fn with_element<T, F: FnMut(&Self::Element) -> T>(&self, f: F, g: AccessToken) -> T {
        self.elem.with_element(f, g)
    }
}

impl Component for AutoSizedNumber {
    #[cfg(debug_assertions)]
    fn audit(&self) {
        self.elem.audit();
        if let Some(computed_width) = self.elem.elem().compute_text_width()
            && (self.expected_width - computed_width).abs() > 0.5
        {
            log_1(
                &format!(
                    "for string \"{}\", expected width {} but browser computed {computed_width}",
                    self.elem.get().0.get(),
                    self.expected_width,
                )
                .into(),
            );
            //            panic!("text width mismatch");
        }
    }
}

impl Slot {
    fn new_empty(factory: &ElementFactory) -> Self {
        DomStruct::new(
            (factory.svg_use(), (AutoSizedNumber::new(factory), ())),
            factory.svg_g(),
        )
    }
}

impl OperatorFraction {
    fn new(factory: &ElementFactory) -> Self {
        let mut ret = Self {
            symbols: DomStruct::new(
                (
                    DomVec::new(factory.svg_g()),
                    (DomVec::new(factory.svg_g()), ()),
                ),
                factory.svg_g(),
            ),
            info: None,
            animated_indent: None,
            targets: Default::default(),
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

    fn draw(&mut self) {
        let Self {
            info: Some(info),
            animated_indent,
            targets,
            ..
        } = self
        else {
            panic!();
        };
        let target_x_final = (X_OFFSET_PX + indent_px(info.indent)
            - indent_px(BLOCK_BOUNDARY_INDENT) / 2
            - MARGIN / 2) as f64;
        let target_x = if let Some(actual_indent) = animated_indent {
            BASE_X_OFFSET_PX as f64 + *actual_indent
                - (indent_px(BLOCK_BOUNDARY_INDENT) / 2 + MARGIN / 2) as f64
        } else {
            target_x_final
        };
        let target_y = info.line_no * LINE_SPACING + LINE_SPACING / 2 + LINE_OFFSET_PX;
        targets.target = Some((target_x as f32, target_y as f32));
        targets.final_target = Some((target_x_final as f32, target_y as f32));

        self.symbols.set_attribute(
            "transform",
            &format!("translate({} {})", target_x, target_y),
        );
    }

    fn update(
        &mut self,
        factory: &ElementFactory,
        info: FractionInfo,
        animated_indent: Option<f64>,
    ) {
        self.create_symbols(factory, &info);
        self.info = Some(info);
        self.animated_indent = animated_indent;
        self.draw();
    }

    fn create_symbols(&mut self, factory: &ElementFactory, info: &FractionInfo) {
        if let Some(current_info) = &self.info
            && current_info.ty == info.ty
        {
            return;
        }

        let AnnotatedOperatorType { inputs, outputs } = &info.ty;

        let in_len = inputs.len() as i32;
        let out_len = outputs.len() as i32;
        self.inputs().truncate(inputs.len());
        while self.inputs().len() < inputs.len() {
            self.inputs().push(Slot::new_empty(factory));
        }

        let num_stranded: usize = outputs.iter().map(|ty| !ty.used as usize).sum();

        self.outputs().truncate(outputs.len() + num_stranded);
        while self.outputs().len() < outputs.len() + num_stranded {
            self.outputs().push(Slot::new_empty(factory));
        }

        self.targets.input_locations_scales_and_types.clear();
        self.targets.output_locations_scales_and_types.clear();

        fn scale(len: i32) -> f32 {
            let max_width = (indent_px(BLOCK_BOUNDARY_INDENT) - 2 * MARGIN + 1) as f32;
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
                .set_attribute("transform", &format!("scale({in_scale} {in_scale})"));
        } else {
            self.inputs().remove_attribute("transform");
        }
        if out_scale != 1.0 {
            self.outputs()
                .set_attribute("transform", &format!("scale({out_scale} {out_scale})"));
        } else {
            self.outputs().remove_attribute("transform");
        }

        let left_edge_in = -SYM_HW * (in_len - 1) as f32;
        let left_edge_out = -SYM_HW * (out_len - 1) as f32;

        fn render(info: &SlotInfo, is_input: bool) -> String {
            let name = info.slot.ty().map(|x| x.to_string());
            format!(
                "#{}_{}{}",
                name.unwrap_or(String::from("mystery")),
                if is_input { "in" } else { "out" },
                if !info.used && is_input && info.slot != crate::utils::Slot::Polymorphic {
                    "_empty"
                } else {
                    ""
                }
            )
        }

        for (i, ty) in inputs.iter().enumerate() {
            let sym = &mut self.inputs()[i].get_mut().0;
            let x = left_edge_in + 2.0 * SYM_HW * i as f32;
            sym.set_attr_num("x", x);
            sym.set_attribute("href", &render(ty, true));
            self.targets.input_locations_scales_and_types.push((
                in_scale * x,
                in_scale,
                ty.slot.ty(),
            ));

            let height = icon_height(ty.slot.ty()) - 1.25;
            let text = &mut self.inputs()[i].get_mut().1.0;

            text.set_pos(x, -height / 2.0);
            text.set_fill("black");
            text.clear();
        }

        let num_stranded = outputs.iter().map(|ty| if ty.used { 0 } else { 1 }).sum();

        for (i, ty) in outputs.iter().enumerate().take(num_stranded) {
            let sym = &mut self.outputs()[i].get_mut().0;
            let x = left_edge_out + 2.0 * SYM_HW * i as f32;
            sym.set_attr_num("x", x);
            sym.set_attribute("href", &(render(ty, false) + "_stranded"));

            let text = &mut self.outputs()[i].get_mut().1.0;
            text.clear();
        }

        for (i, ty) in outputs.iter().enumerate() {
            let idx = num_stranded + i;
            let sym = &mut self.outputs()[idx].get_mut().0;
            let x = left_edge_out + 2.0 * SYM_HW * i as f32;
            sym.set_attr_num("x", x);
            sym.set_attribute("href", &render(ty, false));

            self.targets.output_locations_scales_and_types.push((
                out_scale * x,
                out_scale,
                ty.slot.ty(),
            ));

            let height = icon_height(ty.slot.ty())
                + match ty.slot.ty() {
                    Some(ValType::F32) => 6.5,
                    _ => 3.5,
                };
            let text = &mut self.outputs()[idx].get_mut().1.0;

            text.set_pos(x, height / 2.0);
            text.set_fill("white");
            text.clear();
        }
    }
}

delegate_element_component!(OperatorFraction, symbols, SvggElement);

thread_local! {
    static NEXT_DOM_KEY: Cell<u32> = Default::default();
}

#[derive(Default)]
struct ConnectionTargetingInfo {
    write_x: Tween,
    write_y: Tween,
    read_x: Tween,
    read_y: Tween,

    read_scale: Tween,
    first_control_height: Tween,
    second_control_height: Tween,
    second_control_x: Tween,

    badness: Tween,
}

impl ConnectionTargetingInfo {
    fn is_pending(&self) -> AnimationRequest {
        self.write_x.is_pending()
    }

    fn animate(&mut self, t: f64) {
        self.write_x.animate(t);
        self.write_y.animate(t);
        self.read_x.animate(t);
        self.read_y.animate(t);

        self.read_scale.animate(t);
        self.first_control_height.animate(t);
        self.second_control_height.animate(t);
        self.second_control_x.animate(t);

        self.badness.animate(t);
    }

    fn goto(&mut self, smooth: bool, rhs: &Self) -> AnimationRequest {
        // always move smoothly in x
        self.write_x.goto(true, rhs.write_x.value().unwrap())
            | self.write_y.goto(smooth, rhs.write_y.value().unwrap())
            | self.read_x.goto(true, rhs.read_x.value().unwrap())
            | self.read_y.goto(smooth, rhs.read_y.value().unwrap())
            | self
                .read_scale
                .goto(smooth, rhs.read_scale.value().unwrap())
            | self
                .first_control_height
                .goto(smooth, rhs.first_control_height.value().unwrap())
            | self
                .second_control_height
                .goto(smooth, rhs.second_control_height.value().unwrap())
            | self
                .second_control_x
                .goto(true, rhs.second_control_x.value().unwrap())
            | self.badness.goto(smooth, rhs.badness.value().unwrap())
    }
}

impl RenderedConnection {
    fn new(factory: &ElementFactory) -> Self {
        let mut ret = Self {
            connection: SlotConnection::default(),
            path: PathUnfurl::new(
                (
                    factory.svg_path(),
                    (
                        UnfurlMask::new((factory.svg_path(), ()), factory.svg_mask()),
                        (),
                    ),
                ),
                factory.svg_g(),
            ),
            reveal: Tween::Post(0.0),
            src_targets: Default::default(),
            dst_targets: Default::default(),
            targeting_info: Default::default(),
        };

        ret.line_mut().set_attribute("fill", "none");
        ret.mask_line_mut().set_attribute("stroke", "white");
        ret.mask_line_mut().set_attribute("fill", "none");
        ret.mask_line_mut().set_attr_num("pathLength", 1);

        // link the mask to the primary path
        let dom_key = NEXT_DOM_KEY.get();
        NEXT_DOM_KEY.replace(dom_key + 1);

        ret.line_mut()
            .set_attribute("mask", &format!("url(#mask-{dom_key})"));
        ret.mask_mut()
            .set_attribute("id", &format!("mask-{dom_key}"));
        ret.mask_mut().set_attribute("maskUnits", "userSpaceOnUse");
        ret.mask_mut().set_attribute("width", "300%");
        ret.mask_mut().set_attribute("height", "300%");
        ret.mask_mut().set_attribute("x", "-100%");
        ret.mask_mut().set_attribute("y", "-100%");

        ret
    }

    fn update(
        &mut self,
        connection: SlotConnection,
        smooth: bool,
        locations: &Fractions,
    ) -> AnimationRequest {
        self.connection = connection;

        let targeting_info_current = self.refresh_targets(locations, true);
        let targeting_info_future = self.refresh_targets(locations, false);
        let _ = self.targeting_info.goto(smooth, &targeting_info_current);
        let _ = self.targeting_info.goto(smooth, &targeting_info_future);

        self.reveal.approach(1.0);
        self.draw();
        self.has_pending_animation()
    }

    fn hide(&mut self, smooth: bool, locations: &Fractions) -> AnimationRequest {
        let targeting_info = self.refresh_targets(locations, false);
        self.targeting_info.goto(smooth, &targeting_info) | self.reveal.goto(smooth, 0.0)
    }

    fn line_mut(&mut self) -> &mut ElementHandle<SvgPathElement> {
        &mut self.path.get_mut().0
    }

    fn mask_mut(&mut self) -> &mut UnfurlMask {
        &mut self.path.get_mut().1.0
    }

    fn mask_line_mut(&mut self) -> &mut ElementHandle<SvgPathElement> {
        &mut self.mask_mut().get_mut().0
    }

    fn animate(&mut self, t: f64) -> AnimationRequest {
        self.targeting_info.animate(t);
        self.reveal.animate(t);
        self.draw();
        self.has_pending_animation()
    }

    fn has_pending_animation(&self) -> AnimationRequest {
        self.reveal.is_pending() | self.targeting_info.is_pending()
    }

    fn is_hiding(&self) -> bool {
        self.reveal.target().unwrap() == 0.0
    }

    fn refresh_targets(&mut self, locations: &Fractions, current: bool) -> ConnectionTargetingInfo {
        let (src, dst) = self.connection.is_connected().unwrap();

        if let Some(src_frac) = locations.get(&src.position_id)
            && src_frac.targets.target.is_some()
            && src_frac.targets.output_locations_scales_and_types.len() > src.operand_num
        {
            self.src_targets = src_frac.targets.clone();
        }

        if let Some(dst_frac) = locations.get(&dst.position_id)
            && dst_frac.targets.target.is_some()
            && dst_frac.targets.input_locations_scales_and_types.len() > dst.operand_num
        {
            self.dst_targets = dst_frac.targets.clone();
        }

        let (write_base, read_base) = if current {
            (
                self.src_targets.target.unwrap(),
                self.dst_targets.target.unwrap(),
            )
        } else {
            (
                self.src_targets.final_target.unwrap(),
                self.dst_targets.final_target.unwrap(),
            )
        };

        let (x, write_scale, _src_ty) =
            self.src_targets.output_locations_scales_and_types[src.operand_num];

        let write_x = write_base.0 + x;
        let (relative_x, read_scale, dst_ty) =
            self.dst_targets.input_locations_scales_and_types[dst.operand_num];

        let is_bad = self.connection.written.is_mismatch();

        let reader_offset = if is_bad {
            0.6 * icon_height(dst_ty)
        } else {
            icon_height(dst_ty)
        };

        let read_x = read_base.0 + relative_x;
        let write_y = write_base.1 + 10.0 * write_scale;
        let read_y = read_base.1 - reader_offset * read_scale;

        let first_control_height = write_y + 1.0;
        let slope = (write_x - read_x) / (write_y - read_y);
        let second_control_x = if is_bad { read_x + 1.5 * slope } else { read_x };
        let second_control_height = if is_bad {
            write_y - 0.4 * icon_height(dst_ty)
        } else {
            write_y
        };

        ConnectionTargetingInfo {
            write_x: Tween::Post(write_x as f64),
            write_y: Tween::Post(write_y as f64),
            read_x: Tween::Post(read_x as f64),
            read_y: Tween::Post(read_y as f64),

            read_scale: Tween::Post(read_scale as f64),
            first_control_height: Tween::Post(first_control_height as f64),
            second_control_height: Tween::Post(second_control_height as f64),
            second_control_x: Tween::Post(second_control_x as f64),

            badness: Tween::Post(is_bad as usize as f64),
        }
    }

    fn draw(&mut self) {
        let ConnectionTargetingInfo {
            write_x,
            write_y,
            read_x,
            read_y,

            read_scale,
            first_control_height,
            second_control_height,
            second_control_x,

            badness,
        } = &self.targeting_info;

        let reveal = self.reveal.value().unwrap();
        let badness = badness.value().unwrap();
        let read_scale = read_scale.value().unwrap();

        let width = read_scale
            * if self.is_hiding() {
                10.0 * reveal
            } else {
                10.0
            };
        let width = badness * 1.0 + (1.0 - badness) * width;

        let (src, _) = self.connection.is_connected().unwrap();
        let (_, _, src_ty) = self.src_targets.output_locations_scales_and_types[src.operand_num];

        let (line_mut, (mask, ())) = &mut self.path.get_mut();
        let mask_line_mut = &mut mask.get_mut().0;

        if badness > 0.95 {
            line_mut.set_attribute("stroke", ty_to_color(&src_ty.as_ref()));
            line_mut.set_attr_num("stroke-dasharray", "1");
        } else {
            line_mut.set_attribute("stroke", &ty_to_muted(&src_ty.as_ref()));
            line_mut.remove_attribute("stroke-dasharray");
        }

        line_mut.set_attr_num("stroke-width", width);
        mask_line_mut.set_attr_num("stroke-width", width);

        let opacity = if self.reveal.target().unwrap() == 0.0 {
            reveal
        } else {
            1.0
        };
        line_mut.set_attr_num("opacity", opacity);

        let d = &format!(
            "M {write_x} {write_y} C {write_x},{first_control_height} {second_control_x},{second_control_height}, {read_x},{read_y}"
        );
        line_mut.set_attribute("d", d);
        mask_line_mut.set_attribute("d", d);
        mask_line_mut.set_attribute("stroke-dasharray", &format!("{} {}", reveal, 1.0 - reveal));
    }
}
