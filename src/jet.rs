// Codillon "web support" structs to be used by Components.
// These wrap web_sys types to prevent "unsafe" access to the underlying DOM object.
// The goal is to enforce modularity between Components, and prevent a Component
// from modifying a DOM object belonging to another. This means that Components
// cannot directly access the children or parents of a DOM node.

use crate::utils::FmtError;
use anyhow::{Result, bail};
use delegate::delegate;
use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};
use wasm_bindgen::closure::Closure;
use web_sys::{Element, HtmlBodyElement, HtmlElement, KeyboardEvent, wasm_bindgen::JsCast};

// Traits that give "raw" access to an underlying node or element,
// only usable from the jet (web support) module.
struct _Private();
pub struct AccessToken(_Private);
const TOKEN: AccessToken = AccessToken(_Private());

pub trait WithNode {
    fn with_node(&self, f: impl FnMut(&web_sys::Node), g: AccessToken);
}

// Any HTML element
pub trait AnyElement: AsRef<Element> + AsRef<web_sys::Node> {
    fn element(&self) -> &Element {
        self.as_ref()
    }
}

impl<T: AsRef<Element> + AsRef<web_sys::Node>> AnyElement for T {}

pub trait WithElement {
    type Element: AnyElement;
    fn with_element(&self, f: impl FnMut(&Self::Element), g: AccessToken);
}

impl<T: WithElement> WithNode for T {
    fn with_node(&self, mut f: impl FnMut(&web_sys::Node), g: AccessToken) {
        self.with_element(|elem| f(elem.as_ref()), g);
    }
}

// Wrapper for a Node or Element that removes it from its parent when dropped
struct AutoRemove<T: AsRef<web_sys::Node>>(T);

impl<T: AsRef<web_sys::Node>> From<T> for AutoRemove<T> {
    fn from(t: T) -> Self {
        Self(t)
    }
}

impl<T: AsRef<web_sys::Node>> Deref for AutoRemove<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T: AsRef<web_sys::Node>> DerefMut for AutoRemove<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T: AsRef<web_sys::Node>> Drop for AutoRemove<T> {
    fn drop(&mut self) {
        if let Some(parent) = self.0.as_ref().parent_node() {
            parent.remove_child(self.0.as_ref()).expect("remove_child");
        }
    }
}

// Wrapper for a DOM Text node, allowing access to and modification of its CharacterData's data.
// Access to the underlying Node is only via the WithNode trait (i.e. only in this module).
pub struct TextHandle(AutoRemove<web_sys::Text>);

impl Default for TextHandle {
    fn default() -> Self {
        Self(web_sys::Text::new().expect("Text::new()").into())
    }
}

impl WithNode for TextHandle {
    fn with_node(&self, mut f: impl FnMut(&web_sys::Node), _g: AccessToken) {
        f(&self.0)
    }
}

impl TextHandle {
    delegate! {
    to self.0 {
        pub fn data(&self) -> String;
    pub fn set_data(&self, value: &str);
    #[unwrap] // no return value anyway
    pub fn append_data(&self, data: &str);
    #[unwrap] // no return value anyway
    pub fn insert_data(&self, offset: u32, data: &str);

    #[expr($.fmt_err())]
    pub fn replace_data(
    &self,
        offset: u32,
        count: u32,
        data: &str,
    ) -> Result<()>;
    }
    }
}

// Event handlers on an element
#[derive(Default)]
pub struct Handlers {
    beforeinput: Option<Closure<dyn Fn(web_sys::InputEvent)>>,
    keydown: Option<Closure<dyn Fn(web_sys::KeyboardEvent)>>,
}

impl Handlers {
    pub fn audit(&self, elem: &impl AsRef<HtmlElement>) {
        audit_handler(&self.beforeinput, elem.as_ref().onbeforeinput());
        audit_handler(&self.keydown, elem.as_ref().onkeydown());

        fn audit_handler<EventType>(
            expected: &Option<Closure<dyn Fn(EventType)>>,
            actual: Option<::js_sys::Function>,
        ) {
            match (expected, actual) {
                (Some(expected), Some(actual)) => {
                    assert_eq!(actual, *expected.as_ref().unchecked_ref())
                }
                (Some(_), None) => panic!("missing event handler"),
                (None, Some(_)) => panic!("unexpected event handler"),
                (None, None) => (),
            }
        }
    }

    pub fn set_onbeforeinput<E: WithElement, F: Fn(InputEventHandle) + 'static>(
        &mut self,
        elem: &mut E,
        handler: F,
    ) where
        E::Element: AsRef<HtmlElement>,
    {
        self.beforeinput = Some(Closure::new(move |ev| handler(InputEventHandle(ev))));
        elem.with_element(
            |elem| {
                let html: &HtmlElement = elem.as_ref();
                html.set_onbeforeinput(Some(
                    self.beforeinput.as_ref().unwrap().as_ref().unchecked_ref(),
                ))
            },
            TOKEN,
        );
    }

    pub fn set_onkeydown<E: WithElement, F: Fn(KeyboardEvent) + 'static>(
        &mut self,
        elem: &mut E,
        handler: F,
    ) where
        E::Element: AsRef<HtmlElement>,
    {
        self.keydown = Some(Closure::new(handler));
        elem.with_element(
            |elem| {
                let html: &HtmlElement = elem.as_ref();
                html.set_onkeydown(Some(
                    self.keydown.as_ref().unwrap().as_ref().unchecked_ref(),
                ))
            },
            TOKEN,
        );
    }
}

pub struct ReactiveComponent<T: ElementComponent> {
    component: T,
    handlers: Handlers,
}

impl<T: ElementComponent> ReactiveComponent<T>
where
    T::Element: AsRef<HtmlElement>,
{
    pub fn new(component: T) -> Self {
        Self {
            component,
            handlers: Handlers::default(),
        }
    }

    pub fn inner(&self) -> &T {
        &self.component
    }

    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.component
    }
}

pub trait ControlHandlers {
    fn set_onbeforeinput<F: Fn(InputEventHandle) + 'static>(&mut self, handler: F);
    fn set_onkeydown<F: Fn(KeyboardEvent) + 'static>(&mut self, handler: F);
}

impl<T: ElementComponent> ControlHandlers for ReactiveComponent<T>
where
    T::Element: AsRef<HtmlElement>,
{
    fn set_onbeforeinput<F: Fn(InputEventHandle) + 'static>(&mut self, handler: F) {
        self.handlers.beforeinput = Some(Closure::new(move |ev| handler(InputEventHandle(ev))));
        self.component.with_element(
            |elem| {
                let html: &HtmlElement = elem.as_ref();
                html.set_onbeforeinput(Some(
                    self.handlers
                        .beforeinput
                        .as_ref()
                        .unwrap()
                        .as_ref()
                        .unchecked_ref(),
                ))
            },
            TOKEN,
        );
    }
    fn set_onkeydown<F: Fn(KeyboardEvent) + 'static>(&mut self, handler: F) {
        self.handlers.keydown = Some(Closure::new(handler));
        self.component.with_element(
            |elem| {
                let html: &HtmlElement = elem.as_ref();
                html.set_onkeydown(Some(
                    self.handlers
                        .keydown
                        .as_ref()
                        .unwrap()
                        .as_ref()
                        .unchecked_ref(),
                ))
            },
            TOKEN,
        );
    }
}

impl<T: ElementComponent> Component for ReactiveComponent<T>
where
    T::Element: AsRef<HtmlElement>,
{
    fn audit(&self) {
        self.component.audit();
        self.component.with_element(
            |elem| {
                self.handlers.audit(&elem);
            },
            TOKEN,
        );
    }
}

impl<T: ElementComponent> WithElement for ReactiveComponent<T> {
    type Element = T::Element;
    fn with_element(&self, f: impl FnMut(&Self::Element), g: AccessToken) {
        self.component.with_element(f, g)
    }
}

// Wrapper for a DOM Element, allowing access to and modification of its attributes
// and event handlers, and the ability to set and append to its child nodes.
pub struct ElementHandle<T: AnyElement> {
    elem: AutoRemove<T>,
    attributes: HashMap<String, String>,
}

impl<T: AnyElement> WithElement for ElementHandle<T> {
    type Element = T;
    fn with_element(&self, mut f: impl FnMut(&T), _g: AccessToken) {
        f(&self.elem.0)
    }
}

impl<T: AnyElement> ElementHandle<T> {
    fn new(elem: T) -> Self {
        Self {
            elem: elem.into(),
            attributes: HashMap::default(),
        }
    }

    pub fn append_node(&self, child: &impl WithNode) {
        child.with_node(
            |node| self.elem.element().append_with_node_1(node).unwrap(), // no return value anyway
            TOKEN,
        )
    }

    pub fn insert_node(&self, idx: usize, child: &impl WithNode) {
        let reference = self
            .elem
            .element()
            .child_nodes()
            .item(idx.try_into().expect("idx -> u32"))
            .expect("valid idx");
        child.with_node(
            |child_node| {
                self.elem
                    .element()
                    .insert_before(child_node, Some(&reference))
                    .unwrap();
            },
            TOKEN,
        );
    }

    pub fn attach_node(&self, child: &impl WithNode) {
        child.with_node(
            |node| self.elem.element().replace_children_with_node_1(node),
            TOKEN,
        )
    }

    pub fn attach_nodes(&self, children: ArrayHandle) {
        self.elem.element().replace_children_with_node(&children.0);
    }

    pub fn set_attribute(&mut self, name: &str, value: &str) {
        match self.attributes.insert(name.to_string(), value.to_string()) {
            None => self.elem.element().set_attribute(name, value).unwrap(),
            Some(x) if x != value => self.elem.element().set_attribute(name, value).unwrap(),
            _ => {}
        }
    }

    pub fn remove_attribute(&mut self, name: &str) {
        if self.attributes.remove(name).is_some() {
            self.elem.element().remove_attribute(name).unwrap()
        }
    }

    pub fn get_attribute(&self, name: &str) -> Option<&String> {
        self.attributes.get(name)
    }

    pub fn get_child_node_list(&self) -> NodeListHandle {
        NodeListHandle(self.elem.element().child_nodes())
    }

    pub fn scroll_into_view(&self) {
        let opts = web_sys::ScrollIntoViewOptions::new();
        opts.set_behavior(web_sys::ScrollBehavior::Smooth);
        opts.set_block(web_sys::ScrollLogicalPosition::Nearest);
        self.elem
            .element()
            .scroll_into_view_with_scroll_into_view_options(&opts);
    }
}

impl<T: AnyElement> Component for ElementHandle<T> {
    fn audit(&self) {
        for (key, value) in &self.attributes {
            if let Some(dom_value) = self.elem.element().get_attribute(key) {
                assert_eq!(dom_value, *value);
            } else {
                panic!("missing {key} (expected value {value})");
            }
        }

        for dom_key in self.elem.element().get_attribute_names() {
            assert!(self.attributes.contains_key(&dom_key.as_string().unwrap()));
        }
    }
}

// Wrapper for a DOM Document, allowing modification of the body and
// the ability to create Elements (as ElementHandles).
pub struct DocumentHandle<BodyType: ElementComponent<Element = HtmlBodyElement>> {
    document: web_sys::Document,
    body: Option<BodyType>,
}

impl<BodyType: ElementComponent<Element = HtmlBodyElement>> WithElement
    for DocumentHandle<BodyType>
{
    type Element = BodyType::Element;
    fn with_element(&self, mut f: impl FnMut(&Self::Element), g: AccessToken) {
        if let Some(body) = &self.body {
            body.with_element(|elem| f(elem), g);
        }
    }
}

impl<BodyType: ElementComponent<Element = HtmlBodyElement>> Default for DocumentHandle<BodyType> {
    fn default() -> Self {
        Self {
            document: web_sys::window().unwrap().document().unwrap(),
            body: None,
        }
    }
}

#[derive(Clone)]
pub struct ElementFactory(web_sys::Document);

impl<BodyType: ElementComponent<Element = HtmlBodyElement>> DocumentHandle<BodyType> {
    pub fn body(&self) -> Option<&BodyType> {
        self.body.as_ref()
    }

    pub fn body_mut(&mut self) -> Option<&mut BodyType> {
        self.body.as_mut()
    }

    pub fn set_body(&mut self, body: BodyType) {
        body.with_element(|elem| self.document.set_body(Some(elem.as_ref())), TOKEN);
        self.body = Some(body);
    }

    pub fn element_factory(&self) -> ElementFactory {
        ElementFactory(self.document.clone())
    }

    pub fn audit(&self) {
        match (&self.body, self.document.body()) {
            (Some(body), Some(dom_body)) => {
                body.with_node(|node| assert!(dom_body.is_same_node(Some(node))), TOKEN);
                body.audit();
            }
            (Some(_), None) => panic!("missing body"),
            (None, Some(_)) => panic!("unexpected body"),
            (None, None) => (),
        }
    }
}

impl ElementFactory {
    fn create_element<T: JsCast>(&self, t: &str) -> T {
        self.0
            .create_element(t)
            .unwrap()
            .dyn_into::<T>()
            .unwrap_or_else(|_| panic!("expecting {t} element"))
    }

    fn create_svg_element<T: JsCast>(&self, t: &str) -> T {
        self.0
            .create_element_ns(Some("http://www.w3.org/2000/svg"), t)
            .unwrap()
            .dyn_into::<T>()
            .unwrap_or_else(|_| panic!("expecting {t} element"))
    }

    pub fn div(&self) -> ElementHandle<web_sys::HtmlDivElement> {
        ElementHandle::new(self.create_element("div"))
    }

    pub fn span(&self) -> ElementHandle<web_sys::HtmlSpanElement> {
        ElementHandle::new(self.create_element("span"))
    }

    pub fn p(&self) -> ElementHandle<web_sys::HtmlParagraphElement> {
        ElementHandle::new(self.create_element("p"))
    }

    pub fn br(&self) -> ElementHandle<web_sys::HtmlBrElement> {
        ElementHandle::new(self.create_element("br"))
    }

    pub fn body(&self) -> ElementHandle<web_sys::HtmlBodyElement> {
        ElementHandle::new(self.create_element("body"))
    }

    pub fn svg(&self) -> ElementHandle<web_sys::SvgElement> {
        ElementHandle::new(self.create_svg_element("svg"))
    }

    pub fn svg_line(&self) -> ElementHandle<web_sys::SvgLineElement> {
        ElementHandle::new(self.create_svg_element("line"))
    }

    pub fn svg_defs(&self) -> ElementHandle<web_sys::SvgDefsElement> {
        ElementHandle::new(self.create_svg_element("defs"))
    }

    pub fn svg_marker(&self) -> ElementHandle<web_sys::SvgMarkerElement> {
        ElementHandle::new(self.create_svg_element("marker"))
    }

    pub fn svg_g(&self) -> ElementHandle<web_sys::SvggElement> {
        ElementHandle::new(self.create_svg_element("g"))
    }

    pub fn svg_circle(&self) -> ElementHandle<web_sys::SvgCircleElement> {
        ElementHandle::new(self.create_svg_element("circle"))
    }

    pub fn svg_path(&self) -> ElementHandle<web_sys::SvgPathElement> {
        ElementHandle::new(self.create_svg_element("path"))
    }

    pub fn svg_use(&self) -> ElementHandle<web_sys::SvgUseElement> {
        ElementHandle::new(self.create_svg_element("use"))
    }

    pub fn input(&self) -> ElementHandle<web_sys::HtmlInputElement> {
        ElementHandle::new(self.create_element("input"))
    }
}

impl ElementHandle<web_sys::HtmlInputElement> {
    pub fn set_oninput<F>(&mut self, handler: F)
    where
        F: 'static + FnMut(web_sys::Event),
    {
        let closure = Closure::wrap(Box::new(handler) as Box<dyn FnMut(web_sys::Event)>);
        self.with_element(
            |elem| {
                elem.set_oninput(Some(closure.as_ref().unchecked_ref()));
            },
            TOKEN,
        );
        closure.forget();
    }
}

// Wrapper for a DOM NodeList, allowing audit that each entry matches an expected node.
pub struct NodeListHandle(web_sys::NodeList);

impl NodeListHandle {
    pub fn length(&self) -> usize {
        self.0.length() as usize
    }

    pub fn audit_node(&self, index: usize, child: &impl WithNode) {
        child.with_node(
            |node| {
                if let Some(actual) = self.0.item(index.try_into().expect("index -> u32"))
                    && actual.is_same_node(Some(node))
                {
                    return;
                }
                panic!("node {} mismatch (#{}/{})", index, index + 1, self.length())
            },
            TOKEN,
        );
    }
}

// Wrapper for a DOM Array, allowing modification of its entries.
pub struct ArrayHandle(js_sys::Array);

impl ArrayHandle {
    pub fn length(&self) -> usize {
        self.0.length() as usize
    }

    pub fn new_with_length(len: usize) -> Self {
        Self(js_sys::Array::new_with_length(
            len.try_into().expect("len -> u32"),
        ))
    }

    pub fn set(&mut self, index: usize, child: &impl WithNode) {
        child.with_node(
            |node| {
                self.0
                    .set(index.try_into().expect("index -> u32"), node.into())
            },
            TOKEN,
        )
    }
}

// Wrapper for an "unmanaged" Node (will not be removed from DOM when dropped)
pub struct NodeRef(web_sys::Node);

impl WithNode for NodeRef {
    fn with_node(&self, mut f: impl FnMut(&web_sys::Node), _g: AccessToken) {
        f(&self.0)
    }
}

impl NodeRef {
    pub fn is_a<T: wasm_bindgen::JsCast>(&self) -> bool {
        self.0.dyn_ref::<T>().is_some()
    }

    pub fn is_same_node<T: WithNode>(&self, other: &T) -> bool {
        let mut is_same = false;
        other.with_node(|node| is_same = self.0.is_same_node(Some(node)), TOKEN);
        is_same
    }
}

impl From<web_sys::Node> for NodeRef {
    fn from(node: web_sys::Node) -> Self {
        Self(node)
    }
}

// Wrapper for a StaticRange (giving access to its start and end nodes as NodeRefs)
pub struct StaticRangeHandle(web_sys::Range);

impl StaticRangeHandle {
    delegate! {
        to self.0 {
        #[unwrap]
        pub fn start_offset(&self) -> u32;
        #[unwrap]
        pub fn end_offset(&self) -> u32;
        pub fn collapsed(&self) -> bool;
        #[expr(Ok(NodeRef($.fmt_err()?)))]
        pub fn start_container(&self) -> Result<NodeRef>;
        #[expr(Ok(NodeRef($.fmt_err()?)))]
        pub fn end_container(&self) -> Result<NodeRef>;
    }
    }
}

// Wrapper for an InputEvent (giving access to its target as a StaticRangeHandle)
pub struct InputEventHandle(web_sys::InputEvent);

impl InputEventHandle {
    delegate! {
    to self.0 {
        pub fn prevent_default(&self);
        pub fn type_(&self) -> String;
        pub fn input_type(&self) -> String;
        pub fn data(&self) -> Option<String>;
        pub fn data_transfer(&self) -> Option<web_sys::DataTransfer>;
    }
    }

    pub fn get_first_target_range(&self) -> Result<StaticRangeHandle> {
        let ranges = self.0.get_target_ranges();
        if ranges.length() == 0 {
            bail!("no target ranges");
        }

        Ok(StaticRangeHandle(
            ranges.get(0).unchecked_into::<web_sys::Range>(),
        ))
    }
}

// Compare the position of two nodes in a document
pub fn compare_document_position(a: &impl WithNode, b: &impl WithNode) -> std::cmp::Ordering {
    let mut is_same = false;
    let mut pos: u16 = 0;
    a.with_node(
        |a_node| {
            b.with_node(
                |b_node| {
                    is_same = a_node.is_same_node(Some(b_node));
                    pos = a_node.compare_document_position(b_node);
                },
                TOKEN,
            )
        },
        TOKEN,
    );
    if is_same
        || (pos
            & (web_sys::Node::DOCUMENT_POSITION_CONTAINED_BY
                | web_sys::Node::DOCUMENT_POSITION_CONTAINS)
            != 0)
    {
        std::cmp::Ordering::Equal
    } else if pos & web_sys::Node::DOCUMENT_POSITION_FOLLOWING != 0 {
        std::cmp::Ordering::Less
    } else {
        std::cmp::Ordering::Greater
    }
}

// Set the cursor position or selection
pub fn set_selection_range(
    anchor: &impl WithNode,
    anchor_offset: u32,
    focus: &impl WithNode,
    focus_offset: u32,
) {
    anchor.with_node(
        |anchor_node| {
            focus.with_node(
                |focus_node| {
                    web_sys::window()
                        .expect("window")
                        .get_selection()
                        .expect("selection error")
                        .expect("selection not found")
                        .set_base_and_extent(anchor_node, anchor_offset, focus_node, focus_offset)
                        .expect("set_base_and_extent")
                },
                TOKEN,
            )
        },
        TOKEN,
    )
}

pub fn set_selection(sel: &SelectionHandle) {
    set_selection_range(
        &sel.anchor_node().unwrap(),
        sel.anchor_offset(),
        &sel.focus_node().unwrap(),
        sel.focus_offset(),
    )
}

// Wrapper for a Selection (giving access to its anchor and focus nodes as NodeRefs)
pub struct SelectionHandle(web_sys::Selection);

impl SelectionHandle {
    delegate! {
        to self.0 {
        pub fn anchor_offset(&self) -> u32;
        pub fn focus_offset(&self) -> u32;
        pub fn is_collapsed(&self) -> bool;
        #[expr($.map(From::from))]
        pub fn anchor_node(&self) -> Option<NodeRef>;
        #[expr($.map(From::from))]
        pub fn focus_node(&self) -> Option<NodeRef>;
    }
    }
}

// Get the selection
pub fn get_selection() -> SelectionHandle {
    SelectionHandle(
        web_sys::window()
            .expect("window")
            .get_selection()
            .expect("selection error")
            .expect("selection not found"),
    )
}

pub fn now_ms() -> f64 {
    web_sys::window()
        .expect("window")
        .performance()
        .expect("performance")
        .now()
}

pub trait RangeLike {
    fn node1(&self) -> Option<NodeRef>;
    fn node2(&self) -> Option<NodeRef>;
    fn offset1(&self) -> u32;
    fn offset2(&self) -> u32;
}

impl RangeLike for SelectionHandle {
    fn node1(&self) -> Option<NodeRef> {
        self.anchor_node()
    }
    fn node2(&self) -> Option<NodeRef> {
        self.focus_node()
    }
    fn offset1(&self) -> u32 {
        self.anchor_offset()
    }
    fn offset2(&self) -> u32 {
        self.focus_offset()
    }
}

impl RangeLike for StaticRangeHandle {
    fn node1(&self) -> Option<NodeRef> {
        self.start_container().ok()
    }
    fn node2(&self) -> Option<NodeRef> {
        self.end_container().ok()
    }
    fn offset1(&self) -> u32 {
        self.start_offset()
    }
    fn offset2(&self) -> u32 {
        self.end_offset()
    }
}

// A trait for a safe "Component", allowing wrapped access to its root Node and audit
// that the DOM subtree matches the Component's expectations.
pub trait Component: WithNode {
    fn audit(&self);
}

// ElementComponent is a trait for a "Component" that is also an HTML Element (e.g. not Text).
pub trait ElementComponent: Component + WithElement {}
impl<U: Component + WithElement> ElementComponent for U {}
