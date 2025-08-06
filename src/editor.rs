// The Codillon code editor (doesn't do much, but does capture beforeinput and logs to console)

use crate::{
    dom_struct::DomStruct,
    dom_text::DomText,
    dom_vec::DomVec,
    web_support::{
        AccessToken, Component, ElementFactory, ElementReader, NodeReader, SelectionHandle,
        WithElement, WithNode,
    },
};
use delegate::delegate;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use wasm_bindgen::JsCast;
use web_sys::{HtmlBrElement, HtmlDivElement, HtmlSpanElement, InputEvent};

type DomBr = DomStruct<(), HtmlBrElement>;
type LineContents = (DomText, (DomBr, ()));
struct EditLine {
    component: DomStruct<LineContents, HtmlDivElement>,
    _id: usize,
}

impl WithElement for EditLine {
    type Element = HtmlDivElement;
    delegate! {
        to self.component
        {
            fn with_element(&self, f: impl FnMut(&Self::Element), g: AccessToken);
        }
    }
}

impl Component for EditLine {
    delegate! {
        to self.component {
            fn audit(&self);
        }
    }
}

impl EditLine {
    pub const EDITLINE_ID_ATTR: &str = "data-editline-id";
    pub fn new(contents: &str, factory: &ElementFactory, id: usize) -> Self {
        let mut dom_editline = DomStruct::new(
            (
                DomText::new(contents),
                (DomStruct::<(), HtmlBrElement>::new((), factory.br()), ()),
            ),
            factory.div(),
        );
        dom_editline.set_attribute(Self::EDITLINE_ID_ATTR, &id.to_string());
        Self {
            component: dom_editline,
            _id: id,
        }
    }

    fn find_id_from_node<T: AsRef<web_sys::Node> + JsCast>(node: NodeReader<T>) -> Option<usize> {
        let mut elem: ElementReader<web_sys::Element> = match node.dyn_into::<web_sys::Element>() {
            Ok(node) => node.into(),
            Err(node) => node.parent_element().expect("Get a parent element parent"),
        };

        loop {
            if let Some(id) = elem.get_attribute(Self::EDITLINE_ID_ATTR) {
                return Some(id.parse::<usize>().expect("parse id"));
            }
            elem = elem.parent_element()?;
        }
    }
}

struct _Editor {
    _next_id: usize,
    _id_map: HashMap<usize, usize>,
    _dom_selection: SelectionHandle,
    _factory: ElementFactory,
    component: DomVec<EditLine, HtmlDivElement>,
}

impl _Editor {
    fn new(factory: ElementFactory, dom_selection: SelectionHandle) -> Self {
        let component = DomVec::new(factory.div());
        Self {
            _next_id: 0,
            _id_map: HashMap::default(),
            _dom_selection: dom_selection,
            _factory: factory,
            component,
        }
    }

    fn push_line(&mut self, string: &str) {
        self.component
            .push(EditLine::new(string, &self._factory, self._next_id));
        self._id_map.insert(self._next_id, self.component.len() - 1);
        self._next_id += 1;
    }

    fn handle_input(&mut self, ev: InputEvent) {
        ev.prevent_default();
        web_sys::console::log_1(
            &format!(
                "got: {} + {} + at {:?}",
                ev.input_type(),
                ev.data().unwrap_or_default(),
                self.get_logic_seletion()
            )
            .into(),
        );
    }

    fn get_logic_seletion(&self) -> Option<LogicSelection> {
        let anchor = self._dom_selection.get_anchor_node()?;
        let focus = self._dom_selection.get_focus_node()?;

        let anchor_id = EditLine::find_id_from_node(anchor);
        let focus_id = EditLine::find_id_from_node(focus);

        match (anchor_id, focus_id) {
            (Some(anchor_id), Some(focus_id)) => {
                let anchor_idx: usize = *self._id_map.get(&anchor_id).expect("can't find line");
                let focus_idx: usize = *self._id_map.get(&focus_id).expect("can't find line");
                Some(LogicSelection::new(
                    (anchor_idx, self._dom_selection.get_anchor_offset()),
                    (focus_idx, self._dom_selection.get_focus_offset()),
                ))
            }
            (Some(anchor_id), None) => {
                let anchor_idx: usize = *self._id_map.get(&anchor_id).expect("can't find line");
                Some(LogicSelection::new(
                    (anchor_idx, self._dom_selection.get_anchor_offset()),
                    (
                        self.component.len() - 1,
                        self.component
                            .last()
                            .expect("Empty lines")
                            .component
                            .get()
                            .0
                            .data()
                            .chars()
                            .count(),
                    ),
                ))
            }
            (None, Some(focus_id)) => {
                let focus_idx: usize = *self._id_map.get(&focus_id).expect("can't find line");
                Some(LogicSelection::new(
                    (0, 0),
                    (focus_idx, self._dom_selection.get_focus_offset()),
                ))
            }
            (None, None) => None,
        }
    }
}

pub struct Editor(Rc<RefCell<_Editor>>);

impl Editor {
    pub fn new(factory: ElementFactory, dom_selection: SelectionHandle) -> Self {
        let mut inner = _Editor::new(factory, dom_selection);
        inner.component.set_attribute("class", "textentry");
        inner.component.set_attribute("contenteditable", "true");
        inner.component.set_attribute("spellcheck", "false");

        let ret = Editor(Rc::new(RefCell::new(inner)));

        let editor_ref = Rc::clone(&ret.0);
        ret.0
            .borrow_mut()
            .component
            .set_onbeforeinput(move |ev| editor_ref.borrow_mut().handle_input(ev));

        ret.0.borrow_mut().push_line("Hello, world.");
        ret
    }
}

impl WithElement for Editor {
    type Element = HtmlDivElement;
    fn with_element(&self, f: impl FnMut(&HtmlDivElement), g: AccessToken) {
        self.0.borrow().component.with_element(f, g);
    }
}

impl Component for Editor {
    fn audit(&self) {
        self.0.borrow().audit()
    }
}

impl WithNode for _Editor {
    fn with_node(&self, f: impl FnMut(&web_sys::Node), g: AccessToken) {
        self.component.with_node(f, g);
    }
}

impl Component for _Editor {
    fn audit(&self) {
        self.component.audit()
    }
}

/// LogicSelection is the selection area in logical model's view.
/// Usually it is dom selection restricted to editable area.
#[derive(Debug, Clone, Copy)]
struct LogicSelection {
    pub anchor: (usize, usize), // #Ln, #Col
    pub focus: (usize, usize),
}

impl LogicSelection {
    pub fn new_cursor(r: usize, c: usize) -> LogicSelection {
        LogicSelection {
            anchor: (r, c),
            focus: (r, c),
        }
    }

    pub fn new(anchor: (usize, usize), focus: (usize, usize)) -> LogicSelection {
        LogicSelection { anchor, focus }
    }

    pub fn is_cursor(&self) -> bool {
        self.anchor == self.focus
    }

    pub fn to_area(self) -> std::ops::Range<(usize, usize)> {
        std::cmp::min(self.anchor, self.focus)..std::cmp::max(self.anchor, self.focus)
    }
}
