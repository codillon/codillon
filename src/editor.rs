// The Codillon code editor (doesn't do much, but does capture beforeinput and logs to console)

use crate::{
    dom_struct::DomStruct,
    dom_text::DomText,
    dom_vec::DomVec,
    web_support::{AccessToken, Component, ElementFactory, WithElement, WithNode},
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use web_sys::{HtmlBrElement, HtmlDivElement, HtmlSpanElement, InputEvent};

type DomBr = DomStruct<(), HtmlBrElement>;
type LineContents = (DomText, (DomBr, ()));
type EditLine = DomStruct<LineContents, HtmlSpanElement>;

struct _Editor {
    _next_id: usize,
    _id_map: HashMap<usize, usize>,
    component: DomVec<EditLine, HtmlDivElement>,
}

pub struct Editor(Rc<RefCell<_Editor>>);

impl Editor {
    pub fn new(factory: &ElementFactory) -> Self {
        let mut inner = _Editor {
            _next_id: 0,
            _id_map: HashMap::default(),
            component: DomVec::new(factory.div()),
        };

        inner.component.set_attribute("class", "textentry");
        inner.component.set_attribute("contenteditable", "true");
        inner.component.set_attribute("spellcheck", "false");

        let mut ret = Editor(Rc::new(RefCell::new(inner)));

        let editor_ref = Rc::clone(&ret.0);
        ret.0
            .borrow_mut()
            .component
            .set_onbeforeinput(move |ev| Editor(editor_ref.clone()).handle_input(ev));

        ret.push_line(factory, "Hello, world.");

        ret
    }

    fn push_line(&mut self, factory: &ElementFactory, string: &str) {
        self.0.borrow_mut().component.push(EditLine::new(
            (DomText::new(string), (DomBr::new((), factory.br()), ())),
            factory.span(),
        ));
    }

    fn handle_input(&mut self, ev: InputEvent) {
        ev.prevent_default();
        web_sys::console::log_1(
            &format!(
                "got: {} + {}",
                ev.input_type(),
                ev.data().unwrap_or_default()
            )
            .into(),
        );
    }
}

impl WithNode for Editor {
    fn with_node(&self, f: impl FnMut(&web_sys::Node), g: AccessToken) {
        self.0.borrow().with_node(f, g);
    }
}

impl WithElement<HtmlDivElement> for Editor {
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
