use crate::web_support::{
    AccessToken, Component, ElementFactory, ElementReader, NodeReader, WithElement,
    components::{DomStruct, DomText, TextGuard},
};
use delegate::delegate;
use wasm_bindgen::JsCast;
use web_sys::{HtmlBrElement, HtmlDivElement};

type DomBr = DomStruct<(), HtmlBrElement>;
type LineContents = (DomText, (DomBr, ()));
pub struct EditLine {
    pub(super) component: DomStruct<LineContents, HtmlDivElement>,
    pub(super) _id: usize,
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

    pub fn find_id_from_node<T: AsRef<web_sys::Node> + JsCast>(
        node: NodeReader<T>,
    ) -> Option<usize> {
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

    delegate! {
        to self.component.get().0
        {
            pub fn text(&self) -> &String;
        }
        to self.component.get_mut().0
        {
            #[allow(dead_code)]
            pub fn text_mut(&mut self) -> TextGuard;
        }
    }
}
