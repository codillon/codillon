use crate::{
    dom_struct::DomStruct,
    dom_text::DomText,
    dom_vec::DomVec,
    jet::{
        AccessToken, Component, ControlHandlers, ElementFactory, ReactiveComponent, WithElement,
    },
};
use std::{rc::Rc, sync::OnceLock};
use wasmparser::for_each_operator;
use web_sys::{HtmlDivElement, MouseEvent};

macro_rules! define_visited_operators {
    ($( @$payload:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident ($($arity:tt)*) )*) => {
        const VISITED_OPERATOR_NAMES: &[&str] = &[$( stringify!($visit), )*];
    }
}

for_each_operator!(define_visited_operators);

fn all_instruction_names() -> &'static [String] {
    static NAMES: OnceLock<Vec<String>> = OnceLock::new();
    NAMES
        .get_or_init(|| {
            VISITED_OPERATOR_NAMES
                .iter()
                .map(|name| {
                    let name = name.strip_prefix("visit_").unwrap_or(name);
                    let dotted_prefixes = [
                        "i32", "i64", "f32", "f64", "v128", "memory", "table", "global", "local",
                        "ref", "elem", "data",
                    ];
                    for prefix in dotted_prefixes {
                        if name.starts_with(prefix)
                            && name.as_bytes().get(prefix.len()) == Some(&b'_')
                        {
                            let mut s = String::with_capacity(name.len());
                            s.push_str(prefix);
                            s.push('.');
                            s.push_str(&name[prefix.len() + 1..]);
                            return s;
                        }
                    }
                    name.to_string()
                })
                .collect()
        })
        .as_slice()
}

pub fn suggest(prefix: &str, limit: usize) -> Vec<String> {
    all_instruction_names()
        .iter()
        .filter(|name| name.starts_with(prefix) && name.as_str() != prefix)
        .take(limit)
        .cloned()
        .collect()
}

type HintItem = ReactiveComponent<DomStruct<(DomText, ()), HtmlDivElement>>;

pub struct Autocomplete {
    bar: DomVec<HintItem, HtmlDivElement>,
    factory: ElementFactory,
    handler: Rc<dyn Fn(&str)>,
    suggestions: Vec<String>,
    prefix: String,
}

impl Autocomplete {
    pub fn new(factory: &ElementFactory) -> Self {
        let mut bar: DomVec<HintItem, HtmlDivElement> = DomVec::new(factory.div());
        bar.set_attribute("class", "autocomplete-hint-bar");
        bar.set_attribute("style", "display:none");
        Self {
            bar,
            factory: factory.clone(),
            handler: Rc::new(|_| {}),
            suggestions: Vec::new(),
            prefix: String::new(),
        }
    }

    pub fn set_onselect<F: Fn(&str) + 'static>(&mut self, f: F) {
        self.handler = Rc::new(f);
    }

    pub fn get_suffix(&mut self, accepted: &str) -> String {
        let suffix = accepted[self.prefix.len()..].to_owned();
        self.hide();
        suffix
    }

    pub fn get_suggestions(&self) -> &[String] {
        &self.suggestions
    }

    pub fn show(&mut self, prefix: String, suggestions: Vec<String>) {
        self.prefix = prefix;
        self.suggestions = suggestions.clone();
        self.update(&suggestions);
    }

    pub fn hide(&mut self) {
        self.suggestions.clear();
        self.prefix.clear();
        self.update(&[]);
    }

    fn update(&mut self, suggestions: &[String]) {
        self.bar.truncate(0);
        if suggestions.is_empty() {
            self.bar.set_attribute("style", "display:none");
            return;
        }
        self.bar.remove_attribute("style");
        for s in suggestions {
            let mut item =
                ReactiveComponent::new(DomStruct::new((DomText::new(s), ()), self.factory.div()));
            item.inner_mut().set_attribute("class", "autocomplete-item");
            let (handler, accepted) = (Rc::clone(&self.handler), s.clone());
            item.set_onmousedown(move |ev: MouseEvent| {
                ev.prevent_default();
                ev.stop_propagation();
                handler(&accepted);
            });
            self.bar.push(item);
        }
    }
}

impl Component for Autocomplete {
    fn audit(&self) {
        self.bar.audit();
    }
}

impl WithElement for Autocomplete {
    type Element = HtmlDivElement;
    fn with_element(&self, f: impl FnMut(&HtmlDivElement), g: AccessToken) {
        self.bar.with_element(f, g)
    }
}
