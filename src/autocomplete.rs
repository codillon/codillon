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

// At compile time, iterate through the "visit_" function names for Wasm features we support.
macro_rules! define_supported_op_visitors {
    ($( @$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident ($($arity:tt)*) )*) => {
        const VISITOR_NAMES: &[&str] = &[$(
	    define_supported_op_visitors!(visit_one @$proposal $visit)
	),*];
    };

    (visit_one @mvp $visit:ident) => { stringify!($visit) };
    (visit_one @tail_call $visit:ident) => { stringify!($visit) };
    (visit_one @sign_extension $visit:ident) => { stringify!($visit) };
    (visit_one @saturating_float_to_int $visit:ident) => { stringify!($visit) };
    (visit_one @bulk_memory $visit:ident) => { stringify!($visit) };
    (visit_one @$proposal:ident $visit:ident) => { "" };
}

for_each_operator!(define_supported_op_visitors);

// At runtime, strip off the "visit_" prefix and replace initial period with an underscore
// to match the operator name.
fn all_operator_names() -> &'static [String] {
    static NAMES: OnceLock<Vec<String>> = OnceLock::new();
    NAMES
        .get_or_init(|| {
            VISITOR_NAMES
                .iter()
                .filter_map(|name| {
                    let name = name.strip_prefix("visit_")?;
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
                            return Some(s);
                        }
                    }
                    Some(name.to_string())
                })
                .collect()
        })
        .as_slice()
}

type HintItem = ReactiveComponent<DomStruct<(DomText, ()), HtmlDivElement>>;

pub struct Autocomplete {
    bar: DomVec<HintItem, HtmlDivElement>,
    factory: ElementFactory,
    handler: Rc<dyn Fn(&str)>,
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
        }
    }

    pub fn set_handler<F: Fn(&str) + 'static>(&mut self, f: F) {
        self.handler = Rc::new(f);
    }

    pub fn first_suggestion(&self) -> Option<&str> {
        Some(self.bar.get(0)?.inner().get().0.get())
    }

    pub fn update(&mut self, prefix: &str) {
        self.bar.truncate(0);

        let mut suggestions = all_operator_names()
            .iter()
            .filter(|name| name.starts_with(prefix) && name.as_str() != prefix)
            .peekable();

        if prefix.is_empty() || suggestions.peek().is_none() {
            self.bar.set_attribute("style", "display:none");
            return;
        }
        self.bar.remove_attribute("style");
        for s in suggestions {
            let mut item =
                ReactiveComponent::new(DomStruct::new((DomText::new(s), ()), self.factory.div()));
            item.inner_mut().set_attribute("class", "autocomplete-item");
            let (handler, accepted) = (Rc::clone(&self.handler), s);
            item.set_onmousedown(move |ev: MouseEvent| {
                ev.prevent_default();
                ev.stop_propagation();
                handler(accepted);
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
