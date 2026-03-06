use crate::visited_operators::get_all_instruction_names;
use crate::{dom_struct::DomStruct, dom_text::DomText, dom_vec::DomVec, jet::ElementFactory};
use web_sys::{HtmlDivElement, MouseEvent};

pub type HintBarStruct = DomVec<DomStruct<(DomText, ()), HtmlDivElement>, HtmlDivElement>;

pub fn suggest(prefix: &str, limit: usize) -> Vec<String> {
    let mut names = get_all_instruction_names();
    if !prefix.is_empty() {
        names.retain(|s| s.starts_with(prefix) && s != prefix);
    }
    names.truncate(limit);
    names
}

pub fn setup_hint_bar(factory: &ElementFactory) -> HintBarStruct {
    let mut bar: HintBarStruct = DomVec::new(factory.div());
    bar.set_attribute("class", "autocomplete-hint-bar");
    bar.set_attribute("style", "display:none");
    bar
}

pub fn register_dismiss_on_document_mouse_down(
    factory: &ElementFactory,
    on_dismiss: impl Fn() + 'static,
) {
    factory.add_document_mousedown_listener(move |_: MouseEvent| on_dismiss());
}

pub fn update_hint_bar(
    factory: &ElementFactory,
    bar: &mut HintBarStruct,
    suggestions: &[String],
    on_accept: impl Fn(String) + 'static,
) {
    bar.truncate(0);

    if suggestions.is_empty() {
        bar.set_attribute("style", "display:none");
        return;
    }

    bar.remove_attribute("style");

    let on_accept = std::rc::Rc::new(on_accept);
    for s in suggestions {
        let mut item = DomStruct::new((DomText::new(s), ()), factory.div());
        item.set_attribute("class", "autocomplete-item");

        let (acc, sc) = (on_accept.clone(), s.clone());
        item.set_onmousedown(move |ev: MouseEvent| {
            ev.prevent_default();
            ev.stop_propagation();
            acc(sc.clone());
        });

        bar.push(item);
    }
}
