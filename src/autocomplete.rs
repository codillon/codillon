use crate::visited_operators::get_all_instruction_names;
use crate::{
    dom_struct::DomStruct,
    dom_text::DomText,
    dom_vec::DomVec,
    jet::{ElementFactory, WithElement},
};
use wasm_bindgen::JsCast;
use web_sys::{HtmlDivElement, MouseEvent, WheelEvent};

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

    bar.with_element(
        move |bar_elem| {
            let bw = bar_elem.clone();
            let wheel = wasm_bindgen::closure::Closure::wrap(Box::new(move |ev: WheelEvent| {
                let d = ev.delta_x() + ev.delta_y();
                if d != 0.0 {
                    ev.prevent_default();
                    bw.set_scroll_left(bw.scroll_left() + (d * 2.0) as i32);
                }
            }) as Box<dyn FnMut(_)>);
            bar_elem
                .add_event_listener_with_callback("wheel", wheel.as_ref().unchecked_ref())
                .ok();
            wheel.forget();
        },
        crate::jet::access_token(),
    );
    bar
}

pub fn register_dismiss_on_document_mouse_down(on_dismiss: impl Fn() + 'static) {
    let on_doc_down =
        wasm_bindgen::closure::Closure::wrap(
            Box::new(move |_: MouseEvent| on_dismiss()) as Box<dyn FnMut(_)>
        );
    if let Some(doc) = web_sys::window().and_then(|w| w.document()) {
        doc.add_event_listener_with_callback("mousedown", on_doc_down.as_ref().unchecked_ref())
            .ok();
    }
    on_doc_down.forget();
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
        let c = wasm_bindgen::closure::Closure::wrap(Box::new(move |ev: MouseEvent| {
            ev.prevent_default();
            ev.stop_propagation();
            acc(sc.clone());
        }) as Box<dyn FnMut(_)>);

        item.with_element(
            |elem| {
                elem.add_event_listener_with_callback("mousedown", c.as_ref().unchecked_ref())
                    .ok();
            },
            crate::jet::access_token(),
        );
        c.forget();

        bar.push(item);
    }
}
