// autocomplete.rs - WebAssembly instruction autocomplete made with help of Gemini 3.1pro.

include!(concat!(env!("OUT_DIR"), "/wasm_instrs.rs"));

use wasm_bindgen::JsCast;
use web_sys::HtmlDivElement;

fn document() -> web_sys::Document {
    web_sys::window().unwrap().document().unwrap()
}

pub fn suggest(prefix: &str, limit: usize) -> Vec<String> {
    if prefix.is_empty() {
        return Vec::new();
    }
    let idx = WASM_INSTR_NAMES.partition_point(|&s| s < prefix);
    WASM_INSTR_NAMES[idx..]
        .iter()
        .copied()
        .take_while(|&s| s.starts_with(prefix))
        .filter(|&s| s != prefix)
        .take(limit)
        .map(String::from)
        .collect()
}

pub fn completion_suffix<'a>(full: &'a str, prefix: &str) -> &'a str {
    &full[prefix.trim_start().len()..]
}

pub fn setup_hint_bar(mut on_click_outside: impl FnMut() + 'static) -> HtmlDivElement {
    let doc = document();
    let bar = doc
        .create_element("div")
        .unwrap()
        .dyn_into::<HtmlDivElement>()
        .unwrap();
    bar.set_class_name("autocomplete-hint-bar");
    bar.set_attribute("style", "display:none").ok();

    let bar_clone = bar.clone();
    let capture_click = wasm_bindgen::closure::Closure::wrap(Box::new(move |ev: web_sys::Event| {
        if let Some(target) = ev.target() {
            let t: Option<web_sys::Node> = target.dyn_into::<web_sys::Node>().ok();
            let b: web_sys::Node = bar_clone.clone().into();
            if t.is_none_or(|n| !b.contains(Some(&n))) {
                on_click_outside();
            }
        }
    }) as Box<dyn FnMut(_)>);
    doc.add_event_listener_with_callback("click", capture_click.as_ref().unchecked_ref())
        .ok();
    capture_click.forget();

    bar
}

pub fn attach_hint_bar(bar: &HtmlDivElement) {
    document()
        .document_element()
        .expect("document element")
        .append_child(bar)
        .ok();
}

pub fn update_hint_bar(
    bar: &HtmlDivElement,
    suggestions: &[String],
    on_accept: impl FnMut(String) + 'static,
) {
    bar.set_inner_html("");
    if suggestions.is_empty() {
        bar.set_attribute("style", "display:none").ok();
        return;
    }
    if bar.parent_node().is_none()
        && let Some(root) = document().document_element()
    {
        root.append_child(bar).ok();
    }

    bar.set_attribute("style", "display:flex").ok();
    let doc = document();
    let on_accept = std::rc::Rc::new(std::cell::RefCell::new(on_accept));

    for s in suggestions {
        let s_clone = s.clone();
        let item = doc.create_element("div").unwrap();
        item.set_class_name("autocomplete-item");
        item.set_text_content(Some(s));

        let on_acc = on_accept.clone();
        let c = wasm_bindgen::closure::Closure::wrap(Box::new(move |ev: web_sys::Event| {
            ev.stop_propagation();
            (*on_acc.borrow_mut())(s_clone.clone());
        }) as Box<dyn FnMut(_)>);
        item.add_event_listener_with_callback("click", c.as_ref().unchecked_ref())
            .ok();
        c.forget();
        bar.append_child(&item).ok();
    }
}
