use crate::visited_operators::get_all_instruction_names;
use wasm_bindgen::JsCast;
use web_sys::HtmlDivElement;

fn document() -> web_sys::Document {
    web_sys::window().unwrap().document().unwrap()
}

pub fn suggest(prefix: &str, limit: usize) -> Vec<String> {
    let names = get_all_instruction_names();
    if prefix.is_empty() {
        return names;
    }
    names
        .into_iter()
        .filter(|s: &String| s.starts_with(prefix) && s != prefix)
        .take(limit)
        .collect()
}

pub fn completion_suffix<'a>(full: &'a str, prefix: &str) -> &'a str {
    &full[prefix.trim_start().len()..]
}

pub fn setup_hint_bar(mut on_click_outside: impl FnMut() + 'static) -> HtmlDivElement {
    let doc = document();
    let bar: HtmlDivElement = doc.create_element("div").unwrap().dyn_into().unwrap();
    bar.set_class_name("autocomplete-hint-bar");
    bar.set_attribute("style", "display:none").ok();

    let b = bar.clone();
    let click = wasm_bindgen::closure::Closure::<dyn FnMut(_)>::wrap(Box::new(
        move |ev: web_sys::MouseEvent| {
            let outside = ev
                .target()
                .and_then(|t: web_sys::EventTarget| t.dyn_into::<web_sys::Node>().ok())
                .is_none_or(|n| {
                    !<HtmlDivElement as AsRef<web_sys::Node>>::as_ref(&b).contains(Some(&n))
                });
            if outside {
                on_click_outside();
            }
        },
    ));
    doc.add_event_listener_with_callback("mousedown", click.as_ref().unchecked_ref())
        .ok();
    click.forget();

    let bw = bar.clone();
    let wheel = wasm_bindgen::closure::Closure::<dyn FnMut(_)>::wrap(Box::new(
        move |ev: web_sys::WheelEvent| {
            if ev.delta_y() != 0.0 {
                ev.prevent_default();
                bw.set_scroll_left(bw.scroll_left() + ev.delta_y() as i32);
            }
        },
    ));
    bar.add_event_listener_with_callback("wheel", wheel.as_ref().unchecked_ref())
        .ok();
    wheel.forget();

    bar
}

pub fn update_hint_bar(
    bar: &HtmlDivElement,
    suggestions: &[String],
    on_accept: impl FnMut(String) + 'static,
) {
    bar.set_inner_html("");
    if suggestions.is_empty() {
        bar.set_attribute("style", "display:none").ok();
        if let Some(p) = bar.parent_node() {
            p.remove_child(bar).ok();
        }
        return;
    }
    if let Some(body) = document().body() {
        body.append_child(bar).ok();
    }
    bar.remove_attribute("style").ok();

    let on_accept = std::rc::Rc::new(std::cell::RefCell::new(on_accept));
    for s in suggestions {
        let item: web_sys::Element = document().create_element("div").unwrap();
        item.set_class_name("autocomplete-item");
        item.set_text_content(Some(s));
        let (acc, sc) = (on_accept.clone(), s.clone());
        let c = wasm_bindgen::closure::Closure::<dyn FnMut(_)>::wrap(Box::new(
            move |ev: web_sys::MouseEvent| {
                ev.prevent_default();
                ev.stop_propagation();
                (*acc.borrow_mut())(sc.clone());
            },
        ));
        item.add_event_listener_with_callback("mousedown", c.as_ref().unchecked_ref())
            .ok();
        c.forget();
        bar.append_child(&item).ok();
    }
}
