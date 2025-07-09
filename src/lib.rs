use leptos::{
    html::Div,
    prelude::*,
};
use wasm_bindgen::JsCast;
use web_sys::{Event, MouseEvent, KeyboardEvent};
mod website;

pub mod utils;

#[component]
pub fn App() -> impl IntoView {
    let website = RwSignal::new(website::Website::default());
    let div_ref: NodeRef<Div> = NodeRef::new();

    let on_input = move |e: Event| {
        e.prevent_default();
        let element = div_ref.get().unwrap().unchecked_into::<web_sys::HtmlDivElement>();
        website.update(|web| {
            web.content.clear();
            let child_nodes = element.child_nodes();
            for i in 0..child_nodes.length() {
                if let Some(node) = child_nodes.item(i) {
                    if let Some(text) = node.text_content() {
                        web.content.push(website::CodelineEntry {
                            line: text.to_string(),
                        });
                    }
                }
            }
        });
        leptos::logging::log!("Line: {}", get_current_line(&element).unwrap());
        leptos::logging::log!("Website content: {:?}", website.get().get_content());
    };

    let on_keydown = move |e: KeyboardEvent| {
        let key = e.key();
        let element = div_ref.get().unwrap().unchecked_into::<web_sys::HtmlDivElement>();
        let web = website.get();
        let lines = web.get_content();
        if !utils::is_well_formed_instr(&lines[get_current_line(&element).unwrap()].line) {
            if key == "Enter" || key == "ArrowDown" || key == "ArrowUp" {
                e.prevent_default();
                leptos::logging::log!("Attempted to enter on malformed instr!");
                return;
            }
        }
    };

    let on_click = move |e: MouseEvent| {
        let element = div_ref.get().unwrap().unchecked_into::<web_sys::HtmlDivElement>();
        leptos::logging::log!("Line on click: {}", get_current_line(&element).unwrap());

    };

    let on_mousedown = move |e: MouseEvent| {
        let element = div_ref.get().unwrap().unchecked_into::<web_sys::HtmlDivElement>();
        let web = website.get();
        let lines = web.get_content();
        if !utils::is_well_formed_instr(&lines[get_current_line(&element).unwrap()].line) {
            leptos::logging::log!("Attempted to enter on malformed instr!");
            e.prevent_default();
            return;
        }
    };

    view! {
        <div
            contenteditable="true"
            tabindex="0"
            node_ref=div_ref
            on:input=on_input
            on:click=on_click
            on:mousedown=on_mousedown
            on:keydown=on_keydown
            spellcheck="false"
            style="white-space: pre-wrap; font-family: monospace; outline:none; min-height: 200px; border: 1px solid #ccc; padding: 8px;"
        >
            <div><br/></div>
        </div>
    }
}

fn get_current_line(div: &web_sys::HtmlDivElement) -> Option<usize> {
    let range = window()
        .get_selection()
        .unwrap()
        .unwrap()
        .get_range_at(0)
        .unwrap();

    let start_container = range.start_container().unwrap();

    let child_nodes = div.child_nodes();

    for i in 0..child_nodes.length() {
        if let Some(child) = child_nodes.item(i) {
            if child.is_same_node(Some(&start_container)) {
                return Some(i as usize);
            }
            let grandchild_nodes = child.child_nodes();
            for j in 0..grandchild_nodes.length() {
                if let Some(grandchild) = grandchild_nodes.item(j) {
                    if grandchild.is_same_node(Some(&start_container)) {
                        return Some(i as usize);
                    }
                }
            }
        }
    }
    None
}