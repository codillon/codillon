use crate::editor::*;
use leptos::{prelude::*, *};
use web_sys::{Element, Node, Range, Selection, wasm_bindgen::JsCast};

pub type DivRef = NodeRef<leptos::html::Div>;
pub type DomNode = Node;
pub type DomRange = Range;
pub type DomSelection = Selection;

pub fn get_current_domselection() -> DomSelection {
    web_sys::window()
        .expect("window not found")
        .get_selection()
        .expect("selection error")
        .expect("selection not found")
}

// Given an HTML Node, finds the Codillion-assigned unique ID of the EditLine.
// These are stored in HTML attributes of the line-by-line Div elements.
pub fn find_id_from_node(orig_node: &Node) -> Option<usize> {
    let mut node = orig_node.clone();
    loop {
        if let Ok(elem) = node.clone().dyn_into::<Element>()
            && let Some(id_str) = elem.get_attribute("data-codillon-line-id")
        {
            return id_str.parse::<usize>().ok();
        }
        match node.parent_node() {
            Some(n) => node = n,
            None => return None,
        }
    }
}

// The editor component is a single contenteditable div, surrounding a collection of EditLines
// (each in their own div).
#[component]
pub fn Editor() -> impl IntoView {
    let (editor, set_editor) = signal(Editor::new());

    // If the selection or cursor changes, update it *after* updating the text.
    let rerender_signal = RwSignal::new(());
    Effect::watch(
        move || rerender_signal.get(),
        move |_, _, _| set_editor.write_untracked().update_selection(),
        false,
    );

    view! {
        <div
            class="textentry"
            contenteditable
            spellcheck="false"
            on:beforeinput=move |ev| { set_editor.write().handle_input(ev) }
            on:mousedown=move |_| { set_editor.write().rationalize_selection() }
            on:keydown=move |ev| {
                set_editor.write().handle_arrow(ev);
                set_editor.write().rationalize_selection();
            }
            on:mouseup=move |_| { set_editor.write().rationalize_selection() }
            on:keyup=move |_| { set_editor.write().rationalize_selection() }
        >
            <For each=move || editor.read().lines().get() key=|line| *line.read().id() let(child)>
                <div
                    data-codillon-line-id=move || *child.read().id()
                    node_ref=child.read().div_ref()
                    class=move || if child.read().instr().is_err() { "grey" } else { "" }
                    // on:mousedown=move |ev| {
                    //     if child.read().instr().is_ok() {
                    //         ev.prevent_default();
                    //         ev.stop_propagation();
                    //     }
                    // }
                >
                    {move || {
                        rerender_signal.write();
                        child.read().display_text().to_string()
                    }}
                </div>
            </For>
        </div>
    }
}
