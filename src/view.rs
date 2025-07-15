use crate::editor::*;
use leptos::{prelude::*, *};
use web_sys::{Element, Node, Range, Selection, wasm_bindgen::JsCast};

pub type DivRef = NodeRef<leptos::html::Div>;
pub type DomNode = Node;
pub type DomRange = Range;
pub type DomSelection = Selection;

pub fn get_current_selection() -> DomSelection {
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
    let selection_signal = editor.read_untracked().selection();
    Effect::watch(
        move || selection_signal.get(),
        move |_, _, _| set_editor.write_untracked().update_selection(),
        false,
    );

    view! {
        <div
            class="textentry"
            contenteditable
            spellcheck="false"
            on:beforeinput=move |ev| { set_editor.write_untracked().handle_input(ev) }
            on:mousedown=move |_| { set_editor.write_untracked().rationalize_selection() }
            on:keydown=move |ev| {
                editor.read_untracked().maybe_cancel(ev);
                set_editor.write_untracked().rationalize_selection();
            }
            on:mouseup=move |_| { set_editor.write_untracked().rationalize_selection() }
            on:keyup=move |_| { set_editor.write_untracked().rationalize_selection() }
        >
            <For each=move || editor.read().lines().lines() key=|line| *line.read().id() let(child)>
                <div
                    data-codillon-line-id=move || *child.read().id()
                    node_ref=child.read_untracked().div_ref()
                >
                    {move || {
                        selection_signal.write();
                        child.read().as_str().to_string()
                    }}
                </div>
            </For>
        </div>
    }
}
