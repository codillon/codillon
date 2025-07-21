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
    Effect::watch(
        move || editor.read_untracked().set_selection().track(),
        move |_, _, _| set_editor.write_untracked().update_selection(),
        false,
    );

    view! {
        <div
            class="textentry"
            contenteditable
            spellcheck="false"
            on:beforeinput=move |ev| { set_editor.write().handle_input(ev) }
            on:mousedown=move |_| {
                set_editor.write().rationalize_selection();
            }
            on:keydown=move |ev| {
                set_editor.write().handle_arrow(ev);
                set_editor.write().rationalize_selection();
            }
            on:mouseup=move |_| {
                set_editor.write().rationalize_selection();
            }
            on:keyup=move |_| {
                set_editor.write().rationalize_selection();
            }
        >
            <For each=move || editor.read().lines().get() key=|line| *line.read().id() let(child)>
                <div
                    data-codillon-line-id=move || *child.read().id()
                    node_ref=child.read().div_ref()
                    class=move || if child.read().instr().is_err() { "grey" } else { "" }
                    on:mousedown=move |ev| {
                        if let Some(malformed_line_id) = editor.read_untracked().malformed_line_id()
                            && *child.read_untracked().id() != malformed_line_id
                        {
                            let enforcement_area = editor.read_untracked().get_enforcement_area();
                            editor
                                .read_untracked()
                                .set_selection()
                                .set(
                                    Some(
                                        LogicSelection::new(enforcement_area.0, enforcement_area.1),
                                    ),
                                );
                            ev.prevent_default();
                            ev.stop_propagation();
                        }
                    }
                >
                    {move || {
                        editor.read_untracked().set_selection().write();
                        child.read().display_text().to_string()
                    }}
                </div>
            </For>
        </div>
    }
}
