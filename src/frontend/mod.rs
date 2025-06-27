//! This module contains the frontend components for index page.
use std::ptr::dangling;

use leptos::tachys::html::event::KeyboardEvent;
use leptos::tachys::html::event::MouseEvent;
use leptos::{html, prelude::*};

mod boxlist;
mod textbox;

#[component]
pub fn App() -> impl IntoView {
    let container_ref: NodeRef<html::Div> = NodeRef::new();
    let (current_line_number, set_current_line_number) = signal(1);

    let on_key_down = move |ev: KeyboardEvent| {
        leptos::logging::log!("Key pressed: {}", ev.key());
    };

    // From screen coordinates, determine which line we've clicked on.
    let handle_click = move |ev: MouseEvent| {
        let x = ev.client_x();
        let y = ev.client_y();
        leptos::logging::log!("x: {}, y: {}", x, y);
        if let Some(document) = window().document() {
            if let Some(el) = document.element_from_point(x as f32, y as f32) {
                if let Some(text_line_el) = el.closest(".textLine").ok().flatten() {
                    if let Some(index_attr) = text_line_el.get_attribute("data-index") {
                        leptos::logging::log!("Line clicked: {}", index_attr);
                        if let Ok(idx) = index_attr.parse::<i32>() {
                            set_current_line_number.set(idx);
                        }
                    }
                }
            }
        }
    };

    // Ensure we're focused on component mount.
    Effect::new(move |_| {
        if let Some(div) = container_ref.get() {
            let _ = div.focus();
        }
    });

    view! {
        <div class="main" tabindex="0" node_ref=container_ref  on:keydown=on_key_down on:click=handle_click>
        <boxlist::Boxlist />
    </div> }
}

/// Hold all the code lines in a linked list as a buffer.
/// Each line is represented by a `CodeLineEntry`, it possesses the code text
/// in a `RwSignal` to allow reactive updates.
#[derive(Debug, Clone, Default)]
struct EditorBuffer {
    lines: LinkedList<CodeLineEntry>,
    id_counter: usize,
}

impl EditorBuffer {
    #[allow(dead_code)]
    pub fn concat(&self) -> String {
        self.lines
            .iter()
            .map(|entry| entry.value.get())
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn push_line(&mut self) {
        self.id_counter += 1;
        self.lines.push_back(CodeLineEntry::new(self.id_counter));
    }

    pub fn pop_line(&mut self) {
        self.lines.pop_back();
    }
}

/// For now, it only holds a single line of code with a `RwSignal`
/// `RwSignal` will cause reactive updates when it is modified.
///
/// The id is unique to the containing EditorBuffer and is maintained across
/// insertions and deletions elsewhere in the buffer. This lets Leptos and
/// the browser avoid re-rendering unchanged lines.
#[derive(Debug, Clone)]
struct CodeLineEntry {
    pub value: RwSignal<String>,
    id: usize,
}

impl CodeLineEntry {
    /// ### Returns
    /// An instance holding an empty String.
    pub fn new(id: usize) -> CodeLineEntry {
        CodeLineEntry {
            value: RwSignal::new(String::new()),
            id,
        }
    }
}
