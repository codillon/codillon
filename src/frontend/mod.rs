//! This module contains the frontend components for index page.
use leptos::prelude::*;
use std::collections::LinkedList;

mod boxlist;
mod func_indicator;
mod textbox;

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

/// Hold all the code lines in a linked list as a buffer.
/// Each line is represented by a `CodeLineEntry`, it possesses the code text
/// in a `RwSignal` to allow reactive updates.
#[derive(Debug, Clone, Default)]
pub struct EditorBuffer {
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

    pub fn all_lines_well_formed(&self) -> bool {
        self.lines
            .iter()
            .all(|entry| crate::utils::is_well_formed_instr(&entry.value.get()))
    }

    pub fn push_line(&mut self) {
        self.id_counter += 1;
        self.lines.push_back(CodeLineEntry::new(self.id_counter));
    }

    pub fn pop_line(&mut self) {
        self.lines.pop_back();
    }
}

#[component]
pub fn App() -> impl IntoView {
    let (editor_buffer, set_editor_buffer) = signal(EditorBuffer::default());
    view! {
        <boxlist::Boxlist editor_buffer=editor_buffer set_editor_buffer=set_editor_buffer />
        <func_indicator::FuncIndicator editor_buffer=editor_buffer />
    }
}
