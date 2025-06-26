use super::GLOB_ID_ALLOCATOR;
use super::textbox::Textbox;
use leptos::prelude::*;
use std::collections::LinkedList;

/// Hold all the code lines in a linked list as a buffer.
/// Each line is represented by a `CodeLineEntry`, it possesses the code text
/// in a `RwSignal` to allow reactive updates.
#[derive(Debug, Clone)]
struct EditorBuffer(LinkedList<CodeLineEntry>);

impl EditorBuffer {
    #[allow(dead_code)]
    pub fn concat(&self) -> String {
        self.0
            .iter()
            .map(|entry| entry.value.get())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

/// For now, it only holds a single line of code with a `RwSignal`
/// `RwSignal` will cause reactive updates when it is modified.
///
/// The id filed is the global unique id for object management handled by leptos
#[derive(Debug, Clone)]
struct CodeLineEntry {
    id: usize,
    pub value: RwSignal<String>,
}

impl CodeLineEntry {
    /// ### Returns
    /// An instance holding an empty String.
    pub fn new() -> CodeLineEntry {
        CodeLineEntry {
            id: GLOB_ID_ALLOCATOR.new_id(),
            value: RwSignal::new(String::new()),
        }
    }
}

/// This function creates a list of textboxs with a push button and a pop button.
/// Initially, it will have 0 textboxes.
#[component]
pub fn Boxlist() -> impl IntoView {
    let editor_buffer = EditorBuffer(LinkedList::new());
    let (editor_buffer, set_editor_buffer) = signal(editor_buffer);

    // Will be trigger by the push_line button, see below
    let push_line = move |_| {
        set_editor_buffer.write().0.push_back(CodeLineEntry::new());
    };

    let pop_line = move |_| {
        set_editor_buffer.write().0.pop_back();
    };

    view! {
        <div>
            <button on:click=push_line>"Add Line"</button>
            <button on:click=pop_line>"Remove Line"</button>
            <ForEnumerate
                each=move || editor_buffer.get().0
                key=|entry| entry.id
                children=move |index, entry| {
                    view! {
                        <br />
                        {index}
                        ": "
                        <Textbox text=entry.value />
                    }
                }
            />
        </div>
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_editor_buffer() {
        let mut editor_buffer = EditorBuffer(LinkedList::new());
        editor_buffer.0.push_back(CodeLineEntry::new());
        editor_buffer.0.push_back(CodeLineEntry::new());
        editor_buffer.0.push_back(CodeLineEntry::new());
        editor_buffer.0.push_back(CodeLineEntry::new());
        assert_eq!(editor_buffer.0.len(), 4);
        editor_buffer.0.pop_back();
        editor_buffer.0.pop_back();
        assert_eq!(editor_buffer.0.len(), 2);
        *editor_buffer.0.iter_mut().next().unwrap().value.write() = String::from("Hello");
        *editor_buffer.0.iter_mut().nth(1).unwrap().value.write() = String::from("Leptos");
        assert_eq!(editor_buffer.concat(), "Hello\nLeptos".to_string());
    }
}
