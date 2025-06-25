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
    pub fn dump(&self) -> String {
        // Use `get_untracked` because we do not need a re-render
        self.0
            .iter()
            .map(|entry| entry.0.get_untracked())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

/// For now, it only holds a single line of code with a `RwSignal`
/// `RwSignal` will cause reactive updates when it is modified.
#[derive(Debug, Clone)]
struct CodeLineEntry(RwSignal<String>);

/// This function creates a list of textboxs with a push button and a pop button.
/// Initially, it will have 0 textboxes.
#[component]
pub fn Boxlist() -> impl IntoView {
    let editor_buffer = EditorBuffer(LinkedList::new());
    let (editor_buffer, set_editor_buffer) = signal(editor_buffer);

    let push_line = move |_| {
        set_editor_buffer.update(|editor_buffer| {
            editor_buffer
                .0
                .push_back(CodeLineEntry(RwSignal::new("".to_string())))
        });
    };

    let pop_line = move |_| {
        set_editor_buffer.update(|editor_buffer| {
            editor_buffer.0.pop_back(); // Will Return None if empty. But we ignore the return value anyway
        });
    };

    view! {
        <div>
            <button on:click=push_line>"Add Line"</button>
            <button on:click=pop_line>"Remove Line"</button>
            <For
                each=move || editor_buffer.get().0.into_iter().enumerate()
                // Use index as key
                key=|(index, _)| *index
                children=move |(index, entry)| {
                    view! {
                        <br/>
                        {index} ": "
                        <Textbox text=entry.0 />
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
        editor_buffer
            .0
            .push_back(CodeLineEntry(RwSignal::new("".to_string())));
        editor_buffer
            .0
            .push_back(CodeLineEntry(RwSignal::new("".to_string())));
        editor_buffer
            .0
            .push_back(CodeLineEntry(RwSignal::new("".to_string())));
        editor_buffer
            .0
            .push_back(CodeLineEntry(RwSignal::new("".to_string())));
        assert_eq!(editor_buffer.0.len(), 4);
        editor_buffer.0.pop_back();
        editor_buffer.0.pop_back();
        assert_eq!(editor_buffer.0.len(), 2);
        editor_buffer
            .0
            .iter_mut()
            .next()
            .unwrap()
            .0
            .update(|s| *s = "Hello".to_string());
        editor_buffer
            .0
            .iter_mut()
            .nth(1)
            .unwrap()
            .0
            .update(|s| *s = "Leptos".to_string());
        assert_eq!(editor_buffer.dump(), "Hello\nLeptos".to_string());
    }
}
