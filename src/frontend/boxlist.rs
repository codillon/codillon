use super::EditorBuffer;
use super::textbox::Textbox;
use leptos::prelude::*;

/// This function creates a list of textboxs with a push button and a pop button.
/// Initially, it will have 1 textbox.
#[component]
pub fn Boxlist(focused_line: ReadSignal<usize>) -> impl IntoView {
    let (editor_buffer, set_editor_buffer) = signal(EditorBuffer::default());

    // Will be triggered by the button, see below
    let push_line = move |_| {
        set_editor_buffer.write().push_line();
    };

    let pop_line = move |_| {
        set_editor_buffer.write().pop_line();
    };

    Effect::new(move |_| {
        set_editor_buffer.write().push_line();
    });

    view! {
        <div class="editor">
            <div class="buttonContainer">
                <button on:click=push_line>"Add Line"</button>
                <button on:click=pop_line>"Remove Line"</button>
            </div>
            <div class="editorLines">
            <ForEnumerate
                each=move || editor_buffer.get().lines
                key=|entry| entry.id
                children=move |index, entry| {
                    let is_focused = move || focused_line.get() == index.get() + 1;
                    view! {
                    <div class="textLine" data-index=entry.id data-focused=move || is_focused().to_string()>
                         <span class="lineLabel">{index}| </span>

                        <Textbox text=entry.value />
                    </div>

                    }
                }
            />
            </div>
        </div>
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_editor_buffer() {
        let mut editor_buffer = EditorBuffer::default();
        editor_buffer.push_line();
        editor_buffer.push_line();
        editor_buffer.push_line();
        editor_buffer.push_line();
        assert_eq!(editor_buffer.lines.len(), 4);
        editor_buffer.pop_line();
        editor_buffer.pop_line();
        assert_eq!(editor_buffer.lines.len(), 2);
        editor_buffer
            .lines
            .iter_mut()
            .next()
            .unwrap()
            .value
            .set(String::from("Hello"));
        editor_buffer
            .lines
            .iter_mut()
            .nth(1)
            .unwrap()
            .value
            .set(String::from("Leptos"));
        assert_eq!(editor_buffer.concat(), "Hello\nLeptos".to_string());
    }
}
