use super::document::CodeLineEntry;
use super::inputs::ButtonClickType;
use leptos::prelude::*;

const CORRECT_EMOJI: &str = "✅";
const INCORRECT_EMOJI: &str = "❌";

#[component]
pub fn Textbox(well_formed: Signal<bool>, text: RwSignal<String>) -> impl IntoView {
    view! {
        <input type="text" bind:value=text placeholder="Enter Some Instruction" />
        {move || if well_formed.get() { CORRECT_EMOJI } else { INCORRECT_EMOJI }}
    }
}

#[component]
pub fn Boxlist(
    lines: Signal<Vec<CodeLineEntry>>,
    button_click: WriteSignal<ButtonClickType>,
) -> impl IntoView {
    let push_line = move |_| {
        button_click.set(ButtonClickType::AddLine);
    };

    let pop_line = move |_| {
        button_click.set(ButtonClickType::RemoveLine);
    };

    view! {
        <div>
            <button on:click=push_line>"Add Line"</button>
            <button on:click=pop_line>"Remove Line"</button>
            <ForEnumerate
                each=move || lines.get()
                key=|entry| entry.id
                children=move |index, entry| {
                    view! {
                        <br />
                        {index}
                        ": "
                        <Textbox well_formed=entry.well_formed.into() text=entry.text_input.into()/>
                    }
                }
            />
        </div>
    }
}
