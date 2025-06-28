use leptos::prelude::*;
use super::document::CodeLineEntry;
use super::inputs::ButtonClickType;
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
pub fn Boxlist(inputs: ReadSignal<Vec<RwSignal<String>>>, lines: ReadSignal<Vec<CodeLineEntry>>, button_click: WriteSignal<ButtonClickType>) -> impl IntoView {
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
                each=move || lines.get().into_iter().zip(inputs.get().into_iter())
                key=|(entry, _input)| entry.id
                children=move |index, (entry, input)| {
                    view! {
                        <br />
                        {index}
                        ": "
                        <Textbox well_formed=entry.well_formed text=input/>
                    }
                }
            />
        </div>
    }
}