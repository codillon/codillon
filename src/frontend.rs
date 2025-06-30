use super::document::CodeLineEntry;
use super::inputs::ButtonClickType;
use leptos::prelude::*;

const CORRECT_EMOJI: &str = "✅";
const INCORRECT_EMOJI: &str = "❌";

/// This component provides a textbox for one instruction and use emoji to indicate correctness.
///
/// ### Parameters
/// `well_formed`: the signal indicating correctness
///
/// `text`: the signal for the text of this textbox
#[component]
pub fn Textbox(well_formed: Signal<bool>, text: RwSignal<String>) -> impl IntoView {
    view! {
        <input type="text" bind:value=text placeholder="Enter Some Instruction" />
        {move || if well_formed.get() { CORRECT_EMOJI } else { INCORRECT_EMOJI }}
    }
}

/// This component creates a list of textboxs with a push button and a pop button.
/// Initially, it will have 0 textboxes.
///
/// ### Parameters
/// `lines`: Read from this signal to get the content
///
/// `button_click`: bind the buttons to this signal
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
                key=|entry| entry.unique_id
                children=move |index, entry| {
                    view! {
                        <br />
                        {index}
                        ": "
                        <Textbox
                            well_formed=entry.well_formed.into()
                            text=entry.text_input.into()
                        />
                    }
                }
            />
        </div>
    }
}
