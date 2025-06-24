//! This is the textbox component for the index page.

use crate::utils::is_well_formed_instr;
use leptos::prelude::*;

/// ### Textbox Component
/// This component provides a textbox for one instruction and use color as a correctness indicator.
#[component]
pub fn textbox() -> impl IntoView {
    let (text, set_text) = signal("".to_string());
    let validation_class = move || {
        if is_well_formed_instr(&text.get()) {
            "correct_form"
        } else {
            "incorrect_form"
        }
    };

    view! {
        <input
            type="text"
            prop:value=text
            on:input=move |ev| {
                set_text.set(event_target_value(&ev));
            }
            placeholder="Enter Some Instruction"
        />
        <div class={validation_class}>
            "The instruction is: " {text}
        </div>
    }
}
