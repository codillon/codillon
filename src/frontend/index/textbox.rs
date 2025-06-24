//! This is the textbox component for the index page.

use crate::utils::is_well_formed_instr;
use leptos::prelude::*;

/// ### Textbox Component
/// This component provides a textbox for one instruction and use emoji to indicate correctness.
#[component]
pub fn textbox() -> impl IntoView {
    let (text, set_text) = signal("".to_string());

    // Create a derived signal that memoizes the validation result
    let is_valid = move || is_well_formed_instr(&text.get());
    const CORRECT_EMOJI: &str = "✅";
    const INCORRECT_EMOJI: &str = "❌";

    view! {
        <input
            type="text"
            prop:value=text
            on:input=move |ev| {
                set_text.set(event_target_value(&ev));
            }
            placeholder="Enter Some Instruction"
        />
        {move || if is_valid() {
            CORRECT_EMOJI
        } else {
            INCORRECT_EMOJI
        }}
    }
}
