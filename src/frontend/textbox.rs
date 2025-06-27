//! This is the textbox component for the index page.

use crate::utils::is_well_formed_instr;
use leptos::prelude::*;

/// ### Textbox Component
/// This component provides a textbox for one instruction and use emoji to indicate correctness.
///
/// ### Parameters
/// `text` the signal for the text of this textbox
#[component]
pub fn Textbox(text: RwSignal<String>) -> impl IntoView {
    // Create a derived signal that memoizes the validation result
    let is_valid = move || is_well_formed_instr(&text.get());
    const CORRECT_EMOJI: &str = "✅";
    const INCORRECT_EMOJI: &str = "❌";

    view! {
        <div class="textLineContents">{text}</div>
        {move || if is_valid() { CORRECT_EMOJI } else { INCORRECT_EMOJI }}
    }
}
