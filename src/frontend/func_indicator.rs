use super::EditorBuffer;
use crate::utils::is_well_formed_func;
use leptos::prelude::*;

/// Single indicator for function well-formedness
///
/// Checks if the entire function is syntactically well-formed and if each instruction is well-formed
///
/// # Parameters
/// editor_buffer: A signal that holds the current state of the EditorBuffer
///
/// # Returns
/// A view that displays a happy emoji if the function is well-formed, otherwise a sad emoji
#[component]
pub fn FuncIndicator(editor_buffer: ReadSignal<EditorBuffer>) -> impl IntoView {
    view! {
        <div style="position:fixed; top: 1rem; right: 1rem; font-size: 3rem;">
            {move || if editor_buffer.get().all_lines_well_formed() && is_well_formed_func(&editor_buffer.get().concat()) { "😆" } else { "😕" }}
        </div>
    }
}