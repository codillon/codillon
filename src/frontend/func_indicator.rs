use super::EditorBuffer;
use crate::utils::is_well_formed_func;
use leptos::prelude::*;

#[component]
pub fn FuncIndicator(editor_buffer: ReadSignal<EditorBuffer>) -> impl IntoView {
    view! {
        <div style="position:fixed; top: 1rem; right: 1rem; font-size: 3rem;">
            {move || if editor_buffer.get().all_lines_well_formed() && is_well_formed_func(&editor_buffer.get().concat()) { "😆" } else { "😕" }}
        </div>
    }
}
