use leptos::prelude::*;

mod document;
mod frontend;
mod inputs;
mod utils;

/// Render the whole App
#[component]
pub fn App() -> impl IntoView {
    let inputs = inputs::Inputs::default();
    let doc = document::Document::new(
        inputs.keystroke.into(),
        inputs.click_one_line.read_only().into(),
    );

    view! {
        <frontend::Editor
            lines=doc.lines
            active_line=doc.active_line.read_only()
            click_one_line=inputs.click_one_line.write_only()
        />
        <hr />
        <frontend::GlobalStatus well_formed=doc.well_formed frames=doc.frames is_frozen=doc.is_frozen/>
    }
}
