use leptos::prelude::*;

mod document;
mod frontend;
mod inputs;
mod utils;

/// Hold logical items of our website
#[derive(Debug)]
pub struct Website {
    // Hold signals corresponding with direct user input
    pub inputs: inputs::Inputs,
    // Hold other logic signals
    pub doc: document::Document,
}

impl Default for Website {
    fn default() -> Self {
        let inputs = inputs::Inputs::default();
        let keystroke = inputs.keystroke.clone().read_only();
        Website {
            inputs,
            doc: document::Document::new(keystroke.into()),
        }
    }
}

impl Website {
    pub fn app() -> impl IntoView {
        let website = Website::default();
        let lines = website.doc.lines;

        view! {
            <frontend::Editor lines active_line=website.doc.active_line.read_only() />
            <hr />
            <frontend::GlobalStatus well_formed=website.doc.well_formed frames=website.doc.frames />
        }
    }
}
