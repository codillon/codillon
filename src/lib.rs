use leptos::prelude::*;

mod document;
mod frontend;
mod inputs;
mod utils;

#[derive(Debug)]
pub struct Website {
    pub inputs: inputs::Inputs,
    pub doc: document::Document,
}

impl Default for Website {
    fn default() -> Self {
        let inputs = inputs::Inputs::default();
        let button_click = inputs.button_on_click.clone().read_only();
        Website {
            inputs,
            doc: document::Document::new(button_click),
        }
    }
}

impl Website {
    pub fn app() -> impl IntoView {
        let website = Website::default();
        let button_click = website.inputs.button_on_click;
        let lines = website.doc.lines;

        view! {
            <frontend::Boxlist lines=lines button_click=button_click.write_only()/>
        }
    }
}
