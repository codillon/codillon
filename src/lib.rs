use leptos::*;

mod utils;
mod inputs;
mod document;
mod frontend;

#[derive(Debug)]
pub struct Website {
    pub inputs: inputs::Inputs,
    pub doc: document::Document,
}

impl Default for Website {
    fn default() -> Self {
        let inputs = inputs::Inputs::default();
        let button_click = inputs.button_on_click.clone().read_only();
        let textbox = inputs.textbox_inputs.write_only();
        Website {
            inputs,
            doc: document::Document::new(button_click, textbox)
        }
    }
    
}

impl Website {
    pub fn app() -> impl IntoView {
        let website = Website::default();
        let boxlist_inputs = website.inputs.textbox_inputs;
        let button_click = website.inputs.button_on_click;
        let lines = website.doc.lines;

        view! {
            <frontend::Boxlist inputs=boxlist_inputs.read_only() lines=lines.read_only() button_click=button_click.write_only()/>
        }
    }
}
