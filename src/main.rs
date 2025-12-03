use anyhow::Result;
use codillon::{dom_struct::DomStruct, editor::Editor, jet::DocumentHandle, jet::ElementHandle};
use web_sys::HtmlInputElement;
use wasm_bindgen::JsCast;
use std::cell::RefCell;

type Body = DomStruct<(Editor, (ElementHandle<HtmlInputElement>, ())), web_sys::HtmlBodyElement>;
type Document = DocumentHandle<Body>;

thread_local! {
    static DOCUMENT: RefCell<Document> = Document::default().into();
}

fn setup() -> Result<()> {
    DOCUMENT.with_borrow_mut(|doc| {
        let factory = doc.element_factory();
        let body = factory.body();
        let editor = Editor::new(factory.clone());

        let mut slider = factory.input();
        slider.set_attribute("type", "range");
        slider.set_attribute("min", "0");
        slider.set_attribute("value", "0");
        slider.set_attribute("class", "step-slider");

        let slider_editor = editor.clone();
        // Slider closure for updating program state
        slider.set_oninput(move |event: web_sys::Event| {
            if let Some(input) = event
                .target()
                .and_then(|t| t.dyn_into::<web_sys::HtmlInputElement>().ok())
            {
                let value = input.value().parse::<usize>().unwrap_or(0);
                slider_editor.slider_change(value);
            }
        });

        doc.set_body(Body::new((editor, (slider, ())), body));
        doc.audit();
    });

    web_sys::console::log_1(&"successful audit".into());

    Ok(())
}

fn main() {
    console_error_panic_hook::set_once();
    if let Err(x) = setup() {
        panic!("error: {x}")
    }
}
