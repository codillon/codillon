use anyhow::Result;
use codillon::{dom_struct::DomStruct, editor::Editor, jet::DocumentHandle};
use std::cell::RefCell;
use wasm_bindgen::JsCast;

type Body = DomStruct<(Editor, ()), web_sys::HtmlBodyElement>;
type Document = DocumentHandle<Body>;

thread_local! {
    static DOCUMENT: RefCell<Document> = Document::default().into();
}

fn setup() -> Result<()> {
    DOCUMENT.with_borrow_mut(|doc| {
        let factory = doc.element_factory();
        let body = factory.body();
        doc.set_body(Body::new((Editor::new(factory), ()), body));
        doc.audit();
    });
    let document = web_sys::window()
        .expect("window exists")
        .document()
        .expect("document exists");
    let debug_div = document
        .create_element("div")
        .expect("debug delement created")
        .dyn_into::<web_sys::HtmlDivElement>()
        .expect("dynamic cast");
    debug_div.set_attribute("id", "codillon-debug").ok();
    debug_div.set_text_content(Some("Debug panel"));
    document
        .body()
        .expect("body exists")
        .append_child(&debug_div)
        .ok();

    web_sys::console::log_1(&"successful audit".into());

    Ok(())
}

fn main() {
    console_error_panic_hook::set_once();
    if let Err(x) = setup() {
        panic!("error: {x}")
    }
}
