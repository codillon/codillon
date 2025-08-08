use anyhow::Result;
use codillon::{editor::Editor, web_support::DocumentHandle, web_support::components::DomStruct};
use std::cell::RefCell;

type Body = DomStruct<(Editor, ()), web_sys::HtmlBodyElement>;
type Document = DocumentHandle<Body>;

thread_local! {
    static DOCUMENT: RefCell<Document> = Document::default().into();
}

fn setup() -> Result<()> {
    DOCUMENT.with_borrow_mut(|doc| {
        let factory = doc.element_factory();
        let selection = doc.get_selection().expect("doc selection");
        let body = factory.body();
        doc.set_body(Body::new((Editor::new(factory, selection), ()), body));
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
