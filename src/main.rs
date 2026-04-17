use anyhow::Result;
use codillon::{dom_struct::DomStruct, editor::Editor, jet::DocumentHandle};
use std::cell::RefCell;

type Body = DomStruct<(Editor, ()), web_sys::HtmlBodyElement>;
type Document = DocumentHandle<Body>;

thread_local! {
    static DOCUMENT: RefCell<Document> = Document::default().into();
}

fn setup() -> Result<()> {
    DOCUMENT.with_borrow_mut(|doc| -> Result<()> {
        let factory = doc.element_factory();
        let body = factory.body();
        doc.set_body(Body::new((Editor::new(factory)?, ()), body));
        doc.audit();
        Ok(())
    })?;

    Ok(())
}

fn main() {
    console_error_panic_hook::set_once();
    if let Err(x) = setup() {
        panic!("error: {x}")
    }
}
