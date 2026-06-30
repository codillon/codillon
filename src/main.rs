use anyhow::Result;
use codillon::editor::{Body, Document, EditorHolder};
use std::cell::RefCell;

thread_local! {
    static DOCUMENT: RefCell<Document> = Document::default().into();
}

fn setup() -> Result<()> {
    DOCUMENT.with_borrow_mut(|doc| -> Result<()> {
        let factory = doc.element_factory();
        let body = factory.body();
        doc.set_body(Body::new((EditorHolder::new(factory)?, ()), body));

        #[cfg(debug_assertions)]
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
