use anyhow::Result;
use codillon::{
    dom_struct::DomStruct,
    editor::Editor,
    jet::{ControlHandlers, DocumentHandle, ReactiveComponent},
};
use std::cell::RefCell;

type Body = ReactiveComponent<DomStruct<(Editor, ()), web_sys::HtmlBodyElement>>;
type Document = DocumentHandle<Body>;

thread_local! {
    static DOCUMENT: RefCell<Document> = Document::default().into();
}

fn setup() -> Result<()> {
    DOCUMENT.with_borrow_mut(|doc| {
        let factory = doc.element_factory();
        let body_elem = factory.body();
        let editor = Editor::new(factory);
        doc.set_body(Body::new(DomStruct::new((editor, ()), body_elem)));
        doc.body_mut().unwrap().set_onmousedown(move |_| {
            DOCUMENT.with_borrow(|doc| {
                if let Some(body) = doc.body() {
                    body.inner().get().0.hide_autocomplete();
                }
            });
        });
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
