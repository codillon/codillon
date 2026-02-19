use anyhow::Result;
use codillon::{dom_struct::DomStruct, editor::Editor, jet::DocumentHandle};
use std::cell::RefCell;

type Body = DomStruct<(Editor, ()), web_sys::HtmlBodyElement>;
type Document = DocumentHandle<Body>;

thread_local! {
    static DOCUMENT: RefCell<Document> = Document::default().into();
}

fn setup() -> Result<()> {
    let editor = DOCUMENT.with_borrow_mut(|doc| {
        let factory = doc.element_factory();
        let body = factory.body();
        let editor = Editor::new(factory);
        let editor_clone = editor.clone();
        doc.set_body(Body::new((editor, ()), body));
        doc.audit();
        editor_clone
    });

    editor.attach_hint_bar();

    web_sys::console::log_1(&"successful audit".into());

    Ok(())
}

fn main() {
    console_error_panic_hook::set_once();
    if let Err(x) = setup() {
        panic!("error: {x}")
    }
}
