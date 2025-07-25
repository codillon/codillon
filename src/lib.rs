mod common;
mod editor;
mod line;
mod utils;
pub fn start() {
    let editor = editor::editor();
    editor.initialize();
    editor.mount();
}
