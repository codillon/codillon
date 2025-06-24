use leptos::prelude::*;
mod utils;
use codillon::frontend::index;

fn main() {
    leptos::mount::mount_to_body(index::textbox::Textbox);
}
