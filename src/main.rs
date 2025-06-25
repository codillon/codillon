use leptos::prelude::*;
mod utils;
use codillon::frontend::index;
mod utils;

fn main() {
    utils::is_well_formed_instr("(i32.const 5)");
    leptos::mount::mount_to_body(index::textbox::Textbox);
}
