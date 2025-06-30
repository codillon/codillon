use codillon::Website;

fn main() {
    console_error_panic_hook::set_once();
    leptos::mount::mount_to_body(Website::app);
}
