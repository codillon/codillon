use codillon::Website;

fn main() {
    // To show rust stack backtrace in console when crashed.
    console_error_panic_hook::set_once();
    leptos::mount::mount_to_body(Website::app);
}
