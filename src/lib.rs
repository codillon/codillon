use leptos::{
    ev::{self, KeyboardEvent},
    prelude::*,
};
use leptos_use::{use_event_listener, use_window};
mod website;

pub mod utils;

#[component]
pub fn App() -> impl IntoView {
    let website = RwSignal::new(website::Website::default());
    let _listener = use_event_listener(use_window(), ev::keydown, move |e: KeyboardEvent| {
        match e.key().as_str() {
            "ArrowUp" | "ArrowDown" => {
                e.prevent_default(); // prevent browser scroll, TODO: add the scroll control ourselves
            }
            _ => {}
        }
        website.write().keystroke(&e.key());
    });

    view! {
        {move || {
            let cursor = website.read_untracked().get_cursor();
            website
                .get()
                .get_content()
                .iter()
                .enumerate()
                .map(|(index, entry)| {
                    view! {
                        <div>
                            <span>{index} " :"</span>

                            {if index == cursor.0 {
                                let first_part = &entry.line[..cursor.1];
                                let second_part = &entry.line[cursor.1..];
                                (view! {
                                    <span>{first_part}</span>
                                    <span class="caret">"|"</span>
                                    <span>{second_part}</span>
                                })
                                    .into_any()
                            } else {
                                (view! { <span>{entry.line.clone()}</span> }).into_any()
                            }}
                        </div>
                    }
                })
                .collect_view()
        }}
    }
}
