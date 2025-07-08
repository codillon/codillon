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
        website.write().keystroke(&e.key());
    });

    view! {
        {move || {
            website
                .get()
                .get_content()
                .iter()
                .enumerate()
                .map(|(index, entry)| {
                    view! {
                        <div>
                            <span>{index} " :"</span>
                            <span>{entry.line.clone()}</span>
                            {if index == website.read_untracked().get_cursor() {
                                (view! { <span class="caret">"|"</span> }).into_any()
                            } else {
                                (view! {}).into_any()
                            }}
                        </div>
                    }
                })
                .collect_view()
        }}
    }
}
