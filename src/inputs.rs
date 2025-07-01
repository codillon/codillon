use leptos::{
    ev::{self, KeyboardEvent},
    prelude::*,
};
use leptos_use::{use_event_listener, use_window};

#[derive(Debug)]
pub struct Inputs {
    pub keystroke: RwSignal<String>,
}

impl Default for Inputs {
    fn default() -> Self {
        let keystroke = RwSignal::new(String::new());
        let _listener = use_event_listener(use_window(), ev::keydown, move |e: KeyboardEvent| {
            keystroke.set(e.key());
        });

        Self { keystroke }
    }
}
