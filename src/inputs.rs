use leptos::{
    ev::{self, KeyboardEvent},
    prelude::*,
};
use leptos_use::{use_event_listener, use_window};

/// Store users' direct input signals
#[derive(Debug)]
pub struct Inputs {
    // Signal for content of keystroke
    pub keystroke: RwSignal<String>,
    // Signal for one code line being clicked, saves unique_id
    pub click_one_line: RwSignal<usize>,
}

impl Default for Inputs {
    fn default() -> Self {
        let keystroke = RwSignal::new(String::new());
        let _listener = use_event_listener(use_window(), ev::keydown, move |e: KeyboardEvent| {
            keystroke.set(e.key());
        });

        Self {
            keystroke,
            click_one_line: RwSignal::new(0),
        }
    }
}
