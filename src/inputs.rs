use leptos::prelude::*;

// A simple enum for `Add Line` and `Remove Line` buttons
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ButtonClickType {
    AddLine,
    RemoveLine,
    InitialState,
}

#[derive(Debug)]
pub struct Inputs {
    pub button_on_click: RwSignal<ButtonClickType>,
}

impl Default for Inputs {
    fn default() -> Self {
        Self {
            button_on_click: RwSignal::new(ButtonClickType::InitialState),
        }
    }
}
