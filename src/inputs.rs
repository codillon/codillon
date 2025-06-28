use leptos::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ButtonClickType {
    AddLine,
    RemoveLine,
    InitialState,
}

#[derive(Debug)]
pub struct Inputs {
    pub textbox_inputs: RwSignal<Vec<RwSignal<String>>>,
    pub button_on_click: RwSignal<ButtonClickType>,
}

impl Default for Inputs {
    fn default() -> Self {
        Self {
            textbox_inputs: RwSignal::new(Vec::new()),
            button_on_click: RwSignal::new(ButtonClickType::InitialState),
        }
    }
}
