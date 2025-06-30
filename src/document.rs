use super::inputs::ButtonClickType;
use leptos::prelude::*;

use crate::utils::is_well_formed_instr;

#[derive(Debug, Clone, PartialEq)]
pub struct CodeLineEntry {
    pub text_input: ArcRwSignal<String>,
    pub well_formed: ArcSignal<bool>,
    pub id: usize,
}

impl CodeLineEntry {
    pub fn new(id: usize) -> CodeLineEntry {
        let text_input = ArcRwSignal::new(String::new());
        let cloned_text_input = text_input.clone();
        let well_formed = ArcSignal::derive(move || is_well_formed_instr(&cloned_text_input.get()));

        CodeLineEntry {
            text_input,
            well_formed,
            id,
        }
    }
}

#[derive(Debug)]
pub struct Document {
    pub lines: Signal<Vec<CodeLineEntry>>, // keep Signal for external read
}

impl Document {
    pub fn new(button_on_click: ReadSignal<ButtonClickType>) -> Document {
        let mut id_counter: usize = 0;
        let lines = RwSignal::new(Vec::new());

        Effect::new(move |_| {
            match button_on_click.get() {
                ButtonClickType::AddLine => {
                    lines.write().push(CodeLineEntry::new(id_counter));
                    id_counter += 1;
                }
                ButtonClickType::RemoveLine => {
                    lines.write().pop();
                }
                ButtonClickType::InitialState => {
                    // No-op
                }
            }
        });

        Document {
            lines: lines.into(),
        }
    }
}
