use super::inputs::ButtonClickType;
use leptos::prelude::*;

use crate::utils::is_well_formed_instr;

#[derive(Debug, Clone)]
pub struct CodeLineEntry {
    pub text: Signal<String>,
    pub well_formed: Signal<bool>,
    pub id: usize
}

impl CodeLineEntry {
    pub fn new(text_input: ReadSignal<String>, id: usize) -> CodeLineEntry {
        CodeLineEntry {
            text: Signal::derive(move || text_input.get()),
            well_formed: Signal::derive(move || is_well_formed_instr(&text_input.get())),
            id
        }
    }
}

#[derive(Debug)]
pub struct Document {
    pub lines: RwSignal<Vec<CodeLineEntry>>
}

impl Document {
    pub fn new(
        button_on_click: ReadSignal<ButtonClickType>,
        box_text_inputs: WriteSignal<Vec<RwSignal<String>>>,
    ) -> Document {
        let lines = RwSignal::new(Vec::new());

        let mut id = 0;
        // Effect is discouraged, but I think we have to use because we need to build signal connection dynamically
        Effect::new(move |_|
            match button_on_click.get() {
                ButtonClickType::AddLine => 
                {
                    let new_input = RwSignal::new(String::new());
                    lines.write().push(CodeLineEntry::new(new_input.read_only(), id));
                    id += 1;
                    box_text_inputs.write().push(new_input);
                }
                ButtonClickType::RemoveLine =>
                {
                    lines.write().pop();
                    box_text_inputs.write().pop();
                }
                ButtonClickType::InitialState => {
                    // Do nothing for initial state, unreachable
                }
            }
        );
        Document { lines }
    }
}
