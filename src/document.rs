use super::inputs::ButtonClickType;
use leptos::prelude::*;

use crate::utils::is_well_formed_instr;

/// It holds all logical signals corresponding to a single code line.
#[derive(Debug, Clone, PartialEq)]
pub struct CodeLineEntry {
    // Will be bind to a textbox
    // Use Arc because they need to be wrapped in Rwsignal<Vec<_>>.
    // See [link](https://book.leptos.dev/appendix_life_cycle.html#signals-can-be-used-after-they-are-disposed)
    pub text_input: ArcRwSignal<String>,

    // Logical signal indicating correctness
    pub well_formed: ArcSignal<bool>,

    // The id is unique to the containing EditorBuffer and is maintained across
    // insertions and deletions elsewhere in the buffer. This lets Leptos and
    // the browser avoid re-rendering unchanged lines.
    pub unique_id: usize,
}

impl CodeLineEntry {
    pub fn new(unique_id: usize) -> CodeLineEntry {
        let text_input = ArcRwSignal::new(String::new());
        let cloned_text_input = text_input.clone();
        let well_formed = ArcSignal::derive(move || is_well_formed_instr(&cloned_text_input.get()));

        CodeLineEntry {
            text_input,
            well_formed,
            unique_id,
        }
    }
}

// Hold logical signals of the website
#[derive(Debug)]
pub struct Document {
    pub lines: Signal<Vec<CodeLineEntry>>,
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
