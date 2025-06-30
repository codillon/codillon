use crate::utils::is_well_formed_func;

use super::inputs::ButtonClickType;
use super::utils::{Frame, frame_match, is_well_formed_instrline};
use leptos::prelude::*;

// Hold properties of this code line
#[derive(Debug, Clone)]
pub struct InstrInfo {
    pub well_formed: bool,
    pub kind: InstrKind,
}

impl Default for InstrInfo {
    fn default() -> Self {
        InstrInfo {
            well_formed: true,
            kind: InstrKind::Other,
        }
    }
}

/// For frame matching.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum InstrKind {
    Entry, // Consists of: block, loop, if
    Else,
    End,
    Other,
    Malformed, // Will be remove in the future after enforcement is implemented
}

/// It holds all logical signals corresponding to a single code line.
#[derive(Debug, Clone, PartialEq)]
pub struct CodeLineEntry {
    // Will be bind to a textbox
    // Use Arc because they need to be wrapped in Rwsignal<Vec<_>>.
    // See [link](https://book.leptos.dev/appendix_life_cycle.html#signals-can-be-used-after-they-are-disposed)
    pub text_input: ArcRwSignal<String>,

    // Logical signal
    pub info: ArcSignal<InstrInfo>,

    // The id is unique to the containing EditorBuffer and is maintained across
    // insertions and deletions elsewhere in the buffer. This lets Leptos and
    // the browser avoid re-rendering unchanged lines.
    pub unique_id: usize,
}

impl CodeLineEntry {
    pub fn new(unique_id: usize) -> CodeLineEntry {
        let text_input = ArcRwSignal::new(String::new());
        let cloned_text_input = text_input.clone();
        let info = ArcSignal::derive(move || {
            let result = is_well_formed_instrline(&cloned_text_input.get());
            InstrInfo {
                well_formed: result.is_ok(),
                kind: result.unwrap_or(InstrKind::Malformed),
            }
        });
        CodeLineEntry {
            text_input,
            info,
            unique_id,
        }
    }
}

// Hold logical signals of the website
#[derive(Debug)]
pub struct Document {
    // Signals related to a single codeline
    pub lines: Signal<Vec<CodeLineEntry>>,

    // Matched Frames of the whole program
    pub frames: Signal<Vec<Frame>>,

    // Indicating correctness of the whole func
    pub well_formed: Signal<bool>,
}

impl Document {
    pub fn new(button_on_click: ReadSignal<ButtonClickType>) -> Document {
        let mut id_counter: usize = 0;
        let lines: RwSignal<Vec<CodeLineEntry>> = RwSignal::new(Vec::new());

        let frames = Signal::derive(move || {
            frame_match(lines.get().iter().map(|entry| entry.info.get())).unwrap_or_default()
        });
        let well_formed: Signal<bool> = Signal::derive(move || {
            let lines = lines.get();
            if lines.iter().any(|entry| !entry.info.get().well_formed) {
                return false;
            }

            let text = lines
                .iter()
                .map(|entry| entry.text_input.get())
                .collect::<Vec<_>>()
                .join("\n");

            if !is_well_formed_func(&text) {
                return false;
            }

            true
        });
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
            frames,
            well_formed,
        }
    }
}
