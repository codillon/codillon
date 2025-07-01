use super::utils::{Frame, frame_match, is_well_formed_instrline};
use crate::utils::is_well_formed_func;
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

    // Indication the line which will react to user's keystroke. Saves Some(LineNumber), initialized to 0
    pub active_line: RwSignal<Option<usize>>,
}

impl Document {
    pub fn new(keystroke: Signal<String>) -> Document {
        let mut id_counter: usize = 0;
        let lines: RwSignal<Vec<CodeLineEntry>> = RwSignal::new(Vec::new());
        let active_line = RwSignal::new(None);
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

        // Effect for keystroke handling
        Effect::new(move |_| {
            let key = keystroke.get();

            let mut lines_write = lines.write();
            match key.as_str() {
                "Enter" => {
                    let insert_index = active_line.get_untracked().map_or(0, |idx| idx + 1);
                    lines_write.insert(insert_index, CodeLineEntry::new(id_counter));
                    active_line.set(Some(insert_index));
                    id_counter += 1;
                }
                "Backspace" => {
                    if let Some(active_idx) = active_line.get_untracked() {
                        if active_idx < lines_write.len() {
                            let entry = &lines_write[active_idx];
                            let mut text = entry.text_input.get_untracked();
                            if !text.is_empty() {
                                text.pop();
                                entry.text_input.set(text);
                            } else if lines_write.len() > 1 {
                                lines_write.remove(active_idx);
                                if lines_write.is_empty() {
                                    active_line.set(None);
                                } else if active_idx == 0 {
                                    active_line.set(Some(0));
                                } else {
                                    active_line.set(Some(active_idx - 1));
                                }
                            }
                        }
                    }
                }
                "ArrowUp" => {
                    if let Some(active_idx) = active_line.get_untracked() {
                        if active_idx > 0 {
                            active_line.set(Some(active_idx - 1));
                        }
                    }
                }
                "ArrowDown" => {
                    if let Some(active_idx) = active_line.get_untracked() {
                        if active_idx + 1 < lines_write.len() {
                            active_line.set(Some(active_idx + 1));
                        }
                    }
                }
                _ => {
                    if let Some(active_idx) = active_line.get_untracked() {
                        if active_idx < lines_write.len() {
                            let entry = &lines_write[active_idx];
                            let mut text = entry.text_input.get_untracked();
                            if key.len() == 1 {
                                text.push_str(&key);
                                entry.text_input.set(text);
                            }
                        }
                    }
                }
            }
        });

        Document {
            lines: lines.into(),
            frames,
            well_formed,
            active_line,
        }
    }
}
