use super::utils::{Frame, frame_match, is_well_formed_func, is_well_formed_instrline};
use leptos::prelude::*;
use std::cmp::max;

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

    // Logical signal for processed information of this signal codeline
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
    // Signal for inserting/deleting line
    pub lines: Signal<Vec<CodeLineEntry>>,

    // Matched Frames of the whole program
    pub frames: Signal<Vec<Frame>>,

    // Indicating correctness of the whole func
    pub well_formed: Signal<bool>,

    // Indication the line which will react to user's keystroke. Saves Some(LineNumber), initialized to None
    pub active_line: RwSignal<Option<usize>>,

    // Forbits everything except the active line
    pub is_frozen: Signal<bool>,
}

impl Document {
    pub fn new(keystroke: Signal<String>, click_one_line: Signal<usize>) -> Document {
        let lines: ArcRwSignal<Vec<CodeLineEntry>> = ArcRwSignal::new(Vec::new());
        let active_line = ArcRwSignal::new(None);

        let frames = Self::create_frames_signal(lines.clone());
        let well_formed = Self::create_well_formed_signal(lines.clone());
        let is_frozen = Self::create_is_frozen_signal(lines.clone(), active_line.clone());

        Self::setup_keystroke_handler(keystroke, lines.clone(), active_line.clone(), is_frozen);
        Self::setup_click_handler(
            click_one_line,
            lines.clone(),
            active_line.clone(),
            is_frozen,
        );

        Document {
            lines: lines.into(),
            frames,
            well_formed,
            active_line: active_line.into(),
            is_frozen,
        }
    }

    fn create_frames_signal(lines: ArcRwSignal<Vec<CodeLineEntry>>) -> Signal<Vec<Frame>> {
        Signal::derive(move || {
            frame_match(lines.get().iter().map(|entry| entry.info.get())).unwrap_or_default()
        })
    }

    fn create_is_frozen_signal(
        lines: ArcRwSignal<Vec<CodeLineEntry>>,
        active_line: ArcRwSignal<Option<usize>>,
    ) -> Signal<bool> {
        Signal::derive(move || {
            if let Some(active_line_num) = active_line.get() {
                leptos::logging::log!("ActiveLine Line {}", active_line_num);
                lines.with(|lines| {
                    let entry: Option<CodeLineEntry> = lines.get(active_line_num).cloned();
                    if let Some(entry) = entry {
                        !entry.info.get().well_formed
                    } else {
                        false
                    }
                })
            } else {
                false
            }
        })
    }

    fn create_well_formed_signal(lines: ArcRwSignal<Vec<CodeLineEntry>>) -> Signal<bool> {
        Signal::derive(move || {
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
        })
    }

    fn setup_keystroke_handler(
        keystroke: Signal<String>,
        lines: ArcRwSignal<Vec<CodeLineEntry>>,
        active_line: ArcRwSignal<Option<usize>>,
        is_frozen: Signal<bool>,
    ) {
        let lines_clone = lines.clone();
        let active_line_clone = active_line.clone();

        let mut id_counter = 0;
        Effect::new(move |_| {
            let key = keystroke.get();
            let is_frozen = is_frozen.get_untracked();

            let mut lines_write = lines_clone.write();
            match key.as_str() {
                "Enter" => {
                    if let Some(active_idx) = active_line_clone.get_untracked() {
                        if is_frozen {
                            return;
                        }
                        lines_write.insert(active_idx + 1, CodeLineEntry::new(id_counter));
                        active_line_clone.set(Some(active_idx + 1));
                        id_counter += 1;
                    } else {
                        lines_write.insert(0, CodeLineEntry::new(id_counter));
                        active_line_clone.set(Some(0));
                        id_counter += 1;
                    }
                }
                "Backspace" => {
                    if let Some(active_idx) = active_line_clone.get_untracked() {
                        if active_idx < lines_write.len() {
                            let entry = &lines_write[active_idx];
                            let text = entry.text_input.get_untracked();
                            if !text.is_empty() {
                                entry.text_input.write().pop();
                            } else {
                                lines_write.remove(active_idx);
                                if lines_write.is_empty() {
                                    active_line_clone.set(None);
                                } else {
                                    active_line_clone
                                        .set(Some(max(active_idx.saturating_sub(1), 0)));
                                }
                            }
                        }
                    }
                }
                "ArrowUp" => {
                    if let Some(active_idx) = active_line_clone.get_untracked() {
                        if active_idx > 0 && !is_frozen {
                            active_line_clone.set(Some(active_idx - 1));
                        }
                    }
                }
                "ArrowDown" => {
                    if let Some(active_idx) = active_line_clone.get_untracked() {
                        if active_idx + 1 < lines_write.len() && !is_frozen {
                            active_line_clone.set(Some(active_idx + 1));
                        }
                    }
                }
                _ => {
                    if let Some(active_idx) = active_line_clone.get_untracked() {
                        if active_idx < lines_write.len() {
                            lines_write[active_idx].text_input.write().push_str(&key);
                        }
                    }
                }
            }
        });
    }

    fn setup_click_handler(
        click_one_line: Signal<usize>,
        lines: ArcRwSignal<Vec<CodeLineEntry>>,
        active_line: ArcRwSignal<Option<usize>>,
        is_frozen: Signal<bool>,
    ) {
        Effect::new(move |_| {
            let clicked_id = click_one_line.get();
            let is_frozen = is_frozen.get_untracked();
            if is_frozen {
                return;
            }

            for (idx, codeline) in lines.get_untracked().into_iter().enumerate() {
                if codeline.unique_id == clicked_id {
                    active_line.set(Some(idx));
                    break;
                }
            }
        });
    }
}
