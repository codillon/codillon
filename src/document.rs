use super::utils::{Frame, frame_match, is_well_formed_func, is_well_formed_instrline};
use leptos::prelude::*;
// Hold properties of this code line
#[derive(Debug, Clone, PartialEq)]
pub struct InstrInfo {
    pub prev_good_instr: String,
    pub well_formed: bool,
    pub kind: InstrKind,
}

impl Default for InstrInfo {
    fn default() -> Self {
        InstrInfo {
            prev_good_instr: String::new(),
            well_formed: true,
            kind: InstrKind::Other,
        }
    }
}

/// Save the line/col position of the cursor
#[derive(Debug, Clone, Copy)]
pub struct CusorPosition(pub usize, pub usize);

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
    // As the buffer of the context of the line
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
        let info = ArcMemo::new(move |prev_info| {
            let result = is_well_formed_instrline(&cloned_text_input.get());
            if result.is_ok()
            {
                InstrInfo {
                    prev_good_instr: cloned_text_input.get_untracked(),
                    well_formed: true,
                    kind: result.unwrap()
                }
            }
            else
            {
                let prev_info: InstrInfo = prev_info.cloned().unwrap_or_default();
                InstrInfo {
                    prev_good_instr: prev_info.prev_good_instr,
                    well_formed: false,
                    kind: InstrKind::Malformed
                }
            }
        });

        CodeLineEntry {
            text_input,
            info: info.into(),
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

    // Saves the cursor's postion (and the line is active line)
    pub cursor: RwSignal<CusorPosition>,

    // Forbits everything except the active line
    pub is_frozen: Signal<bool>,
}

impl Document {
    pub fn new(keystroke: Signal<String>, click_one_line: Signal<(usize, usize)>) -> Document {
        let lines: ArcRwSignal<Vec<CodeLineEntry>> = ArcRwSignal::new(vec![CodeLineEntry::new(0)]);
        let active_line = ArcRwSignal::new(CusorPosition(0, 0));

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
            cursor: active_line.into(),
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
        active_line: ArcRwSignal<CusorPosition>,
    ) -> Signal<bool> {
        Signal::derive(move || {
            let cursor = active_line.get();
            lines.with(|lines| {
                let entry: Option<CodeLineEntry> = lines.get(cursor.0).cloned();
                if let Some(entry) = entry {
                    !entry.info.get().well_formed
                } else {
                    false
                }
            })
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
        active_line: ArcRwSignal<CusorPosition>,
        is_frozen: Signal<bool>,
    ) {
        let lines_clone = lines.clone();
        let active_line_clone = active_line.clone();

        let mut id_counter = 1;
        Effect::new(move |_| {
            let key = keystroke.get();
            let is_frozen = is_frozen.get_untracked();

            let mut lines_write = lines_clone.write();
            match key.as_str() {
                "Enter" => {
                    if is_frozen {
                        return;
                    }
                    // We don't allow enter to split one codeline for well-formness
                    let cursor = active_line_clone.get_untracked();
                    lines_write.insert(cursor.0 + 1, CodeLineEntry::new(id_counter));
                    active_line_clone.set(CusorPosition(cursor.0 + 1, 0));
                    id_counter += 1;
                }
                "Backspace" => {
                    let cursor = active_line_clone.get_untracked();
                    if cursor.0 < lines_write.len() {
                        let entry = &lines_write[cursor.0];
                        let mut text = entry.text_input.get_untracked();

                        if cursor.1 > 0 {
                            // Remove character before cursor
                            let mut chars: Vec<char> = text.chars().collect();
                            chars.remove(cursor.1 - 1);
                            text = chars.into_iter().collect();
                            entry.text_input.set(text);
                            active_line_clone.set(CusorPosition(cursor.0, cursor.1 - 1));
                        } else if cursor.0 > 0 && text.is_empty() {
                            // At start of empty line, delete the line and move to the end of previous line
                            let prev_line = &lines_write[cursor.0 - 1];
                            let prev_text = prev_line.text_input.get_untracked();
                            let prev_len = prev_text.chars().count();

                            lines_write.remove(cursor.0);
                            active_line_clone.set(CusorPosition(cursor.0 - 1, prev_len));
                        }
                    }
                }
                "ArrowUp" => {
                    let cursor = active_line_clone.get_untracked();
                    if cursor.0 > 0 && !is_frozen {
                        // Keep same column position or adjust if new line is shorter
                        let target_col = cursor.1;
                        let upper_line = &lines_write[cursor.0 - 1];
                        let upper_len = upper_line.text_input.get_untracked().chars().count();
                        let new_col = target_col.min(upper_len);
                        active_line_clone.set(CusorPosition(cursor.0 - 1, new_col));
                    }
                }
                "ArrowDown" => {
                    let cursor = active_line_clone.get_untracked();
                    if cursor.0 + 1 < lines_write.len() && !is_frozen {
                        // Keep same column position or adjust if new line is shorter
                        let target_col = cursor.1;
                        let lower_line = &lines_write[cursor.0 + 1];
                        let lower_len = lower_line.text_input.get_untracked().chars().count();
                        let new_col = target_col.min(lower_len);
                        active_line_clone.set(CusorPosition(cursor.0 + 1, new_col));
                    }
                }
                "ArrowLeft" => {
                    let cursor = active_line_clone.get_untracked();
                    if cursor.1 > 0 {
                        // Move cursor left in current line
                        active_line_clone.set(CusorPosition(cursor.0, cursor.1 - 1));
                    } else if cursor.0 > 0 && !is_frozen {
                        // Move to end of previous line
                        let prev_line = &lines_write[cursor.0 - 1];
                        let prev_len = prev_line.text_input.get_untracked().chars().count();
                        active_line_clone.set(CusorPosition(cursor.0 - 1, prev_len));
                    }
                }
                "ArrowRight" => {
                    let cursor = active_line_clone.get_untracked();
                    let current_line = &lines_write[cursor.0];
                    let line_len = current_line.text_input.get_untracked().chars().count();

                    if cursor.1 < line_len {
                        // Move cursor right in current line
                        active_line_clone.set(CusorPosition(cursor.0, cursor.1 + 1));
                    } else if cursor.0 + 1 < lines_write.len() && !is_frozen {
                        // Move to beginning of next line
                        active_line_clone.set(CusorPosition(cursor.0 + 1, 0));
                    }
                }
                _ => {
                    if key.chars().count() > 1
                    {
                        leptos::logging::log!("Ignore special keystroke {key}");
                        return;
                    }
                    let cursor = active_line_clone.get_untracked();
                    if cursor.0 < lines_write.len() {
                        // Insert at cursor position
                        let entry = &lines_write[cursor.0];
                        let text = entry.text_input.get_untracked();
                        let (before, after) = text.split_at(cursor.1);

                        entry.text_input.set(format!("{}{}{}", before, key, after));
                        active_line_clone.set(CusorPosition(
                            cursor.0,
                            cursor.1 + key.chars().count(),
                        ));
                    }
                }
            }
        });
    }

    fn setup_click_handler(
        click_one_line: Signal<(usize, usize)>,
        lines: ArcRwSignal<Vec<CodeLineEntry>>,
        active_line: ArcRwSignal<CusorPosition>,
        is_frozen: Signal<bool>,
    ) {
        Effect::new(move |_| {
            let clicked_pos = click_one_line.get();
            let cursor_pos = active_line.get_untracked();
            let is_frozen = is_frozen.get_untracked();


            for (idx, codeline) in lines.get_untracked().into_iter().enumerate() {
                if codeline.unique_id == clicked_pos.0 {
                    if is_frozen && idx != cursor_pos.0
                    {
                        return;
                    }
                    active_line.set(CusorPosition(idx, clicked_pos.1));
                    break;
                }
            }
        });
    }
}
