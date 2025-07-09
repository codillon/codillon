use crate::utils::*;
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct CodelineEntry {
    pub line: String,
}

#[derive(Debug, Clone)]
pub struct Website {
    content: Vec<CodelineEntry>,
    frames: HashMap<usize, usize>,
    cursor: (usize, usize), //  Line #, Col #
    selection: Option<Frame>,
}

impl Default for Website {
    fn default() -> Self {
        Website {
            content: vec![CodelineEntry::default()],
            frames: HashMap::new(),
            cursor: (0, 0),
            selection: None,
        }
    }
}

impl Website {
    pub fn get_content(&self) -> &Vec<CodelineEntry> {
        &self.content
    }

    pub fn get_frames(&self) -> &HashMap<usize, usize> {
        &self.frames
    }

    pub fn get_cursor(&self) -> (usize, usize) {
        self.cursor
    }

    pub fn update_cursor(&mut self, line_index: usize, line_offset: usize) {
        if line_index != self.cursor.0 && !Self::check(&self.content) {
            // Only check for validity if we're moving to a new line.
            return;
        }

        self.cursor.0 = std::cmp::min(line_index, self.content.len() - 1);
        self.cursor.1 = std::cmp::min(line_offset, self.active_line().chars().count());
    }

    pub fn keystroke(&mut self, key: &str) {
        match key {
            "Enter" => self.enter_at_cursor(),
            "Backspace" => self.backspace_at_cursor(),
            "ArrowDown" => self.cursor_move_down(),
            "ArrowUp" => self.cursor_move_up(),
            "ArrowLeft" => self.cursor_move_left(),
            "ArrowRight" => self.cursor_move_right(),
            _ => {
                if key.chars().count() > 1 {
                    leptos::logging::log!("Unhandled Special Keystroke {}", key);
                    return;
                }
                self.insert_normal_char_at_cursor(key.chars().next().unwrap());
            }
        }
    }

    pub fn get_selection(&self) -> &Option<Frame> {
        &self.selection
    }

    fn insert_normal_char_at_cursor(&mut self, ch: char) {
        let (first_part, second_part) = self.active_line().split_at(self.cursor.1);
        *self.mut_active_line() = [first_part, &ch.to_string(), second_part].join("");
        self.cursor.1 += 1;
        self.automatic_append_end_after_cursor();
        if Self::check(&self.content) {
            self.update_frames();
        }
    }

    fn automatic_append_end_after_cursor(&mut self) {
        match self.active_line() {
            "block" | "loop" | "if" => {
                let mut new_content = self.content.clone();
                new_content.insert(
                    self.cursor.0 + 1,
                    CodelineEntry {
                        line: "end".to_string(),
                    },
                );
                self.try_commit_new_content(new_content);
            }
            _ => (),
        }
    }

    fn backspace_at_cursor(&mut self) {
        if self.cursor.1 > 0 {
            let first_part = &self.active_line()[..self.cursor.1 - 1];
            let second_part = &self.active_line()[self.cursor.1..];
            *self.mut_active_line() = [first_part, second_part].join("");
            self.cursor.1 -= 1;
            if self.frames.get(&self.cursor.0).is_some() && Self::check(&self.content) {
                self.update_frames();
            }
        } else if self.selection.is_some() {
            self.try_delete_selection();
        } else if self.frames.get(&self.cursor.0).is_some() {
            let related_line = self.frames.get(&self.cursor.0).unwrap();
            self.selection = Some(
                std::cmp::min(self.cursor.0, *related_line)
                    ..=std::cmp::max(self.cursor.0, *related_line),
            );
        } else if self.cursor.0 > 0 {
            let mut new_line = self.content[self.cursor.0 - 1].line.clone();
            new_line.push_str(self.active_line());
            let mut new_content = self.content.clone();
            new_content.splice(
                self.cursor.0 - 1..self.cursor.0 + 1,
                std::iter::once(CodelineEntry { line: new_line }),
            );
            if self.try_commit_new_content(new_content) {
                self.cursor.0 -= 1;
                self.cursor.1 = self.active_line().chars().count();
            }
        }
    }
    fn cursor_move_right(&mut self) {
        self.selection = None;
        if self.cursor.1 < self.active_line().chars().count() {
            self.cursor.1 += 1;
        } else if self.cursor.0 < self.content.len() - 1 && Self::check(&self.content) {
            self.cursor.0 += 1;
            self.cursor.1 = 0;
        }
    }

    /// ### Returns
    /// Success or not.
    fn try_delete_selection(&mut self) -> bool {
        let mut new_content = self.content.clone();
        let selection = self.selection.clone().unwrap();
        new_content.splice(
            selection.clone(),
            std::iter::once(CodelineEntry {
                line: "end".to_string(),
            }),
        );

        if self.try_commit_new_content(new_content.clone()) {
            self.cursor.0 = *selection.start();
            return true;
        }

        new_content.remove(*selection.start());

        if new_content.is_empty() {
            new_content.push(CodelineEntry {
                line: String::new(),
            });
        }
        
        if self.try_commit_new_content(new_content) {
            self.cursor.0 = selection.start().saturating_sub(1);
            self.cursor.1 = self.active_line().chars().count();
            true
        } else {
            false
        }
    }

    /// ### Returns
    /// Success or not.
    fn try_commit_new_content(&mut self, new_content: Vec<CodelineEntry>) -> bool {
        if Self::check(&new_content) {
            self.selection = None;
            let _ = std::mem::replace(&mut self.content, new_content);
            self.update_frames();
            true
        } else {
            false
        }
    }

    fn cursor_move_left(&mut self) {
        self.selection = None;
        if self.cursor.1 > 0 {
            self.cursor.1 -= 1;
        } else if self.cursor.0 > 0 && Self::check(&self.content) {
            self.cursor.0 -= 1;
            self.cursor.1 = self.active_line().chars().count();
        }
    }

    fn cursor_move_up(&mut self) {
        self.selection = None;
        if Self::check(&self.content) {
            self.cursor.0 = self.cursor.0.saturating_sub(1);
            self.cursor.1 = std::cmp::min(self.cursor.1, self.active_line().chars().count());
        }
    }

    fn cursor_move_down(&mut self) {
        self.selection = None;
        if Self::check(&self.content) {
            self.cursor.0 = std::cmp::min(self.cursor.0 + 1, self.content.len() - 1);
            self.cursor.1 = std::cmp::min(self.cursor.1, self.active_line().chars().count());
        }
    }

    fn active_line(&self) -> &str {
        &self.content[self.cursor.0].line
    }

    fn mut_active_line(&mut self) -> &mut String {
        &mut self.content[self.cursor.0].line
    }

    fn enter_at_cursor(&mut self) {
        self.selection = None;
        let mut new_content = self.content.clone();
        let (first_part, second_part) = self.active_line().split_at(self.cursor.1);

        new_content.splice(
            self.cursor.0..self.cursor.0 + 1,
            [
                CodelineEntry {
                    line: first_part.to_string(),
                },
                CodelineEntry {
                    line: second_part.to_string(),
                },
            ],
        );

        if self.try_commit_new_content(new_content) {
            self.cursor.0 += 1;
            self.cursor.1 = 0;
        }
    }

    fn check(content: &[CodelineEntry]) -> bool {
        content
            .iter()
            .all(|entry| is_well_formed_instr(&entry.line))
            && is_well_formed_func(
                &content
                    .iter()
                    .map(|entry| entry.line.clone())
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
    }

    fn update_frames(&mut self) {
        self.frames.clear();
        frame_match(self.content.iter().map(|entry| entry.line.as_str()))
            .iter()
            .for_each(|frame| {
                self.frames.insert(*frame.start(), *frame.end());
                self.frames.insert(*frame.end(), *frame.start());
            });
    }
}
