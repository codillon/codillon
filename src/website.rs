use crate::utils::*;

#[derive(Debug, Clone, Default)]
pub struct CodelineEntry {
    pub line: String,
}

#[derive(Debug, Clone)]
pub struct Website {
    content: Vec<CodelineEntry>,
    cursor: (usize, usize), //  Line #, Col #
}

impl Default for Website {
    fn default() -> Self {
        Website {
            content: vec![CodelineEntry::default()],
            cursor: (0, 0),
        }
    }
}

impl Website {
    pub fn get_content(&self) -> &Vec<CodelineEntry> {
        &self.content
    }

    pub fn get_cursor(&self) -> (usize, usize) {
        self.cursor
    }

    pub fn update_line_index(&mut self, index: usize) {
        if self.content.is_empty() {
            return;
        }
        if !Self::check(&self.content) {
            // Cursor cannot move if the code is invalid.
            return;
        }

        if index >= self.content.len() {
            self.cursor.0 = self.content.len() - 1;
        } else {
            self.cursor.0 = index;
        }
        self.update_cursor_index();
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

    fn insert_normal_char_at_cursor(&mut self, ch: char) {
        let (first_part, second_part) = self.active_line().split_at(self.cursor.1);
        *self.mut_active_line() = [first_part, &ch.to_string(), second_part].join("");
        self.cursor.1 += 1;
        self.automatic_append_end_after_cursor();
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
                if Self::check(&new_content) {
                    let _ = std::mem::replace(&mut self.content, new_content);
                }
            }
            _ => (),
        }
    }

    fn update_cursor_index(&mut self) {
        // Ensure that the cursor index is within the content
        if self.cursor.1 <= self.active_line().len() {
            return;
        }
        self.cursor.1 = 0;
    }

    fn backspace_at_cursor(&mut self) {
        if self.cursor.1 > 0 {
            let first_part = &self.active_line()[..self.cursor.1 - 1];
            let second_part = &self.active_line()[self.cursor.1..];
            *self.mut_active_line() = [first_part, second_part].join("");
            self.cursor.1 -= 1;
        } else if self.cursor.0 > 0 {
            let mut new_line = self.content[self.cursor.0 - 1].line.clone();
            new_line.push_str(self.active_line());
            let mut new_content = self.content.clone();
            new_content.splice(
                self.cursor.0 - 1..self.cursor.0 + 1,
                std::iter::once(CodelineEntry { line: new_line }),
            );
            if Self::check(&new_content) {
                let _ = std::mem::replace(&mut self.content, new_content);
                self.cursor.0 -= 1;
                self.cursor.1 = self.active_line().chars().count();
            }
        }
    }
    fn cursor_move_right(&mut self) {
        if self.cursor.1 < self.active_line().chars().count() {
            self.cursor.1 += 1;
        } else if self.cursor.0 < self.content.len() - 1 && Self::check(&self.content) {
            self.cursor.0 += 1;
            self.cursor.1 = 0;
        }
    }

    fn cursor_move_left(&mut self) {
        if self.cursor.1 > 0 {
            self.cursor.1 -= 1;
        } else if self.cursor.0 > 0 && Self::check(&self.content) {
            self.cursor.0 -= 1;
            self.cursor.1 = self.active_line().chars().count();
        }
    }

    fn cursor_move_up(&mut self) {
        if Self::check(&self.content) {
            self.cursor.0 = self.cursor.0.saturating_sub(1);
            self.cursor.1 = std::cmp::min(self.cursor.1, self.active_line().chars().count());
        }
    }

    fn cursor_move_down(&mut self) {
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

        if Self::check(&new_content) {
            self.cursor.0 += 1;
            self.cursor.1 = 0;
            let _ = std::mem::replace(&mut self.content, new_content);
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
}
