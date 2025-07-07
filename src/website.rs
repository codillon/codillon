use std::cmp;

use crate::utils::*;

#[derive(Debug, Clone, Default)]
pub struct CodelineEntry {
    pub line: String,
}

#[derive(Debug, Clone)]
pub struct Website {
    content: Vec<CodelineEntry>,
    cursor: usize,
}

impl Default for Website {
    fn default() -> Self {
        Website {
            content: vec![CodelineEntry::default()],
            cursor: 0,
        }
    }
}

impl Website {
    pub fn get_content(&self) -> &Vec<CodelineEntry> {
        &self.content
    }

    pub fn keystroke(&mut self, key: &str) {
        match key {
            "Enter" => self.insert_line_at_cursor(),
            "Backspace" => {
                let line_len = self.content[self.cursor].line.len();
                if line_len > 0 {
                    self.content[self.cursor].line.pop();
                } else {
                    self.remove_line_at_cursor();
                }
            }
            "ArrowDown" => {
                if Self::check(&self.content) {
                    self.cursor = cmp::min(self.cursor + 1, self.content.len() - 1)
                }
            }
            "ArrowUp" => {
                if Self::check(&self.content) {
                    self.cursor = self.cursor.saturating_sub(1);
                }
            }
            _ => {
                if key.chars().count() > 1 {
                    leptos::logging::log!("Unhandled Special Keystroke {}", key);
                    return;
                }
                self.content[self.cursor].line.push_str(key);
            }
        }
    }

    fn insert_line_at_cursor(&mut self) {
        if Self::check(&self.content) {
            self.cursor += 1;
            self.content.insert(self.cursor, CodelineEntry::default());
        }
    }

    fn remove_line_at_cursor(&mut self) {
        if self.cursor == 0 {
            return;
        }

        let mut new_content = self.content.clone();
        new_content.remove(self.cursor);
        if Self::check(&new_content) {
            let _ = std::mem::replace(&mut self.content, new_content);
            self.cursor -= 1;
        }
    }

    fn check(content: &Vec<CodelineEntry>) -> bool {
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
