use crate::line::Position;

// Chrome uses 1 second and VSCode uses 300 - 500 ms, but for comparatively short
// WebAssembly lines, 150 ms felt more natural.
pub const GROUP_INTERVAL_MS: f64 = 150.0;
// 10 minutes - arbitrary
pub const KEEP_DURATION_MS: f64 = 10.0 * 60.0 * 1000.0;

#[derive(Clone)]
pub struct Selection {
    pub start_line: usize,
    pub start_pos: Position,
    pub end_line: usize,
    pub end_pos: Position,
}

#[derive(Clone)]
pub struct Edit {
    pub start_line: usize,
    pub old_lines: Vec<String>,
    // new lines
    pub new_lines: Vec<String>,
    pub selection_before: Selection,
    pub selection_after: Selection,
    pub time_ms: f64,
}

pub struct ActionHistory {
    pub undo_stack: Vec<Edit>,
    pub redo_stack: Vec<Edit>,
    pub last_time_ms: f64,
}

impl Default for ActionHistory {
    fn default() -> Self {
        ActionHistory {
            undo_stack: Vec::new(),
            redo_stack: Vec::new(),
            last_time_ms: 0.0,
        }
    }
}

impl ActionHistory {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn store_edit(&mut self, edit: Edit) {
        self.redo_stack.clear();
        let now_ms = edit.time_ms;
        // Combine edit if close together
        if now_ms - self.last_time_ms <= GROUP_INTERVAL_MS
            && let Some(last_edit) = self.undo_stack.last_mut()
            && last_edit.start_line == edit.start_line
            && last_edit.new_lines.len() == 1
            && edit.new_lines.len() == 1
        {
            last_edit.new_lines = edit.new_lines;
            last_edit.selection_after = edit.selection_after;
            last_edit.time_ms = now_ms;
        } else {
            self.undo_stack.push(edit);
        }
        // Remove old edits
        let num_discard = self
            .undo_stack
            .iter()
            .take_while(|ed| (now_ms - ed.time_ms) > KEEP_DURATION_MS)
            .count();
        if num_discard > 0 {
            self.undo_stack.drain(0..num_discard);
        }
        self.last_time_ms = now_ms;
    }

    pub fn undo(&mut self) -> Option<Edit> {
        if let Some(edit) = self.undo_stack.pop() {
            self.redo_stack.push(edit.clone());
            self.last_time_ms = 0.0;
            Some(edit)
        } else {
            None
        }
    }

    pub fn redo(&mut self) -> Option<Edit> {
        if let Some(edit) = self.redo_stack.pop() {
            self.undo_stack.push(edit.clone());
            self.last_time_ms = 0.0;
            Some(edit)
        } else {
            None
        }
    }
}
