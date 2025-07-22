use crate::{line::*, view::*};
use leptos::{prelude::*, *};
use std::collections::{HashMap, HashSet};
use web_sys::{InputEvent, KeyboardEvent, wasm_bindgen::JsCast};

// The "Editor" holds a vector of EditLines, as well as bookkeeping information related to
// creating new lines and finding lines given their unique long-lived ID.
pub struct Editor {
    next_id: usize,
    id_map: HashMap<usize, usize>,
    lines: RwSignal<Vec<RwSignal<EditLine>>>,
    selection: RwSignal<Option<SetSelection>>,
}

// The SetSelection struct describes changes to be made to the browser's global selection.
// E.g. after typing a letter of text, the cursor should be advanced to follow the new text.
#[derive(Clone, Copy)]
pub enum SetSelection {
    Cursor(usize, usize),    // line index, pos
    MultiLine(usize, usize), // anchor line number, focus line number
}

impl Default for Editor {
    fn default() -> Self {
        Self::new()
    }
}

impl Editor {
    pub fn new() -> Self {
        Self {
            next_id: 8,
            lines: RwSignal::new(vec![
                RwSignal::new(EditLine::new(3, String::from("Hello, world"))),
                RwSignal::new(EditLine::new(5, String::from("Hello, world"))),
                RwSignal::new(EditLine::new(7, String::from("Hello, world"))),
            ]), // demo initial contents
            id_map: HashMap::from([(3, 0), (5, 1), (7, 2)]),
            selection: RwSignal::new(None),
        }
    }

    pub fn lines(&self) -> RwSignal<Vec<RwSignal<EditLine>>> {
        self.lines
    }

    pub fn selection(&self) -> RwSignal<Option<SetSelection>> {
        self.selection
    }

    fn insert_line(&mut self, line_no: usize, start_pos: usize) {
        // Re-map lines that will be affected by this change.
        for val in self.id_map.values_mut() {
            if *val > line_no {
                *val += 1;
            }
        }
        // Update the position map.
        self.id_map.insert(self.next_id, line_no + 1);

        let remainder = self.lines.write()[line_no].write().split_self(start_pos);

        let new_line = RwSignal::new(EditLine::new(self.next_id, remainder));

        // Add the new line below the current line
        if line_no == self.lines.read().len() - 1 {
            self.lines.write().push(new_line);
        } else {
            self.lines.write().insert(line_no + 1, new_line);
        }

        // Verify the integrity of the id map.
        debug_assert!(self.id_map_is_valid());

        self.next_id += 1;

        self.selection
            .set(Some(SetSelection::Cursor(line_no + 1, 0)));
    }

    // Deletes lines on the range (start_id -> end_id]
    // The cursor will remain on the line denoted by start_id
    // at position 0 (with text cleared).
    fn delete_selected_lines(&mut self, start_line: usize, end_line: usize) {
        debug_assert!(start_line < end_line);

        // Find all ID's to delete.
        let mut ids_to_remove: HashSet<usize> = HashSet::new();
        for k in self.id_map.keys() {
            let current_line = self.id_map[k];
            if current_line > start_line && current_line <= end_line {
                ids_to_remove.insert(*k);
            }
        }

        // Re-map lines that will be affected by this change.
        for val in self.id_map.values_mut() {
            if *val > end_line {
                *val -= end_line - start_line;
            }
        }

        for id in ids_to_remove {
            self.id_map.remove(&id);
        }

        for _ in 0..end_line - start_line {
            self.lines.write().remove(start_line + 1);
        }

        // Verify the integrity of the id map.
        debug_assert!(self.id_map_is_valid());

        // Clear current text.
        self.lines().write()[start_line].write().clear_line();

        self.selection
            .set(Some(SetSelection::Cursor(start_line, 0)));
    }

    // Deletes the current line, moving any orphaned text to the previous line.
    // Can only be called if there is a previous line.
    fn delete_line_backward(&mut self, current_line_id: usize) {
        let current_line_no = self.id_map[&current_line_id];
        debug_assert!(current_line_no > 0);
        let the_current_line = self.lines().read()[current_line_no].read();
        let current_line_text = the_current_line.logical_text();
        let previous_line_text_len = self.lines().read()[current_line_no - 1]
            .read()
            .logical_text()
            .len();
        self.id_map.remove(&current_line_id);

        // Re-map lines that will be affected by this change.
        for val in self.id_map.values_mut() {
            if *val > current_line_no {
                *val -= 1;
            }
        }

        self.lines.write().remove(current_line_no);
        debug_assert!(self.id_map_is_valid());

        self.lines().write()[current_line_no - 1]
            .write()
            .append_text(current_line_text.to_string());

        self.selection.set(Some(SetSelection::Cursor(
            current_line_no - 1,
            previous_line_text_len,
        )));
    }

    // Deletes the next line, appending any orphaned text to the current line.
    // Can only be called if there is a next line.
    fn delete_line_forward(&mut self, current_line_id: usize) {
        let current_line_no = self.id_map[&current_line_id];
        debug_assert!(current_line_no < self.lines().read().len() - 1);
        let the_next_line = self.lines().read()[current_line_no + 1].read();
        let next_line_text = the_next_line.logical_text();
        let current_line_text_len = self.lines().read()[current_line_no]
            .read()
            .logical_text()
            .len();
        let previous_line_entry = self
            .id_map
            .iter()
            .find(|entry| *entry.1 == current_line_no + 1);

        if let Some(entry) = previous_line_entry {
            let id = *entry.0;
            let line_no = *entry.1;
            self.id_map.remove(&id);

            // Re-map lines that will be affected by this change.
            for val in self.id_map.values_mut() {
                if *val > current_line_no + 1 {
                    *val -= 1;
                }
            }

            self.lines.write().remove(line_no);
            debug_assert!(self.id_map_is_valid());

            self.lines().write()[current_line_no]
                .write()
                .append_text(next_line_text.to_string());

            self.selection.set(Some(SetSelection::Cursor(
                current_line_no,
                current_line_text_len,
            )));
        }
    }

    // Handle an input to the Editor window, by dispatching to the appropriate EditLine.
    pub fn handle_input(&mut self, ev: InputEvent) {
        ev.prevent_default();

        let target_range = ev
            .get_target_ranges()
            .get(0)
            .clone()
            .unchecked_into::<web_sys::Range>();
        let start_id = find_id_from_node(&target_range.start_container().expect("container"));
        let end_id = find_id_from_node(&target_range.end_container().expect("container"));

        // When the InputEvent is a deletion on a line boundary, it *sometimes* contains
        // the ID of the line that is the target of the operation (as either the start or end ID).
        // The cosmetic space prevents this behavior from being reliable, so we can get a reliable
        // read of the current line from the current selection.
        let selected_line_id = if let Some(selection_node) = get_current_selection().focus_node() {
            find_id_from_node(&selection_node)
        } else {
            None
        };

        if let Some(start_id) = start_id
            && let Some(end_id) = end_id
            && let Some(line_id) = selected_line_id
        {
            let is_collapsed = get_current_selection().is_collapsed();
            let start_line_no: &usize = self.id_map.get(&start_id).expect("can't find line");
            let end_line_no: &usize = self.id_map.get(&end_id).expect("can't find line");

            // First check if there's an active selection that spans different lines.
            if start_line_no != end_line_no && !is_collapsed {
                // Delete all lines associated with the selections.
                // TODO: Re-run the desired operation.
                self.delete_selected_lines(*start_line_no, *end_line_no);
                return;
            }

            match ev.input_type().as_str() {
                "insertParagraph" | "insertLineBreak" => {
                    let line_no: &usize = self.id_map.get(&start_id).expect("can't find line");
                    let the_line = self.lines.read()[*line_no];
                    let (start_pos, _) = the_line.write().get_inline_range(&ev);
                    self.insert_line(*line_no, start_pos);
                }
                "deleteContentBackward" => {
                    let line_no = self.id_map.get(&line_id).expect("can't find line");
                    let the_line = self.lines.read()[*line_no];

                    let pos = the_line.write().get_inline_range_end(&ev);
                    // Pos = 0 ignores the cosmetic space
                    if pos == 0 && *line_no > 0 {
                        self.delete_line_backward(line_id);
                    } else {
                        // Let default delete behavior pass if we're deleting a single-line selection.
                        let new_cursor_pos = self.lines.write()[*line_no].write().handle_input(&ev);
                        self.selection
                            .set(Some(SetSelection::Cursor(*line_no, new_cursor_pos)));
                    }
                }
                "deleteContentForward" => {
                    let line_no = self.id_map.get(&line_id).expect("can't find line");
                    let the_line = self.lines.read()[*line_no];
                    let pos = the_line.write().get_inline_range_start(&ev);

                    if (the_line.read().logical_text().is_empty()
                        || pos == the_line.read().logical_text().chars().count())
                        && *line_no < self.lines.read().len() - 1
                    {
                        self.delete_line_forward(line_id);
                    } else {
                        let new_cursor_pos = self.lines.write()[*line_no].write().handle_input(&ev);
                        self.selection
                            .set(Some(SetSelection::Cursor(*line_no, new_cursor_pos)));
                    }
                }
                _ => {
                    let line_no = self.id_map.get(&start_id).expect("can't find line");
                    let new_cursor_pos = self.lines.write()[*line_no].write().handle_input(&ev);
                    self.selection
                        .set(Some(SetSelection::Cursor(*line_no, new_cursor_pos)));
                }
            }
        }

        self.rationalize_selection();
    }

    // Special-case the handling of some arrow keys:
    // 1. Cancel an ArrowDown in the last line (to prevent the cursor from leaving the editor window)
    // 2. Make sure to skip over a cosmetic space in a logically empty line.
    pub fn handle_arrow(&mut self, ev: KeyboardEvent) {
        match ev.key().as_str() {
            "ArrowDown" => {
                let selection = get_current_selection();
                if selection.is_collapsed()
                    && let Some(focus) = selection.focus_node()
                    && let Some(id) = find_id_from_node(&focus)
                    && id
                        == *self
                            .lines
                            .read_untracked()
                            .last()
                            .expect("last line")
                            .read()
                            .id()
                {
                    ev.prevent_default();
                }
            }
            "ArrowLeft" => {
                let selection = get_current_selection();
                if selection.is_collapsed()
                    && let Some(focus) = selection.focus_node()
                    && let Some(id) = find_id_from_node(&focus)
                    && let idx = self.id_map.get(&id).expect("line from id")
                    && *idx > 0
                    && self.lines.read()[*idx].read().logical_text().is_empty()
                {
                    ev.prevent_default();
                    self.selection.set(Some(SetSelection::Cursor(
                        *idx - 1,
                        self.lines.read()[*idx - 1]
                            .read()
                            .display_text()
                            .chars()
                            .count(),
                    )));
                }
            }
            "ArrowRight" => {
                let selection = get_current_selection();
                if selection.is_collapsed()
                    && let Some(focus) = selection.focus_node()
                    && let Some(id) = find_id_from_node(&focus)
                    && let idx = self.id_map.get(&id).expect("line from id")
                    && *idx < self.lines.read().len() - 1
                    && self.lines.read()[*idx].read().logical_text().is_empty()
                {
                    ev.prevent_default();
                    self.selection.set(Some(SetSelection::Cursor(*idx + 1, 0)));
                }
            }
            _ => (),
        }
    }

    // "Rationalize" the selection to (1) make multi-line selections include the entire lines, and
    // (2) correct situations where the selection starts or ends outside the editor area.
    pub fn rationalize_selection(&mut self) {
        let selection = get_current_selection();
        let Some(anchor) = selection.anchor_node() else {
            return;
        };
        let focus = selection.focus_node().expect("focus");

        let anchor_id = find_id_from_node(&anchor);
        let focus_id = find_id_from_node(&focus);

        match (anchor_id, focus_id) {
            (Some(anchor_id), Some(focus_id)) => {
                let anchor_offset = selection.anchor_offset() as usize;
                self.handle_in_bounds_selection(anchor_id, anchor_offset, focus_id);
            }
            _ => self.handle_out_of_bounds_selection(&selection),
        }
    }

    // If the selection includes areas outside the editor window, rationalize it
    // to fit inside the window.
    fn handle_out_of_bounds_selection(&mut self, selection: &DomSelection) {
        if selection.is_collapsed() {
            if selection.focus_offset() == 0 {
                self.selection.set(Some(SetSelection::Cursor(0, 0)));
            } else {
                self.selection.set(Some(SetSelection::Cursor(
                    self.lines.read_untracked().len() - 1,
                    self.lines
                        .read()
                        .last()
                        .expect("last line")
                        .read()
                        .display_text()
                        .chars()
                        .count(),
                )));
            }
            return;
        }

        let anchor_id = find_id_from_node(&selection.anchor_node().expect("anchor"));
        let focus_id = find_id_from_node(&selection.focus_node().expect("focus"));

        match (anchor_id, focus_id) {
            (Some(anchor_id), None) => {
                let anchor_offset = selection.anchor_offset() as usize;
                self.handle_in_bounds_selection(
                    anchor_id,
                    anchor_offset,
                    *self
                        .lines
                        .read_untracked()
                        .last()
                        .expect("last line")
                        .read()
                        .id(),
                );
            }
            (None, Some(focus_id)) => {
                self.handle_in_bounds_selection(
                    *self
                        .lines
                        .read_untracked()
                        .first()
                        .expect("first line")
                        .read()
                        .id(),
                    0,
                    focus_id,
                );
            }
            (None, None) => {
                if selection.anchor_offset() > selection.focus_offset() {
                    self.selection.set(Some(SetSelection::MultiLine(
                        self.lines.read_untracked().len() - 1,
                        0,
                    )));
                } else {
                    self.selection.set(Some(SetSelection::MultiLine(
                        0,
                        self.lines.read_untracked().len() - 1,
                    )));
                }
            }
            _ => {
                unreachable!();
            }
        }
    }

    // If the selection is fully in-bounds, rationalize it by expanding multi-line selections
    // to include the entire lines in question.
    fn handle_in_bounds_selection(
        &mut self,
        anchor_id: usize,
        anchor_offset: usize,
        focus_id: usize,
    ) {
        if anchor_id == focus_id {
            return; // single-line selection, no action needed
        }
        // Apparent multiline selection, but is this really a single-line or n-1 line selection?
        let mut anchor_idx: usize = *self.id_map.get(&anchor_id).expect("can't find line");
        let focus_idx: usize = *self.id_map.get(&focus_id).expect("can't find line");
        let anchor_line = &self.lines.read()[anchor_idx].read();

        // Is the anchor at the very end of a line (for a forward selection),
        // or the very beginning of a line (for a backward selection)?
        if anchor_idx < focus_idx && anchor_offset == anchor_line.display_text().chars().count() {
            anchor_idx += 1;
        } else if focus_idx < anchor_idx && anchor_offset == 0 {
            anchor_idx -= 1;
        }

        if anchor_idx == focus_idx {
            // It was really a single-line selection that overlapped
            // the end-of-previous or beginning-of-next line. Do nothing.
        } else {
            // It's a real multi-line selection. Expand to the full lines on next tick.
            self.selection
                .set(Some(SetSelection::MultiLine(anchor_idx, focus_idx)));
        }
    }

    // Execute a SetSelection to transfer Codillion's idea of the current selection / cursor position
    // to the browser. We want Leptos to run this (and update the selection) *after* updating
    // line contents, which will warp the cursor back to the beginning of the line.
    pub fn update_selection(&self) {
        match self.selection.get_untracked() {
            Some(SetSelection::Cursor(line_no, pos)) => {
                let the_line = &self.lines.read_untracked()[line_no].read_untracked();
                let text_node = the_line.text_node();
                get_current_selection()
                    .set_base_and_extent(&text_node, pos as u32, &text_node, pos as u32)
                    .expect("set selection base and extent");
            }
            Some(SetSelection::MultiLine(anchor_idx, focus_idx)) => {
                let anchor_line = &self.lines.read_untracked()[anchor_idx].read_untracked();
                let anchor_node = anchor_line.text_node();
                let focus_line = &self.lines.read_untracked()[focus_idx].read_untracked();
                let focus_node = focus_line.text_node();

                let anchor_offset = if anchor_idx > focus_idx {
                    anchor_line.display_char_count() as u32
                } else {
                    0
                };

                let focus_offset = if anchor_idx > focus_idx {
                    0
                } else {
                    focus_line.display_char_count() as u32
                };

                get_current_selection()
                    .set_base_and_extent(&anchor_node, anchor_offset, &focus_node, focus_offset)
                    .expect("set multiline selection");
            }
            None => (),
        }

        self.selection.write_untracked().take();
    }

    // Verify that the ID map:
    // Has 1 entry per editor line.
    // Has entries that map to extant, unique lines.
    fn id_map_is_valid(&self) -> bool {
        if self.id_map.len() != self.lines.read_untracked().len() {
            return false;
        }

        // Verify map values (line numbers) are unique and within bounds.
        // Also verify that IDs in map match the line's ID.
        let mut line_numbers: HashSet<usize> = HashSet::new();
        for id in self.id_map.keys() {
            let val = self.id_map[id];
            if !line_numbers.insert(val) || val >= self.lines.read_untracked().len() {
                return false;
            }
            if id != self.lines.read_untracked()[val].read_untracked().id() {
                return false;
            }
        }

        // Verify that lines have unique IDs
        let mut ids = HashSet::new();
        for line in self.lines.get() {
            if !ids.insert(*line.read().id()) {
                return false;
            }
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::{Ok, Result};

    #[test]
    fn test_insert_lines() -> Result<()> {
        let mut editor: Editor = Editor::new();
        // Insert sequential lines.
        editor.insert_line(0, 0);
        editor.insert_line(1, 0);
        editor.insert_line(2, 0);

        // Insert lines in between existing lines
        editor.insert_line(2, 0);
        editor.insert_line(1, 0);

        // Insert lines at idx 0
        editor.insert_line(0, 0);
        editor.insert_line(0, 0);
        Ok(())
    }

    #[test]
    fn test_delete_selected_lines() -> Result<()> {
        let mut editor: Editor = Editor::new();
        // Try deleting the existing lines.
        editor.delete_selected_lines(0, 2);

        // Delete last line only.
        editor.insert_line(0, 0);
        editor.insert_line(0, 0);
        editor.insert_line(0, 0);
        editor.insert_line(0, 0);
        editor.delete_selected_lines(3, 4);

        assert_eq!(editor.lines().read().len(), 4);

        // Delete remaining lines.
        editor.delete_selected_lines(0, 3);
        assert_eq!(editor.lines().read().len(), 1);

        Ok(())
    }

    #[test]
    fn test_delete_forward() -> Result<()> {
        // NB that the noargs constructor creates 3 populated lines
        // with next_id = 8.
        let mut editor: Editor = Editor::new();

        // Delete the latter two lines.
        editor.delete_line_forward(3);
        editor.delete_line_forward(3);
        assert_eq!(editor.lines().read().len(), 1);

        // Add a line and then delete it.
        editor.insert_line(0, 0);
        editor.delete_line_forward(3);
        assert_eq!(editor.lines().read().len(), 1);

        Ok(())
    }

    #[test]
    fn test_delete_backward() -> Result<()> {
        // NB that the noargs constructor creates 3 populated lines
        // with next_id = 8.
        let mut editor: Editor = Editor::new();

        // Delete the latter two lines.
        editor.delete_line_backward(7);
        editor.delete_line_backward(5);
        assert_eq!(editor.lines().read().len(), 1);

        // Add a line and then delete it.
        editor.insert_line(0, 0);
        editor.delete_line_backward(8);
        assert_eq!(editor.lines().read().len(), 1);

        Ok(())
    }
}
