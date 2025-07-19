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

    // deletes content on lines in the inclusive range (start_id -> end_id]
    // Only deletes content on the start line.
    // Places the cursor on the start of the new (empty) line from start_id
    // start and end must be different.

    // How do we reconcile this with forwards and backwards delete of a single line
    fn delete_selected_lines(&mut self, start_id: &usize, end_id: &usize) {
        debug_assert!(*start_id != *end_id);
    }

    // Handles fwd or backwards single line delete (meaning we're at the end / beginning of a line)
    fn delete_single_line() {}

    // Handle an input to the Editor window, by dispatching to the appropriate EditLine.
    // TODO: handle line deletion
    pub fn handle_input(&mut self, ev: InputEvent) {
        ev.prevent_default();

        let target_range = ev
            .get_target_ranges()
            .get(0)
            .clone()
            .unchecked_into::<web_sys::Range>();
        let start_id = find_id_from_node(&target_range.start_container().expect("container"));
        let end_id = find_id_from_node(&target_range.end_container().expect("container"));

        leptos_dom::log!("startID: {}, endID: {}", start_id.unwrap(), end_id.unwrap());
        leptos_dom::log!("is collapsed: {}", get_current_selection().is_collapsed());

        if let Some(start_id) = start_id
            && let Some(end_id) = end_id
        {
            // Match inputs first, then care about whether it's multiline or not.
            let is_collapsed = get_current_selection().is_collapsed();

            // First check if there's an active selection that spans different lines
            if start_id != end_id && !is_collapsed {
                // Delete all lines associated with the selections and re-run the op
                // When re-rationalizing, you need to make sure you set the cursor such that
                // the input range is on the same line!
                self.delete_selected_lines(&start_id, &end_id);
                // Let the event continue? I guess we just let it continue? Unless it's a delete.
            }
            // If the ranges are different but there's no selection, we can just go on with normal matching

            // Current bug: you can't call preprocess if the ranges aren't the same.
            // This only happens on deletes, so you can be a bit flexible here.

            match ev.input_type().as_str() {
                "insertParagraph" | "insertLineBreak" => {
                    let line_no = self.id_map.get(&start_id).expect("can't find line");
                    let the_line = self.lines.read()[*line_no];
                    let (start_pos, _) = the_line.write().get_inline_range(&ev);
                    self.insert_line(*line_no, start_pos);
                }
                "deleteContentBackward" => {
                    let line_no = self.id_map.get(&start_id).expect("can't find line");
                    if is_collapsed {
                        leptos_dom::log!("executing normal delete");
                        // Let default delete behavior pass if we're deleting a single-line selection.
                        let new_cursor_pos = self.lines.write()[*line_no].write().handle_input(&ev);

                        // Normal delete doesn't work because preprocess input is cooked. Makes sense.

                        self.selection
                            .set(Some(SetSelection::Cursor(*line_no, new_cursor_pos)));
                        self.rationalize_selection();
                        return;
                    }
                    let the_line = self.lines.read()[*line_no];
                    // Why are you calling bogus preprocess here?
                    // Because teh ranges aren't the same!. We actually know they are, so cope.
                    let (pos, _) = the_line.write().get_inline_range(&ev);
                    //leptos_dom::log!("startPos: {}, endPos: {}", start_pos, end_pos);
                    // Nice, start pos and endpos appear not to GAF WRT the cosmetic space.
                    if pos == 0 && *line_no > 0 {
                        // Delete current line, combine content with previous line (if there is a previous line)
                        leptos_dom::log!(
                            "This is the case where we would execute deleteContentBackwards line"
                        )
                    }
                }
                "deleteContentForward" => {
                    let line_no = self.id_map.get(&start_id).expect("can't find line");
                    if is_collapsed {
                        // Let default delete behavior pass if we're deleting a single-line selection.
                        let new_cursor_pos = self.lines.write()[*line_no].write().handle_input(&ev);
                        self.selection
                            .set(Some(SetSelection::Cursor(*line_no, new_cursor_pos)));
                        self.rationalize_selection();
                        return;
                    }
                    let the_line = self.lines.read()[*line_no];
                    let (pos, _) = the_line.write().get_inline_range(&ev);
                    //leptos_dom::log!("startPos: {}, endPos: {}", start_pos, end_pos);
                    // Nice, start pos and endpos appear not to GAF WRT the cosmetic space.
                    // Error on reading the line or smt.
                    // oh subtract
                    if (the_line.read().logical_text().is_empty()
                        || pos == the_line.read().logical_text().chars().count() - 1)
                        && *line_no < self.lines.read().len() - 1
                    {
                        // Delete the line below us, if there is one
                        leptos_dom::log!(
                            "This is the case where we would execute deleteContentForward line"
                        )
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

        // if start_id == end_id
        //     && let Some(id) = start_id
        // {
        //     // If the InputEvent requires changing lines, handle the event at the editor-level
        //     match ev.input_type().as_str() {
        //         "insertParagraph" | "insertLineBreak" => {
        //             let line_no = self.id_map.get(&id).expect("can't find line");
        //             let the_line = self.lines.read()[*line_no];
        //             let (start_pos, _) = the_line.write().preprocess_input(&ev);
        //             self.insert_line(*line_no, start_pos);
        //         }
        //         "deleteContentBackward" | "deleteContentForward" => {
        //             // Cursor won't tell you whether you're at the end of the line (or at the start)
        //         }
        //         _ => {
        //             let line_no = self.id_map.get(&id).expect("can't find line");
        //             let new_cursor_pos = self.lines.write()[*line_no].write().handle_input(&ev);
        //             self.selection
        //                 .set(Some(SetSelection::Cursor(*line_no, new_cursor_pos)));
        //         }
        //     }
        // } else {
        //     // It could be a normal delete, or it could be an operator on multiline.
        //     // You need to figure out whether it's collapsed or not. Not collapsed -> delete content too (unless it's just between lines)
        // }

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
        let mut line_numbers = HashSet::new();
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
        let mut editor = Editor::new();
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
}
