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
    set_selection: RwSignal<Option<LogicSelection>>, // Be used to update selection if Some
}

// The LogicSelection struct describes the logic positon of the selection area
// E.g. after typing a letter of text, the cursor should be advanced to follow the new text.
#[derive(Debug, Clone, Copy)]
pub struct LogicSelection {
    pub anchor: (usize, usize),
    pub focus: (usize, usize),
}

impl LogicSelection {
    pub fn new_cursor(r: usize, c: usize) -> LogicSelection {
        LogicSelection {
            anchor: (r, c),
            focus: (r, c),
        }
    }

    pub fn new(anchor: (usize, usize), focus: (usize, usize)) -> LogicSelection {
        LogicSelection { anchor, focus }
    }

    pub fn is_collapsed(&self) -> bool {
        self.anchor == self.focus
    }

    pub fn to_rationalized_multiline(&self, editor: &Editor) -> LogicSelection {
        let mut new_selection = *self;
        if self.anchor.0 == self.focus.0 {
            return new_selection;
        }
        let (down_curosr, up_cursor) = if self.focus.0 > self.anchor.0 {
            (&mut new_selection.focus, &mut new_selection.anchor)
        } else {
            (&mut new_selection.anchor, &mut new_selection.focus)
        };

        down_curosr.1 = editor.lines.get_untracked()[down_curosr.0]
            .read_untracked()
            .display_char_count();
        up_cursor.1 = 0;

        new_selection
    }

    pub fn apply_enforcement_area(
        &self,
        enforcement_area: ((usize, usize), (usize, usize)),
    ) -> LogicSelection {
        let mut new_selection = *self;
        new_selection.focus = new_selection
            .focus
            .min(enforcement_area.1)
            .max(enforcement_area.0);
        new_selection.anchor = new_selection
            .anchor
            .min(enforcement_area.1)
            .max(enforcement_area.0);
        new_selection
    }
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
                RwSignal::new(EditLine::new(3, String::from("i32.const 1"))),
                RwSignal::new(EditLine::new(5, String::from("i32.const 2"))),
                RwSignal::new(EditLine::new(7, String::from("i32.add"))),
            ]), // demo initial contents
            id_map: HashMap::from([(3, 0), (5, 1), (7, 2)]),
            set_selection: RwSignal::new(None),
        }
    }

    pub fn lines(&self) -> RwSignal<Vec<RwSignal<EditLine>>> {
        self.lines
    }

    pub fn set_selection(&self) -> &RwSignal<Option<LogicSelection>> {
        &self.set_selection
    }

    pub fn malformed_line_no(&self) -> Option<usize> {
        for (idx, line) in self.lines().read().iter().enumerate() {
            if line.read().instr().is_err() {
                return Some(idx);
            }
        }

        None
    }

    pub fn malformed_line_id(&self) -> Option<usize> {
        self.malformed_line_no()
            .map(|line_no| *self.lines.read()[line_no].read().id())
    }

    // The logic selection is defined as:
    // if self.set_selection is Some:
    //     self.set_selection
    // else:
    //     area after restricting dom selection into the text editor
    pub fn get_current_logic_selection(&self) -> Option<LogicSelection> {
        if let Some(logic_selection) = self.set_selection().get() {
            return Some(logic_selection);
        }
        let dom_selection = get_current_domselection();
        let anchor = dom_selection.anchor_node()?;
        let focus = dom_selection.focus_node()?;

        let anchor_id = find_id_from_node(&anchor);
        let focus_id = find_id_from_node(&focus);

        match (anchor_id, focus_id) {
            (Some(anchor_id), Some(focus_id)) => {
                let anchor_idx: usize = *self.id_map.get(&anchor_id).expect("can't find line");
                let focus_idx: usize = *self.id_map.get(&focus_id).expect("can't find line");
                Some(LogicSelection::new(
                    (anchor_idx, dom_selection.anchor_offset() as usize),
                    (focus_idx, dom_selection.focus_offset() as usize),
                ))
            }
            (Some(anchor_id), None) => {
                let anchor_idx: usize = *self.id_map.get(&anchor_id).expect("can't find line");
                Some(LogicSelection::new(
                    (anchor_idx, dom_selection.anchor_offset() as usize),
                    (
                        self.lines().read_untracked().len() - 1,
                        self.lines()
                            .read_untracked()
                            .last()
                            .expect("Empty lines")
                            .read_untracked()
                            .logical_text()
                            .len(),
                    ),
                ))
            }
            (None, Some(focus_id)) => {
                let focus_idx: usize = *self.id_map.get(&focus_id).expect("can't find line");
                Some(LogicSelection::new(
                    (
                        0,
                        self.lines()
                            .read_untracked()
                            .first()
                            .expect("Empty lines")
                            .read_untracked()
                            .logical_text()
                            .len(),
                    ),
                    (focus_idx, dom_selection.focus_offset() as usize),
                ))
            }
            (None, None) => None,
        }
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

        self.set_selection()
            .set(Some(LogicSelection::new_cursor(line_no + 1, 0)));
    }

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

        if start_id == end_id
            && let Some(id) = start_id
        {
            // If the InputEvent requires changing lines, handle the event at the editor-level
            match ev.input_type().as_str() {
                "insertParagraph" | "insertLineBreak" => {
                    let line_no = self.id_map.get(&id).expect("can't find line");
                    let the_line = self.lines.read()[*line_no];
                    let (start_pos, _) = the_line.write().preprocess_input(&ev);
                    self.insert_line(*line_no, start_pos);
                }
                _ => {
                    let line_no = self.id_map.get(&id).expect("can't find line");
                    let new_cursor_pos = self.lines.write()[*line_no].write().handle_input(&ev);
                    self.set_selection()
                        .set(Some(LogicSelection::new_cursor(*line_no, new_cursor_pos)));
                }
            }
        } else {
            leptos_dom::log!("unhandled: multiline select input");
        }

        self.rationalize_selection();
    }

    /// (1) Inside code area
    /// (2) According to enforcement rule
    /// ### Returns
    /// The begin position and ending position of the area.
    pub fn get_enforcement_area(&self) -> ((usize, usize), (usize, usize)) {
        if let Some(idx) = self.malformed_line_no() {
            (
                (idx, 0),
                (idx, self.lines().read()[idx].read().display_char_count()),
            )
        } else {
            (
                (0, 0),
                (
                    self.lines().read().len() - 1,
                    self.lines
                        .read()
                        .last()
                        .expect("Empty Lines")
                        .read()
                        .display_char_count(),
                ),
            )
        }
    }

    // Special-case the handling of some arrow keys:
    // 1. Cancel an ArrowDown in the last line (to prevent the cursor from leaving the editor window)
    // 2. Make sure to skip over a cosmetic space in a logically empty line.
    // 3. Frozen according to our enforcement rule
    pub fn handle_arrow(&mut self, ev: KeyboardEvent) {
        match ev.key().as_str() {
            "ArrowUp" => {
                if let Some(selection) = self.get_current_logic_selection()
                    && selection.is_collapsed()
                    && let Some(malformed_line_id) = self.malformed_line_id()
                    && malformed_line_id
                        == *self.lines().read_untracked()[selection.focus.0]
                            .read_untracked()
                            .id()
                {
                    ev.prevent_default();
                }
            }

            "ArrowDown" => {
                if let Some(selection) = self.get_current_logic_selection()
                    && selection.is_collapsed()
                {
                    if let Some(malformed_line_id) = self.malformed_line_id()
                        && malformed_line_id
                            == *self.lines().read_untracked()[selection.focus.0]
                                .read_untracked()
                                .id()
                    {
                        ev.prevent_default();
                    } else if selection.focus.0 == self.lines.read_untracked().len() - 1 {
                        ev.prevent_default();
                    }
                }
            }
            "ArrowLeft" => {
                if let Some(selection) = self.get_current_logic_selection()
                    && selection.is_collapsed()
                {
                    if selection.focus.1 == 0
                        && let Some(malformed_line_id) = self.malformed_line_id()
                        && malformed_line_id
                            == *self.lines().read_untracked()[selection.focus.0]
                                .read_untracked()
                                .id()
                    {
                        ev.prevent_default();
                    } else if selection.focus.0 > 0
                        && self.lines.read_untracked()[selection.focus.0]
                            .read_untracked()
                            .logical_text()
                            .is_empty()
                    {
                        ev.prevent_default();
                        self.set_selection().set(Some(LogicSelection::new_cursor(
                            selection.focus.0 - 1,
                            self.lines.read()[selection.focus.0 - 1]
                                .read()
                                .display_char_count(),
                        )));
                    }
                }
            }
            "ArrowRight" => {
                if let Some(selection) = self.get_current_logic_selection()
                    && selection.is_collapsed()
                {
                    let focus_line =
                        self.lines.read_untracked()[selection.focus.0].read_untracked();
                    if selection.focus.1 == focus_line.display_char_count()
                        && let Some(malformed_line_id) = self.malformed_line_id()
                        && malformed_line_id == *focus_line.id()
                    {
                        ev.prevent_default();
                    } else if selection.focus.0 < self.lines.read_untracked().len() - 1
                        && focus_line.logical_text().is_empty()
                    {
                        ev.prevent_default();
                        self.set_selection
                            .set(Some(LogicSelection::new_cursor(selection.focus.0 + 1, 0)));
                    }
                }
            }
            _ => (),
        }
    }

    // "Rationalize" the selection to (1) make multi-line selections include the entire lines, and
    // (2) correct situations where the selection starts or ends outside the editor area.
    // (3) restrict to enforcement area
    pub fn rationalize_selection(&mut self) {
        self.set_selection()
            .set(self.get_current_logic_selection().map(|selection| {
                selection
                    .to_rationalized_multiline(self)
                    .apply_enforcement_area(self.get_enforcement_area())
            }));
    }

    // Execute a SetSelection to transfer Codillion's idea of the current selection / cursor position
    // to the browser. We want Leptos to run this (and update the selection) *after* updating
    // line contents, which will warp the cursor back to the beginning of the line.
    pub fn update_selection(&mut self) {
        let Some(logic_selection) = self.set_selection().get_untracked() else {
            return;
        };
        let anchor_line = &self.lines().read_untracked()[logic_selection.anchor.0].read_untracked();
        let anchor_node = anchor_line.text_node();
        let focus_line = &self.lines().read_untracked()[logic_selection.focus.0].read_untracked();
        let focus_node = focus_line.text_node();

        get_current_domselection()
            .set_base_and_extent(
                &anchor_node,
                logic_selection.anchor.1 as u32,
                &focus_node,
                logic_selection.focus.1 as u32,
            )
            .expect("set multiline selection");

        self.set_selection().write_untracked().take();
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
