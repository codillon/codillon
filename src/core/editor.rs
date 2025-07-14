use crate::core::{EditLine, SetSelection};
use crate::view::dom::*;
use leptos::prelude::*;
use leptos::*;
use reactive_stores::Store;
use reactive_stores::StoreFieldIterator;
use std::collections::HashMap;

// The "Editor" holds a vector of EditLines, as well as bookkeeping information related to
// creating new lines and finding lines given their unique long-lived ID.
pub struct Editor {
    //    next_id: usize, // TODO: will be used when insertParagraph creates a new line
    id_map: HashMap<usize, usize>,
    lines: Store<CodeLines>,
    selection: RwSignal<Option<SetSelection>>,
}

// The individual lines of code.
#[derive(Store)]
pub struct CodeLines {
    #[store(key: usize = |line| *line.id())]
    lines: Vec<EditLine>,
}

impl Default for Editor {
    fn default() -> Self {
        Self::new()
    }
}

impl Editor {
    pub fn new() -> Self {
        Self {
            //            next_id: 6,
            lines: Store::new(CodeLines {
                lines: vec![EditLine::new(3), EditLine::new(5), EditLine::new(7)], // demo initial contents
            }),
            id_map: HashMap::from([(3, 0), (5, 1), (7, 2)]),
            selection: RwSignal::new(None),
        }
    }

    pub fn lines(&self) -> &Store<CodeLines> {
        &self.lines
    }

    pub fn selection(&self) -> RwSignal<Option<SetSelection>> {
        self.selection
    }

    // Handle an input to the Editor window, by dispatching to the appropriate EditLine.
    // TODO: handle insertParagraph and insertLineBreak (add a line)
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
            let line_no = self.id_map.get(&id).expect("can't find line");
            let new_cursor_pos = self
                .lines
                .lines()
                .at_unkeyed(*line_no)
                .write()
                .handle_input(ev);
            self.selection
                .set(Some(SetSelection::Cursor(*line_no, new_cursor_pos)));
        } else {
            leptos_dom::log!("unhandled: multiline select input");
        }

        self.rationalize_selection();
    }

    // Cancel an ArrowDown in the last line (to prevent the cursor from leaving the editor window)
    pub fn maybe_cancel(&self, ev: KeyboardEvent) {
        if ev.key() != "ArrowDown" {
            return;
        }
        let selection = get_current_selection();
        if selection.is_collapsed()
            && let Some(focus) = selection.focus_node()
            && let Some(id) = find_id_from_node(&focus)
            && id
                == *self
                    .lines
                    .lines()
                    .read_untracked()
                    .last()
                    .expect("last line")
                    .id()
        {
            ev.prevent_default();
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
                    self.lines.lines().read_untracked().len() - 1,
                    self.lines
                        .lines()
                        .read_untracked()
                        .last()
                        .expect("last line")
                        .text()
                        .len(),
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
                        .lines()
                        .read_untracked()
                        .last()
                        .expect("last line")
                        .id(),
                );
            }
            (None, Some(focus_id)) => {
                self.handle_in_bounds_selection(
                    *self
                        .lines
                        .lines()
                        .read_untracked()
                        .first()
                        .expect("first line")
                        .id(),
                    0,
                    focus_id,
                );
            }
            (None, None) => {
                if selection.anchor_offset() > selection.focus_offset() {
                    self.selection.set(Some(SetSelection::MultiLine(
                        self.lines.lines().read_untracked().len() - 1,
                        0,
                    )));
                } else {
                    self.selection.set(Some(SetSelection::MultiLine(
                        0,
                        self.lines.lines().read_untracked().len() - 1,
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
        let anchor_line = &self.lines.lines().read_untracked()[anchor_idx];

        // Is the anchor at the very end of a line (for a forward selection),
        // or the very beginning of a line (for a backward selection)?
        if anchor_idx < focus_idx && anchor_offset == anchor_line.text().len() {
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
                let the_line = &self.lines.lines().read_untracked()[line_no];
                let text_node = the_line.text_node();
                get_current_selection()
                    .set_base_and_extent(&text_node, pos as u32, &text_node, pos as u32)
                    .expect("set selection base and extent");
            }
            Some(SetSelection::MultiLine(anchor_idx, focus_idx)) => {
                let anchor_line = &self.lines.lines().read_untracked()[anchor_idx];
                let anchor_node = anchor_line.text_node();
                let focus_line = &self.lines.lines().read_untracked()[focus_idx];
                let focus_node = focus_line.text_node();

                let anchor_offset = if anchor_idx > focus_idx {
                    anchor_line.text().len() as u32
                } else {
                    0
                };

                let focus_offset = if anchor_idx > focus_idx {
                    0
                } else {
                    focus_line.text().len() as u32
                };

                get_current_selection()
                    .set_base_and_extent(&anchor_node, anchor_offset, &focus_node, focus_offset)
                    .expect("set multiline selection");
            }
            None => (),
        }

        self.selection.write_untracked().take();
    }
}
