pub mod utils;

use leptos::prelude::*;
use leptos::*;
use reactive_stores::Store;
use reactive_stores::StoreFieldIterator;
use std::collections::HashMap;
use web_sys::wasm_bindgen::JsCast;
use web_sys::*;

// The "Editor" holds a vector of EditLines, as well as bookkeeping information related to
// creating new lines and finding lines given their unique long-lived ID.
pub struct Editor {
    //    next_id: usize, // TODO: will be used when insertParagraph creates a new line
    id_map: HashMap<usize, usize>,
    lines: Store<CodeLines>,
    selection: RwSignal<Option<SetSelection>>,
}

// The SetSelection struct describes changes to be made to the browser's global selection.
// E.g. after typing a letter of text, the cursor should be advanced to follow the new text.
#[derive(Clone, Copy)]
enum SetSelection {
    Cursor(usize, usize),    // line index, pos
    MultiLine(usize, usize), // anchor line number, focus line number
}

// The individual lines of code.
#[derive(Store)]
pub struct CodeLines {
    #[store(key: usize = |line| line.id)]
    lines: Vec<EditLine>,
}

impl Editor {
    fn new() -> Self {
        Self {
            //            next_id: 6,
            lines: Store::new(CodeLines {
                lines: vec![EditLine::new(3), EditLine::new(5), EditLine::new(7)], // demo initial contents
            }),
            id_map: HashMap::from([(3, 0), (5, 1), (7, 2)]),
            selection: RwSignal::new(None),
        }
    }

    // Handle an input to the Editor window, by dispatching to the appropriate EditLine.
    // TODO: handle insertParagraph and insertLineBreak (add a line)
    // TODO: handle line deletion
    fn handle_input(&mut self, ev: InputEvent) {
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
    fn maybe_cancel(&self, ev: KeyboardEvent) {
        if ev.key() != "ArrowDown" {
            return;
        }
        let selection = get_current_selection();
        if selection.is_collapsed()
            && let Some(focus) = selection.focus_node()
            && let Some(id) = find_id_from_node(&focus)
            && id
                == self
                    .lines
                    .lines()
                    .read_untracked()
                    .last()
                    .expect("last line")
                    .id
        {
            ev.prevent_default();
        }
    }

    // "Rationalize" the selection to (1) make multi-line selections include the entire lines, and
    // (2) correct situations where the selection starts or ends outside the editor area.
    fn rationalize_selection(&mut self) {
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
    fn handle_out_of_bounds_selection(&mut self, selection: &Selection) {
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
                        .text
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
                    self.lines
                        .lines()
                        .read_untracked()
                        .last()
                        .expect("last line")
                        .id,
                );
            }
            (None, Some(focus_id)) => {
                self.handle_in_bounds_selection(
                    self.lines
                        .lines()
                        .read_untracked()
                        .first()
                        .expect("first line")
                        .id,
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
        if anchor_idx < focus_idx && anchor_offset == anchor_line.text.len() {
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
    fn update_selection(&self) {
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
                    anchor_line.text.len() as u32
                } else {
                    0
                };

                let focus_offset = if anchor_idx > focus_idx {
                    0
                } else {
                    focus_line.text.len() as u32
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

// DOM helper functions

fn get_current_selection() -> Selection {
    web_sys::window()
        .expect("window not found")
        .get_selection()
        .expect("selection error")
        .expect("selection not found")
}

// Given an HTML Node, finds the Codillion-assigned unique ID of the EditLine.
// These are stored in HTML attributes of the line-by-line Div elements.
fn find_id_from_node(orig_node: &Node) -> Option<usize> {
    let mut node = orig_node.clone();
    loop {
        if let Ok(elem) = node.clone().dyn_into::<Element>()
            && let Some(id_str) = elem.get_attribute("data-codillon-line-id")
        {
            return id_str.parse::<usize>().ok();
        }
        match node.parent_node() {
            Some(n) => node = n,
            None => return None,
        }
    }
}

// The editor component is a single contenteditable div, surrounding a collection of EditLines
// (each in their own div).
#[component]
pub fn Editor() -> impl IntoView {
    let (editor, set_editor) = signal(Editor::new());

    // If the selection or cursor changes, update it *after* updating the text.
    let selection_signal = editor.read_untracked().selection;
    Effect::watch(
        move || selection_signal.get(),
        move |_, _, _| set_editor.write_untracked().update_selection(),
        false,
    );

    view! {
        <div
            class="textentry"
            contenteditable
            spellcheck="false"
            on:beforeinput=move |ev| { set_editor.write_untracked().handle_input(ev) }
            on:mousedown=move |_| { set_editor.write_untracked().rationalize_selection() }
            on:keydown=move |ev| {
                editor.read_untracked().maybe_cancel(ev);
                set_editor.write_untracked().rationalize_selection();
            }
            on:mouseup=move |_| { set_editor.write_untracked().rationalize_selection() }
            on:keyup=move |_| { set_editor.write_untracked().rationalize_selection() }
        >
            <For each=move || editor.read().lines.lines() key=|line| line.read().id let(child)>
                <div
                    data-codillon-line-id=move || child.read().id
                    node_ref=child.read_untracked().div_ref
                >
                    {move || {
                        selection_signal.write();
                        child.read().as_string().to_string()
                    }}
                </div>
            </For>
        </div>
    }
}

// The `EditLine` reflects the state of an individual line.
// TODO: WebAssembly syntax checking
#[derive(Default)]
pub struct EditLine {
    id: usize,
    text: String,
    div_ref: NodeRef<leptos::html::Div>,
}

impl EditLine {
    fn new(id: usize) -> Self {
        Self {
            id,
            text: String::from("Hello, world"),
            div_ref: NodeRef::new(),
        }
    }

    fn as_string(&self) -> &str {
        &self.text
    }

    const COSMETIC_SPACE: char = '\u{FEFF}';

    fn rationalize(&mut self, cursor_pos: &mut usize) {
        // Adjust the line so the cursor still shows up even if the text is empty,
        // by replacing empty strings with a zero-width space character and removing it
        // later -- adjusting cursor position to match.

        self.text.retain(|c| c != Self::COSMETIC_SPACE);
        *cursor_pos = (*cursor_pos).min(self.text.len());
        let no_initial_ws = self.text.trim_start();
        *cursor_pos = (*cursor_pos).saturating_sub(self.text.len() - no_initial_ws.len());
        self.text = no_initial_ws.to_string();

        if self.text.is_empty() {
            self.text.push(Self::COSMETIC_SPACE);
        }
    }

    // Handle insert and delete events for this line.
    fn handle_input(&mut self, ev: InputEvent) -> usize {
        let range = ev
            .get_target_ranges()
            .get(0)
            .clone()
            .unchecked_into::<web_sys::Range>();

        let text_node = self.text_node();

        if range.start_container().expect("container") != text_node
            || range.end_container().expect("container") != text_node
        {
            panic!("InputEvent targets a range outside the text node for this EditLine")
        }

        let mut start_pos = range.start_offset().expect("offset") as usize;
        let mut end_pos = range.end_offset().expect("offset") as usize;

        if self.text.starts_with(Self::COSMETIC_SPACE) {
            self.text.clear();
        }

        start_pos = start_pos.min(self.text.len());
        end_pos = end_pos.min(self.text.len());

        if start_pos > end_pos {
            (start_pos, end_pos) = (end_pos, start_pos);
        }

        let mut cursor_pos = start_pos;

        match ev.input_type().as_str() {
            "insertText" => {
                let new_text = &ev.data().unwrap_or_default();
                self.text.replace_range(start_pos..end_pos, new_text);
                cursor_pos = start_pos + new_text.len();
            }
            "deleteContentBackward" => {
                if start_pos == end_pos && start_pos > 0 {
                    self.text.replace_range(start_pos - 1..start_pos, "");
                    cursor_pos = start_pos - 1;
                } else {
                    self.text.replace_range(start_pos..end_pos, "");
                }
            }
            "deleteContentForward" => {
                if start_pos == end_pos && start_pos < self.text.len() {
                    self.text.replace_range(start_pos..start_pos + 1, "");
                } else {
                    self.text.replace_range(start_pos..end_pos, "");
                }
            }
            other => leptos_dom::log!("unhandled: {other}"),
        }

        self.rationalize(&mut cursor_pos);
        cursor_pos
    }

    fn text_node(&self) -> Node {
        let node = self
            .div_ref
            .get_untracked()
            .expect("div")
            .first_child()
            .expect("text");
        if node.node_type() != Node::TEXT_NODE {
            panic!("non-text node found");
        }
        node
    }
}
