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
    cursor_line: RwSignal<Option<usize>>,
}

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
            cursor_line: RwSignal::new(None),
        }
    }

    // Given an HTML Node, finds the Codillion-assigned unique ID of the EditLine.
    // These are stored in HTML attributes of the line-by-line Div elements.
    fn find_id_from_node(&self, mut node: Node) -> Option<usize> {
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

    // Handle an input to the Editor window, by dispatching to the appropriate EditLine.
    // TODO: properly handle when the selection spans lines (expand the selection to the whole lines,
    //                                                       and delete lines as necessary on input)
    // TODO: handle insertParagraph and insertLineBreak (add a line)
    // TODO: handle line deletion
    fn handle_input(&mut self, ev: InputEvent) {
        ev.prevent_default();

        leptos_dom::log!("{}", ev.input_type());

        let selection_range = get_current_selection()
            .get_range_at(0)
            .expect("selection range");
        let start_id =
            self.find_id_from_node(selection_range.start_container().expect("container"));
        let end_id = self.find_id_from_node(selection_range.end_container().expect("container"));

        if start_id == end_id
            && let Some(id) = start_id
        {
            self.dispatch(id, ev);
        } else {
            leptos_dom::log!("resetting cursor. Ids are {:?} {:?}", start_id, end_id);
            self.cursor_line.set(Some(0));
            self.lines.lines().at_unkeyed(0).write().cursor_pos = Some(0);
        }
    }

    // Dispatch an InputEvent to the appropriate EditLine.
    fn dispatch(&mut self, id: usize, ev: InputEvent) {
        let Some(line_no) = self.id_map.get(&id) else {
            panic!("can't find line");
        };
        let the_line = self.lines.lines().at_unkeyed(*line_no);
        the_line.write().handle_input(ev);
        self.cursor_line.set(Some(*line_no));
    }

    // Update Codillion's idea of the current selection / cursor position to the browser.
    // We need to use "request_animation_frame" or *sometimes* (especially when the cursor position is
    // in the first line), Leptos will re-render the line contents after the selection is updated,
    // so the cursor gets warped back to the beginning of the line.
    fn update_selection(&self) {
        let Some(line_no) = self.cursor_line.get_untracked() else {
            leptos_dom::log!("no cursor line");
            return;
        };
        let the_line = &self.lines.lines().read_untracked()[line_no];
        let Some(pos) = the_line.cursor_pos else {
            leptos_dom::log!("no cursor pos");
            return;
        };
        let text_node = descend_until_text_node(the_line.div_element().into());
        get_current_selection()
            .set_base_and_extent(&text_node, pos as u32, &text_node, pos as u32)
            .expect("set selection base and extent");
    }
}

// General DOM helper functions.

fn get_current_selection() -> Selection {
    web_sys::window()
        .expect("window not found")
        .get_selection()
        .expect("selection error")
        .expect("selection not found")
}

// Given an HTML Node, descend until finding a text node.
// This is used to place the cursor in text.
fn descend_until_text_node(mut node: Node) -> Node {
    loop {
        if node.node_type() == Node::TEXT_NODE {
            return node;
        }
        node = node.first_child().expect("child");
    }
}

// The editor component is a single contenteditable div, surrounding a collection of EditLines
// (each in their own div).
#[component]
pub fn Editor() -> impl IntoView {
    let (editor, set_editor) = signal(Editor::new());

    // If the cursor moves, update it *after* updating the text.
    let cursor_signal = editor.read_untracked().cursor_line;
    Effect::watch(
        move || cursor_signal.get(),
        move |_, _, _| set_editor.write().update_selection(),
        false,
    );

    view! {
        <div
            class="textentry"
            contenteditable
            spellcheck="false"
            on:beforeinput=move |ev| { set_editor.write().handle_input(ev) }
        >
            <For each=move || editor.read().lines.lines() key=|line| line.read().id let(child)>
                <div
                    data-codillon-line-id=move || child.read().id
                    node_ref=child.read_untracked().div_ref
                >
                    {move || {
                        set_editor.write().cursor_line.write();
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
    cursor_pos: Option<usize>,
    div_ref: NodeRef<leptos::html::Div>,
}

impl EditLine {
    fn new(id: usize) -> Self {
        Self {
            id,
            text: String::from("Hello, world"),
            cursor_pos: None,
            div_ref: NodeRef::new(),
        }
    }

    fn as_string(&self) -> &str {
        &self.text
    }

    // Convert from a container+offset to a position within the text.
    // The complexity is that sometimes the Div contains the start or end of the selection,
    // and sometimes it's the Div's child (the text node).
    fn offset_to_pos(&self, container: Node, offset: u32) -> usize {
        if container == ***self.div_element() {
            // If the selection container is the Div element itself, the offset is in units of the whole Div.
            offset as usize * self.text.len()
        } else {
            // If the selection container is the Div's child (the text node), the offset is in characters.
            offset as usize
        }
    }

    const COSMETIC_SPACE: char = '\u{FEFF}';

    fn rationalize(&mut self) {
        // Adjust the line so the cursor still shows up even if the text is empty,
        // by replacing empty strings with a zero-width space character and removing it
        // later -- adjusting cursor position to match.

        self.text.retain(|c| c != Self::COSMETIC_SPACE);
        if let Some(ref mut pos) = self.cursor_pos {
            *pos = (*pos).min(self.text.len())
        }
        let no_initial_ws = self.text.trim_start();
        if let Some(ref mut pos) = self.cursor_pos {
            *pos -= self.text.len() - no_initial_ws.len();
        }
        self.text = no_initial_ws.to_string();

        if self.text.is_empty() {
            self.text.push(Self::COSMETIC_SPACE);
        }
    }

    // Handle insert and delete events for this line.
    fn handle_input(&mut self, ev: InputEvent) {
        let range = get_current_selection()
            .get_range_at(0)
            .expect("selection range");
        let mut start_pos = self.offset_to_pos(
            range.start_container().expect("container"),
            range.start_offset().expect("offset"),
        );
        let mut end_pos = self.offset_to_pos(
            range.end_container().expect("container"),
            range.end_offset().expect("offset"),
        );

        if self.text.chars().next() == Some(Self::COSMETIC_SPACE) {
            self.text.clear();
        }

        start_pos = start_pos.min(self.text.len());
        end_pos = end_pos.min(self.text.len());

        if start_pos > end_pos {
            (start_pos, end_pos) = (end_pos, start_pos);
        }

        match ev.input_type().as_str() {
            "insertText" => {
                let new_text = &ev.data().unwrap_or_default();
                self.text.replace_range(start_pos..end_pos, new_text);
                self.cursor_pos = Some(start_pos + new_text.len());
            }
            "deleteContentBackward" => {
                if start_pos == end_pos && start_pos > 0 {
                    self.text.replace_range(start_pos - 1..start_pos, "");
                    self.cursor_pos = Some(start_pos - 1);
                } else {
                    self.text.replace_range(start_pos..end_pos, "");
                    self.cursor_pos = Some(start_pos);
                }
            }
            "deleteContentForward" => {
                if start_pos == end_pos && start_pos < self.text.len() {
                    self.text.replace_range(start_pos..start_pos + 1, "");
                } else {
                    self.text.replace_range(start_pos..end_pos, "");
                }
                self.cursor_pos = Some(start_pos);
            }
            other => leptos_dom::log!("unhandled: {other}"),
        }

        self.rationalize();
    }

    fn div_element(&self) -> HtmlDivElement {
        self.div_ref.get_untracked().expect("div element")
    }
}
