use crate::line::EditLine;
use std::cell::RefCell;
use std::rc::Rc;
use std::{cell::Cell, collections::HashMap};
use wasm_bindgen::prelude::*;
use web_sys::{Document, Element, HtmlDivElement, InputEvent, Node, Window};

/// _Editor is used to:
/// 1. Make the DOM and Logical Model synchornized(Do this usually by logical model drived DOM modification)
/// 2. Impose our wellformness enforcement rule by using functions in `utils.rs` at the logical model level
#[derive(Debug)]
struct _Editor {
    window: Window,
    document: Document,
    node: HtmlDivElement,
    id_map: HashMap<usize, usize>,
    lines: Vec<EditLine>,
    /// Should Only be used in `fn get_next_id(&self) -> usize`
    next_id: Cell<usize>,
}

/// Wrap `_Editor` because some of handlers need move the `Editor` into their block
#[derive(Debug)]
pub struct Editor(Rc<RefCell<_Editor>>);

pub fn editor() -> Editor {
    Editor(Rc::new(RefCell::new(_Editor::default())))
}

impl Default for _Editor {
    fn default() -> Self {
        let window = web_sys::window().expect("Cannot access the window");
        let document = window.document().expect("Cannot access the document");
        let editor_node = document
            .create_element("div")
            .unwrap()
            .dyn_into::<HtmlDivElement>()
            .unwrap();
        editor_node.set_attribute("name", "editor").unwrap();
        editor_node.set_attribute("class", "textentry").unwrap();
        editor_node
            .set_attribute("contenteditable", "true")
            .unwrap();
        editor_node.set_attribute("spellcheck", "false").unwrap();
        let body = document.body().expect("Cannot access teh body");
        body.append_child(&editor_node).unwrap();
        web_sys::console::log_1(&"Editor Created".into());
        let editor = _Editor {
            window,
            document,
            node: editor_node,
            id_map: HashMap::new(),
            lines: Vec::new(),
            next_id: Cell::new(0),
        };

        editor
    }
}

impl Editor {
    pub fn initialize(&self) {
        self.0.borrow_mut().push_line();
        self.initialize_listener();
    }

    fn initialize_listener(&self) {
        let editor_rc = self.0.clone();
        let closure = Closure::wrap(Box::new(move |ev: InputEvent| {
            ev.prevent_default();
            editor_rc.borrow_mut().handle_input(ev);
        }) as Box<dyn FnMut(_)>);

        self.0
            .borrow()
            .node
            .add_event_listener_with_callback("beforeinput", closure.as_ref().dyn_ref().unwrap())
            .unwrap();

        closure.forget();
    }
}
impl _Editor {
    fn push_line(&mut self) {
        let id = self.get_next_id();
        let new_line = EditLine::new(id, &self.document);
        self.id_map.insert(id, self.lines.len());
        let new_line_node = new_line.node().clone();
        self.lines.push(new_line);
        self.node
            .append_child(&new_line_node)
            .expect("Append to editor failed");
    }

    fn get_dom_selection(&self) -> web_sys::Selection {
        self.window
            .get_selection()
            .expect("Cannot get dom selection")
            .expect("Dom selection not found")
    }

    fn get_logic_seletion(&self) -> Option<LogicSelection> {
        let dom_selection = self.get_dom_selection();
        let anchor = dom_selection.anchor_node()?;
        let focus = dom_selection.focus_node()?;

        let anchor_id = find_id_from_node(&anchor);
        let focus_id = find_id_from_node(&focus);

        match (anchor_id, focus_id) {
            (Some(anchor_id), Some(focus_id)) => {
                let anchor_idx: usize = *self.id_map.get(&anchor_id).expect("can't find line");
                let focus_idx: usize = *self.id_map.get(&focus_id).expect("can't find line");
                Some(LogicSelection::new(
                    (
                        anchor_idx,
                        if self.lines[anchor_idx].text().is_empty() {
                            0
                        } else {
                            dom_selection.anchor_offset() as usize
                        },
                    ),
                    (
                        focus_idx,
                        if self.lines[focus_idx].text().is_empty() {
                            0
                        } else {
                            dom_selection.focus_offset() as usize
                        },
                    ),
                ))
            }
            (Some(anchor_id), None) => {
                let anchor_idx: usize = *self.id_map.get(&anchor_id).expect("can't find line");
                Some(LogicSelection::new(
                    (
                        anchor_idx,
                        if self.lines[anchor_idx].text().is_empty() {
                            0
                        } else {
                            dom_selection.anchor_offset() as usize
                        },
                    ),
                    (
                        self.lines.len() - 1,
                        self.lines
                            .last()
                            .expect("Empty lines")
                            .text()
                            .chars()
                            .count(),
                    ),
                ))
            }
            (None, Some(focus_id)) => {
                let focus_idx: usize = *self.id_map.get(&focus_id).expect("can't find line");
                Some(LogicSelection::new(
                    (
                        0,
                        self.lines
                            .first()
                            .expect("Empty lines")
                            .text()
                            .chars()
                            .count(),
                    ),
                    (
                        focus_idx,
                        if self.lines[focus_idx].text().is_empty() {
                            0
                        } else {
                            dom_selection.focus_offset() as usize
                        },
                    ),
                ))
            }
            (None, None) => None,
        }
    }

    pub fn handle_input(&mut self, ev: InputEvent) {
        let unhandled_log = || {
            web_sys::console::log_1(
                &format!(
                    "Unhandled Event Type:{}, Data: {:?}, Selection:{:?}",
                    ev.input_type(),
                    ev.data(),
                    self.get_logic_seletion()
                )
                .into(),
            )
        };

        match ev.input_type().as_str() {
            "insertText" => {
                if let Some(mut selection) = self.get_logic_seletion()
                    && selection.anchor.0 == selection.focus.0
                {
                    let new_text = &ev.data().unwrap_or_default();
                    let mut text_mut = self.lines[selection.focus.0].text_mut();
                    let min_pos = std::cmp::min(selection.anchor.1, selection.focus.1);
                    let (start_pos, end_pos) = (
                        min_pos,
                        std::cmp::max(selection.anchor.1, selection.focus.1),
                    );
                    text_mut.replace_range(start_pos..end_pos, new_text);
                    selection.anchor.1 = min_pos + new_text.chars().count();
                    selection.focus.1 = min_pos + new_text.chars().count();
                    drop(text_mut);
                    self.set_dom_selection(Some(selection));
                } else {
                    unhandled_log();
                }
            }
            _ => {
                unhandled_log();
            }
        }
    }

    fn get_next_id(&self) -> usize {
        let id = self.next_id.get();
        self.next_id.replace(id + 1);
        id
    }

    fn set_dom_selection(&self, logic_selection: Option<LogicSelection>) {
        if let Some(LogicSelection { anchor, focus }) = logic_selection {
            let anchor_node = &self.lines[anchor.0].text_node();
            let focus_node = &self.lines[focus.0].text_node();

            self.get_dom_selection()
                .set_base_and_extent(anchor_node, anchor.1 as u32, focus_node, focus.1 as u32)
                .expect("Set dom selection");
        } else {
            self.get_dom_selection()
                .remove_all_ranges()
                .expect("Fail in clearing dom selection");
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct LogicSelection {
    pub anchor: (usize, usize), // #Ln, #Col
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

    pub fn is_cursor(&self) -> bool {
        self.anchor == self.focus
    }
}

// Given an HTML Node, finds the Codillion-assigned unique ID of the EditLine.
// These are stored in HTML attributes of the line-by-line Div elements.
pub fn find_id_from_node(orig_node: &Node) -> Option<usize> {
    let mut node = orig_node.clone();
    loop {
        if let Ok(elem) = node.clone().dyn_into::<Element>()
            && let Some(id_str) = elem.get_attribute(EditLine::ID_ATTRIBUTE)
        {
            return id_str.parse::<usize>().ok();
        }
        match node.parent_node() {
            Some(n) => node = n,
            None => return None,
        }
    }
}
