use crate::common::*;
use crate::line::EditLine;
use std::cell::RefCell;
use std::rc::Rc;
use std::{cell::Cell, collections::HashMap};
use wasm_bindgen::prelude::*;
use web_sys::{Element, HtmlDivElement, InputEvent, KeyboardEvent, Node};

/// _Editor is used to:
/// 1. Make the DOM and Logical Model synchornized(by logical-model-driven DOM modification)
/// 2. Impose our wellformness enforcement rule by using functions in `utils.rs` at the logical model level
#[derive(Debug)]
struct _Editor {
    node: HtmlDivElement,
    /*------------------------------------------------------------*/
    id_map: HashMap<usize, usize>,
    lines: Vec<ComponentHolder<EditLine>>,
    /// Should Only be used in `fn get_next_id(&self) -> usize`
    next_id: Cell<usize>,
}

/// Wrap `_Editor` because some of handlers need move the `Editor` into their block
#[derive(Debug)]
pub struct Editor(Rc<RefCell<_Editor>>);

pub fn editor() -> Editor {
    Editor(Rc::default())
}

impl Default for _Editor {
    fn default() -> Self {
        let editor_node = document()
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
        let editor = _Editor {
            node: editor_node,
            id_map: HashMap::new(),
            lines: Vec::new(),
            next_id: Cell::new(0),
        };
        editor
    }
}

impl Editor {
    pub fn mount(&self)
    {
        document().body().unwrap().append_child(self.0.borrow().node_ref().as_ref()).expect("Load");
    }

    pub fn initialize(&self) {
        self.0.borrow_mut().insert_line(0, "Hello world! Frist Line");
        self.0.borrow_mut().insert_line(1, "Hello world! Thrid Line");
        self.0.borrow_mut().insert_line(1, "Hello world! Second Line");
        self.initialize_listener();
    }

    fn initialize_listener(&self) {
        let editor_rc = self.0.clone();
        let handle_input_closure = Closure::wrap(Box::new(move |ev: InputEvent| {
            editor_rc.borrow_mut().handle_input(ev);
        }) as Box<dyn FnMut(_)>);

        let editor_rc = self.0.clone();
        let handle_keydown_closure = Closure::wrap(Box::new(move |ev: KeyboardEvent| {
            editor_rc.borrow_mut().handle_keydown(ev);
        }) as Box<dyn FnMut(_)>);

        self.0
            .borrow()
            .node
            .add_event_listener_with_callback(
                "beforeinput",
                handle_input_closure.as_ref().dyn_ref().unwrap(),
            )
            .unwrap();

        self.0
            .borrow()
            .node
            .set_onkeydown(Some(handle_keydown_closure.as_ref().unchecked_ref()));

        handle_input_closure.forget();
        handle_keydown_closure.forget();
    }
}

impl Component for _Editor {
    fn node_ref(&self) -> impl AsRef<web_sys::Element> {
        &self.node
    }
}

impl _Editor {
    fn insert_line(&mut self, index: usize, text: &str) {
        let id = self.get_next_id();
        let mut new_line: ComponentHolder<_> = EditLine::new(id).into();
        self.id_map.values_mut().for_each(|idx| {
            if *idx >= index {
                *idx += 1;
            }
        });
        self.id_map.insert(id, index);
        *new_line.ref_mut().text_mut() = text.to_string();
        self.lines.insert(index, new_line);
        self.dom_insert_child(index, self.lines[index].as_ref());
    }

    fn get_logic_seletion(&self) -> Option<LogicSelection> {
        let dom_selection = get_dom_selection();
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
                        if self.lines[anchor_idx].as_ref().text().is_empty() {
                            0
                        } else {
                            dom_selection.anchor_offset() as usize
                        },
                    ),
                    (
                        focus_idx,
                        if self.lines[focus_idx].as_ref().text().is_empty() {
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
                        if self.lines[anchor_idx].as_ref().text().is_empty() {
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
                            .as_ref()
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
                            .as_ref()
                            .text()
                            .chars()
                            .count(),
                    ),
                    (
                        focus_idx,
                        if self.lines[focus_idx].as_ref().text().is_empty() {
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
        ev.prevent_default();

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
                    let area = selection.to_area();
                    let new_text = &ev.data().unwrap_or_default();
                    self.lines[selection.focus.0]
                        .ref_mut()
                        .text_mut()
                        .replace_range(area.start.1..area.end.1, new_text);
                    selection = LogicSelection::new_cursor(
                        area.start.0,
                        area.start.1 + new_text.chars().count(),
                    );
                    self.set_dom_selection(Some(selection));
                } else {
                    unhandled_log();
                }
            }
            "deleteContentBackward" => {
                if let Some(mut selection) = self.get_logic_seletion()
                    && selection.anchor.0 == selection.focus.0
                {
                    let area = selection.to_area();
                    if selection.is_cursor() && selection.focus.1 > 0 {
                        self.lines[selection.focus.0]
                            .ref_mut()
                            .text_mut()
                            .replace_range(area.start.1 - 1..area.end.1, "");
                        selection = LogicSelection::new_cursor(area.start.0, area.start.1 - 1);
                    } else if !selection.is_cursor() {
                        self.lines[selection.focus.0]
                            .ref_mut()
                            .text_mut()
                            .replace_range(area.start.1..area.end.1, "");
                        selection = LogicSelection::new_cursor(area.start.0, area.start.1);
                    }

                    self.set_dom_selection(Some(selection));
                } else {
                    unhandled_log();
                }
            }
            "deleteContentForward" => {
                if let Some(selection) = self.get_logic_seletion()
                    && selection.anchor.0 == selection.focus.0
                {
                    let area = selection.to_area();
                    if selection.is_cursor()
                        && selection.focus.1
                            < self.lines[selection.focus.0]
                                .as_ref()
                                .text()
                                .chars()
                                .count()
                    {
                        self.lines[selection.focus.0]
                            .ref_mut()
                            .text_mut()
                            .replace_range(area.start.1..area.end.1 + 1, "");
                    } else if !selection.is_cursor() {
                        self.lines[selection.focus.0]
                            .ref_mut()
                            .text_mut()
                            .replace_range(area.start.1..area.end.1, "");
                    }

                    self.set_dom_selection(Some(LogicSelection::new_cursor(
                        area.start.0,
                        area.start.1,
                    )));
                } else {
                    unhandled_log();
                }
            }
            "insertParagraph" | "insertLineBreak" => {
                if let Some(selection) = self.get_logic_seletion()
                    && selection.anchor.0 == selection.focus.0
                {
                    let area = selection.to_area();
                    let first_part =
                        self.lines[area.start.0].as_ref().text()[..area.start.1].to_string();
                    let second_part =
                        self.lines[area.start.0].as_ref().text()[area.end.1..].to_string();
                    *self.lines[area.start.0].ref_mut().text_mut() = first_part;
                    self.insert_line(area.start.0 + 1, &second_part);
                    self.set_dom_selection(Some(LogicSelection::new_cursor(area.start.0 + 1, 0)));
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

    fn handle_keydown(&self, ev: KeyboardEvent) {
        web_sys::console::log_1(&format!("Keydown {}", ev.key()).into());
        match ev.key().as_str() {
            "ArrowLeft" => {
                if let Some(selection) = self.get_logic_seletion()
                    && selection.is_cursor()
                    && selection.focus.0 > 0
                    && self.lines[selection.focus.0].as_ref().text().is_empty()
                {
                    ev.prevent_default();
                    self.set_dom_selection(Some(LogicSelection::new_cursor(
                        selection.focus.0 - 1,
                        self.lines[selection.focus.0 - 1]
                            .as_ref()
                            .text()
                            .chars()
                            .count(),
                    )));
                };
            }
            "ArrowRight" => {
                if let Some(selection) = self.get_logic_seletion()
                    && selection.is_cursor()
                    && selection.focus.0 + 1 < self.lines.len()
                    && self.lines[selection.focus.0].as_ref().text().is_empty()
                {
                    ev.prevent_default();
                    self.set_dom_selection(Some(LogicSelection::new_cursor(
                        selection.focus.0 + 1,
                        0,
                    )));
                };
            }
            _ => (),
        }
    }

    fn set_dom_selection(&self, logic_selection: Option<LogicSelection>) {
        if let Some(LogicSelection { anchor, focus }) = logic_selection {
            let anchor_node = self.lines[anchor.0].as_ref().text_node();
            let focus_node = self.lines[focus.0].as_ref().text_node();
            get_dom_selection()
                .set_base_and_extent(&anchor_node, anchor.1 as u32, &focus_node, focus.1 as u32)
                .expect("Set dom selection");
        } else {
            get_dom_selection()
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

    pub fn to_area(&self) -> std::ops::Range<(usize, usize)> {
        std::cmp::min(self.anchor, self.focus)..std::cmp::max(self.anchor, self.focus)
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
