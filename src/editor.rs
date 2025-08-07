// The Codillon code editor (doesn't do much, but does capture beforeinput and logs to console)
mod line;
use crate::web_support::{
    AccessToken, Component, ElementFactory, SelectionHandle, WithElement, WithNode,
    components::DomVec,
};
use line::EditLine;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use web_sys::{HtmlDivElement, InputEvent};

struct _Editor {
    _next_id: usize,
    _id_map: HashMap<usize, usize>,
    _dom_selection: SelectionHandle,
    _factory: ElementFactory,
    component: DomVec<EditLine, HtmlDivElement>,
}

impl _Editor {
    fn new(factory: ElementFactory, dom_selection: SelectionHandle) -> Self {
        let component = DomVec::new(factory.div());
        Self {
            _next_id: 0,
            _id_map: HashMap::default(),
            _dom_selection: dom_selection,
            _factory: factory,
            component,
        }
    }

    fn push_line(&mut self, string: &str) {
        self.component
            .push(EditLine::new(string, &self._factory, self._next_id));
        self._id_map.insert(self._next_id, self.component.len() - 1);
        self._next_id += 1;
    }

    pub fn insert_line(&mut self, index: usize, string: &str) {
        self.component
            .insert(index, EditLine::new(string, &self._factory, self._next_id));
        self._id_map.values_mut().for_each(|line_no| {
            if *line_no >= index {
                *line_no += 1;
            }
        });
        self._id_map.insert(self._next_id, index);
        self._next_id += 1;
    }

    fn handle_input(&mut self, ev: InputEvent) {
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
                    self.component[selection.focus.0]
                        .component
                        .get_mut()
                        .0
                        .replace_data(area.start.1..area.end.1, new_text);
                    selection = LogicSelection::new_cursor(
                        area.start.0,
                        area.start.1 + new_text.chars().count(),
                    );
                    self.apply_selection(Some(selection));
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
                        self.component[selection.focus.0]
                            .component
                            .get_mut()
                            .0
                            .replace_data(area.start.1 - 1..area.end.1, "");
                        selection = LogicSelection::new_cursor(area.start.0, area.start.1 - 1);
                    } else if !selection.is_cursor() {
                        self.component[selection.focus.0]
                            .component
                            .get_mut()
                            .0
                            .replace_data(area.start.1..area.end.1, "");
                        selection = LogicSelection::new_cursor(area.start.0, area.start.1);
                    }

                    self.apply_selection(Some(selection));
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
                            < self.component[selection.focus.0].text().chars().count()
                    {
                        self.component[selection.focus.0]
                            .component
                            .get_mut()
                            .0
                            .replace_data(area.start.1..area.end.1 + 1, "");
                    } else if !selection.is_cursor() {
                        self.component[selection.focus.0]
                            .component
                            .get_mut()
                            .0
                            .replace_data(area.start.1..area.end.1, "");
                    }

                    self.apply_selection(Some(LogicSelection::new_cursor(
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
                    let second_part = self.component[area.start.0].text()[area.end.1..].to_string();
                    let line_chars_cnt = self.component[area.start.0].text().chars().count();
                    self.component[area.start.0]
                        .component
                        .get_mut()
                        .0
                        .replace_data(area.start.1..line_chars_cnt, "");
                    self.insert_line(area.start.0 + 1, &second_part);
                    self.apply_selection(Some(LogicSelection::new_cursor(area.start.0 + 1, 0)));
                } else {
                    unhandled_log();
                }
            }
            _ => {
                unhandled_log();
            }
        }
        #[cfg(debug_assertions)]
        {
            web_sys::console::log_1(&"successful audit".into());
            self.audit();
        }
    }

    fn get_logic_seletion(&self) -> Option<LogicSelection> {
        let anchor = self._dom_selection.get_anchor_node()?;
        let focus = self._dom_selection.get_focus_node()?;

        let anchor_id = EditLine::find_id_from_node(anchor);
        let focus_id = EditLine::find_id_from_node(focus);

        let mut selection = match (anchor_id, focus_id) {
            (Some(anchor_id), Some(focus_id)) => {
                let anchor_idx: usize = *self._id_map.get(&anchor_id).expect("can't find line");
                let focus_idx: usize = *self._id_map.get(&focus_id).expect("can't find line");
                Some(LogicSelection::new(
                    (anchor_idx, self._dom_selection.get_anchor_offset()),
                    (focus_idx, self._dom_selection.get_focus_offset()),
                ))
            }
            (Some(anchor_id), None) => {
                let anchor_idx: usize = *self._id_map.get(&anchor_id).expect("can't find line");
                Some(LogicSelection::new(
                    (anchor_idx, self._dom_selection.get_anchor_offset()),
                    (
                        self.component.len() - 1,
                        self.component.last().unwrap().text().chars().count(),
                    ),
                ))
            }
            (None, Some(focus_id)) => {
                let focus_idx: usize = *self._id_map.get(&focus_id).expect("can't find line");
                Some(LogicSelection::new(
                    (0, 0),
                    (focus_idx, self._dom_selection.get_focus_offset()),
                ))
            }
            (None, None) => None,
        };

        if let Some(selection) = &mut selection {
            if self.component[selection.anchor.0].text().is_empty() {
                selection.anchor.1 = 0;
            }
            if self.component[selection.focus.0].text().is_empty() {
                selection.focus.1 = 0;
            }
        }
        selection
    }
    fn apply_selection(&mut self, logic_selection: Option<LogicSelection>) {
        if let Some(LogicSelection { anchor, focus }) = logic_selection {
            let anchor_node = &self.component[anchor.0].component.get().0;
            let focus_node = &self.component[focus.0].component.get().0;
            self._dom_selection
                .set_selection(anchor_node, anchor.1, focus_node, focus.1);
        } else {
            self._dom_selection.remove_selection();
        }
    }
}

pub struct Editor(Rc<RefCell<_Editor>>);

impl Editor {
    pub fn new(factory: ElementFactory, dom_selection: SelectionHandle) -> Self {
        let mut inner = _Editor::new(factory, dom_selection);
        inner.component.set_attribute("class", "textentry");
        inner.component.set_attribute("contenteditable", "true");
        inner.component.set_attribute("spellcheck", "false");

        let ret = Editor(Rc::new(RefCell::new(inner)));

        let editor_ref = Rc::clone(&ret.0);
        ret.0
            .borrow_mut()
            .component
            .set_onbeforeinput(move |ev| editor_ref.borrow_mut().handle_input(ev));

        ret.0.borrow_mut().push_line("Hello, world.");
        ret
    }
}

impl WithElement for Editor {
    type Element = HtmlDivElement;
    fn with_element(&self, f: impl FnMut(&HtmlDivElement), g: AccessToken) {
        self.0.borrow().component.with_element(f, g);
    }
}

impl Component for Editor {
    fn audit(&self) {
        self.0.borrow().audit()
    }
}

impl WithNode for _Editor {
    fn with_node(&self, f: impl FnMut(&web_sys::Node), g: AccessToken) {
        self.component.with_node(f, g);
    }
}

impl Component for _Editor {
    fn audit(&self) {
        self.component.audit()
    }
}

/// LogicSelection is the selection area in logical model's view.
/// Usually it is dom selection restricted to editable area.
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

    pub fn to_area(self) -> std::ops::Range<(usize, usize)> {
        std::cmp::min(self.anchor, self.focus)..std::cmp::max(self.anchor, self.focus)
    }
}
