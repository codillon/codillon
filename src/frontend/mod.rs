//! This module contains the frontend components for index page.
use super::utils::{is_well_formed_func, is_well_formed_instr};
use leptos::ev::{click, keydown};
use leptos::prelude::*;
use leptos_use::{use_event_listener, use_window};
use wasm_bindgen::JsCast;

mod boxlist;
mod textbox;

#[component]
pub fn App() -> impl IntoView {
    let document = RwSignal::new(Document::new());
    let focused_line = RwSignal::new(0);

    let _ = use_event_listener(use_window(), click, move |event| {
        let id = event
            .target()
            .expect("click on target error")
            .dyn_into::<web_sys::Element>()
            .expect("element error")
            .get_attribute("data-line-idx")
            .expect("expected data-line-idx")
            .parse::<usize>()
            .unwrap();
        let mut doc = document.get();
        let current_line = doc.lines.get_mut(focused_line.get()).unwrap();
        if is_well_formed_instr(&current_line.proposed_input.get()) {
            current_line.contents = current_line.proposed_input.get();
        }
        if current_line.proposed_input.get() == current_line.contents {
            focused_line.set(id);
        }
    });

    let _ = use_event_listener(use_window(), keydown, move |event| {
        let key = event.key();
        document.update(|doc| {
            let line = doc.lines.get_mut(focused_line.get()).unwrap();
            if key.len() == 1 {
                //update proposed input
                line.proposed_input.set(line.proposed_input.get() + &key);
                //check if proposed input instr is well formed
                if is_well_formed_instr(&line.proposed_input.get()) {
                    //update info and doc well-formed
                    line.info = line.instruction_type();
                    doc.well_formed.set(is_well_formed_func(&doc.concat()));
                } else {
                    //doc is automatically not well-formed
                    doc.well_formed.set(false);
                }
            } else if key == "Backspace" {
                let mut current = line.proposed_input.get();
                if current.is_empty() && doc.num_instructions > 1 {
                    doc.remove_line(focused_line.get());
                    if focused_line.get() > 0 {
                        focused_line.set(focused_line.get() - 1);
                    }
                } else {
                    current.pop();
                    line.proposed_input.set(current);
                    //check if proposed input instr is well formed
                    if is_well_formed_instr(&line.proposed_input.get()) {
                        //update info and doc well-formed
                        line.info = line.instruction_type();
                        doc.well_formed.set(is_well_formed_func(&doc.concat()));
                    } else {
                        //doc is automatically not well-formed
                        doc.well_formed.set(false);
                    }
                }
            } else if key == "Enter" {
                if is_well_formed_instr(&line.proposed_input.get()) {
                    line.contents = line.proposed_input.get();
                }
                if line.contents == line.proposed_input.get() {
                    // If the proposed input is well-formed, add a new line
                    doc.add_line();
                    focused_line.set(focused_line.get() + 1);
                }
            } else if key == "ArrowDown" {
                // If the proposed input is well-formed, update the contents
                if is_well_formed_instr(&line.proposed_input.get()) {
                    line.contents = line.proposed_input.get();
                }
                // Move to next line if line is well-formed
                if focused_line.get() < doc.num_instructions - 1
                    && line.contents == line.proposed_input.get()
                {
                    focused_line.set(focused_line.get() + 1);
                }
            } else if key == "ArrowUp" {
                // If the proposed input is well-formed, update the contents
                if is_well_formed_instr(&line.proposed_input.get()) {
                    line.contents = line.proposed_input.get();
                }
                //Move to previous line if line is well-formed
                if focused_line.get() > 0 && line.contents == line.proposed_input.get() {
                    focused_line.set(focused_line.get() - 1);
                }
            }
        });
    });

    view! {
        <div class="code-box">
            <For
                each=move || document.get().lines.clone()
                key=|line| line.unique_id
                children=move |mut line: CodeLine| {
                    let input = line.proposed_input;
                    let unique_id = line.unique_id;

                    view! {
                        <div data-line-idx=line.unique_id class="textbox">
                            {move || format!("{}. {}", unique_id, input.get())}
                            <Show when=move || focused_line.get() == unique_id>
                                <span class="cursor"></span>
                            </Show>
                        </div>
                    }
                }
            />
        </div>
        <div class="status">
            {move || {
                if document.get().well_formed.get() { "Well-formed" } else { "Not Well-formed" }
            }}
        </div>
    }
}

#[derive(Clone)]
struct Document {
    lines: Vec<CodeLine>,
    well_formed: RwSignal<bool>,
    num_instructions: usize,
}

impl Document {
    pub fn new() -> Self {
        Self {
            lines: vec![
                CodeLine {
                    proposed_input: RwSignal::new("".to_string()),
                    contents: "".to_string(),
                    unique_id: 0,
                    info: InstrInfo::Other,
                },
                CodeLine {
                    proposed_input: RwSignal::new("".to_string()),
                    contents: "".to_string(),
                    unique_id: 1,
                    info: InstrInfo::Other,
                },
            ],
            well_formed: RwSignal::new(true),
            num_instructions: 2,
        }
    }
    pub fn concat(&self) -> String {
        self.lines
            .iter()
            .map(|line| line.contents.clone())
            .collect::<Vec<String>>()
            .join("\n")
    }

    pub fn add_line(&mut self) {
        self.lines.push(CodeLine {
            proposed_input: RwSignal::new("".to_string()),
            contents: "".to_string(),
            unique_id: self.num_instructions,
            info: InstrInfo::Other,
        });
        self.num_instructions += 1;
    }

    pub fn remove_line(&mut self, idx: usize) {
        if idx < self.lines.len() {
            self.lines.remove(idx);
            self.num_instructions -= 1;
            for (i, line) in self.lines.iter_mut().enumerate() {
                line.unique_id = i;
            }
        }
    }
}

#[derive(Clone)]
struct CodeLine {
    proposed_input: RwSignal<String>,
    contents: String,
    unique_id: usize,
    info: InstrInfo,
}

impl CodeLine {
    pub fn instruction_type(&self) -> InstrInfo {
        match self.contents.as_str() {
            "if" => InstrInfo::Entry,
            "block" => InstrInfo::Entry,
            "loop" => InstrInfo::Entry,
            "else" => InstrInfo::Else,
            "end" => InstrInfo::End,
            _ => InstrInfo::Other,
        }
    }
}

#[derive(Clone)]
enum InstrInfo {
    Entry,
    Else,
    End,
    Other,
}
