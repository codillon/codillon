use super::document::Document;
use crate::utils::{is_well_formed_func, is_well_formed_instr};
use leptos::ev::{click, keydown};
use leptos::logging::log;
use leptos::prelude::*;
use leptos_use::{use_event_listener, use_window};
use wasm_bindgen::JsCast;

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
        document.update(|doc| {
            let cur_idx = doc.index_of(focused_line.get()).unwrap();
            let current_line = doc.lines.get_mut(cur_idx).unwrap();
            if is_well_formed_func(&current_line.proposed_input.get()) {
                current_line.contents = current_line.proposed_input.get();
                focused_line.set(id);
            }
        });
    });

    let _ = use_event_listener(use_window(), keydown, move |event| {
        let key = event.key();
        let og_line = focused_line.get();
        document.update(|doc| {
            let num_lines = doc.lines.len();
            let focused_idx = doc.index_of(focused_line.get()).unwrap();
            let was_empty = doc.lines[focused_idx].proposed_input.get().is_empty();
            {
                let line = &mut doc.lines[focused_idx];
                if key.len() == 1 {
                    //update proposed input
                    line.proposed_input.update(|s| s.push_str(&key));
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
                    if !was_empty {
                        line.proposed_input.update(|s| {
                            s.pop();
                        });
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
                    if is_well_formed_func(&line.proposed_input.get()) {
                        line.contents = line.proposed_input.get();
                    }
                } else if key == "ArrowDown" {
                    // If the proposed input is well-formed, update the contents
                    if is_well_formed_instr(&line.proposed_input.get()) {
                        line.contents = line.proposed_input.get();
                    }
                    // Move to next line if line is well-formed
                    if focused_idx < num_lines - 1 && line.contents == line.proposed_input.get() {
                        focused_line.set(doc.lines[focused_idx + 1].unique_id);
                    }
                } else if key == "ArrowUp" {
                    // If the proposed input is well-formed, update the contents
                    if is_well_formed_instr(&line.proposed_input.get()) {
                        line.contents = line.proposed_input.get();
                    }
                    //Move to previous line if line is well-formed
                    if focused_idx > 0 && line.contents == line.proposed_input.get() {
                        focused_line.set(doc.lines[focused_idx - 1].unique_id);
                    }
                }
            }
            if key == "Backspace" && was_empty && num_lines > 1 {
                // If the proposed input is empty, remove the line
                doc.remove_line(focused_idx);
                focused_line.set(if focused_idx > 0 {
                    doc.lines[focused_idx - 1].unique_id
                } else {
                    doc.lines[0].unique_id
                });
                log!("Length of lines: {}", doc.lines.len());
            } else if key == "Enter"
                && is_well_formed_func(&doc.lines[focused_idx].proposed_input.get())
            {
                doc.insert_after(focused_idx);
                focused_line.set(doc.lines[focused_idx + 1].unique_id);
            }
        });

        log!(
            "Key pressed: {}, line id: {}, line idx: {}, proposed: {}, content: {}",
            key,
            og_line,
            document.get().index_of(og_line).unwrap(),
            document.get().lines[og_line].proposed_input.get(),
            document.get().lines[og_line].contents
        );
    });

    view! {
        <div class="editor">
            <ForEnumerate
                each=move || document.with(|doc| doc.lines.clone())
                key=|line| line.unique_id
                children=move |index, line| {
                    let input = line.proposed_input;
                    let unique_id = line.unique_id;
                    view! {
                        <div data-line-idx=unique_id class="textbox">
                            {move || {
                                let idx = index.get() + 1;
                                format!("{}: {}", idx, input.get())
                            }}
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
