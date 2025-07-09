use leptos::{
    ev::{self, InputEvent, KeyboardEvent}, html::Div, math::mover, prelude::*
};
use leptos_use::{use_event_listener, use_window};
use web_sys::{HtmlElement, Event, Window, Selection};
use wasm_bindgen::JsCast;
mod website;

pub mod utils;

#[component]
pub fn App() -> impl IntoView {
    let website = RwSignal::new(website::Website::default());
    let div_ref: NodeRef<Div> = NodeRef::new();

    let on_input = move |e: Event| {
        leptos::logging::log!("Website content: {:?}", website.get().get_content());
        e.prevent_default();
        if let Some(div) = div_ref.get() {
            let element = div.unchecked_into::<web_sys::HtmlDivElement>();
            if let Some(text) = element.text_content() {
                website.update(|web|{
                    web.content.clear();
                    for line in text.lines() {
                        web.content.push(website::CodelineEntry {
                            line: line.to_string(),
                        });
                    }
                })
            }
        }
    };



    let on_keydown = {
        //let website = website.clone();
        move |e: KeyboardEvent| {
            let key = e.key();
            if let Some(div) = div_ref.get() {
                let element = div.unchecked_into::<web_sys::HtmlDivElement>();
                let focused_line_idx = get_cursor_position(&element).unwrap_or(0);
                if let Some(text) = element.text_content() {
                    if utils::is_well_formed_instr(website.get().get_content()[focused_line_idx].line.as_str()) {
                        if key == "Enter" || key == "ArrowUp" || key == "ArrowDown" || key == "ArrowLeft" || key == "ArrowRight" {
                            e.prevent_default();
                            return;
                        }
                    }
                }
            }
        }
    };

    view!{
        <div
            contenteditable="true"
            tabindex="0"
            node_ref=div_ref
            on:input=on_input
            spellcheck="false"
            style="white-space: pre-wrap; font-family: monospace; outline:none; min-height: 200px; border: 1px solid #ccc; padding: 8px;"
        >
            {move || {
                website.get()
                    .get_content()
                    .iter()
                    .enumerate()
                    .map(|entry| format!("{}", entry.line)).collect::<String>()
            }}
        </div>
    }

    

    /*let _listener = use_event_listener(use_window(), ev::keydown, move |e: KeyboardEvent| {
        match e.key().as_str() {
            "ArrowUp" | "ArrowDown" => {
                e.prevent_default(); // prevent browser scroll, TODO: add the scroll control ourselves
            }
            _ => {}
        }
        website.write().keystroke(&e.key());
    });

    view! {
        {move || {
            let cursor = website.read_untracked().get_cursor();
            website
                .get()
                .get_content()
                .iter()
                .enumerate()
                .map(|(index, entry)| {
                    view! {
                        <div>
                            <span>{index} " :"</span>

                            {if index == cursor.0 {
                                let (first_part, second_part) = entry.line.split_at(cursor.1);
                                (view! {
                                    <span class="codetext">{first_part}</span>
                                    <span class="caret">"|"</span>
                                    <span class="codetext">{second_part}</span>
                                })
                                    .into_any()
                            } else {
                                (view! { <span>{entry.line.clone()}</span> }).into_any()
                            }}
                        </div>
                    }
                })
                .collect_view()
        }}
    }*/
}

fn get_cursor_position(div: &web_sys::HtmlDivElement) -> Option<usize> {
    let window = window();
    let selection = window.get_selection().ok()?;
    let offset = selection.unwrap().anchor_offset();
    let text = div.text_content()?;
    let offset = offset.min(text.len() as u32) as usize;
    let text_before_cursor = &text[..offset];
    Some(text_before_cursor.matches('\n').count())
}
