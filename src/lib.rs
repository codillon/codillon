use leptos::{
    ev::{self, KeyboardEvent, MouseEvent},
    prelude::*,
};
use leptos_use::use_event_listener;
use web_sys::window;

mod website;

pub mod utils;

#[component]
pub fn App() -> impl IntoView {
    let website = RwSignal::new(website::Website::default());

    let window = window().unwrap_or_else(|| panic!("window should be available"));
    let document = window
        .document()
        .unwrap_or_else(|| panic!("document should be available"));

    let _keyboardListener =
        use_event_listener(window.clone(), ev::keydown, move |e: KeyboardEvent| {
            match e.key().as_str() {
                "ArrowUp" | "ArrowDown" => {
                    e.prevent_default(); // prevent browser scroll, TODO: add the scroll control ourselves
                }
                _ => {}
            }
            website.write().keystroke(&e.key());
        });

    let _mouseListener = use_event_listener(window.clone(), ev::click, move |e: MouseEvent| {
        if let Some(el) = document.element_from_point(e.client_x() as f32, e.client_y() as f32) {
            if let Some(text_line_el) = el.closest(".codeline").ok().flatten() {
                if let Some(index_attr) = text_line_el.get_attribute("data-index") {
                    if let Ok(Some(selection)) = window.get_selection() {
                        if let Ok(range) = selection.get_range_at(0) {
                            if let Ok(offset) = range.start_offset() {
                                if let Ok(line_idx) = index_attr.parse::<usize>() {
                                    website.write().update_cursor(line_idx, offset as usize);
                                }
                            }
                        }
                    }
                }
            }
        }
    });

    view! {
        {move || {
            let website = website.read();
            let cursor = website.get_cursor();
            let frames = website.get_frames();
            let selection = website.get_selection();
            website
                .get_content()
                .iter()
                .enumerate()
                .map(|(index, entry)| {
                    let is_selected = selection
                        .as_ref()
                        .map_or(false, |range| range.contains(&index));

                    view! {
                        <div class="codeline" data-selected=is_selected data-index=index>

                            {
                                let related_line = frames.get(&index);
                                if let Some(related_line) = related_line {
                                    (view! {
                                        <span class="line-number" data-related=true>
                                            {index}
                                            "("
                                            {*related_line}
                                            "):"
                                        </span>
                                    })
                                        .into_any()
                                } else {
                                    (view! { <span class="line-number">{index} ":"</span> })
                                        .into_any()
                                }
                            }

                            {if index == cursor.0 {
                                (view! {
                                    <span
                                        class="codetext has-cursor"
                                        style=format!("--cursor-col: {}ch;", cursor.1)
                                    >
                                        {entry.line.clone()}
                                    </span>
                                })
                                    .into_any()
                            } else {
                                (view! { <span class="codetext">{entry.line.clone()}</span> })
                                    .into_any()
                            }}
                        </div>
                    }
                })
                .collect_view()
        }}
    }
}
