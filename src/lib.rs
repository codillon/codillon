use leptos::web_sys::window;
use leptos::{
    ev::{self, KeyboardEvent, MouseEvent},
    prelude::*,
};
use leptos_use::use_event_listener;

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
            if let Some(text_line_el) = el.closest(".textLine").ok().flatten() {
                if let Some(index_attr) = text_line_el.get_attribute("data-index") {
                    if let Ok(idx) = index_attr.parse::<usize>() {
                        website.write().update_line_index(idx);
                    }
                }
            }
        }
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
                    let is_selected = index == cursor.0;
                    view! {
                        <div class=if is_selected { "codeline-selected" } else { "codeline" } data-index=index>
                            <span class="line-number">{index} " :"</span>

                            {if is_selected {
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
