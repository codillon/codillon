use leptos::ev::keydown;
use leptos::mount::mount_to_body;
use leptos::prelude::*;
use leptos_use::{use_event_listener, use_window};

#[component]
fn App() -> impl IntoView {
    let lines = RwSignal::new(vec!["Line 1".to_string(), "Line 2".to_string()]);
    let current_line = RwSignal::new(0);
    let _ = use_event_listener(use_window(), keydown, move |evt| {
        leptos::logging::log!("Key pressed: {}", evt.key());
        let key = evt.key();
        if key.len() == 1 {
            lines.update(|lines_vec| {
                lines_vec
                    .get_mut(current_line.get())
                    .unwrap_or(&mut String::new())
                    .push_str(&key);
            });
        } else if key == "Backspace" {
            lines.update(|lines_vec| {
                lines_vec
                    .get_mut(current_line.get())
                    .unwrap_or(&mut String::new())
                    .pop();
            });
        } else if key == "ArrowUp" {
            if current_line.get() > 0 {
                current_line.set(current_line.get() - 1);
            }
        } else if key == "ArrowDown" {
            if current_line.get() + 1 < lines.get().len() {
                current_line.set(current_line.get() + 1);
            }
        } else if key == "Enter" {
            lines.update(|lines_vec| {
                lines_vec.insert(current_line.get() + 1, String::new());
                current_line.set(current_line.get() + 1);
            });
        }
    });

    view! {
        <div>
            <For
                each=move || lines.get()
                key=|line| line.clone()
                children=move |line| {
                    view! {
                        <div style="border: 1px solid black; padding: 8px; margin: 4px;">
                            {line}
                        </div>
                    }
                }
            />
        </div>
    }
}

fn main() {
    mount_to_body(App);
}
