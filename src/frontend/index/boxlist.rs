use super::textbox::Textbox;
use leptos::prelude::*;

/// ### BoxList component
/// This function creates a list of textboxs with a push button and a pop button.
#[component]
pub fn BoxList(initial_length: usize) -> impl IntoView {
    let initial_counters = (0..initial_length).collect::<Vec<_>>();
    let (counters, set_counters) = signal(initial_counters);

    let push_line = move |_| {
        let line_num: usize = counters.get().len();
        set_counters.update(|counters| counters.push(line_num));
    };

    let pop_line = move |_| {
        set_counters.update(|counters| {
            counters.pop(); // Will Return None if empty, we ignore the return value anyway
        });
    };

    view! {
        <div>
            <button on:click=push_line>"Add Line"</button>
            <button on:click=pop_line>"Remove Line"</button>
            <For
                each=move || counters.get()
                // Even if we do not use the key, we still need to set them because leptos system rely on unique keys
                key=|line_num| *line_num
                children=move |line_num| {
                    view! {
                        <li>
                            {line_num} ": "
                            <Textbox />
                        </li>
                    }
                }
            />
        </div>
    }
}
