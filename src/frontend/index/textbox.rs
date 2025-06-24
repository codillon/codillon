use leptos::prelude::*;

#[component]
pub fn textbox() -> impl IntoView {
    let (text, set_text) = signal("".to_string());

    view! {
        <input
            type="text"
            prop:value=text
            on:input=move |ev| {
                set_text.set(event_target_value(&ev));
            }
            placeholder="Enter Some Text"
        />
        <p>
            "The text is :" {text}
        </p>
    }
}
