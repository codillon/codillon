//! This module contains the frontend components for index page.
use leptos::prelude::*;

mod boxlist;
mod textbox;

#[component]
pub fn App() -> impl IntoView {
    view! { <boxlist::Boxlist /> }
}
