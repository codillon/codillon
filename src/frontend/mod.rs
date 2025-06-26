//! This module contains the frontend components for index page.
use leptos::prelude::*;

mod boxlist;
mod textbox;

/// This Global Id Allocator provides unique ids for objects.
/// The id may be used by leptos to for handling objects dynamically.
/// The exact number of id is not guaranteed
static GLOB_ID_ALLOCATOR: GlobalIdAllocator =
    GlobalIdAllocator(std::sync::atomic::AtomicUsize::new(0));

#[derive(Debug)]
struct GlobalIdAllocator(std::sync::atomic::AtomicUsize);

impl GlobalIdAllocator {
    pub fn new_id(&self) -> usize {
        self.0.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
    }
}

#[component]
pub fn App() -> impl IntoView {
    view! { <boxlist::Boxlist /> }
}
