use super::textbox::Textbox;
use leptos::prelude::*;

#[component]
pub fn DynamicList(initial_length: usize) -> impl IntoView {
    let initial_counters = (0..initial_length)
        .map(|id| (id, ArcRwSignal::new(id + 1)))
        .collect::<Vec<_>>();

    let (counters, set_counters) = signal(initial_counters);

    let push_line = move |_| {
        let id: usize = counters.get().len() + 1;
        let sig = ArcRwSignal::new(id);
        set_counters.update(move |counters| counters.push((id, sig.clone())));
    };

    let pop_line = move |_| {
        set_counters.update(|counters| {
            if !counters.is_empty() {
                counters.pop();
            }
        });
    };

    view! {
        <div>
            <button on:click=push_line>"Add Line"</button>
            <button on:click=pop_line>"Remove Line"</button>
            <ul>
                <For
                    each=move || counters.get()
                    key=|counter| counter.0
                    children=move |(_id, count)| {
                        let count = RwSignal::from(count);
                        view! {
                            <li>
                                {count.get()} ": "
                                <Textbox />
                            </li>
                        }
                    }
                />
            </ul>
        </div>
    }
}
