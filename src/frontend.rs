use super::document::CodeLineEntry;
use super::utils::Frame;
use crate::document::InstrInfo;
use leptos::ev::MouseEvent;
use leptos::prelude::*;

const CORRECT_EMOJI: &str = "✅";
const INCORRECT_EMOJI: &str = "❌";

/// This component provides a textbox for one instruction and use emoji to indicate correctness.
///
/// ### Parameters
/// `info`: hold processed information of one code line
///
/// `text`: the signal for the text of this textbox
///
/// `is_active`: activity indicator
///
/// `clicked`: write signal for being clicked
#[component]
pub fn CodeLine(
    info: Signal<InstrInfo>,
    text: ReadSignal<String>,
    is_active: Signal<bool>,
    clicked: WriteSignal<usize>,
    unique_id: usize,
) -> impl IntoView {
    view! {
        <div
            tabindex="0"
            on:click=move |_ev: MouseEvent| {
                clicked.set(unique_id);
            }
            class=move || {
                if is_active.get() {
                    "code-line code-line-active"
                } else {
                    "code-line code-line-inactive"
                }
            }
        >
            <code class="code-content">{move || text.get()}</code>
            <span class="emoji">
                {move || if info.get().well_formed { CORRECT_EMOJI } else { INCORRECT_EMOJI }}
            </span>
        </div>
    }
}

/// This component creates a list of textboxs with a push button and a pop button.
/// Initially, it will have 0 textboxes.
///
/// ### Parameters
/// `lines`: Read from this signal to get the content
///
/// `active_line`: Signal for the active line's number
#[component]
pub fn Editor(
    lines: Signal<Vec<CodeLineEntry>>,
    active_line: ReadSignal<Option<usize>>,
    click_one_line: WriteSignal<usize>,
) -> impl IntoView {
    let is_active = move |index: ReadSignal<usize>| {
        move || {
            active_line
                .get()
                .is_some_and(|active_index| active_index == index.get())
        }
    };

    view! {
        <div>
            <div class="code-lines-container">
                <ForEnumerate
                    each=move || lines.get()
                    key=|entry| entry.unique_id
                    children=move |index, entry| {
                        // 0-based line number
                        view! {
                            <div class="code-line-wrapper">
                                <div class="line-number">{move || index.get().to_string()}</div>
                                {
                                    view! {
                                        <CodeLine
                                            info=entry.info.into()
                                            text=entry.text_input.read_only().into()
                                            is_active=Signal::derive(is_active(index))
                                            clicked=click_one_line
                                            unique_id=entry.unique_id
                                        />
                                    }
                                }
                            </div>
                        }
                    }
                />
            </div>
        </div>
    }
}

/// To show the well-formness of the whole function and framematching results.
#[component]
pub fn GlobalStatus(
    well_formed: Signal<bool>,
    frames: Signal<Vec<Frame>>,
    is_frozen: Signal<bool>,
) -> impl IntoView {
    view! {
        <div>
            <div>
                "Func Wellformness: "
                {move || if well_formed.get() { CORRECT_EMOJI } else { INCORRECT_EMOJI }}
            </div>
            <div>
                "Frames:"
                {move || {
                    frames
                        .get()
                        .into_iter()
                        .map(|frame| {
                            view! {
                                <br />
                                {format!("{frame:?}")}
                            }
                        })
                        .collect::<Vec<_>>()
                }}
            </div>
            <div>"Frozen:" {move || is_frozen.get()}</div>
        </div>
    }
}
