use super::document::CodeLineEntry;
use super::utils::Frame;
use crate::document::InstrInfo;
use leptos::prelude::*;

const CORRECT_EMOJI: &str = "✅";
const INCORRECT_EMOJI: &str = "❌";

/// This component provides a textbox for one instruction and use emoji to indicate correctness.
///
/// ### Parameters
/// `well_formed`: the signal indicating correctness
///
/// `text`: the signal for the text of this textbox
///
/// `is_active`: activity indicator
#[component]
pub fn CodeLine(
    info: Signal<InstrInfo>,
    text: ReadSignal<String>,
    is_active: Signal<bool>,
) -> impl IntoView {
    view! {
        <div
            tabindex="0"
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
/// `button_click`: bind the buttons to this signal
#[component]
pub fn Editor(
    lines: Signal<Vec<CodeLineEntry>>,
    active_line: ReadSignal<Option<usize>>,
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
                        let line_number = Signal::derive(move || index.get());
                        // 0-based line number
                        view! {
                            <div class="code-line-wrapper">
                                <div class="line-number">
                                    {move || line_number.get().to_string()}
                                </div>
                                <CodeLine
                                    info=entry.info.into()
                                    text=entry.text_input.read_only().into()
                                    is_active=Signal::derive(is_active(index))
                                />
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
pub fn GlobalStatus(well_formed: Signal<bool>, frames: Signal<Vec<Frame>>) -> impl IntoView {
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
        </div>
    }
}
