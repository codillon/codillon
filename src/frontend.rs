use super::document::{CodeLineEntry, CusorPosition, InstrInfo};
use super::utils::Frame;
use leptos::ev::MouseEvent;
use leptos::prelude::*;
use web_sys::{window};

const CORRECT_EMOJI: &str = "✅";
const INCORRECT_EMOJI: &str = "❌";

/// This component provides a textbox for one instruction and use emoji to indicate correctness.
///
/// ### Parameters
/// `info`: hold processed information of one code line
///
/// `text`: the signal for the text of this textbox
///
/// `active_cursor_col`: if Some, this is the active line. Saves column number
///
/// `clicked`: write signal for being clicked
#[component]
pub fn CodeLine(
    info: Signal<InstrInfo>,
    text: ReadSignal<String>,
    active_cursor_col: Signal<Option<usize>>,
    clicked: WriteSignal<(usize, usize)>,
    unique_id: usize,
) -> impl IntoView {
    view! {
        <div
            tabindex="0"
            on:click=move |_ev: MouseEvent| {
                request_animation_frame(move || {
                    if let Some(window) = window() {
                        if let Ok(Some(selection)) = window.get_selection() {
                            if let Ok(range) = selection.get_range_at(0) {
                                if let Ok(offset) = range.start_offset() {
                                    clicked.set((unique_id, offset as usize));
                                }
                            }
                        }
                    }
                });
            }

            class=move || {
                if active_cursor_col.get().is_some() {
                    "code-line code-line-active"
                } else {
                    "code-line code-line-inactive"
                }
            }
        >
            <code class="code-content">
                <span
                    class=move || {
                        if active_cursor_col.get().is_some() {
                            "code-line-text has-cursor"
                        } else {
                            "code-line-text"
                        }
                    }
                    style=move || {
                        active_cursor_col
                            .get()
                            .map(|col| format!("--cursor-col: {}ch;", col))
                            .unwrap_or_default()
                    }
                >
                    {move || text.get()}
                </span>
            </code>
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
    active_line: ReadSignal<CusorPosition>,
    click_one_line: WriteSignal<(usize, usize)>,
) -> impl IntoView {
    let is_active = move |index: ReadSignal<usize>| {
        move || {
            let cursor = active_line.get();
            if cursor.0 == index.get()
            {
                Some(cursor.1)
            }
            else {
                None
            }
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
                                            active_cursor_col=Signal::derive(is_active(index))
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
    cursor: Signal<CusorPosition>,
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
            <div>
                "Cursor: "
                {move || {
                    let cursor = cursor.get();
                    format!("Ln {}, Col {}", cursor.0, cursor.1)
                }}
            </div>
        </div>
    }
}
