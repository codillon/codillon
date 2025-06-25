use super::textbox::Textbox;
use leptos::prelude::*;
use std::collections::LinkedList;

#[derive(Debug, Clone)]
struct CodeData {
    rows: LinkedList<CodeEntry>, // pub(self) for convenient testing
}

impl CodeData {
    pub fn new(initial_length: usize) -> CodeData {
        CodeData {
            rows: (0..initial_length)
                .map(|_| CodeEntry {
                    line: RwSignal::new(String::new()),
                })
                .collect(),
        }
    }

    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        self.rows.len()
    }

    pub fn push_back(&mut self) {
        self.rows.push_back(CodeEntry::new());
    }

    /// ### Returns
    /// Some(entry) if it is not an empty list; None if empty
    pub fn pop_back(&mut self) -> Option<CodeEntry> {
        self.rows.pop_back()
    }

    /// ### Returns
    /// A String of all rows joined by '\n'
    #[allow(dead_code)]
    pub fn dump(&self) -> String {
        self.rows
            .iter()
            .map(|entry| entry.line.get_untracked())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

#[derive(Debug, Clone)]
struct CodeEntry {
    line: RwSignal<String>,
}

impl CodeEntry {
    pub fn new() -> CodeEntry {
        CodeEntry {
            line: RwSignal::new(String::new()),
        }
    }
}

/// ### BoxList component
/// This function creates a list of textboxs with a push button and a pop button.
///
/// ### Parameters
/// `initial_length`: the number of textboxes at the beginning
#[component]
pub fn Boxlist(initial_length: usize) -> impl IntoView {
    let codedata = CodeData::new(initial_length);
    let (codedata, set_codedata) = signal(codedata);

    let push_line = move |_| {
        set_codedata.update(|counters| counters.push_back());
    };

    let pop_line = move |_| {
        set_codedata.update(|codedata| {
            codedata.pop_back(); // Will Return None if empty, we ignore the return value anyway
        });
    };

    view! {
        <div>
            <button on:click=push_line>"Add Line"</button>
            <button on:click=pop_line>"Remove Line"</button>
            <For
                each=move || codedata.get().rows.into_iter().enumerate()
                // Use index as key
                key=|(index, _)| *index
                children=move |(index, entry)| {
                    view! {
                        <br/>
                        {index} ": "
                        <Textbox text=entry.line />
                    }
                }
            />
        </div>
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_codedata() {
        let mut codedata = CodeData::new(3);
        codedata.push_back();
        assert_eq!(codedata.len(), 4);
        codedata.pop_back();
        codedata.pop_back();
        assert_eq!(codedata.len(), 2);
        codedata
            .rows
            .iter_mut()
            .next()
            .unwrap()
            .line
            .update(|s| *s = "Hello".to_string());
        codedata
            .rows
            .iter_mut()
            .nth(1)
            .unwrap()
            .line
            .update(|s| *s = "Leptos".to_string());
        assert_eq!(codedata.dump(), "Hello\nLeptos".to_string());
    }
}
