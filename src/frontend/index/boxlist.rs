use super::textbox::Textbox;
use leptos::prelude::*;
use reactive_stores::Store;

#[derive(Store, Debug, Clone)]
struct CodeData {
    #[store(key: usize = |row| row.line_num)]
    pub(self) rows: Vec<CodeEntry>, // pub(self) for convenient testing
}

impl CodeData {
    pub fn new(initial_length: usize) -> CodeData {
        CodeData {
            rows: (0..initial_length)
                .map(|i| CodeEntry {
                    line_num: i,
                    line: RwSignal::new(String::new()),
                })
                .collect(),
        }
    }

    pub fn len(&self) -> usize {
        self.rows.len()
    }

    pub fn push_back(&mut self) {
        self.rows.push(CodeEntry::new(self.len()));
    }

    /// ### Returns
    /// Some(entry) if it is not an empty list; None if empty
    pub fn pop_back(&mut self) -> Option<CodeEntry> {
        self.rows.pop()
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

#[derive(Store, Debug, Clone)]
struct CodeEntry {
    line_num: usize,
    line: RwSignal<String>,
}

impl CodeEntry {
    pub fn new(line_num: usize) -> CodeEntry {
        CodeEntry {
            line_num,
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
                each=move || codedata.get().rows
                // Even if we do not use the key, we still need to set them because leptos system rely on unique keys
                key=|entry| entry.line_num
                children=move |entry| {
                    view! {
                        <br/>
                        {entry.line_num} ": "
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
        codedata.rows[0].line.update(|s| *s = "Hello".to_string());
        codedata.rows[1].line.update(|s| *s = "Leptos".to_string());
        assert_eq!(codedata.dump(), "Hello\nLeptos".to_string());
    }
}
