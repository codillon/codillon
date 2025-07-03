use leptos::prelude::RwSignal;

#[derive(Clone)]
pub struct Document {
    pub lines: Vec<CodeLine>,
    pub well_formed: RwSignal<bool>,
    pub next_id: usize,
}

impl Document {
    pub fn new() -> Self {
        Self {
            lines: vec![CodeLine {
                proposed_input: RwSignal::new("".to_string()),
                contents: "".to_string(),
                unique_id: 0,
                info: InstrInfo::Other,
            }],
            well_formed: RwSignal::new(true),
            next_id: 1,
        }
    }
    pub fn concat(&self) -> String {
        self.lines
            .iter()
            .map(|line| line.contents.clone())
            .collect::<Vec<String>>()
            .join("\n")
    }

    pub fn insert_after(&mut self, idx: usize) {
        self.lines.insert(
            idx + 1,
            CodeLine {
                proposed_input: RwSignal::new("".to_string()),
                contents: "".to_string(),
                unique_id: self.next_id,
                info: InstrInfo::Other,
            },
        );
        self.next_id += 1;
    }

    pub fn insert_with_end(&mut self, idx: usize) {
        self.insert_after(idx);
        self.lines.insert(
            idx + 2,
            CodeLine {
                proposed_input: RwSignal::new("end".to_string()),
                contents: "end".to_string(),
                unique_id: self.next_id,
                info: InstrInfo::End,
            },
        );
        self.next_id += 1;
    }

    pub fn remove_line(&mut self, idx: usize) {
        if idx < self.lines.len() {
            self.lines.remove(idx);
        }
    }

    pub fn index_of(&self, id: usize) -> Option<usize> {
        self.lines.iter().position(|line| line.unique_id == id)
    }

    pub fn frame_matching(&self) -> Option<i32> {
        let mut stack = 0;
        for line in self.lines.iter() {
            match line.info {
                InstrInfo::Entry => stack += 1,
                InstrInfo::Else => continue,
                InstrInfo::End => {
                    stack -= 1;
                }
                InstrInfo::Other => continue,
            }
        }
        Some(stack)
    }
}

#[derive(Clone)]
pub struct CodeLine {
    pub proposed_input: RwSignal<String>,
    pub contents: String,
    pub unique_id: usize,
    pub info: InstrInfo,
}

impl CodeLine {
    pub fn instruction_type(&self) -> InstrInfo {
        match self.contents.as_str() {
            "if" => InstrInfo::Entry,
            "block" => InstrInfo::Entry,
            "loop" => InstrInfo::Entry,
            "else" => InstrInfo::Else,
            "end" => InstrInfo::End,
            _ => InstrInfo::Other,
        }
    }
}

#[derive(Clone)]
pub enum InstrInfo {
    Entry,
    Else,
    End,
    Other,
}
