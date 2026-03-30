use crate::{
    dom_struct::DomStruct,
    dom_text::DomText,
    jet::{AccessToken, Component, ElementFactory, WithElement, now_ms},
};
use std::time::Duration;
use web_sys::HtmlDivElement;

type StatusDiv = DomStruct<(DomText, ()), HtmlDivElement>;

pub struct SaveStatus {
    contents: StatusDiv,
    fail_save_ms: Option<f64>,
    version: u64,
    saved_version: u64,
}

impl SaveStatus {
    pub fn new(factory: &ElementFactory) -> Self {
        let mut contents = DomStruct::new((DomText::new(""), ()), factory.div());
        contents.set_attribute("class", "save-status");

        Self {
            contents,
            fail_save_ms: None,
            version: 0,
            saved_version: 0,
        }
    }

    pub fn mark_dirty(&mut self) {
        self.version += 1;
        self.refresh();
    }

    pub fn is_dirty(&self) -> bool {
        assert!(self.saved_version <= self.version);
        // Content has changed since last confirmed save
        self.version != self.saved_version
    }

    pub fn get_version(&self) -> u64 {
        self.version
    }

    pub fn notify_save_result(&mut self, success: bool, version: u64) {
        if success {
            self.saved_version = self.saved_version.max(version);
            self.fail_save_ms = None;
        } else if self.is_dirty() && self.fail_save_ms.is_none() {
            self.fail_save_ms = Some(now_ms());
        }
        self.refresh();
    }

    pub fn refresh(&mut self) {
        if let Some(time) = self.fail_save_ms {
            let seconds = ((now_ms() - time) / 1000.0) as u64;
            self.contents.get_mut().0.set_data(&format!(
                "\u{2717} Last saved {} ago",
                humantime::format_duration(Duration::from_secs(seconds))
            ));
            self.contents.set_attribute("class", "save-status dirty");
        } else {
            self.contents.set_attribute("class", "save-status");
        }
    }
}

impl WithElement for SaveStatus {
    type Element = HtmlDivElement;
    fn with_element(&self, f: impl FnMut(&HtmlDivElement), g: AccessToken) {
        self.contents.with_element(f, g)
    }
}

impl Component for SaveStatus {
    fn audit(&self) {
        self.contents.audit()
    }
}
