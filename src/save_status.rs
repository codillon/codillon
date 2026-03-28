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
}

impl SaveStatus {
    pub fn new(factory: &ElementFactory) -> Self {
        let mut contents = DomStruct::new((DomText::new(""), ()), factory.div());
        contents.set_attribute("class", "save-status");

        Self {
            contents,
            fail_save_ms: None,
        }
    }

    pub fn is_dirty(&self) -> bool {
        self.fail_save_ms.is_some()
    }

    pub fn notify_save_result(&mut self, success: bool) {
        if success {
            self.fail_save_ms = None;
        } else if self.fail_save_ms.is_none() {
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
