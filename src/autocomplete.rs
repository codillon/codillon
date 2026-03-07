//Made with the help of Claude Opus 4.6 through Antigravity
use crate::editor::Editor;
use crate::visited_operators::get_all_instruction_names;
use crate::{
    dom_struct::DomStruct,
    dom_text::DomText,
    dom_vec::DomVec,
    jet::{ControlHandlers, ElementFactory, ReactiveComponent},
};
use web_sys::{HtmlDivElement, MouseEvent};

type HintItem = ReactiveComponent<DomStruct<(DomText, ()), HtmlDivElement>>;
pub type HintBarStruct = DomVec<HintItem, HtmlDivElement>;

pub fn suggest(prefix: &str, limit: usize) -> Vec<String> {
    let mut names = get_all_instruction_names();
    if !prefix.is_empty() {
        names.retain(|s| s.starts_with(prefix) && s != prefix);
    }
    names.truncate(limit);
    names
}

pub fn setup_hint_bar(factory: &ElementFactory) -> HintBarStruct {
    let mut bar: HintBarStruct = DomVec::new(factory.div());
    bar.set_attribute("class", "autocomplete-hint-bar");
    bar.set_attribute("style", "display:none");
    bar
}

pub fn update_hint_bar(
    factory: &ElementFactory,
    bar: &mut HintBarStruct,
    suggestions: &[String],
    editor: &Editor,
) {
    bar.truncate(0);

    if suggestions.is_empty() {
        bar.set_attribute("style", "display:none");
        return;
    }

    bar.remove_attribute("style");

    for s in suggestions {
        let mut item = ReactiveComponent::new(DomStruct::new((DomText::new(s), ()), factory.div()));
        item.inner_mut().set_attribute("class", "autocomplete-item");

        let (editor, accepted) = (editor.clone(), s.clone());
        item.set_onmousedown(move |ev: MouseEvent| {
            ev.prevent_default();
            ev.stop_propagation();
            editor.accept_autocomplete(&accepted);
        });

        bar.push(item);
    }
}
