use super::NodeReader;
use crate::web_support::WithNode;

pub struct SelectionHandle(pub(super) web_sys::Selection);

impl SelectionHandle {
    pub fn get_focus_node(&self) -> Option<NodeReader<web_sys::Node>> {
        self.0.focus_node().map(NodeReader)
    }

    pub fn get_anchor_node(&self) -> Option<NodeReader<web_sys::Node>> {
        self.0.anchor_node().map(NodeReader)
    }

    pub fn get_focus_offset(&self) -> usize {
        self.0.focus_offset() as usize
    }

    pub fn get_anchor_offset(&self) -> usize {
        self.0.anchor_offset() as usize
    }

    pub fn remove_selection(&mut self) {
        self.0.remove_all_ranges().expect("remove selection");
    }

    pub fn set_selection(
        &mut self,
        anchor: &impl WithNode,
        anchor_offset: usize,
        focus: &impl WithNode,
        focus_offset: usize,
    ) {
        anchor.with_node(
            |anchor| {
                focus.with_node(
                    |focus| {
                        self.0
                            .set_base_and_extent(
                                anchor,
                                anchor_offset as u32,
                                focus,
                                focus_offset as u32,
                            )
                            .expect("set dom selection")
                    },
                    super::TOKEN,
                );
            },
            super::TOKEN,
        );
    }
}
