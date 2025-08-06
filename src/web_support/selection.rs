use super::NodeReader;

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
}


/// LogicSelection is the selection area in logical model's view.
/// Usually it is dom selection restricted to editable area.
#[derive(Debug, Clone, Copy)]
pub struct LogicSelection {
    pub anchor: (usize, usize), // #Ln, #Col
    pub focus: (usize, usize),
}

impl LogicSelection {
    pub fn new_cursor(r: usize, c: usize) -> LogicSelection {
        LogicSelection {
            anchor: (r, c),
            focus: (r, c),
        }
    }

    pub fn new(anchor: (usize, usize), focus: (usize, usize)) -> LogicSelection {
        LogicSelection { anchor, focus }
    }

    pub fn is_cursor(&self) -> bool {
        self.anchor == self.focus
    }

    pub fn to_area(self) -> std::ops::Range<(usize, usize)> {
        std::cmp::min(self.anchor, self.focus)..std::cmp::max(self.anchor, self.focus)
    }
}
