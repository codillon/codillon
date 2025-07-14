// The SetSelection struct describes changes to be made to the browser's global selection.
// E.g. after typing a letter of text, the cursor should be advanced to follow the new text.
//
// Note: Separating in case we'll ever want to independently test selection logic.
#[derive(Clone, Copy)]
pub enum SetSelection {
    Cursor(usize, usize),    // line index, pos
    MultiLine(usize, usize), // anchor line number, focus line number
}
