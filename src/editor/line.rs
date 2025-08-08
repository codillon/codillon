use web_sys::{HtmlBrElement, HtmlSpanElement};

use crate::web_support::components::{DomStruct, DomText};

pub type DomBr = DomStruct<(), HtmlBrElement>;
pub type LineContents = (DomText, (DomBr, ()));
pub type EditLine = DomStruct<LineContents, HtmlSpanElement>;
