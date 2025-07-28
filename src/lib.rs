use crate::websys_wrapper::*;
mod utils;
mod websys_wrapper;

pub fn start() {
    let mut div = append_node_to_body(SafeDiv::<SafeSpan>::new());
    div.push(SafeSpan::new());
    div.push(SafeSpan::new());
    div.push(SafeSpan::new());

    *div.get_mut(0).unwrap().text_mut() = "Hel".to_string();
    *div.get_mut(1).unwrap().text_mut() = "l Wo".to_string();
    *div.get_mut(2).unwrap().text_mut() = "rld!".to_string();
    
    std::mem::forget(div);
}
