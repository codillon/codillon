use std::ops::{Deref, DerefMut};

use delegate::delegate;
use wasm_bindgen::JsCast;

pub use crate::websys_wrapper::common::*;


#[derive(Debug)]
pub struct SafeDiv<T: SafeNode> {
    node: web_sys::HtmlDivElement,
    children: Vec<AttachedNode<T>>
}

impl<T: SafeNode> UnsafeNode for SafeDiv<T> {
    fn default() -> Self {
        Self {
            node: document()
                .create_element("div")
                .unwrap()
                .dyn_into()
                .unwrap(),
            children: Vec::new()
        }
    }
    fn node(&self) -> &web_sys::HtmlElement {
        &self.node
    }
}

impl<T: SafeNode> SafeNode for SafeDiv<T>
{
    fn audit(&self) -> anyhow::Result<()> {
        for child in self.children.iter()
        {
            child.audit()?;
        }
        Ok(())
    }
}

impl<T: SafeNode> Parentable for SafeDiv<T> {}

impl<T: SafeNode> SafeDiv<T> {
    pub fn push(&mut self, child: AttachableNode<T>)
    {
        self.children.push(self.append_child(child));
    }

    delegate! {
        to self.children
        {
            pub fn get_mut(&mut self, index: usize) -> Option<&mut AttachedNode<T>>;
        }
    }
}

impl<T: SafeNode> Deref for SafeDiv<T>
{
    type Target = Vec<AttachedNode<T>>;
    fn deref(&self) -> &Self::Target {
        &self.children
    }
}