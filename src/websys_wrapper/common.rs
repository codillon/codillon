//! Unsafe nodes should only appear under this module.
use std::ops::{Deref, DerefMut};

use anyhow;
use delegate::delegate;
use web_sys::window;

pub fn document() -> web_sys::Document {
    window().unwrap().document().unwrap()
}

#[allow(private_bounds)]
pub trait SafeNode: UnsafeNode + Sized {
    fn new() -> AttachableNode<Self> {
        AttachableNode(Self::default())
    }

    fn audit(&self) -> anyhow::Result<()>;

    delegate! {
        to self.node()
        {
            #[unwrap] // No return value
            fn set_attribute(&self, name: &str, value: &str);
            fn get_attribute(&self, name: &str) -> Option<String>;
        }
    }
}

pub(super) trait UnsafeNode {
    fn default() -> Self;

    /// We use `HtmlElement` instead of `Node` because we do not want to dive too
    /// deeper into the DOM. A HtmlElement is always shown in browser's inspection
    /// view so it is great.
    fn node(&self) -> &web_sys::HtmlElement;
}

#[derive(Debug)]
pub struct AttachableNode<T: SafeNode>(T);

#[derive(Debug)]
pub struct AttachedNode<T: SafeNode>(T);

impl<T: SafeNode> Deref for AttachedNode<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: SafeNode> DerefMut for AttachedNode<T>
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: SafeNode> Drop for AttachedNode<T> {
    fn drop(&mut self) {
        self.0.node().remove();
    }
}

pub(super) trait Parentable: SafeNode {
    fn append_child<T: SafeNode>(&self, child: AttachableNode<T>) -> AttachedNode<T> {
        self.node()
            .append_child(child.0.node())
            .expect("Append Node");
        AttachedNode(child.0)
    }

    fn insert_child<T: SafeNode>(
        &mut self,
        index: usize,
        node: AttachableNode<T>,
    ) -> AttachedNode<T> {
        self.node()
            .insert_before(
                node.0.node(),
                self.node().child_nodes().item(index as u32).as_ref(),
            )
            .expect("Insert Node");
        AttachedNode(node.0)
    }
}

pub fn append_node_to_body<T: SafeNode>(child: AttachableNode<T>) -> AttachedNode<T> {
    document().body().unwrap().append_child(child.0.node()).expect("Append Node to Body");
    AttachedNode(child.0)
}