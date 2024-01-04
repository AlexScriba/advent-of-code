use std::{rc::Rc, cell::RefCell};

pub type Node = Rc<RefCell<NodeContents>>;

#[derive(Eq)]
pub struct NodeContents {
    pub name: String,
    pub left: Option<Node>,
    pub right: Option<Node>
}

impl NodeContents {
    pub fn new(name: String) -> NodeContents {
        NodeContents {
            name,
            left: None,
            right: None,
        }
    }
}

impl PartialEq for NodeContents {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

pub fn new_node(name: String) -> Node {
    Rc::new(RefCell::new(NodeContents::new(name)))
}
