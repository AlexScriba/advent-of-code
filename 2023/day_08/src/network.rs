use std::{collections::HashMap, rc::Rc};

use crate::node::{new_node, Node};

pub struct Network {
    nodes: HashMap<String, Node>,
}

impl Network {
    pub fn new() -> Network {
        Network {
            // nodes: RefCell::new(HashMap::new()),
            nodes: HashMap::new(),
        }
    }

    pub fn insert_nodes(&mut self, raw_nodes: &[(String, (String, String))]) {
        for (name, (left, right)) in raw_nodes {
            let curr_node = self.get_node_or_insert(name);

            let left_node = self.get_node_or_insert(left);
            let right_node = self.get_node_or_insert(right);

            curr_node.borrow_mut().left = Some(left_node);
            curr_node.borrow_mut().right = Some(right_node);
        }
    }

    pub fn get_node_or_insert(&mut self, name: &String) -> Node {
        Rc::clone(
            self.nodes
                .entry(name.to_string())
                .or_insert(new_node(name.to_string())),
        )
    }

    pub fn get_node(&self, name: &str) -> Node {
        let node = self.nodes.get(name);

        match node {
            None => panic!("Node not found"),
            Some(n) => Rc::clone(n),
        }
    }

    pub fn get_node_names(&self) -> Vec<&String> {
        self.nodes.keys().into_iter().collect::<Vec<&String>>()
    }
}
