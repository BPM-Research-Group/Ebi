use ebi_objects::{
    ProcessTree,
    ebi_objects::process_tree::{Node, Operator},
};

pub trait HasEmptyTraces {
    fn has_empty_traces(&self) -> bool;
}

impl HasEmptyTraces for ProcessTree {
    fn has_empty_traces(&self) -> bool {
        if self.tree.is_empty() {
            false
        } else {
            has_empty_traces_node(self, 0)
        }
    }
}

pub fn has_empty_traces_node(tree: &ProcessTree, node: usize) -> bool {
    match tree.tree[node] {
        Node::Tau => true,
        Node::Activity(_) => false,
        Node::Operator(Operator::Xor, _) | Node::Operator(Operator::Or, _) => tree
            .get_children(node)
            .any(|node| has_empty_traces_node(tree, node)),
        Node::Operator(Operator::Concurrent, _)
        | Node::Operator(Operator::Sequence, _)
        | Node::Operator(Operator::Interleaved, _) => tree
            .get_children(node)
            .all(|node| has_empty_traces_node(tree, node)),
        Node::Operator(Operator::Loop, _) => has_empty_traces_node(tree, tree.get_child(node, 0)),
    }
}
