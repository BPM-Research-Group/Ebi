use ebi_objects::{
    ProcessTree, TreeNode, TreeOperator,
};

pub trait HasEmptyTraces {
    fn has_empty_traces(&self) -> bool;
}

impl HasEmptyTraces for ProcessTree {
    fn has_empty_traces(&self) -> bool {
        if self.is_empty() {
            false
        } else {
            has_empty_traces_node(self, 0)
        }
    }
}

pub fn has_empty_traces_node(tree: &ProcessTree, node: usize) -> bool {
    match tree[node] {
        TreeNode::Tau => true,
        TreeNode::Activity(_) => false,
        TreeNode::Operator(TreeOperator::Xor, _) | TreeNode::Operator(TreeOperator::Or, _) => tree
            .get_children(node)
            .any(|node| has_empty_traces_node(tree, node)),
        TreeNode::Operator(TreeOperator::Concurrent, _)
        | TreeNode::Operator(TreeOperator::Sequence, _)
        | TreeNode::Operator(TreeOperator::Interleaved, _) => tree
            .get_children(node)
            .all(|node| has_empty_traces_node(tree, node)),
        TreeNode::Operator(TreeOperator::Loop, _) => has_empty_traces_node(tree, tree.get_child(node, 0)),
    }
}
