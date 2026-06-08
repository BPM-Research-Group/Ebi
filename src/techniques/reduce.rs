use ebi_objects::{
    ProcessTree,
    ebi_objects::process_tree::{Node, Operator},
};

use crate::techniques::empty_traces::has_empty_traces_node;

pub trait ReduceLanguageEquivalently {
    fn reduce_language_equivalently(&mut self);
}

impl ReduceLanguageEquivalently for ProcessTree {
    fn reduce_language_equivalently(&mut self) {
        let rules: Vec<Box<dyn TreeRule>> = vec![
            Box::new(Singularity),
            Box::new(Associativity),
            Box::new(Tau),
        ];
        let mut changed = true;
        while changed {
            changed = false;
            for node in (0..self.tree.len()).rev() {
                for rule in &rules {
                    changed |= rule.apply(self, node);
                }
            }
        }
    }
}

trait TreeRule {
    /// Apply the rule to the node; returns whether the rule changed the tree.
    fn apply(&self, tree: &mut ProcessTree, node: usize) -> bool;
}

struct Singularity;

impl TreeRule for Singularity {
    fn apply(&self, tree: &mut ProcessTree, node: usize) -> bool {
        match tree.tree[node] {
            Node::Tau | Node::Activity(_) => false,
            Node::Operator(Operator::Loop, _) => false,
            Node::Operator(Operator::Concurrent, 1)
            | Node::Operator(Operator::Interleaved, 1)
            | Node::Operator(Operator::Or, 1)
            | Node::Operator(Operator::Sequence, 1)
            | Node::Operator(Operator::Xor, 1) => {
                tree.tree.remove(node);
                true
            }
            Node::Operator(Operator::Concurrent, _)
            | Node::Operator(Operator::Interleaved, _)
            | Node::Operator(Operator::Or, _)
            | Node::Operator(Operator::Sequence, _)
            | Node::Operator(Operator::Xor, _) => false,
        }
    }
}

struct Associativity;

impl TreeRule for Associativity {
    fn apply(&self, tree: &mut ProcessTree, node: usize) -> bool {
        match tree.tree[node] {
            Node::Tau | Node::Activity(_) => false,
            Node::Operator(Operator::Interleaved, _) => false,
            Node::Operator(Operator::Xor, number_of_children)
            | Node::Operator(Operator::Sequence, number_of_children)
            | Node::Operator(Operator::Concurrent, number_of_children)
            | Node::Operator(Operator::Or, number_of_children) => {
                for child in tree.get_children(node) {
                    if tree.tree[child].is_operator_and_matches(&tree.tree[node]) {
                        let sub_number_of_children = tree.tree[child].number_of_children();
                        tree.tree.remove(child);
                        _ = tree.tree[node].set_number_of_children(
                            number_of_children - 1 + sub_number_of_children,
                        );
                        return true;
                    }
                }
                return false;
            }
            Node::Operator(Operator::Loop, number_of_children) => {
                for (c, child) in tree.get_children(node).enumerate() {
                    if c == 0 {
                        if tree.tree[child].is_operator_and_matches(&tree.tree[node]) {
                            let sub_number_of_children = tree.tree[child].number_of_children();
                            tree.tree.remove(child);
                            _ = tree.tree[node].set_number_of_children(
                                number_of_children - 1 + sub_number_of_children,
                            );
                            return true;
                        }
                    } else {
                        if tree.tree[child].is_operator_xor() {
                            let sub_number_of_children = tree.tree[child].number_of_children();
                            tree.tree.remove(child);
                            _ = tree.tree[node].set_number_of_children(
                                number_of_children - 1 + sub_number_of_children,
                            );
                            return true;
                        }
                    }
                }
                return false;
            }
        }
    }
}

struct Tau;

impl TreeRule for Tau {
    fn apply(&self, tree: &mut ProcessTree, node: usize) -> bool {
        //optimisation: everything is tau
        if !tree.tree[node].is_leaf()
            && !tree.tree[node..tree.traverse(node)]
                .iter()
                .any(|child| child.is_activity())
        {
            //subtree has no activities -> replace with tau
            tree.tree.drain(node..tree.traverse(node));
            tree.tree.insert(node, Node::Tau);
            return true;
        }

        match tree.tree[node] {
            Node::Tau | Node::Activity(_) => false,
            Node::Operator(Operator::Concurrent, number_of_children)
            | Node::Operator(Operator::Sequence, number_of_children)
            | Node::Operator(Operator::Interleaved, number_of_children)
                if number_of_children > 1 =>
            {
                for child in tree.get_children(node) {
                    //remove tau altogether
                    if tree.tree[child].is_tau() {
                        _ = tree.tree[node].set_number_of_children(number_of_children - 1);
                        tree.tree.remove(child);
                        return true;
                    }
                }
                return false;
            }
            Node::Operator(Operator::Or, number_of_children) if number_of_children > 1 => {
                for child in tree.get_children(node) {
                    if tree.tree[child].is_tau() {
                        //move tau to xor(tau) above or

                        //remove tau
                        tree.tree.remove(child);
                        _ = tree.tree[node].set_number_of_children(number_of_children - 1);

                        //add xor(tau)
                        tree.tree.insert(node, Node::Tau);
                        tree.tree.insert(node, Node::Operator(Operator::Xor, 2));

                        return true;
                    }
                }
                return false;
            }
            Node::Operator(Operator::Xor, number_of_children) if number_of_children > 1 => {
                if let Some(tau_child) = tree
                    .get_children(node)
                    .find(|child| tree.tree[*child].is_tau())
                {
                    //there is a tau child

                    if tree
                        .get_children(node)
                        .any(|child| child != tau_child && has_empty_traces_node(tree, child))
                    {
                        //there is another child with the empty trace
                        //remove the tau
                        tree.tree.remove(tau_child);
                        _ = tree.tree[node].set_number_of_children(number_of_children - 1);
                        return true;
                    }
                }
                return false;
            }
            Node::Operator(Operator::Loop, number_of_children) if number_of_children > 1 => {

                if let Some(tau_child) = tree
                    .get_children(node)
                    .skip(1)
                    .find(|child| tree.tree[*child].is_tau())
                {
                    //there is a tau child

                    if tree
                        .get_children(node)
                        .skip(1)
                        .any(|child| child != tau_child && has_empty_traces_node(tree, child))
                    {
                        //there is another child with the empty trace
                        //remove the tau
                        tree.tree.remove(tau_child);
                        _ = tree.tree[node].set_number_of_children(number_of_children - 1);
                        return true;
                    }
                }

                if tree.tree[tree.get_child(node, 0)].is_tau() {
                    println!("target");
                    //loop has a body tau; pull it up
                    //remove the body tau
                    tree.tree.remove(tree.get_child(node, 0));
                    //change the loop to an xor
                    tree.tree[node] = Node::Operator(Operator::Xor, number_of_children - 1);
                    //insert redo-tau
                    tree.tree.insert(tree.traverse(node), Node::Tau);
                    //insert loop
                    tree.tree.insert(node, Node::Operator(Operator::Loop, 2));
                    //insert tau
                    tree.tree.insert(node, Node::Tau);
                    //insert xor
                    tree.tree.insert(node, Node::Operator(Operator::Xor, 2));
                    return true;
                }

                return false;
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::techniques::reduce::ReduceLanguageEquivalently;
    use ebi_objects::{
        ActivityKey, ActivityKeyTranslator, ProcessTree, activity,
        ebi_objects::process_tree::{Node, Operator},
        seq, tau, tloop, xor,
    };

    #[test]
    fn singularity_xor() {
        let mut tree = xor!(tau!());
        let target = tau!();

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_string(), target.to_string());
    }

    #[test]
    fn singularity_seq() {
        let mut tree = seq!(tau!());
        let target = tau!();

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_string(), target.to_string());
    }

    #[test]
    fn associativity_xor() {
        let mut tree = xor!(tau!(), xor!(tau!()));
        let target = tau!();

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_string(), target.to_string());
    }

    #[test]
    fn associativity_seq() {
        let mut tree = seq!(tau!(), xor!(tau!()));
        let target = tau!();

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_string(), target.to_string());
    }

    #[test]
    fn loop_tau() {
        let mut tree = seq!(tau!(), xor!(tau!(), tloop!(tau!(), tau!())));
        let target = tau!();

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_string(), target.to_string());
    }

    #[test]
    fn loop_redo_tau() {
        let a = activity!("a");
        println!("{:?}", a);
        println!("{}", a);

        let b = activity!("b");
        println!("{:?}", b);
        println!("{}", b);

        let xor = xor!(a, b);
        println!("{:?}", xor);
        println!("{}", xor);

        let xortaub = xor!(tau!(), activity!("b"));
        println!("{:?}", xortaub);
        println!("{}", xortaub);

        let mut tree = tloop!(activity!("a"), xor!(tau!(), activity!("b")), tau!());
        let target = tloop!(activity!("a"), activity!("b"), tau!());

        println!("{:?}", tree);

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_string(), target.to_string());
    }

    #[test]
    fn pull_loop_body_tau() {
        let mut tree = tloop!(tau!(), activity!("a"), activity!("b"));
        let target = xor!(tau!(), tloop!(xor!(activity!("a"), activity!("b")), tau!()));

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_string(), target.to_string());
    }
}
