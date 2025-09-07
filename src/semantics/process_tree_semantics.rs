use std::{
    fmt::Display,
    ops::{Index, IndexMut},
};

use anyhow::{Result, anyhow};
use ebi_objects::{
    Activity, ProcessTree,
    ebi_objects::{
        labelled_petri_net::TransitionIndex,
        process_tree::{Node, Operator},
    },
};
use strum_macros::Display;

use crate::{ebi_framework::displayable::Displayable, semantics::semantics::Semantics};

#[macro_export]
macro_rules! tree_semantics {
    ($t: ident) => {
        impl Semantics for $t {
            type SemState = NodeStates;

            fn get_initial_state(&self) -> Option<<Self as Semantics>::SemState> {
                if self.tree.is_empty() {
                    None
                } else {
                    let mut state = NodeStates {
                        states: vec![NodeState::Closed; self.get_number_of_nodes()],
                        terminated: false,
                    };
                    enable_node(self, &mut state, self.root());
                    Some(state)
                }
            }

            fn execute_transition(
                &self,
                state: &mut <Self as Semantics>::SemState,
                transition: TransitionIndex,
            ) -> Result<()> {
                if transition >= self.transition2node.len() {
                    state.terminated = true;
                    state.states.fill(NodeState::Closed);
                } else {
                    let node = self
                        .transition2node
                        .get(transition)
                        .ok_or_else(|| anyhow!("transition does not exist"))?;
                    start_node(self, state, *node, None);
                    // log::debug!("execute node {}", node);
                    close_node(self, state, *node);
                }
                // log::debug!("state after execution {}", state);
                Ok(())
            }

            fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool {
                state.terminated
            }

            fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
                // log::debug!("\t is transition silent {}", transition);
                if let Some(node) = self.transition2node.get(transition) {
                    match self.tree.get(*node) {
                        Some(Node::Tau) => true,
                        _ => false,
                    }
                } else {
                    true
                }
            }

            fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
                let node = self.transition2node.get(transition)?;
                match self.tree[*node] {
                    Node::Tau => None,
                    Node::Activity(activity) => Some(activity),
                    Node::Operator(_, _) => None,
                }
            }

            fn get_enabled_transitions(
                &self,
                state: &<Self as Semantics>::SemState,
            ) -> Vec<TransitionIndex> {
                let mut result = vec![];

                for (transition_index, node) in self.transition2node.iter().enumerate() {
                    if can_execute(self, state, *node) {
                        result.push(transition_index);
                    }
                }

                if !state.terminated && can_terminate(self, state, self.root()) {
                    result.push(self.transition2node.len());
                }

                result
            }

            fn get_number_of_transitions(&self) -> usize {
                self.tree.iter().filter(|node| node.is_leaf()).count() + 1 //the last transition is explicit termination, which is required by the semantics of Ebi
            }
        }
    };
}

#[macro_export]
macro_rules! tree_semantics_helpers {
    ($t:ident) => {
        /**
         * Start executing a node.
         */
        fn start_node(
            tree: &$t,
            state: &mut <$t as Semantics>::SemState,
            node: usize,
            child: Option<usize>,
        ) {
            if state[node] != NodeState::Started {
                // log::debug!("start node {} from child {:?}", node, child);
                state[node] = NodeState::Started;

                match tree.tree[node] {
                    Node::Tau => {}
                    Node::Activity(_) => {}
                    Node::Operator(Operator::Concurrent, _) => {}
                    Node::Operator(Operator::Interleaved, _) => {}
                    Node::Operator(Operator::Loop, _) => {}
                    Node::Operator(Operator::Or, _) => {}
                    Node::Operator(Operator::Sequence, _) => {}
                    Node::Operator(Operator::Xor, _) => {
                        //for an xor, the siblings of the child must be withdrawn
                        for child2 in tree.get_children(node) {
                            if let Some(child) = child {
                                if child2 != child {
                                    withdraw_enablement(tree, state, child2);
                                }
                            }
                        }
                    }
                }

                //recurse to parent
                if let Some((parent, _)) = tree.get_parent(node) {
                    start_node(tree, state, parent, Some(node));
                }
            }
        }

        fn withdraw_enablement(tree: &$t, state: &mut <$t as Semantics>::SemState, node: usize) {
            // log::debug!("withdraw enablement of node {}", node);
            for grandchild in node..tree.traverse(node) {
                state[grandchild] = NodeState::Closed;
            }
        }

        fn close_node(tree: &$t, state: &mut <$t as Semantics>::SemState, node: usize) {
            // log::debug!("close node {}", node);

            //close this node and all of its children
            for grandchild in node..tree.traverse(node) {
                state[grandchild] = NodeState::Closed;
            }

            //this may open another node, based on the operator of the parent
            if let Some((parent, child_rank)) = tree.get_parent(node) {
                match tree.tree[parent] {
                    Node::Tau => unreachable!(),
                    Node::Activity(_) => unreachable!(),
                    Node::Operator(Operator::Sequence, number_of_children) => {
                        //for a sequence parent, we enable the next child
                        // log::debug!("close node {}, parent is sequence node {}", node, parent);
                        if child_rank < number_of_children - 1 {
                            let next_child = tree.get_child(parent, child_rank + 1);
                            enable_node(tree, state, next_child);
                        } else {
                            //if there is no next child, we recurse on the parent
                            close_node(tree, state, parent);
                        }
                    }
                    Node::Operator(Operator::Concurrent, _)
                    | Node::Operator(Operator::Or, _)
                    | Node::Operator(Operator::Interleaved, _) => {
                        //for a concurrent or or parent, the parent can be closed if all of its children have been closed
                        if tree
                            .get_children(parent)
                            .all(|child| state[child] == NodeState::Closed)
                        {
                            //close the parent
                            close_node(tree, state, parent);
                        }
                    }
                    Node::Operator(Operator::Xor, _) => {
                        //for a xor parent, the parent can be closed as we executed one of its children
                        close_node(tree, state, parent);
                    }
                    Node::Operator(Operator::Loop, number_of_children) => {
                        //for a loop parent, we open the next child(ren)
                        if child_rank == 0 {
                            //enable the siblings
                            for child_rank in 1..number_of_children {
                                enable_node(tree, state, tree.get_child(parent, child_rank));
                            }
                        } else {
                            //enable the first child
                            enable_node(tree, state, tree.get_child(parent, 0));
                        }
                    }
                }
            }
        }

        fn enable_node(tree: &$t, state: &mut <$t as Semantics>::SemState, node: usize) {
            state[node] = NodeState::Enabled;

            match tree.tree[node] {
                Node::Tau => {}
                Node::Activity(_) => {}
                Node::Operator(Operator::Concurrent, _)
                | Node::Operator(Operator::Interleaved, _)
                | Node::Operator(Operator::Or, _)
                | Node::Operator(Operator::Xor, _) => {
                    //enable all children
                    for child in tree.get_children(node) {
                        enable_node(tree, state, child);
                    }
                }
                Node::Operator(Operator::Sequence, _) | Node::Operator(Operator::Loop, _) => {
                    //enable the first child
                    enable_node(tree, state, tree.get_child(node, 0));
                }
            }
        }

        pub(crate) fn can_execute(tree: &$t, state: &<$t as Semantics>::SemState, node: usize) -> bool {
            if let Some(NodeState::Closed) = state.get(node) {
                return false;
            }
            if let Some(NodeState::Started) = state.get(node) {
                return false;
            }

            //for every interleaved parent, check whether we're not executing two nodes concurrently
            let mut previous_parent = node;
            for (parent, _) in tree.get_parents(node) {
                if let Some(Node::Operator(Operator::Interleaved, _)) = tree.tree.get(parent) {
                    //count the number of started children
                    let started_children = tree.get_children(parent).fold(0, |count, child| {
                        if state[child] == NodeState::Started {
                            count + 1
                        } else {
                            count
                        }
                    });

                    if started_children == 0 {
                        //this is the first starting child; no problem
                    } else if started_children == 1 {
                        //there is already a child of this interleaved parent started
                        if state[previous_parent] != NodeState::Started {
                            //another child already started; this node cannot fire now
                            return false;
                        }
                    } else {
                        unreachable!()
                    }
                }

                previous_parent = parent;
            }

            true
        }

        /**
         * Returns whether it is possible to withdraw the enablement.
         */
        fn can_withdraw_enablement(
            _tree: &$t,
            state: &<$t as Semantics>::SemState,
            node: usize,
        ) -> bool {
            state[node] == NodeState::Enabled
        }

        /**
         * Returns whether it is possible that this node now terminates, or that a leaf has to be executed first.
         */
        pub(crate) fn can_terminate(tree: &$t, state: &<$t as Semantics>::SemState, node: usize) -> bool {
            match tree.tree[node] {
                Node::Tau => state[node] == NodeState::Closed,
                Node::Activity(_) => state[node] == NodeState::Closed,
                Node::Operator(Operator::Concurrent, _)
                | Node::Operator(Operator::Interleaved, _) => {
                    //these nodes can terminate if all of their children are either closed or can terminate
                    tree.get_children(node).all(|child| {
                        state[child] == NodeState::Closed || can_terminate(tree, state, child)
                    })
                }
                Node::Operator(Operator::Or, _) => {
                    //an or can terminate if at least one child has been closed, and the others can be withdrawn
                    let mut one_child_closed = false;
                    for child in tree.get_children(node) {
                        if let Some(NodeState::Closed) = state.get(child) {
                            one_child_closed = true;
                        } else if !can_withdraw_enablement(tree, state, child) {
                            //if there is one child that is not closed and not withdrawn, we cannot terminate the or
                            return false;
                        }
                    }

                    one_child_closed
                }
                Node::Operator(Operator::Loop, number_of_children) => {
                    let body_child = tree.get_child(node, 0);
                    if state[node] == NodeState::Closed {
                        //if the loop is closed, it can terminate
                        return true;
                    }
                    if state[body_child] == NodeState::Enabled {
                        //the first child is enabled, which means that the loop cannot terminate in this state
                        return false;
                    }

                    for child_rank in 1..number_of_children {
                        let redo_child = tree.get_child(node, child_rank);
                        //all the redo children must be able to withdraw enablement
                        if !can_withdraw_enablement(tree, state, redo_child) {
                            return false;
                        }
                    }

                    return true;
                }
                Node::Operator(Operator::Sequence, number_of_children) => {
                    //a sequence node can terminate if all its non-last children are closed and the last child can terminate
                    for child in 0..number_of_children - 1 {
                        if state[child] != NodeState::Closed {
                            return false;
                        }
                    }
                    can_terminate(tree, state, tree.get_child(node, number_of_children - 1))
                }
                Node::Operator(Operator::Xor, _) => {
                    //an xor can terminate if all of its children are closed or can terminate
                    tree.get_children(node).all(|child| {
                        state[child] == NodeState::Closed || can_terminate(tree, state, child)
                    })
                }
            }
        }
    };
}

tree_semantics!(ProcessTree);
tree_semantics_helpers!(ProcessTree);

#[derive(Clone, Display, Debug, Eq, PartialEq, Hash)]
pub enum NodeState {
    Enabled,
    Started,
    Closed,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct NodeStates {
    pub(crate) terminated: bool,
    pub(crate) states: Vec<NodeState>,
}

impl NodeStates {
    pub fn get(&self, index: usize) -> Option<&NodeState> {
        self.states.get(index)
    }
}

impl Displayable for NodeStates {}

impl Display for NodeStates {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.states)
    }
}

impl Index<usize> for NodeStates {
    type Output = NodeState;

    fn index(&self, index: usize) -> &Self::Output {
        self.states.index(index)
    }
}

impl IndexMut<usize> for NodeStates {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.states.index_mut(index)
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_arithmetic::{Fraction, One};
    use ebi_objects::{LabelledPetriNet, ProcessTree, StochasticProcessTree};

    use crate::{semantics::semantics::Semantics, stochastic_semantics::stochastic_semantics::StochasticSemantics};

    #[test]
    fn tree_semantics() {
        let fin = fs::read_to_string("testfiles/aa.ptree").unwrap();
        let tree = fin.parse::<ProcessTree>().unwrap();
        let mut state = tree.get_initial_state().unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), vec![0]);

        tree.execute_transition(&mut state, 0).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), vec![1]);

        tree.execute_transition(&mut state, 1).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), vec![2]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 2).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), Vec::<usize>::new());
        assert!(tree.is_final_state(&state));
    }

    #[test]
    fn tree_semantics_2() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.ptree").unwrap();
        let tree = fin.parse::<ProcessTree>().unwrap();
        let mut state = tree.get_initial_state().unwrap();
        assert_eq!(tree.get_enabled_transitions(&state), vec![0, 2]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 2).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), vec![3, 4]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 3).unwrap();
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 5).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), Vec::<usize>::new());
        assert!(tree.is_final_state(&state));
    }

    #[test]
    fn tree_semantics_3() {
        let fin = fs::read_to_string("testfiles/all_operators.ptree").unwrap();
        let tree = fin.parse::<ProcessTree>().unwrap();

        let mut state = tree.get_initial_state().unwrap();

        assert_eq!(
            tree.get_enabled_transitions(&state),
            vec![0, 2, 3, 5, 6, 8, 9]
        );
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 2).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), vec![3, 5, 6, 8, 9]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 3).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), vec![4, 5, 6, 8, 9]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 6).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), vec![4, 7, 8, 9]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 8).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), vec![4, 7, 9]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 7).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), vec![4, 5, 9]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 5).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), vec![4, 9, 10]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 4).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), vec![3, 9]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 3).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), vec![4, 9, 10]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 10).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), Vec::<usize>::new());
        assert!(tree.is_final_state(&state));
    }

    #[test]
    fn ptree_empty() {
        //without carriage return at first line
        let fin = fs::read_to_string("testfiles/empty.ptree").unwrap();
        let tree = fin.parse::<ProcessTree>().unwrap();
        assert!(tree.get_initial_state().is_none());

        //with carriage return at first line
        let fin = fs::read_to_string("testfiles/empty_2.ptree").unwrap();
        let tree = fin.parse::<ProcessTree>().unwrap();
        assert!(tree.get_initial_state().is_none());

        let _ = Into::<LabelledPetriNet>::into(tree);
    }

    #[test]
    fn stree() {
        let fin = fs::read_to_string("testfiles/seq(a-xor(b-c)).sptree").unwrap();
        let tree = fin.parse::<StochasticProcessTree>().unwrap();

        let mut state = tree.get_initial_state().unwrap();
        assert_eq!(tree.get_enabled_transitions(&state), vec![0]);
        assert!(!tree.is_final_state(&state));

        assert_eq!(
            tree.get_total_weight_of_enabled_transitions(&state)
                .unwrap(),
            Fraction::one()
        );
        assert_eq!(tree.get_transition_weight(&state, 0), &Fraction::one());

        assert!(tree.execute_transition(&mut state, 0).is_ok());

        assert_eq!(tree.get_enabled_transitions(&state), vec![1, 2]);
        assert!(!tree.is_final_state(&state));
        assert_eq!(
            tree.get_total_weight_of_enabled_transitions(&state)
                .unwrap(),
            Fraction::from(3)
        );

        assert!(tree.execute_transition(&mut state, 1).is_ok());

        assert_eq!(tree.get_enabled_transitions(&state), vec![3]);
        assert!(!tree.is_final_state(&state));
        assert_eq!(
            tree.get_total_weight_of_enabled_transitions(&state)
                .unwrap(),
            Fraction::from(4)
        );
        assert_eq!(tree.get_transition_weight(&state, 4), &Fraction::from(4));

        assert!(tree.execute_transition(&mut state, 4).is_ok());
        assert!(tree.is_final_state(&state));
        assert!(tree.get_enabled_transitions(&state).is_empty());
    }
}
