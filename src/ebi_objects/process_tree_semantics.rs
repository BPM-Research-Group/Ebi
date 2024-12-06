use std::{fmt::Display, ops::{Index, IndexMut}};

use anyhow::{Result, anyhow};
use strum_macros::Display;

use crate::{ebi_framework::activity_key::Activity, ebi_traits::{ebi_trait_semantics::Semantics, ebi_trait_stochastic_semantics::TransitionIndex}};

use super::process_tree::{Node, Operator, ProcessTree};

impl Semantics for ProcessTree {
    type SemState = NodeStates;

    fn get_initial_state(&self) -> <Self as Semantics>::SemState {
        let mut state = NodeStates { states: vec![NodeState::Closed; self.get_number_of_nodes()], terminated: self.tree.is_empty() };
        self.enable_node(&mut state, self.get_root());
        state
    }

    fn execute_transition(&self, state: &mut <Self as Semantics>::SemState, transition: TransitionIndex) -> Result<()> {
        if transition >= self.transition2node.len() {
            state.terminated = true
        } else {
            let node = self.transition2node.get(transition).ok_or_else(|| anyhow!("Transition does not exist."))?;
            self.start_node(state, *node, None);
            println!("execute node {}", node);
            self.close_node(state, *node);
        }
        Ok(())
    }

    fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool {
        state.terminated
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        todo!()
    }

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        todo!()
    }

    fn get_enabled_transitions(&self, state: &<Self as Semantics>::SemState) -> Vec<TransitionIndex> {
        let mut result = vec![];

        for (transition_index, node) in self.transition2node.iter().enumerate() {
            if state[*node] == NodeState::Enabled {
                result.push(transition_index);
            }
        }

        if self.can_terminate(state, self.get_root()) {
            result.push(self.transition2node.len());
        }

        result
    }

    fn get_number_of_transitions(&self) -> usize {
        self.tree.iter().filter(|node| node.is_leaf()).count() + 1 //the last transition is explicit termination, which is required by the semantics of Ebi
    }
}

impl ProcessTree {

    /**
     * Start executing a node.
     */
    fn start_node(&self, state: &mut <Self as Semantics>::SemState, node: usize, child: Option<usize>) {
        if state[node] != NodeState::Started {
            println!("start node {} from child {:?}", node, child);
            state[node] = NodeState::Started;

           match self.tree[node] {
                Node::Tau => {},
                Node::Activity(_) => {},
                Node::Operator(Operator::Concurrent, _) => {},
                Node::Operator(Operator::Interleaved, _) => {},
                Node::Operator(Operator::Loop, _) => {},
                Node::Operator(Operator::Or, _) => {},
                Node::Operator(Operator::Sequence, _) => {},
                Node::Operator(Operator::Xor, _) => {
                    //for an xor, the siblings of the child must be withdrawn
                    for child2 in self.get_children(node) {
                        if let Some(child) = child {
                            if child2 != child {
                                self.withdraw_enablement(state, child2);
                            }
                        }
                    }
                },
            }

            //recurse to parent
            if let Some((parent, _)) = self.get_parent(node) {
                self.start_node(state, parent, Some(node));
            }
        }
    }

    fn withdraw_enablement(&self, state: &mut <Self as Semantics>::SemState, node: usize) {
        println!("withdraw enablement of node {}", node);
        for grandchild in node..self.traverse(node) {
            state[grandchild] = NodeState::Closed;
        }
    }

    fn close_node(&self, state: &mut <Self as Semantics>::SemState, node: usize) {

        println!("close node {}", node);
        
        //close this node and all of its children
        for grandchild in node..self.traverse(node) {
            state[grandchild] = NodeState::Closed;
        }

        //this may open another node, based on the operator of the parent
        if let Some((parent, child_rank)) = self.get_parent(node) {
            match self.tree[parent] {
                Node::Tau => unreachable!(),
                Node::Activity(_) => unreachable!(),
                Node::Operator(Operator::Sequence, number_of_children) => {
                    //for a sequence parent, we enable the next child
                    println!("close node {}, parent is sequence node {}", node, parent);
                    if child_rank < number_of_children - 1 {
                        let next_child = self.get_child(parent, child_rank + 1);
                        self.enable_node(state, next_child);
                    } else {
                        //if there is no next child, we recurse on the parent
                        self.close_node(state, parent);
                    }
                },
                Node::Operator(Operator::Concurrent, _) | Node::Operator(Operator::Or, _) | Node::Operator(Operator::Interleaved, _) => {
                    //for a concurrent, or, or interleaved parent, the parent can be closed if all of its children have been closed
                    if self.get_children(parent).all(|child| state[child] == NodeState::Closed) {
                        //close the parent
                        self.close_node(state, parent);
                    }
                },
                Node::Operator(Operator::Xor, number_of_children) => {
                    //for a xor parent, the parent can be closed as we executed one of its children
                    self.close_node(state, parent);
                },
                Node::Operator(Operator::Loop, number_of_children) => {
                    //for a loop parent, we open the next child(ren)
                    if child_rank == 0 {
                        //enable the siblings
                        for child_rank in 1..number_of_children {
                            self.enable_node(state, self.get_child(parent, child_rank));
                        }
                    } else {
                        //enable the first child
                        self.enable_node(state, self.get_child(parent, 0));
                        self.enable_next(state, parent);
                    }
                },
            }
        }
    }

    fn enable_node(&self, state: &mut <Self as Semantics>::SemState, node: usize) {
        state[node] = NodeState::Enabled;

        match self.tree[node] {
            Node::Tau => {},
            Node::Activity(_) => {},
            Node::Operator(Operator::Concurrent, _) | Node::Operator(Operator::Interleaved, _) | Node::Operator(Operator::Or, _) | Node::Operator(Operator::Xor, _) => {
                //enable all children
                for child in self.get_children(node) {
                    self.enable_node(state, child);
                }
            },
            Node::Operator(Operator::Sequence, _) | Node::Operator(Operator::Loop, _) => {
                //enable the first child
                self.enable_node(state, self.get_child(node, 0));
            }
        }
    }

    /**
     * Enable the next node in the tree.
     */
    fn enable_next(&self, state: &mut <Self as Semantics>::SemState, node: usize) {
        todo!()
    }

    /**
     * Returns whether it is possible to withdraw the enablement. If the node is not enabled in the first place, returns true.
     */
    fn can_withdraw_enablement(&self, state: &<Self as Semantics>::SemState, node: usize) -> bool {
        state[node] == NodeState::Enabled
    }

    /**
     * Returns whether it is possible that this node now terminates, or that a leaf has to be executed first.
     */
    fn can_terminate(&self, state: &<Self as Semantics>::SemState, node: usize) -> bool {
        match self.tree[node] {
            Node::Tau => state[node] == NodeState::Closed,
            Node::Activity(_) => state[node] == NodeState::Closed,
            Node::Operator(Operator::Concurrent, number_of_children) => {
                todo!()
            },
            Node::Operator(Operator::Interleaved, number_of_children) => {
                todo!()
            },
            Node::Operator(Operator::Or, number_of_children) => {
                todo!()
            },
            Node::Operator(Operator::Loop, number_of_children) => {
                let body_child = self.get_child(node, 0);
                if state[node] == NodeState::Closed {
                    //if the loop is closed, it can terminate
                    return true;
                }
                if state[body_child] != NodeState::Closed {
                    //the first child is enabled, which means that the loop cannot terminate in this state
                    return false;
                }

                for child_rank in 1..number_of_children {
                    let redo_child = self.get_child(node, child_rank);
                    //all the redo children must be able to withdraw enablement
                    if !self.can_withdraw_enablement(state, redo_child) {
                        return false;
                    }
                }

                return true;
            },
            Node::Operator(Operator::Sequence, number_of_children) => {
                //a sequence node can terminate if all its non-last children are closed and the last child can terminate
                for child in 0..number_of_children - 1 {
                    if state[child] != NodeState::Closed {
                        return false;
                    }
                }
                self.can_terminate(state, self.get_child(node, number_of_children - 1))
            },
            Node::Operator(Operator::Xor, _) => {
                //an xor can terminate if all of its children are closed
                self.get_children(node).all(|child| state[child] == NodeState::Closed)
            },
        }
    }

    /**
     * Return whether a node was closed.
     */
    fn is_node_closed(&self, state: &mut <Self as Semantics>::SemState, node: usize) -> bool {
        match self.tree[node] {
            Node::Tau => state[node] == NodeState::Closed,
            Node::Activity(activity) => state[node] == NodeState::Closed,
            Node::Operator(operator, number_of_children) => {
                //an operator node is closed if all of its children are closed
                let mut child = self.get_child(node, 0);
                if state[child] != NodeState::Closed {
                    return false;
                }
                for _ in 1..number_of_children {
                    child = self.traverse(child);
                    if state[child] != NodeState::Closed {
                        return false;
                    }
                }
                return true;
            },
        }
    }
}

#[derive(Clone,Display,Debug,Eq,PartialEq,Hash)]
pub enum NodeState {
    Enabled,
    Started,
    Closed
}

#[derive(Clone,Debug,Eq,PartialEq,Hash)]
pub struct NodeStates {
    terminated: bool,
    states: Vec<NodeState>
}

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