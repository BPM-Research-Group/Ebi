use crate::semantics::semantics::Semantics;
use ebi_objects::{
    Activity,
    anyhow::{Result, anyhow},
    ebi_objects::{
        labelled_petri_net::TransitionIndex,
        partially_ordered_workflow_language::{PartiallyOrderedWorkflowLanguage, PowlNode},
        process_tree::{NodeState, TreeMarking},
    },
};
use strum_macros::EnumIs;

/// In the space of transitions, every node has three transitions:
/// - one silent that starts the node,
/// - one that executes the node, and
/// - one silent that finishes it.
impl Semantics for PartiallyOrderedWorkflowLanguage {
    type SemState = TreeMarking;

    fn get_initial_state(&self) -> Option<<Self as Semantics>::SemState> {
        if self.tree.is_empty() {
            None
        } else {
            let mut marking = TreeMarking::new(self.number_of_nodes());
            marking
                .states
                .iter_mut()
                .for_each(|x| *x = NodeState::Enabled);
            Some(marking)
        }
    }

    fn execute_transition(
        &self,
        state: &mut <Self as Semantics>::SemState,
        transition: TransitionIndex,
    ) -> Result<()> {
        let (node_index, node_type, transition_type, _, _, _) =
            transition_2_node_type(self, transition)
                .ok_or_else(|| anyhow!("Transition not found."))?;
        match transition_type {
            TransitionType::Start => {
                state[node_index] = NodeState::Started;
                //reset all children
                if !node_type.is_activity() {
                    for child in (node_index + 1)..self.traverse(node_index) {
                        state[child] = NodeState::Enabled;
                    }
                }
            }
            TransitionType::Execute => state[node_index] = NodeState::Executed,
            TransitionType::End => {
                state[node_index] = NodeState::Closed;
                //close all children
                if !node_type.is_activity() {
                    for child in (node_index + 1)..self.traverse(node_index) {
                        state[child] = NodeState::Closed;
                    }
                }
            }
        }
        Ok(())
    }

    fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool {
        if state.states.is_empty() {
            false
        } else {
            state.states[0].is_closed()
        }
    }

    fn is_transition_silent(
        &self,
        transition: TransitionIndex,
        _state: &<Self as Semantics>::SemState,
    ) -> bool {
        if let Some((_, _, transition_type, _, _, activity)) =
            transition_2_node_type(self, transition)
        {
            !(activity.is_some() && transition_type.is_execute())
        } else {
            false
        }
    }

    fn get_transition_activity(
        &self,
        transition: TransitionIndex,
        _state: &<Self as Semantics>::SemState,
    ) -> Option<Activity> {
        let (_, node_type, transition_type, _, _, activity) =
            transition_2_node_type(self, transition)?;
        if transition_type.is_execute() {
            match node_type {
                NodeType::Activity => activity,
                NodeType::PartialOrder | NodeType::ChoiceGraph => None,
            }
        } else {
            None
        }
    }

    fn get_enabled_transitions(
        &self,
        state: &<Self as Semantics>::SemState,
    ) -> Vec<TransitionIndex> {
        enabled_transitions(self, state, 0)
    }
}

#[derive(EnumIs)]
enum NodeType {
    Activity,
    PartialOrder,
    ChoiceGraph,
}

#[derive(EnumIs)]
enum TransitionType {
    Start,
    Execute,
    End,
}

fn transition_2_node_type(
    powl: &PartiallyOrderedWorkflowLanguage,
    transition: usize,
) -> Option<(
    usize,
    NodeType,
    TransitionType,
    bool,
    bool,
    Option<Activity>,
)> {
    let node_index = transition / 3;
    let node = powl.tree.get(node_index)?;
    let transition_type = match transition % 3 {
        0 => TransitionType::Start,
        1 => TransitionType::Execute,
        _ => TransitionType::End,
    };

    Some(match node {
        PowlNode::Activity {
            skippable,
            repeatable,
            activity,
            ..
        } => (
            node_index,
            NodeType::Activity,
            transition_type,
            *skippable,
            *repeatable,
            *activity,
        ),
        PowlNode::PartialOrder {
            skippable,
            repeatable,
            ..
        } => (
            node_index,
            NodeType::PartialOrder,
            transition_type,
            *skippable,
            *repeatable,
            None,
        ),
        PowlNode::ChoiceGraph {
            skippable,
            repeatable,
            ..
        } => (
            node_index,
            NodeType::ChoiceGraph,
            transition_type,
            *skippable,
            *repeatable,
            None,
        ),
    })
}

fn enabled_transitions(
    powl: &PartiallyOrderedWorkflowLanguage,
    state: &TreeMarking,
    node_index: usize,
) -> Vec<TransitionIndex> {
    if node_index >= state.states.len() {
        return vec![];
    }

    let t_start = node_index * 3;
    let t_execute = node_index * 3 + 1;
    let t_close = node_index * 3 + 2;

    match (&powl.tree[node_index], state.states[node_index]) {
        (_, NodeState::Enabled) => {
            //given that we are here in the recursion, we can start the node, which means that we will not recurse further
            vec![t_start]
        }
        (_, NodeState::Closed) => {
            //this node is closed and there's nothing to execute
            vec![]
        }
        //activity started
        (
            PowlNode::Activity {
                skippable: false,
                ..
            },
            NodeState::Started,
        ) => {
            //once activity
            vec![t_execute]
        }
        (
            PowlNode::Activity {
                skippable: true, ..
            },
            NodeState::Started,
        ) => {
            //zero-or-once or zero-or-more activity
            vec![t_execute, t_close]
        }
        //activity executed
        (
            PowlNode::Activity {
                repeatable: false, ..
            },
            NodeState::Executed,
        ) => {
            //once or zero-or-once activity
            vec![t_close]
        }
        (
            PowlNode::Activity {
                repeatable: true, ..
            },
            NodeState::Executed,
        ) => {
            //zero-or-more activity
            vec![t_execute, t_close]
        }
        _ => todo!()
    }
}
