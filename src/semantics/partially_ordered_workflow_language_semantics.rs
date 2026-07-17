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

    /// Strategy: do not perform any checking and just update the current node as requested.
    /// However, do not 
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
                //if our parent is a choice graph, reset all children
                if let Some((parent_index, _)) = self.get_parent(node_index)
                    && self.tree[parent_index].is_choice_graph()
                {
                    for child in (parent_index + 1)..self.traverse(parent_index) {
                        state[child] = NodeState::Enabled;
                    }
                }
                //start the current child
                state[node_index] = NodeState::Started;
            }
            TransitionType::Execute => {
                state[node_index] = NodeState::Executed;
                //reset all children
                if !node_type.is_activity() {
                    for child in (node_index + 1)..self.traverse(node_index) {
                        state[child] = NodeState::Enabled;
                    }
                }
            }
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

        //started
        (
            PowlNode::Activity {
                skippable: false, ..
            }
            | PowlNode::PartialOrder {
                skippable: false, ..
            }
            | PowlNode::ChoiceGraph {
                skippable: false, ..
            },
            NodeState::Started,
        ) => {
            //once or one-or-more
            vec![t_execute]
        }
        (
            PowlNode::Activity {
                skippable: true, ..
            }
            | PowlNode::PartialOrder {
                skippable: true, ..
            }
            | PowlNode::ChoiceGraph {
                skippable: true, ..
            },
            NodeState::Started,
        ) => {
            //zero-or-one or zero-or-more
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
            //zero-or-more or one-or-more activity
            vec![t_execute, t_close]
        }

        //partial order executed
        (
            PowlNode::PartialOrder {
                repeatable,
                number_of_children,
                edges,
                ..
            },
            NodeState::Executed,
        ) => {
            //zero-or-one or zero-or-more
            enabled_transitions_partial_order_execute(
                powl,
                state,
                node_index,
                *repeatable,
                *number_of_children,
                edges,
            )
        }

        //choice graph executed
        (
            PowlNode::ChoiceGraph {
                repeatable,
                edges,
                start_children,
                end_children,
                ..
            },
            NodeState::Executed,
        ) => {
            //zero-or-one or zero-or-more
            enabled_transitions_choice_graph_execute(
                powl,
                state,
                node_index,
                *repeatable,
                edges,
                start_children,
                end_children,
            )
        }
    }
}

//execute a choice graph
fn enabled_transitions_choice_graph_execute(
    powl: &PartiallyOrderedWorkflowLanguage,
    state: &TreeMarking,
    node_index: usize,
    repeatable: bool,
    edges: &[(usize, usize)],
    start_children: &[usize],
    end_children: &[usize],
) -> Vec<usize> {
    let t_execute = node_index * 3 + 1;
    let t_close = node_index * 3 + 2;

    //if any child has started execution, allow it to continue (there can only be one such child in a choice graph)
    for child_index in powl.get_children(node_index) {
        if state.states[child_index] == NodeState::Started
            || state.states[child_index] == NodeState::Executed
        {
            return enabled_transitions(powl, state, child_index);
        }
    }

    //if no child has started yet, we can start the choice graph
    if powl
        .get_children(node_index)
        .all(|child_index| state.states[child_index] == NodeState::Enabled)
    {
        //start the choice graph
        return start_children
            .iter()
            .map(|child_rank| powl.get_child(node_index, *child_rank) * 3)
            .collect();
    }

    let mut result = vec![];
    if end_children
        .iter()
        .map(|child_rank| powl.get_child(node_index, *child_rank))
        .any(|child_index| state.states[child_index] == NodeState::Closed)
    {
        result.push(t_close);
        // one end child is closed; we can close this node
        if repeatable {
            // if it is repeatable, we can execute it again
            result.push(t_execute)
        }
    }

    //a node with an incoming edge from a closed node can start
    for (source_rank, target_rank) in edges {
        let source = powl.get_child(node_index, *source_rank);
        let target = powl.get_child(node_index, *target_rank);

        if state.states[source] == NodeState::Closed && state.states[target] == NodeState::Enabled {
            //source is closed, which means that target can start
            result.push(target * 3);
        }
    }

    result
}

//execute a partial order
fn enabled_transitions_partial_order_execute(
    powl: &PartiallyOrderedWorkflowLanguage,
    state: &TreeMarking,
    node_index: usize,
    repeatable: bool,
    number_of_children: usize,
    edges: &Vec<(usize, usize)>,
) -> Vec<usize> {
    let t_execute = node_index * 3 + 1;
    let t_close = node_index * 3 + 2;

    if powl
        .get_children(node_index)
        .all(|child_index| state[child_index] == NodeState::Closed)
    {
        // all children are closed; we can close (or repeat) this node
        if repeatable {
            return vec![t_execute, t_close];
        } else {
            return vec![t_close];
        }
    }

    let mut result = vec![];

    //gather for each child whether all predecessors are closed
    let mut child_rank_2_all_predecessors_closed = vec![true; number_of_children];
    for (source_rank, target_rank) in edges {
        let source_index = powl.get_child(node_index, *source_rank);
        if state.states[source_index] != NodeState::Closed {
            child_rank_2_all_predecessors_closed[*target_rank] = false;
        }
    }

    for (child_rank, all_predecessors_closed) in
        child_rank_2_all_predecessors_closed.into_iter().enumerate()
    {
        if all_predecessors_closed {
            let child_index = powl.get_child(node_index, child_rank);
            result.extend(enabled_transitions(powl, state, child_index));
        }
    }

    result
}

#[cfg(test)]

mod tests {
    use crate::semantics::semantics::Semantics;
    use ebi_objects::{
        HasActivityKey,
        ebi_objects::partially_ordered_workflow_language::PartiallyOrderedWorkflowLanguage,
    };
    use std::fs;

    #[test]
    fn powl_sem_a() {
        let fin = fs::read_to_string("testfiles/a.powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        let mut state = powl.get_initial_state().unwrap();
        let a = Some(powl.activity_key().process_activity_attempt("a").unwrap());

        assert_eq!(powl.get_enabled_transitions(&state), vec![0]);
        assert!(powl.is_transition_silent(0, &state));

        powl.execute_transition(&mut state, 0).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![1]);
        assert_eq!(powl.get_transition_activity(1, &state), a);
        assert!(!powl.is_final_state(&state));

        powl.execute_transition(&mut state, 1).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![2]);
        assert!(powl.is_transition_silent(2, &state));
        assert!(!powl.is_final_state(&state));

        powl.execute_transition(&mut state, 2).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), Vec::<usize>::new());
        assert!(powl.is_final_state(&state));
    }

    #[test]
    fn powl_sem_a_skippable() {
        let fin = fs::read_to_string("testfiles/a_skippable.powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        let mut state = powl.get_initial_state().unwrap();
        let a = Some(powl.activity_key().process_activity_attempt("a").unwrap());

        assert_eq!(powl.get_enabled_transitions(&state), vec![0]);
        assert!(powl.is_transition_silent(0, &state));

        powl.execute_transition(&mut state, 0).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![1, 2]);
        assert_eq!(powl.get_transition_activity(1, &state), a);
        assert!(!powl.is_final_state(&state));

        powl.execute_transition(&mut state, 1).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![2]);
        assert!(powl.is_transition_silent(2, &state));
        assert!(!powl.is_final_state(&state));

        powl.execute_transition(&mut state, 2).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), Vec::<usize>::new());
        assert!(powl.is_final_state(&state));
    }

    #[test]
    fn powl_sem_or_a_b() {
        let fin = fs::read_to_string("testfiles/or_a_b.powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        let mut state = powl.get_initial_state().unwrap();
        let b = Some(powl.activity_key().process_activity_attempt("b").unwrap());

        assert_eq!(powl.get_enabled_transitions(&state), vec![0]);
        assert!(powl.is_transition_silent(0, &state));

        //start choice
        powl.execute_transition(&mut state, 0).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![1]);
        assert!(powl.is_transition_silent(1, &state));
        assert!(!powl.is_final_state(&state));

        //execute choice
        powl.execute_transition(&mut state, 1).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![3, 6]);
        assert!(powl.is_transition_silent(3, &state));
        assert!(powl.is_transition_silent(6, &state));
        assert!(!powl.is_final_state(&state));

        //start executing b
        powl.execute_transition(&mut state, 6).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![7]);
        assert_eq!(powl.get_transition_activity(7, &state), b);
        assert!(!powl.is_final_state(&state));

        //execute b
        powl.execute_transition(&mut state, 7).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![8]);
        assert!(powl.is_transition_silent(8, &state));
        assert!(!powl.is_final_state(&state));

        //close b
        powl.execute_transition(&mut state, 8).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![2]);
        assert!(powl.is_transition_silent(2, &state));
        assert!(!powl.is_final_state(&state));

        //close or
        powl.execute_transition(&mut state, 2).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), Vec::<usize>::new());
        assert!(powl.is_final_state(&state));
    }

    #[test]
    fn powl_sem_and_a_b() {
        let fin = fs::read_to_string("testfiles/and_a_b.powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        let mut state = powl.get_initial_state().unwrap();
        let a = Some(powl.activity_key().process_activity_attempt("a").unwrap());
        let b = Some(powl.activity_key().process_activity_attempt("b").unwrap());

        assert_eq!(powl.get_enabled_transitions(&state), vec![0]);
        assert!(powl.is_transition_silent(0, &state));

        //start and
        powl.execute_transition(&mut state, 0).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![1]);
        assert!(powl.is_transition_silent(1, &state));
        assert!(!powl.is_final_state(&state));

        //execute and
        powl.execute_transition(&mut state, 1).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![3]);
        assert!(powl.is_transition_silent(3, &state));
        assert!(!powl.is_final_state(&state));

        //start a
        powl.execute_transition(&mut state, 3).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![4]);
        assert_eq!(powl.get_transition_activity(4, &state), a);
        assert!(!powl.is_final_state(&state));

        //execute a
        powl.execute_transition(&mut state, 4).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![5]);
        assert!(powl.is_transition_silent(5, &state));
        assert!(!powl.is_final_state(&state));

        //close a
        powl.execute_transition(&mut state, 5).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![6]);
        assert!(powl.is_transition_silent(6, &state));
        assert!(!powl.is_final_state(&state));

        //start b
        powl.execute_transition(&mut state, 6).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![7]);
        assert_eq!(powl.get_transition_activity(7, &state), b);
        assert!(!powl.is_final_state(&state));

        //execute b
        powl.execute_transition(&mut state, 7).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![8]);
        assert!(powl.is_transition_silent(8, &state));
        assert!(!powl.is_final_state(&state));

        //close b
        powl.execute_transition(&mut state, 8).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![2]);
        assert!(powl.is_transition_silent(2, &state));
        assert!(!powl.is_final_state(&state));

        //close and
        powl.execute_transition(&mut state, 2).unwrap();

        assert!(powl.is_final_state(&state));
    }

    #[test]
    fn powl_sem_loop_a_b() {
        let fin = fs::read_to_string("testfiles/loop(a,b).powl").unwrap();
        let powl = fin.parse::<PartiallyOrderedWorkflowLanguage>().unwrap();

        let mut state = powl.get_initial_state().unwrap();
        let a = Some(powl.activity_key().process_activity_attempt("a").unwrap());
        let b = Some(powl.activity_key().process_activity_attempt("b").unwrap());

        assert_eq!(powl.get_enabled_transitions(&state), vec![0]);
        assert!(powl.is_transition_silent(0, &state));

        //start or
        powl.execute_transition(&mut state, 0).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![1]);
        assert!(powl.is_transition_silent(1, &state));
        assert!(!powl.is_final_state(&state));

        //execute or
        powl.execute_transition(&mut state, 1).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![3]);
        assert!(powl.is_transition_silent(3, &state));
        assert!(!powl.is_final_state(&state));

        //start a
        powl.execute_transition(&mut state, 3).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![4]);
        assert_eq!(powl.get_transition_activity(4, &state), a);
        assert!(!powl.is_final_state(&state));

        //execute a
        powl.execute_transition(&mut state, 4).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![5]);
        assert!(powl.is_transition_silent(5, &state));
        assert!(!powl.is_final_state(&state));

        //close a
        powl.execute_transition(&mut state, 5).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![2, 6]);
        assert!(!powl.is_final_state(&state));
        assert!(powl.is_transition_silent(6, &state));

        //start b
        powl.execute_transition(&mut state, 6).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![7]);
        assert!(!powl.is_final_state(&state));
        assert_eq!(powl.get_transition_activity(7, &state), b);

        //execute b
        powl.execute_transition(&mut state, 7).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![8]);
        assert!(!powl.is_final_state(&state));
        assert!(powl.is_transition_silent(8, &state));

        //close b
        powl.execute_transition(&mut state, 8).unwrap();

        assert_eq!(powl.get_enabled_transitions(&state), vec![3]);
        assert!(!powl.is_final_state(&state));
        assert!(powl.is_transition_silent(3, &state));

        //execute a
        powl.execute_transition(&mut state, 3).unwrap();

    }
}
