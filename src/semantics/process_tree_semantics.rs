use crate::semantics::semantics::Semantics;
use anyhow::Result;
use ebi_objects::{
    Activity, ProcessTree,
    ebi_objects::{
        labelled_petri_net::TransitionIndex,
        process_tree::{
            TreeMarking, execute_transition, get_enabled_transitions, get_initial_state,
            get_number_of_transitions, get_transition_activity, is_final_state,
        },
    },
};

impl Semantics for ProcessTree {
    type SemState = TreeMarking;

    fn get_initial_state(&self) -> Option<<Self as Semantics>::SemState> {
        get_initial_state(self)
    }

    fn execute_transition(
        &self,
        state: &mut <Self as Semantics>::SemState,
        transition: TransitionIndex,
    ) -> Result<()> {
        execute_transition(self, state, transition)
    }

    fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool {
        is_final_state(self, state)
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        self.get_transition_activity(transition).is_none()
    }

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        get_transition_activity(self, transition)
    }

    fn get_enabled_transitions(
        &self,
        state: &<Self as Semantics>::SemState,
    ) -> Vec<TransitionIndex> {
        get_enabled_transitions(self, state)
    }

    fn get_number_of_transitions(&self) -> usize {
        get_number_of_transitions(self)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        semantics::semantics::Semantics,
        stochastic_semantics::stochastic_semantics::StochasticSemantics,
    };
    use ebi_objects::{
        LabelledPetriNet, ProcessTree, StochasticProcessTree,
        ebi_arithmetic::{Fraction, One},
    };
    use std::fs;

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
