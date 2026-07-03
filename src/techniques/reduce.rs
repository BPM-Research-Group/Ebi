pub trait ReduceLanguageEquivalently {
    fn reduce_language_equivalently(&mut self);
}

mod tree {
    use crate::techniques::{
        empty_traces::has_empty_traces_node, reduce::ReduceLanguageEquivalently,
    };
    use ebi_objects::{
        ProcessTree,
        ebi_objects::process_tree::{Node, Operator},
    };

    impl ReduceLanguageEquivalently for ProcessTree {
        fn reduce_language_equivalently(&mut self) {
            let rules: Vec<Box<dyn TreeRule>> = vec![
                Box::new(Singularity),
                Box::new(Associativity),
                Box::new(Tau),
                Box::new(ImplicitConcurrent),
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
                    //option 1: diect tau
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

                    //option 2: tau-under-xor
                    for child in tree.get_children(node) {
                        if let Some(number_of_children) = tree.get_number_of_children(child)
                            && number_of_children >= 2
                            && tree.tree[child].is_operator_xor()
                        {
                            //find a grandchild tau
                            if let Some(grandchild_tau) = tree
                                .get_children(child)
                                .find(|grandchild| tree.tree[*grandchild].is_tau())
                            {
                                //remove the grandchild
                                tree.tree.remove(grandchild_tau);
                                _ = tree.tree[child].set_number_of_children(number_of_children - 1);

                                //lift the or(..) to xor(tau, or(..))
                                tree.tree.insert(node, Node::Tau);
                                tree.tree.insert(node, Node::Operator(Operator::Xor, 2));
                                return true;
                            }
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

    struct ImplicitConcurrent;

    impl TreeRule for ImplicitConcurrent {
        fn apply(&self, tree: &mut ProcessTree, node: usize) -> bool {
            match tree.tree[node] {
                Node::Tau => false,
                Node::Activity(_) => false,
                Node::Operator(Operator::Sequence, _)
                | Node::Operator(Operator::Loop, _)
                | Node::Operator(Operator::Xor, _)
                | Node::Operator(Operator::Or, _) => false,
                Node::Operator(Operator::Interleaved, number_of_children) => {
                    if tree
                        .get_children(node)
                        .all(|child| max_trace_length(tree, child) <= 1)
                    {
                        tree.tree[node] = Node::Operator(Operator::Concurrent, number_of_children);
                        return true;
                    }
                    false
                }
                Node::Operator(Operator::Concurrent, number_of_children) => {
                    if let Some((i, child1)) = tree
                        .get_children(node)
                        .enumerate()
                        .find(|(_, child)| has_empty_traces_node(tree, *child))
                    {
                        if let Some(child2) = tree
                            .get_children(node)
                            .skip(i + 1)
                            .find(|child| has_empty_traces_node(tree, *child))
                        {
                            //remove child2
                            let end2 = tree.traverse(child2);
                            let tchild2 = tree.tree.drain(child2..end2).collect::<Vec<_>>();

                            //remove child1
                            let end1 = tree.traverse(child1);
                            let tchild1 = tree.tree.drain(child1..end1).collect::<Vec<_>>();

                            //insert child2
                            tree.tree.splice((node + 1)..(node + 1), tchild2);

                            //insert child1
                            tree.tree.splice((node + 1)..(node + 1), tchild1);

                            //insert or
                            tree.tree.insert(node + 1, Node::Operator(Operator::Or, 2));

                            //change number of children
                            _ = tree.tree[node].set_number_of_children(number_of_children - 1);

                            return true;
                        }
                    }
                    false
                }
            }
        }
    }

    /// Return the maximum trace length. Returns a value >= 100 for loops.
    fn max_trace_length(tree: &ProcessTree, node: usize) -> usize {
        match tree.tree[node] {
            Node::Tau => 0,
            Node::Activity(_) => 1,
            Node::Operator(Operator::Xor, _) => tree
                .get_children(node)
                .map(|child| max_trace_length(tree, child))
                .max()
                .unwrap(),
            Node::Operator(Operator::Sequence, _)
            | Node::Operator(Operator::Concurrent, _)
            | Node::Operator(Operator::Interleaved, _)
            | Node::Operator(Operator::Or, _) => tree
                .get_children(node)
                .map(|child| max_trace_length(tree, child))
                .sum(),
            Node::Operator(Operator::Loop, _) => {
                if tree
                    .get_children(node)
                    .map(|child| max_trace_length(tree, child))
                    .sum::<usize>()
                    > 0
                {
                    100
                } else {
                    0
                }
            }
        }
    }
}

mod labelled_petri_net {
    use crate::techniques::reduce::{ReduceLanguageEquivalently, single_unit_arc};
    use ebi_objects::{LabelledPetriNet, ebi_objects::labelled_petri_net::TransitionIndex};

    impl ReduceLanguageEquivalently for LabelledPetriNet {
        fn reduce_language_equivalently(&mut self) {
            let place_rules: Vec<Box<dyn LpnPlaceRule>> = vec![
                Box::new(FusionOfSeriesPlaces),
                Box::new(FusionOfSeriesTransitions),
                Box::new(FusionOfParallelTransitions),
                Box::new(EliminationOfSelfLoopPlaces),
            ];
            let transition_rules: Vec<Box<dyn LpnTransitionRule>> = vec![
                Box::new(EliminationOfSelfLoopTransitions),
                Box::new(FusionOfParallelPlaces),
            ];
            'outer: loop {
                for place in 0..self.get_number_of_places() {
                    for rule in &place_rules {
                        if rule.apply(self, place).is_some() {
                            continue 'outer;
                        }
                    }
                }

                for transition in 0..self.get_number_of_transitions() {
                    for rule in &transition_rules {
                        if rule.apply(self, transition).is_some() {
                            continue 'outer;
                        }
                    }
                }

                return;
            }
        }
    }

    trait LpnPlaceRule {
        /// Apply the rule to the place; returns whether the rule changed the net.
        fn apply(&self, lpn: &mut LabelledPetriNet, place: usize) -> Option<()>;
    }

    trait LpnTransitionRule {
        /// Apply the rule to the transition; returns whether the rule changed the net.
        fn apply(&self, lpn: &mut LabelledPetriNet, transition: TransitionIndex) -> Option<()>;
    }

    struct FusionOfSeriesPlaces;

    impl LpnPlaceRule for FusionOfSeriesPlaces {
        fn apply(&self, lpn: &mut LabelledPetriNet, place: usize) -> Option<()> {
            //the place must have only one outgoing transition
            let transition = single_unit_arc(lpn.get_outgoing_transitions(place))?;

            //that transition must be silent
            if !lpn.is_transition_silent(transition) {
                return None;
            }

            //the transition must have one outgoing place
            let place_2 = single_unit_arc(lpn.get_outgoing_places(transition))?;

            //rule applies
            log::info!("fusion of series places");

            //move intial marking from place to place_2
            if let Some(marking) = &mut lpn.initial_marking {
                marking.increase(place_2, marking.get(place)).unwrap();
            }

            //copy incoming arcs of place to place_2
            let incoming = lpn
                .get_incoming_transitions(place)
                .unwrap()
                .collect::<Vec<_>>();
            for (transition_2, cardinality) in incoming {
                lpn.add_transition_place_arc(transition_2, place_2, cardinality)
                    .unwrap();
            }

            //remove transition (will also remove its arcs)
            lpn.remove_transition(transition).unwrap();

            //remove place (will also remove its arcs)
            lpn.remove_place(place).unwrap();

            Some(())
        }
    }

    struct FusionOfSeriesTransitions;

    impl LpnPlaceRule for FusionOfSeriesTransitions {
        fn apply(&self, lpn: &mut LabelledPetriNet, place: usize) -> Option<()> {
            //the place must have one incoming transition with cardinality 1
            let transition = single_unit_arc(lpn.get_incoming_transitions(place))?;

            //the place must have one outgoing transition with cardinality 1
            let transition_2 = single_unit_arc(lpn.get_outgoing_transitions(place))?;

            //place is not marked
            if let Some(marking) = &lpn.initial_marking {
                if marking.get(place) != 0 {
                    return None;
                }
            }

            //transition 2 must be silent
            if !lpn.is_transition_silent(transition_2) {
                return None;
            }

            //transition 2 must not have any other incoming arcs
            if lpn.get_incoming_places(transition_2).unwrap().count() != 1 {
                return None;
            }

            //rule applies
            log::info!("fusion of series transitions");

            //copy outgoing arcs of transition_2 to transition
            let outgoing = lpn
                .get_outgoing_places(transition_2)
                .unwrap()
                .collect::<Vec<_>>();
            for (place_2, cardinality) in outgoing {
                lpn.add_transition_place_arc(transition, place_2, cardinality)
                    .unwrap();
            }

            //remove place
            lpn.remove_place(place).unwrap();

            //remove transition 2
            lpn.remove_transition(transition_2).unwrap();

            Some(())
        }
    }

    struct FusionOfParallelTransitions;

    impl LpnPlaceRule for FusionOfParallelTransitions {
        fn apply(&self, lpn: &mut LabelledPetriNet, place: usize) -> Option<()> {
            for (transition_1, cardinality_1) in lpn
                .get_incoming_transitions(place)
                .unwrap()
                .collect::<Vec<_>>()
            {
                for (transition_2, cardinality_2) in lpn
                    .get_incoming_transitions(place)
                    .unwrap()
                    .collect::<Vec<_>>()
                {
                    if transition_1 != transition_2 && cardinality_1 == cardinality_2 {
                        //transitions need to have the same label
                        if lpn.get_transition_label(transition_1)
                            != lpn.get_transition_label(transition_2)
                        {
                            break;
                        }

                        //transitions must have the same input arcs
                        let mut in_1 = lpn
                            .get_incoming_places(transition_1)
                            .unwrap()
                            .collect::<Vec<_>>();
                        in_1.sort();
                        let mut in_2 = lpn
                            .get_incoming_places(transition_2)
                            .unwrap()
                            .collect::<Vec<_>>();
                        in_2.sort();
                        if in_1 != in_2 {
                            break;
                        }

                        //transitions must have the same output arcs
                        let mut out_1 = lpn
                            .get_outgoing_places(transition_1)
                            .unwrap()
                            .collect::<Vec<_>>();
                        out_1.sort();
                        let mut out_2 = lpn
                            .get_outgoing_places(transition_2)
                            .unwrap()
                            .collect::<Vec<_>>();
                        out_2.sort();
                        if out_1 != out_2 {
                            break;
                        }

                        //rule applies
                        lpn.remove_transition(transition_2).unwrap();

                        return Some(());
                    }
                }
            }

            None
        }
    }

    struct EliminationOfSelfLoopPlaces;

    impl LpnPlaceRule for EliminationOfSelfLoopPlaces {
        fn apply(&self, lpn: &mut LabelledPetriNet, place: usize) -> Option<()> {
            //the place is marked
            if let Some(marking) = &lpn.initial_marking {
                if marking.get(place) == 0 {
                    return None;
                }
            } else {
                return None;
            }

            let mut in_ = lpn
                .get_incoming_transitions(place)
                .unwrap()
                .collect::<Vec<_>>();
            in_.sort();

            let mut out_ = lpn
                .get_outgoing_transitions(place)
                .unwrap()
                .collect::<Vec<_>>();
            out_.sort();

            //incoming transitions must be equal to outgoing transitions
            if in_ != out_ {
                return None;
            }

            //rule applies

            lpn.remove_place(place).unwrap();

            Some(())
        }
    }

    struct EliminationOfSelfLoopTransitions;

    impl LpnTransitionRule for EliminationOfSelfLoopTransitions {
        fn apply(&self, lpn: &mut LabelledPetriNet, transition: TransitionIndex) -> Option<()> {
            //the transition is silent
            if !lpn.is_transition_silent(transition) {
                return None;
            }

            let mut in_ = lpn
                .get_incoming_places(transition)
                .unwrap()
                .collect::<Vec<_>>();
            in_.sort();

            let mut out_ = lpn
                .get_outgoing_places(transition)
                .unwrap()
                .collect::<Vec<_>>();
            out_.sort();

            //incoming places must be equal to outgoing places
            if in_ != out_ {
                return None;
            }

            //rule applies

            lpn.remove_transition(transition).unwrap();

            Some(())
        }
    }

    struct FusionOfParallelPlaces;

    impl LpnTransitionRule for FusionOfParallelPlaces {
        fn apply(&self, lpn: &mut LabelledPetriNet, transition: TransitionIndex) -> Option<()> {
            for (place_1, _) in lpn.get_outgoing_places(transition).unwrap().collect::<Vec<_>>() {
                for (place_2, _) in lpn.get_outgoing_places(transition).unwrap().collect::<Vec<_>>() {
                    //check pairs only twice
                    if place_1 <= place_2 {
                        continue;
                    }

                    //places have the same marking
                    if let Some(marking) = &lpn.initial_marking
                        && marking.get(place_1) != marking.get(place_2)
                    {
                        continue;
                    }

                    //places have the same incoming arcs
                    let mut in_1 = lpn
                        .get_incoming_transitions(place_1)
                        .unwrap()
                        .collect::<Vec<_>>();
                    in_1.sort();

                    let mut in_2 = lpn
                        .get_incoming_transitions(place_2)
                        .unwrap()
                        .collect::<Vec<_>>();
                    in_2.sort();

                    if in_1 != in_2 {
                        continue;
                    }

                    //places have the same outgoing arcs
                    let mut out_1 = lpn
                        .get_outgoing_transitions(place_1)
                        .unwrap()
                        .collect::<Vec<_>>();
                    out_1.sort();

                    let mut out_2 = lpn
                        .get_outgoing_transitions(place_2)
                        .unwrap()
                        .collect::<Vec<_>>();
                    out_2.sort();

                    if out_1 != out_2 {
                        continue;
                    }

                    //rule applies

                    lpn.remove_place(place_2).unwrap();

                    return Some(());
                }
            }

            None
        }
    }
}

pub fn single_unit_arc(it: Option<impl Iterator<Item = (usize, u64)>>) -> Option<usize> {
    let mut it = it?;
    if let Some((transition, cardinality)) = it.next() {
        if cardinality != 1 {
            None
        } else {
            if it.next().is_some() {
                return None;
            }

            Some(transition)
        }
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::techniques::reduce::ReduceLanguageEquivalently;
    use ebi_objects::{
        ActivityKey, ActivityKeyTranslator, ProcessTree, activity, con,
        ebi_objects::process_tree::{Node, Operator},
        int, or, seq, tau, tloop, xor,
    };

    #[test]
    fn singularity_xor() {
        let mut tree = xor!(tau!());
        let target = tau!();

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_hash_string(), target.to_hash_string());
    }

    #[test]
    fn singularity_seq() {
        let mut tree = seq!(tau!());
        let target = tau!();

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_hash_string(), target.to_hash_string());
    }

    #[test]
    fn associativity_xor() {
        let mut tree = xor!(tau!(), xor!(tau!()));
        let target = tau!();

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_hash_string(), target.to_hash_string());
    }

    #[test]
    fn associativity_seq() {
        let mut tree = seq!(tau!(), xor!(tau!()));
        let target = tau!();

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_hash_string(), target.to_hash_string());
    }

    #[test]
    fn loop_tau() {
        let mut tree = seq!(tau!(), xor!(tau!(), tloop!(tau!(), tau!())));
        let target = tau!();

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_hash_string(), target.to_hash_string());
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

        assert_eq!(tree.to_hash_string(), target.to_hash_string());
    }

    #[test]
    fn pull_loop_body_tau() {
        let mut tree = tloop!(tau!(), activity!("a"), activity!("b"));
        let target = xor!(tau!(), tloop!(xor!(activity!("a"), activity!("b")), tau!()));

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_hash_string(), target.to_hash_string());
    }

    #[test]
    fn double_skip_loop() {
        let mut tree = tloop!(
            activity!("a"),
            activity!("a"),
            xor!(tau!(), activity!("b")),
            tau!()
        );
        let target = tloop!(activity!("a"), activity!("a"), tau!(), activity!("b"));

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_hash_string(), target.to_hash_string());
    }

    #[test]
    fn pull_up_or() {
        let mut tree = or!(activity!("a"), xor!(activity!("b"), activity!("c"), tau!()));
        let target = xor!(
            tau!(),
            or!(activity!("a"), xor!(activity!("b"), activity!("c")))
        );

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_hash_string(), target.to_hash_string());
    }

    #[test]
    fn implicit_and() {
        let mut tree = int!(activity!("a"), xor!(activity!("b"), activity!("c"), tau!()));
        let target = con!(activity!("a"), xor!(activity!("b"), activity!("c"), tau!()));

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_hash_string(), target.to_hash_string());
    }

    #[test]
    fn implicit_and_2() {
        let mut tree = con!(
            xor!(tau!(), activity!("a")),
            xor!(activity!("b"), activity!("c"), tau!())
        );
        let target = xor!(
            tau!(),
            or!(activity!("a"), xor!(activity!("b"), activity!("c")))
        );

        tree.reduce_language_equivalently();

        assert_eq!(tree.to_hash_string(), target.to_hash_string());
    }
}
