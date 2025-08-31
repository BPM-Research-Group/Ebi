use ebi_arithmetic::{Fraction, One, Signed};

use crate::{
    ebi_framework::activity_key::{ActivityKeyTranslator, HasActivityKey},
    ebi_objects::{
        directly_follows_graph::DirectlyFollowsGraph, labelled_petri_net::LabelledPetriNet, stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton, stochastic_directly_follows_model::StochasticDirectlyFollowsModel, stochastic_labelled_petri_net::StochasticLabelledPetriNet
    },
    ebi_traits::ebi_trait_semantics::Semantics,
    marking::Marking
};

impl From<StochasticDeterministicFiniteAutomaton> for StochasticLabelledPetriNet {
    fn from(value: StochasticDeterministicFiniteAutomaton) -> Self {
        log::info!("convert SDFA to SLPN");

        if value.get_initial_state().is_none() {
            //SDFA has an empty language, return a livelocked SLPN
            return Self {
                activity_key: value.activity_key,
                initial_marking: Marking::new(0),
                labels: vec![None],
                transition2input_places: vec![vec![]],
                transition2output_places: vec![vec![]],
                transition2input_places_cardinality: vec![vec![]],
                transition2output_places_cardinality: vec![vec![]],
                place2output_transitions: vec![],
                weights: vec![Fraction::one()],
            };
        }

        let mut result = LabelledPetriNet::new();
        let translator =
            ActivityKeyTranslator::new(value.get_activity_key(), result.get_activity_key_mut());
        let mut weights = vec![];

        let source = result.add_place();
        result
            .get_initial_marking_mut()
            .increase(source, 1)
            .unwrap();

        //add places
        let mut state2place = vec![];
        for state in 0..=value.max_state {
            let lpn_place = result.add_place();
            state2place.push(lpn_place);

            //add termination
            if value.get_termination_probability(state).is_positive() {
                let lpn_transition = result.add_transition(None);
                weights.push(value.get_termination_probability(state).clone());
                result
                    .add_place_transition_arc(lpn_place, lpn_transition, 1)
                    .unwrap();
            }
        }

        //add edges
        for (source, (target, (activity, probability))) in value.sources.iter().zip(
            value
                .targets
                .iter()
                .zip(value.activities.iter().zip(value.probabilities.iter())),
        ) {
            //add transition
            let lpn_activity = translator.translate_activity(activity);
            let lpn_transition = result.add_transition(Some(lpn_activity));
            let source_place = state2place[*source];
            let target_place = state2place[*target];
            result
                .add_place_transition_arc(source_place, lpn_transition, 1)
                .unwrap();
            result
                .add_transition_place_arc(lpn_transition, target_place, 1)
                .unwrap();

            weights.push(probability.clone());
        }

        StochasticLabelledPetriNet::from((result, weights))
    }
}

impl From<StochasticDirectlyFollowsModel> for StochasticLabelledPetriNet {
    fn from(value: StochasticDirectlyFollowsModel) -> Self {
        log::info!("convert SDFM to SLPN");

        if value.get_initial_state().is_none() {
            //SDFA has an empty language, return a livelocked SLPN
            return Self::new_empty_language();
        }

        let mut result = StochasticLabelledPetriNet::new();
        let translator =
            ActivityKeyTranslator::new(value.get_activity_key(), result.get_activity_key_mut());
        let source = result.add_place();
        let sink = result.add_place();
        result
            .get_initial_marking_mut()
            .increase(source, 1)
            .unwrap();

        /*
         * empty traces
         */
        if value.has_empty_traces() {
            let transition = result.add_transition(None, value.empty_traces_weight);

            result
                .add_place_transition_arc(source, transition, 1)
                .unwrap();
            result
                .add_transition_place_arc(transition, sink, 1)
                .unwrap();
        }

        /*
         * Nodes (states): after doing a node you end up in the corresponding place.
         */
        let mut node2place = vec![];
        for _ in 0..value.node_2_activity.len() {
            let place = result.add_place();
            node2place.push(place);
        }

        /*
         * Transitions
         */
        for (source_node, (target_node, weight)) in value
            .sources
            .into_iter()
            .zip(value.targets.into_iter().zip(value.weights.into_iter()))
        {
            if weight.is_positive() {
                let from_place = node2place[source_node];
                let to_place = node2place[target_node];
                let activity = translator.translate_activity(&value.node_2_activity[target_node]);
                let transition = result.add_transition(Some(activity), weight);

                result
                    .add_place_transition_arc(from_place, transition, 1)
                    .unwrap();
                result
                    .add_transition_place_arc(transition, to_place, 1)
                    .unwrap();
            }
        }

        /*
         * Starts
         */
        for (node, weight) in value.start_node_weights.into_iter().enumerate() {
            if weight.is_positive() {
                let activity = translator.translate_activity(&value.node_2_activity[node]);
                let transition = result.add_transition(Some(activity), weight);
                result
                    .add_place_transition_arc(source, transition, 1)
                    .unwrap();
                let target_place = node2place[node];
                result
                    .add_transition_place_arc(transition, target_place, 1)
                    .unwrap();
            }
        }

        /*
         * Ends
         */
        for (node, weight) in value.end_node_weights.into_iter().enumerate() {
            if weight.is_positive() {
                let transition = result.add_transition(None, weight);
                let source_place = node2place[node];
                result
                    .add_place_transition_arc(source_place, transition, 1)
                    .unwrap();
                result
                    .add_transition_place_arc(transition, sink, 1)
                    .unwrap();
            }
        }

        result
    }
}

impl From<DirectlyFollowsGraph> for StochasticLabelledPetriNet {
    fn from(value: DirectlyFollowsGraph) -> Self {
        <DirectlyFollowsGraph as Into<StochasticDirectlyFollowsModel>>::into(value).into()
    }
}