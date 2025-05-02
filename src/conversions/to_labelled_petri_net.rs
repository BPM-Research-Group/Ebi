use std::collections::HashMap;

use anyhow::{Error, anyhow};
use bitvec::bitvec;

use crate::{
    ebi_framework::activity_key::{ActivityKeyTranslator, HasActivityKey},
    ebi_objects::{
        deterministic_finite_automaton::DeterministicFiniteAutomaton,
        directly_follows_model::DirectlyFollowsModel,
        labelled_petri_net::{LPNMarking, LabelledPetriNet},
        lola_net::LolaNet,
        petri_net_markup_language::PetriNetMarkupLanguage,
        process_tree::ProcessTree,
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet,
    },
    ebi_traits::ebi_trait_semantics::Semantics,
    marking::Marking,
};

impl From<ProcessTree> for LabelledPetriNet {
    fn from(value: ProcessTree) -> Self {
        log::info!("convert process tree to LPN");

        if value.tree.is_empty() {
            return Self::new_empty_language();
        }

        let mut result = LabelledPetriNet::new();
        let translator =
            ActivityKeyTranslator::new(value.get_activity_key(), result.get_activity_key_mut());
        let source = result.add_place();
        let sink = result.add_place();
        result
            .get_initial_marking_mut()
            .increase(source, 1)
            .unwrap();

        value
            .node_to_lpn(0, &mut result, &translator, source, sink)
            .unwrap();

        result
    }
}

impl From<LolaNet> for LabelledPetriNet {
    fn from(value: LolaNet) -> Self {
        log::info!("convert Lola net to LPN");
        value.0
    }
}

impl From<DirectlyFollowsModel> for LabelledPetriNet {
    fn from(value: DirectlyFollowsModel) -> LabelledPetriNet {
        log::info!("convert DFM to LPN");

        if value.get_initial_state().is_none() {
            //SDFA has an empty language, return a livelocked SLPN
            return Self::new_empty_language();
        }

        let mut result = LabelledPetriNet::new();
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
        if value.empty_traces {
            let transition = result.add_transition(None);

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
        for _ in 0..value.get_number_of_nodes() {
            let place = result.add_place();
            node2place.push(place);
        }

        /*
         * Transitions
         */
        for source_node in 0..value.get_number_of_nodes() {
            for target_node in 0..value.get_number_of_nodes() {
                if value.edges[source_node][target_node] {
                    let from_place = node2place[source_node];
                    let to_place = node2place[target_node];
                    let activity =
                        translator.translate_activity(&value.node_2_activity[target_node]);
                    let transition = result.add_transition(Some(activity));

                    result
                        .add_place_transition_arc(from_place, transition, 1)
                        .unwrap();
                    result
                        .add_transition_place_arc(transition, to_place, 1)
                        .unwrap();
                }
            }
        }

        /*
         * Starts
         */
        for start_node in value.start_nodes.iter() {
            let activity = translator.translate_activity(&value.node_2_activity[*start_node]);
            let transition = result.add_transition(Some(activity));
            result
                .add_place_transition_arc(source, transition, 1)
                .unwrap();
            let target_place = node2place[*start_node];
            result
                .add_transition_place_arc(transition, target_place, 1)
                .unwrap();
        }

        /*
         * Ends
        	*/
        for end_node in value.end_nodes.iter() {
            let transition = result.add_transition(None);
            let source_place = node2place[*end_node];
            result
                .add_place_transition_arc(source_place, transition, 1)
                .unwrap();
            result
                .add_transition_place_arc(transition, sink, 1)
                .unwrap();
        }

        result
    }
}

impl From<StochasticLabelledPetriNet> for LabelledPetriNet {
    fn from(value: StochasticLabelledPetriNet) -> Self {
        log::info!("convert SLPN to LPN");
        Self {
            activity_key: value.activity_key,
            initial_marking: value.initial_marking,
            labels: value.labels,
            place2output_transitions: value.place2output_transitions,
            transition2input_places: value.transition2input_places,
            transition2output_places: value.transition2output_places,
            transition2input_places_cardinality: value.transition2input_places_cardinality,
            transition2output_places_cardinality: value.transition2output_places_cardinality,
        }
    }
}

impl From<DeterministicFiniteAutomaton> for LabelledPetriNet {
    fn from(value: DeterministicFiniteAutomaton) -> Self {
        log::info!("convert DFA to LPN");

        let source = if let Some(s) = value.initial_state {
            s
        } else {
            //DFA has an empty language, return a livelocked LPN
            return Self {
                activity_key: value.activity_key,
                initial_marking: Marking::new(0),
                labels: vec![None],
                transition2input_places: vec![vec![]],
                transition2output_places: vec![vec![]],
                transition2input_places_cardinality: vec![vec![]],
                transition2output_places_cardinality: vec![vec![]],
                place2output_transitions: vec![],
            };
        };

        let mut result = LabelledPetriNet::new();
        let translator =
            ActivityKeyTranslator::new(value.get_activity_key(), result.get_activity_key_mut());

        //add places
        let mut state2place = vec![];
        for state in 0..=value.max_state {
            let lpn_place = result.add_place();
            state2place.push(lpn_place);

            //add termination
            if value.can_terminate_in_state(state) {
                let lpn_transition = result.add_transition(None);
                result
                    .add_place_transition_arc(lpn_place, lpn_transition, 1)
                    .unwrap();
            }
        }

        //initial marking
        result
            .get_initial_marking_mut()
            .increase(source, 1)
            .unwrap();

        //add edges
        for (source, (target, activity)) in value
            .sources
            .iter()
            .zip(value.targets.iter().zip(value.activities.iter()))
        {
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
        }

        result
    }
}

impl TryFrom<PetriNetMarkupLanguage> for LabelledPetriNet {
    type Error = Error;

    fn try_from(pnml: PetriNetMarkupLanguage) -> Result<Self, Self::Error> {
        log::info!("convert PNML to LPN");

        let mut result = LabelledPetriNet::new();

        //create map of places
        let mut place2index = HashMap::new();
        for (place_id, _) in pnml.net.places {
            let place = result.add_place();
            place2index.insert(place_id, place);
        }

        //transitions
        let mut transition2index = HashMap::new();
        for (transition_id, transition) in &pnml.net.transitions {
            let label = match &transition.label {
                Some(activity) => Some(result.get_activity_key_mut().process_activity(activity)),
                None => None,
            };
            let transition = result.add_transition(label);

            transition2index.insert(transition_id, transition);
        }

        //arcs
        for arc in pnml.net.arcs.iter() {
            match arc.from_to {
                process_mining::petri_net::petri_net_struct::ArcType::PlaceTransition(
                    place_id,
                    transition_id,
                ) => {
                    let new_place = place2index
                        .get(&place_id)
                        .ok_or(anyhow!("Undeclared place referenced."))?;
                    let new_transition = transition2index
                        .get(&transition_id)
                        .ok_or(anyhow!("undeclared transition referenced"))?;
                    result.add_place_transition_arc(
                        *new_place,
                        *new_transition,
                        arc.weight.into(),
                    )?;
                }
                process_mining::petri_net::petri_net_struct::ArcType::TransitionPlace(
                    transition_id,
                    place_id,
                ) => {
                    let new_place = place2index
                        .get(&place_id)
                        .ok_or(anyhow!("Undeclared place referenced."))?;
                    let new_transition = transition2index
                        .get(&transition_id)
                        .ok_or(anyhow!("undeclared transition referenced"))?;
                    result.add_transition_place_arc(
                        *new_transition,
                        *new_place,
                        arc.weight.into(),
                    )?;
                }
            };
        }

        //initial marking
        // for (place_id, cardinality) in pnml.net.initial_marking.as_ref().ok_or(anyhow!("The given net has no initial marking. Ebi requires an initial marking for its Petri nets."))?.iter() {
        if let Some(marking) = pnml.net.initial_marking.as_ref() {
            for (place_id, cardinality) in marking {
                let new_place = place2index
                    .get(&place_id.get_uuid())
                    .ok_or(anyhow!("Undeclared place found in the initial marking."))?;
                result
                    .get_initial_marking_mut()
                    .increase(*new_place, *cardinality)?;
            }
        }

        //final markings
        if let Some(final_markings) = &pnml.net.final_markings {
            //The nets used by Ebi do not have final markings, as each of their deadlocks is taken as a final marking.
            //The best we can do here is to verify that no non-deadlocks have been declared as final markings.
            for final_marking in final_markings.iter() {
                //transform to an Ebi-final marking
                let mut new_final_marking = Marking::new(result.get_number_of_places());
                for (place_id, cardinality) in final_marking.iter() {
                    let new_place = place2index
                        .get(&place_id.get_uuid())
                        .ok_or(anyhow!("Undeclared place found."))?;
                    new_final_marking.increase(*new_place, *cardinality)?;
                }

                //verify that this is a deadlock marking
                let mut state = LPNMarking {
                    marking: new_final_marking,
                    enabled_transitions: bitvec![0; result.get_number_of_transitions()],
                    number_of_enabled_transitions: 0,
                };
                result.compute_enabled_transitions(&mut state);
                if !result.is_final_state(&state) {
                    return Err(anyhow!(
                        "This PNML file has a final marking that is not a deadlock. In Ebi, each final marking must be a deadlock. This final marking is {:?}",
                        final_marking
                    ));
                }
            }
        }

        Ok(result)
    }
}

impl From<StochasticDeterministicFiniteAutomaton> for LabelledPetriNet {
    fn from(value: StochasticDeterministicFiniteAutomaton) -> Self {
        let dfa: DeterministicFiniteAutomaton = value.into();
        dfa.into()
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::ebi_objects::{deterministic_finite_automaton::DeterministicFiniteAutomaton, labelled_petri_net::LabelledPetriNet};

    #[test]
    fn dfa_to_lpn() {
        let fin = fs::read_to_string("testfiles/a-loop.dfa").unwrap();
        let dfa = fin.parse::<DeterministicFiniteAutomaton>().unwrap();
        let lpn: LabelledPetriNet = dfa.into();

        let fin2 = fs::read_to_string("testfiles/a-loop.lpn").unwrap();
        let lpn2 = fin2.parse::<LabelledPetriNet>().unwrap();

        assert_eq!(lpn.to_string(), lpn2.to_string());
        
    }
}