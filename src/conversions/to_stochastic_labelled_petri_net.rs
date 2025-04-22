use crate::{
    ebi_framework::activity_key::{ActivityKeyTranslator, HasActivityKey},
    ebi_objects::{
        labelled_petri_net::LabelledPetriNet,
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet,
    },
    marking::Marking,
    math::{
        fraction::Fraction,
        traits::{One, Signed},
    },
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
