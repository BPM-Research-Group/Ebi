use std::collections::HashMap;

use anyhow::{Error, anyhow};
use num::ToPrimitive;
use process_mining::{
    PetriNet,
    petri_net::petri_net_struct::{self, ArcType},
};

use crate::{
    ebi_objects::{
        deterministic_finite_automaton::DeterministicFiniteAutomaton,
        directly_follows_model::DirectlyFollowsModel, labelled_petri_net::LabelledPetriNet,
        petri_net_markup_language::PetriNetMarkupLanguage, process_tree::ProcessTree,
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet, stochastic_process_tree::StochasticProcessTree,
    },
    ebi_traits::ebi_trait_semantics::Semantics,
};

impl TryFrom<LabelledPetriNet> for PetriNetMarkupLanguage {
    type Error = anyhow::Error;

    fn try_from(lpn: LabelledPetriNet) -> std::result::Result<Self, Self::Error> {
        log::info!("Convert LPN into PNML.");

        let mut result = PetriNet::new();

        //create places
        let mut place2new_place = HashMap::new();
        {
            for place in 0..lpn.get_number_of_places() {
                let new_place = result.add_place(None);
                place2new_place.insert(place, new_place);
            }
        }

        //create transitions
        for transition in 0..lpn.get_number_of_transitions() {
            let new_transition = result.add_transition(
                match lpn.get_transition_label(transition) {
                    Some(activity) => {
                        Some(lpn.activity_key.get_activity_label(&activity).to_string())
                    }
                    None => None,
                },
                None,
            );

            //incoming
            {
                //transform to a map of places and arc weights
                let mut map = HashMap::new();
                for (pos, place) in lpn.transition2input_places[transition].iter().enumerate() {
                    *map.entry(*place).or_insert(0) +=
                        u32::try_from(lpn.transition2input_places_cardinality[transition][pos])?;
                }

                //add
                for (place, weight) in map {
                    let new_place = place2new_place
                        .get(&place)
                        .ok_or(anyhow!("Non-existing place referenced."))?;
                    result.add_arc(
                        ArcType::place_to_transition(*new_place, new_transition),
                        Some(weight),
                    );
                }
            }

            //outgoing
            {
                //transform to a map of places and arc weights
                let mut map = HashMap::new();
                for (pos, place) in lpn.transition2output_places[transition].iter().enumerate() {
                    *map.entry(*place).or_insert(0) +=
                        u32::try_from(lpn.transition2output_places_cardinality[transition][pos])?;
                }

                //add
                for (place, weight) in map {
                    let new_place = place2new_place
                        .get(&place)
                        .ok_or(anyhow!("Non-existing place referenced."))?;
                    result.add_arc(
                        ArcType::transition_to_place(new_transition, *new_place),
                        Some(weight.to_u32().ok_or(anyhow!("value out of bounds"))?),
                    );
                }
            }
        }

        //initial marking
        let mut new_initial_marking = petri_net_struct::Marking::new();
        for (place, cardinality) in lpn
            .initial_marking
            .get_place2token()
            .into_iter()
            .enumerate()
        {
            if cardinality > &0u64 {
                let new_place = place2new_place
                    .get(&place)
                    .ok_or(anyhow!("Non-existing place referenced."))?;
                new_initial_marking.insert(*new_place, *cardinality);
            }
        }
        result.initial_marking = Some(new_initial_marking);

        Ok(Self { net: result })
    }
}

macro_rules! from {
    ($t:ident) => {
        impl TryFrom<$t> for PetriNetMarkupLanguage {
            type Error = Error;

            fn try_from(value: $t) -> Result<Self, Self::Error> {
                let lpn: LabelledPetriNet = value.into();
                lpn.try_into()
            }
        }
    };
}

from!(StochasticLabelledPetriNet);
from!(DeterministicFiniteAutomaton);
from!(DirectlyFollowsModel);
from!(ProcessTree);
from!(StochasticProcessTree);
from!(StochasticDeterministicFiniteAutomaton);
