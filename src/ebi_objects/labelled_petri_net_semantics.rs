use bitvec::bitvec;
use anyhow::Context;

use crate::{ebi_framework::activity_key::Activity, ebi_objects::labelled_petri_net::LPNMarking, ebi_traits::{ebi_trait_semantics::Semantics, ebi_trait_stochastic_semantics::TransitionIndex}};

use super::labelled_petri_net::LabelledPetriNet;

impl LabelledPetriNet{

	fn compute_enabled_transition(&self, state: &mut LPNMarking, transition: TransitionIndex) -> bool {
		for (in_place_pos, in_place) in self.transition2input_places[transition].iter().enumerate() {
			if state.marking.place2token[*in_place] < self.transition2input_places_cardinality[transition][in_place_pos] {
				if state.enabled_transitions[transition] {
					state.enabled_transitions.set(transition, false);
					state.number_of_enabled_transitions -= 1;
				}
				return false;
            }
		}

		if !state.enabled_transitions[transition] {
			state.enabled_transitions.set(transition, true);
			state.number_of_enabled_transitions += 1;
		}
		
        true
    }

    pub(crate) fn compute_enabled_transitions(&self, state: &mut LPNMarking) {
		state.number_of_enabled_transitions = 0;
        state.enabled_transitions.fill(false);
        for transition in 0 .. self.get_number_of_transitions() {
			self.compute_enabled_transition(state, transition);
		}
	}

}

impl Semantics for LabelledPetriNet {
    type SemState = LPNMarking;
    
	fn is_final_state(&self, state: &LPNMarking) -> bool {
        state.number_of_enabled_transitions == 0   
    }

    fn get_initial_state(&self) -> Option<LPNMarking> {
        //an LPN does supports the empty language, but only in livelocks
        let mut result = LPNMarking {
            marking: self.initial_marking.clone(),
            enabled_transitions: bitvec![0; self.get_number_of_transitions()],
            number_of_enabled_transitions: 0,
        };
		self.compute_enabled_transitions(&mut result);

        Some(result)
    }

	fn execute_transition(&self, state: &mut LPNMarking, transition: TransitionIndex) -> anyhow::Result<()>{
		for (place_pos, place) in self.transition2input_places[transition].iter().enumerate() {
            let arc_weight = self.transition2input_places_cardinality[transition][place_pos];
			state.marking.decrease(*place, arc_weight).with_context(|| format!("transition {} is not enabled", transition))?;

			//update the enabled transitions; some transitions might be disabled by this execution
			for transition_t in &self.place2output_transitions[*place]  {
				self.compute_enabled_transition(state, *transition_t);
			}
		}

		for (place_pos, place) in self.transition2output_places[transition].iter().enumerate() {
            let arc_weight = self.transition2output_places_cardinality[transition][place_pos];
			state.marking.increase(*place, arc_weight).with_context(|| format!("when firing transition {}", transition))?;

			//update the enabled transitions; some transitions might be enabled by this execution
            for transition_t in &self.place2output_transitions[*place]  {
				self.compute_enabled_transition(state, *transition_t);
			}
		}

        Ok(())
    }

    fn get_enabled_transitions(&self, state: &LPNMarking) -> Vec<TransitionIndex> {
        let mut result = Vec::new();
        result.reserve_exact(state.number_of_enabled_transitions as usize);
        for index in state.enabled_transitions.iter_ones() {
            result.push(index);
        }
        result
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        self.labels[transition].is_none()        
    }

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        self.labels[transition]
    }

    fn get_number_of_transitions(&self) -> usize {
        self.transition2input_places.len()
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{ebi_objects::labelled_petri_net::LabelledPetriNet, ebi_traits::ebi_trait_semantics::Semantics};

    #[test]
    fn lpn_empty() {
        let fin = fs::read_to_string("testfiles/empty.lpn").unwrap();
        let lpn = fin.parse::<LabelledPetriNet>().unwrap();

        let state = lpn.get_initial_state().unwrap();
        assert_eq!(lpn.get_enabled_transitions(&state).len(), 0);
    }
}