use bitvec::bitvec;
use anyhow::{anyhow, Context};

use crate::{ebi_framework::activity_key::{Activity, ActivityKey}, ebi_objects::{labelled_petri_net::LPNMarking, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::{ebi_trait_semantics::Semantics, ebi_trait_stochastic_semantics::{StochasticSemantics, TransitionIndex}}, math::fraction::Fraction};

impl StochasticLabelledPetriNet {

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

        if !self.get_transition_weight(transition).is_positive() {
            return false;
        }
		
        true
    }

    fn compute_enabled_transitions(&self, state: &mut LPNMarking) {
		state.number_of_enabled_transitions = 0;
        state.enabled_transitions.fill(false);
        for transition in 0 .. self.get_number_of_transitions() {
			self.compute_enabled_transition(state, transition);
		}
	}

}

impl Semantics for StochasticLabelledPetriNet {
	type State = LPNMarking;

    fn get_activity_key(&self) -> &ActivityKey {
        &self.activity_key
    }

    fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
        &mut self.activity_key
    }

	fn is_final_state(&self, state: &LPNMarking) -> bool {
        state.number_of_enabled_transitions == 0   
    }

    fn get_initial_state(&self) -> LPNMarking {
        let mut result = LPNMarking {
            marking: self.initial_marking.clone(),
            enabled_transitions: bitvec![0; self.get_number_of_transitions()],
            number_of_enabled_transitions: 0,
        };
		self.compute_enabled_transitions(&mut result);

        result
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

    fn get_enabled_transitions(&self, state: &LPNMarking) -> Vec<usize> {
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
}

impl StochasticSemantics for StochasticLabelledPetriNet {
    fn get_transition_weight(&self, _state: &LPNMarking, transition: usize) -> &Fraction {
        &self.weights[transition]
    }

	fn get_total_weight_of_enabled_transitions(&self, state: &LPNMarking) -> anyhow::Result<Fraction> {
        let mut sum = Fraction::zero();
        for index in state.enabled_transitions.iter_ones() {
            sum += self.get_transition_weight(index);
        }
        if sum == Fraction::zero() {
            return Err(anyhow!("total enabled weight is 0"));
        }
        Ok(sum)
    }
}