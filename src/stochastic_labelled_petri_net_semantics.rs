use std::{fmt::{self, Debug}, hash::{Hash, Hasher}, rc::Rc};

use bitvec::{bitvec, vec::BitVec, prelude::Lsb0};
use anyhow::{anyhow, Context};
use fraction::Zero;

use crate::{activity_key::{self, Activity, ActivityKey}, ebi_objects::{labelled_petri_net::LPNMarking, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::{ebi_semantics::Semantics, ebi_trait_labelled_petri_net::EbiTraitLabelledPetriNet, ebi_trait_stochastic_semantics::{StochasticSemantics, TransitionIndex}}, export::Displayable, marking::Marking, math::fraction::Fraction, net::{Net, StochasticNet}};

#[derive(Clone, Debug)]
pub struct StochasticLabelledPetriNetSemantics {
    net: Rc<StochasticLabelledPetriNet>,
    activity_key: ActivityKey, //needs to be cloned to satisfy the borrow checker
    input_places:  Vec<Vec<usize>>, //hashmaps would be more safe here, but that's a lot of overhead
	output_places: Vec<Vec<usize>>,
    input_places_weights: Vec<Vec<u64>>,
    output_places_weights: Vec<Vec<u64>>,
	input_transitions: Vec<Vec<usize>>,
	output_transitions: Vec<Vec<usize>>
}

impl StochasticLabelledPetriNetSemantics {
    
    pub fn from_slpn(net: Rc<StochasticLabelledPetriNet>) -> StochasticLabelledPetriNetSemantics {

        let transitions = net.get_number_of_transitions();
        let places: usize = net.get_number_of_places();
        let activity_key = net.get_activity_key().clone();
        let mut result = StochasticLabelledPetriNetSemantics {
            net: net,
            activity_key: activity_key,
            input_places: vec![vec![]; transitions],
            output_places: vec![vec![]; transitions],
            input_places_weights: vec![vec![]; transitions],
            output_places_weights: vec![vec![]; transitions],
            input_transitions: vec![vec![]; places],
            output_transitions: vec![vec![]; places]
        };

        for transition in result.net.get_transitions() {
            for place in &transition.incoming {
                result.output_transitions[*place].push(transition.index);
                
                
                if let Some(pos) = result.input_places[transition.index].iter().position(|p| p == place) {
                    result.input_places_weights[transition.index][pos] += 1;
                } else {
                    result.input_places[transition.index].push(*place);
                    result.input_places_weights[transition.index].push(1);
                }

            }
            for place in &transition.outgoing {
                result.input_transitions[*place].push(transition.index);

                if let Some(pos) = result.output_places[transition.index].iter().position(|p| p == place) {
                    result.output_places_weights[transition.index][pos] += 1;
                } else {
                    result.output_places[transition.index].push(*place);
                    result.output_places_weights[transition.index].push(1);
                }
            }
        }
        
        result
    }

	fn compute_enabled_transition(&self, state: &mut LPNMarking, transition: TransitionIndex) -> bool {
		for (in_place_pos, in_place) in self.input_places[transition].iter().enumerate() {
			if state.marking.place2token[*in_place] < self.input_places_weights[transition][in_place_pos] {
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

    fn compute_enabled_transitions(&self, state: &mut LPNMarking) {
		state.number_of_enabled_transitions = 0;
        state.enabled_transitions.fill(false);
        for transition in 0 .. self.net.get_number_of_transitions() {
			self.compute_enabled_transition(state, transition);
		}
	}

}

impl Semantics for StochasticLabelledPetriNetSemantics {
	type State = LPNMarking;

    fn get_activity_key(&self) -> &ActivityKey {
        self.net.get_activity_key()
    }

	fn is_final_state(&self, state: &LPNMarking) -> bool {
        state.number_of_enabled_transitions == 0   
    }

    fn get_initial_state(&self) -> LPNMarking {
        let mut result = LPNMarking {
            marking: self.net.get_initial_marking().clone(),
            enabled_transitions: bitvec![0; self.net.get_number_of_transitions()],
            number_of_enabled_transitions: 0,
        };
		self.compute_enabled_transitions(&mut result);

        result
    }

	fn execute_transition(&self, state: &mut LPNMarking, transition: TransitionIndex) -> anyhow::Result<()>{
		for (place_pos, place) in self.input_places[transition].iter().enumerate() {
            let arc_weight = self.input_places_weights[transition][place_pos];
			state.marking.decrease(*place, arc_weight).with_context(|| format!("transition {} is not enabled", transition))?;

			//update the enabled transitions; some transitions might be disabled by this execution
			for transition_t in &self.output_transitions[*place]  {
				self.compute_enabled_transition(state, *transition_t);
			}
		}

		for (place_pos, place) in self.output_places[transition].iter().enumerate() {
            let arc_weight = self.output_places_weights[transition][place_pos];
			state.marking.increase(*place, arc_weight).with_context(|| format!("when firing transition {}", transition))?;

			//update the enabled transitions; some transitions might be enabled by this execution
            for transition_t in &self.output_transitions[*place]  {
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
        self.net.get_transitions()[transition].is_silent()
    }

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        self.net.get_transitions()[transition].get_label()
    }
}

impl StochasticSemantics for StochasticLabelledPetriNetSemantics {
    fn get_transition_weight(&self, _state: &LPNMarking, transition: usize) -> &Fraction {
        self.net.get_weight(&self.net.get_transitions()[transition])
    }

	fn get_total_weight_of_enabled_transitions(&self, state: &LPNMarking) -> anyhow::Result<Fraction> {
        let mut sum = Fraction::zero();
        for index in state.enabled_transitions.iter_ones() {
            sum += self.get_transition_weight(&state, index);
        }
        if sum == Fraction::zero() {
            return Err(anyhow!("total enabled weight is 0"));
        }
        Ok(sum)
    }
}