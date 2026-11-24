use crate::{
    semantics::stochastic_deterministic_semantics::stochastic_deterministic_semantics_trait::StochasticDeterministicSemantics,
    techniques::non_decreasing_livelock::NonDecreasingLivelock,
};
use ebi_objects::{
    anyhow::{anyhow, Result},
    ebi_arithmetic::{Fraction, One, Zero},
    Activity, AutomatonSemantics, AutomatonState, HasActivityKey,
    StochasticDeterministicFiniteAutomaton,
};

impl StochasticDeterministicSemantics for StochasticDeterministicFiniteAutomaton {
    type DetState = AutomatonState;

    fn get_deterministic_initial_state(&self) -> Result<Option<AutomatonState>> {
        Ok(self.initial_state())
    }

    fn execute_deterministic_activity(
        &self,
        state: &AutomatonState,
        activity: Activity,
    ) -> Result<AutomatonState> {
        let (found, i) = self.binary_search(*state, self.activity_key().activity_to_id(activity));
        if found {
            Ok(self.targets[i])
        } else {
            Err(anyhow!("activity not enabled"))
        }
    }

    fn get_deterministic_termination_probability(&self, state: &AutomatonState) -> Fraction {
        self.terminating_probabilities[*state].clone()
    }

    fn get_deterministic_activity_probability(
        &self,
        state: &AutomatonState,
        activity: Activity,
    ) -> Fraction {
        let (found, i) = self.binary_search(*state, self.activity_key().activity_to_id(activity));
        match found {
            true => self.probabilities[i].clone(),
            false => Fraction::zero(),
        }
    }

    fn get_deterministic_enabled_activities(&self, state: &AutomatonState) -> Vec<Activity> {
        let mut result = vec![];

        let (_, mut i) = self.binary_search(*state, 0);
        while i < self.sources.len() && self.sources[i] == *state {
            result.push(self.activities[i]);
            i += 1;
        }

        result
    }

    fn get_deterministic_silent_livelock_probability(&self, _state: &Self::DetState) -> Fraction {
        Fraction::zero()
    }

    fn get_deterministic_non_decreasing_livelock_probability(
        &self,
        state: &mut Self::DetState,
    ) -> Result<Fraction> {
        if self.is_part_of_non_decreasing_livelock(state)? {
            Ok(Fraction::one())
        } else {
            Ok(Fraction::zero())
        }
    }
}
