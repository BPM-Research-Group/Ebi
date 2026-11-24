use crate::{
    ebi_framework::displayable::Displayable,
    techniques::infinitely_many_traces::InfinitelyManyTraces,
};
use ebi_objects::{anyhow::Result, ebi_arithmetic::Fraction, Activity, HasActivityKey};

pub trait StochasticDeterministicSemantics: HasActivityKey + InfinitelyManyTraces {
    type DetState: Displayable;

    /**
     * (Re)set the semantics to the initial state.
     */
    fn get_deterministic_initial_state(&self) -> Result<Option<Self::DetState>>;

    /**
     * Get the state that results from executing the activity.
     * Returns whether the execution was successful.
     */
    fn execute_deterministic_activity(
        &self,
        state: &Self::DetState,
        activity: Activity,
    ) -> Result<Self::DetState>;

    /**
     *
     * @return the probability of terminating in this state.
     * Note that the sum of termination and the probability of all enabled activities may be smaller than 1. The reminder is a silent livelock.
     */
    fn get_deterministic_termination_probability(&self, state: &Self::DetState) -> Fraction;

    /**
     *
     * @param activity
     * @return the probability of executing the activity in the given state.
     * Note that the sum of termination and the probability of all enabled activities may be smaller than 1. The remainder is a silent livelock.
     */
    fn get_deterministic_activity_probability(
        &self,
        state: &Self::DetState,
        activity: Activity,
    ) -> Fraction;

    fn get_deterministic_enabled_activities(&self, state: &Self::DetState) -> Vec<Activity>;

    /**
     * The probability that from the given state, the model will never leave a livelock of silent activities.
     */
    fn get_deterministic_silent_livelock_probability(&self, state: &Self::DetState) -> Fraction;

    /**
     * The probability that from the given state, the model will never leave a livelock. Note that this is not the probability to -end up- in a livelock, but the probability that the state is -in- a livelock.
     */
    fn get_deterministic_non_decreasing_livelock_probability(
        &self,
        state: &mut Self::DetState,
    ) -> Result<Fraction>;
}
