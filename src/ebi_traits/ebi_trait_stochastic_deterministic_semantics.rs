use std::io::BufRead;

use anyhow::{Result, anyhow};

use crate::{
    ebi_framework::{
        activity_key::{Activity, HasActivityKey},
        displayable::Displayable,
        ebi_input::EbiInput,
        ebi_object::EbiTraitObject,
        ebi_trait::FromEbiTraitObject,
        importable::Importable,
    },
    ebi_objects::{labelled_petri_net::LPNMarking, stochastic_process_tree_semantics::NodeStates},
    math::fraction::Fraction,
    techniques::{
        deterministic_semantics_for_stochastic_semantics::PMarking,
        infinitely_many_traces::InfinitelyManyTraces,
    },
};

pub enum EbiTraitStochasticDeterministicSemantics {
    Usize(Box<dyn StochasticDeterministicSemantics<DetState = usize, LivState = usize>>),
    LPNMarkingDistribution(
        Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<LPNMarking>,
                    LivState = LPNMarking,
                >,
        >,
    ),
    NodeStatesDistribution(
        Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<NodeStates>,
                    LivState = NodeStates,
                >,
        >,
    ),
}

impl FromEbiTraitObject for EbiTraitStochasticDeterministicSemantics {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::StochasticDeterministicSemantics(e), _) => {
                Ok(Box::new(e))
            }
            _ => Err(anyhow!(
                "cannot read {} {} as a stochastic deterministic semantics",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

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

pub trait ToStochasticDeterministicSemantics: Importable + Sized {
    fn to_stochastic_deterministic_semantics(self) -> EbiTraitStochasticDeterministicSemantics;

    fn import_as_stochastic_deterministic_semantics(
        reader: &mut dyn BufRead,
    ) -> Result<EbiTraitStochasticDeterministicSemantics> {
        Ok(Self::import(reader)?.to_stochastic_deterministic_semantics())
    }
}
