use crate::{
    ebi_framework::{
        displayable::Displayable, ebi_input::EbiInput, ebi_trait::FromEbiTraitObject,
        ebi_trait_object::EbiTraitObject, trait_importers::ToStochasticDeterministicSemanticsTrait,
    },
    semantics::labelled_petri_net_semantics::LPNMarking,
    techniques::{
        deterministic_semantics_for_stochastic_semantics::PMarking,
        infinitely_many_traces::InfinitelyManyTraces,
    },
};
use anyhow::{Result, anyhow};
use ebi_objects::{
    Activity, CompressedEventLog, DirectlyFollowsGraph, EventLog, EventLogPython,
    EventLogTraceAttributes, EventLogXes, FiniteStochasticLanguage, HasActivityKey,
    StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticLabelledPetriNet, StochasticNondeterministicFiniteAutomaton, StochasticProcessTree,
    ebi_arithmetic::Fraction,
    ebi_objects::{
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
        event_log_csv::EventLogCsv, process_tree::TreeMarking,
    },
};

pub enum EbiTraitStochasticDeterministicSemantics {
    Usize(Box<dyn StochasticDeterministicSemantics<DetState = usize, LivState = usize>>),
    UsizeDistribution(
        Box<dyn StochasticDeterministicSemantics<DetState = PMarking<usize>, LivState = usize>>,
    ),
    LPNMarkingDistribution(
        Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<LPNMarking>,
                    LivState = LPNMarking,
                >,
        >,
    ),
    TreeMarkingDistribution(
        Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<TreeMarking>,
                    LivState = TreeMarking,
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

macro_rules! via_fslang {
    ($t:ident) => {
        impl ToStochasticDeterministicSemanticsTrait for $t {
            fn to_stochastic_deterministic_semantics_trait(
                self,
            ) -> EbiTraitStochasticDeterministicSemantics {
                Into::<FiniteStochasticLanguage>::into(self)
                    .to_stochastic_deterministic_semantics_trait()
            }
        }
    };
}
via_fslang!(CompressedEventLog);
via_fslang!(CompressedEventLogTraceAttributes);
via_fslang!(EventLog);
via_fslang!(EventLogTraceAttributes);
via_fslang!(EventLogXes);
via_fslang!(EventLogCsv);
via_fslang!(EventLogPython);

impl ToStochasticDeterministicSemanticsTrait for StochasticProcessTree {
    fn to_stochastic_deterministic_semantics_trait(
        self,
    ) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::TreeMarkingDistribution(Box::new(self))
    }
}

impl ToStochasticDeterministicSemanticsTrait for StochasticLabelledPetriNet {
    fn to_stochastic_deterministic_semantics_trait(
        self,
    ) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::LPNMarkingDistribution(Box::new(self))
    }
}
impl ToStochasticDeterministicSemanticsTrait for DirectlyFollowsGraph {
    fn to_stochastic_deterministic_semantics_trait(
        self,
    ) -> EbiTraitStochasticDeterministicSemantics {
        let dfm: StochasticDirectlyFollowsModel = self.into();
        EbiTraitStochasticDeterministicSemantics::UsizeDistribution(Box::new(dfm))
    }
}

impl ToStochasticDeterministicSemanticsTrait for StochasticNondeterministicFiniteAutomaton {
    fn to_stochastic_deterministic_semantics_trait(
        self,
    ) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::UsizeDistribution(Box::new(self))
    }
}

impl ToStochasticDeterministicSemanticsTrait for StochasticDirectlyFollowsModel {
    fn to_stochastic_deterministic_semantics_trait(
        self,
    ) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::UsizeDistribution(Box::new(self))
    }
}

impl ToStochasticDeterministicSemanticsTrait for StochasticDeterministicFiniteAutomaton {
    fn to_stochastic_deterministic_semantics_trait(
        self,
    ) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::Usize(Box::new(self))
    }
}

impl ToStochasticDeterministicSemanticsTrait for FiniteStochasticLanguage {
    fn to_stochastic_deterministic_semantics_trait(
        self,
    ) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::Usize(Box::new(Into::<
            StochasticDeterministicFiniteAutomaton,
        >::into(self)))
    }
}
