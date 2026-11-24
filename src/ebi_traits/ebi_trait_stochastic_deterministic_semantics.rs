use crate::{
    ebi_framework::{
        ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
        trait_importers::ToStochasticDeterministicSemanticsTrait,
    },
    semantics::{
        labelled_petri_net_semantics::LPNMarking,
        stochastic_deterministic_semantics::{
            deterministic_semantics_for_stochastic_semantics::PMarking,
            stochastic_deterministic_semantics_trait::StochasticDeterministicSemantics,
        },
    },
    trait_definition_finalisation,
};
use ebi_objects::{
    anyhow::{anyhow, Result},
    ebi_objects::{
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
        event_log_csv::EventLogCsv, process_tree::TreeMarking,
    },
    AutomatonState, CompressedEventLog, DirectlyFollowsGraph, EventLog, EventLogOcel,
    EventLogPython, EventLogTraceAttributes, EventLogXes, FiniteStochasticLanguage,
    StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticLabelledPetriNet, StochasticNondeterministicFiniteAutomaton, StochasticProcessTree,
};

/// Allows for traversal of a state space and its probabilities if the activities determine the state deterministically.
/// 
/// This is a wrapper enum in order to be able to implement algorithms that are agnostic of the marking/state type, amongst other things.
/// This enum does not allow access to the underlying Semantics trait methods directly, but through a match the generics work out.
/// See the implementation of [`crate::techniques::probability_queries::ProbabilityQueries`] for an example.
/// 
/// For documentation on Ebi traits and mathematical definitions, please refer to the [manual](https://github.com/BPM-Research-Group/Ebi/releases/latest/download/manual.pdf).
pub enum EbiTraitStochasticDeterministicSemantics {
    Usize(Box<dyn StochasticDeterministicSemantics<DetState = usize, LivState = usize>>),
    AutomatonState(
        Box<
            dyn StochasticDeterministicSemantics<
                DetState = AutomatonState,
                LivState = AutomatonState,
            >,
        >,
    ),
    AutomatonStateDistribution(
        Box<
            dyn StochasticDeterministicSemantics<
                DetState = PMarking<AutomatonState>,
                LivState = AutomatonState,
            >,
        >,
    ),
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

impl EbiTraitStochasticDeterministicSemantics {
    pub fn trait_explanation() -> &'static str
    where
        Self: Sized,
    {
        concat!(
            "The trait ``stochastic deterministic semantics'' allows for state space traversal in a deterministic fashion, that is, in each state every activity appears at most once and silent steps are not present.",
            trait_definition_finalisation!()
        )
    }
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
via_fslang!(EventLogOcel);
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
        EbiTraitStochasticDeterministicSemantics::AutomatonStateDistribution(Box::new(dfm))
    }
}

impl ToStochasticDeterministicSemanticsTrait for StochasticNondeterministicFiniteAutomaton {
    fn to_stochastic_deterministic_semantics_trait(
        self,
    ) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::AutomatonStateDistribution(Box::new(self))
    }
}

impl ToStochasticDeterministicSemanticsTrait for StochasticDirectlyFollowsModel {
    fn to_stochastic_deterministic_semantics_trait(
        self,
    ) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::AutomatonStateDistribution(Box::new(self))
    }
}

impl ToStochasticDeterministicSemanticsTrait for StochasticDeterministicFiniteAutomaton {
    fn to_stochastic_deterministic_semantics_trait(
        self,
    ) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::AutomatonState(Box::new(self))
    }
}

impl ToStochasticDeterministicSemanticsTrait for FiniteStochasticLanguage {
    fn to_stochastic_deterministic_semantics_trait(
        self,
    ) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::AutomatonState(Box::new(Into::<
            StochasticDeterministicFiniteAutomaton,
        >::into(self)))
    }
}
