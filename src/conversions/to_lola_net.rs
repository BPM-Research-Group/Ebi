use crate::ebi_objects::{
    deterministic_finite_automaton::DeterministicFiniteAutomaton,
    directly_follows_model::DirectlyFollowsModel, labelled_petri_net::LabelledPetriNet,
    lola_net::LolaNet, process_tree::ProcessTree,
    stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
    stochastic_labelled_petri_net::StochasticLabelledPetriNet, stochastic_process_tree::StochasticProcessTree,
};

impl From<LabelledPetriNet> for LolaNet {
    fn from(value: LabelledPetriNet) -> Self {
        log::info!("Convert LPN into Lola net.");
        Self(value)
    }
}

macro_rules! from {
    ($t:ident) => {
        impl From<$t> for LolaNet {
            fn from(value: $t) -> Self {
                let lpn: LabelledPetriNet = value.into();
                lpn.into()
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