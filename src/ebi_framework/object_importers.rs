use anyhow::Result;
use pastey::paste;
use std::io::BufRead;

use ebi_objects::{
    DeterministicFiniteAutomaton, DirectlyFollowsModel, EbiObject, FiniteLanguage,
    FiniteStochasticLanguage, LabelledPetriNet, ProcessTree,
    StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticLabelledPetriNet, StochasticProcessTree, traits::importable::Importable,
};

macro_rules! import_as_object {
    ($t:ident, $u:expr) => {
        paste! {
            pub trait [<To $t Object>] {
                fn [<import_as_ $u _object>](reader: &mut dyn BufRead) -> Result<EbiObject>;
            }

            impl<T: Importable> [<To $t Object>] for T
            where
                T: Into<$t>,
            {
                fn [<import_as_ $u _object>](reader: &mut dyn BufRead) -> Result<EbiObject> {
                    let x = Self::import(reader)?.into();
                    Ok(EbiObject::$t(x))
                }
            }
        }
    };
}

import_as_object!(LabelledPetriNet, labelled_petri_net);
import_as_object!(DirectlyFollowsModel, directly_follows_model);
import_as_object!(
    StochasticDirectlyFollowsModel,
    stochastic_directly_follows_model
);
import_as_object!(StochasticLabelledPetriNet, stochastic_labelled_petri_net);
import_as_object!(
    StochasticDeterministicFiniteAutomaton,
    stochastic_deterministic_finite_automaton
);
import_as_object!(FiniteLanguage, finite_language);
import_as_object!(FiniteStochasticLanguage, finite_stochastic_language);
import_as_object!(DeterministicFiniteAutomaton, deterministic_finite_automaton);
import_as_object!(ProcessTree, process_tree);
import_as_object!(StochasticProcessTree, stochastic_process_tree);
