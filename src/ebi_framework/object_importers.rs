use anyhow::Result;
use pastey::paste;
use std::io::BufRead;

use ebi_objects::{
    DeterministicFiniteAutomaton, DirectlyFollowsModel, EbiObject, EventLog, EventLogCsv,
    EventLogXes, FiniteLanguage, FiniteStochasticLanguage, LabelledPetriNet, LanguageOfAlignments,
    ProcessTree, StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticLabelledPetriNet, StochasticLanguageOfAlignments,
    StochasticNondeterministicFiniteAutomaton, StochasticProcessTree,
    traits::importable::{Importable, ImporterParameterValues},
};

macro_rules! import_as_object {
    ($t:ident, $u:expr) => {
        paste! {
            pub trait [<To $t Object>] {
                fn [<to_ $u _object>](self) -> EbiObject;
            }

            pub trait [<ImportAs $t Object>] {
                /// A To will first import, and then convert. The latter conversion may not fail.
                fn [<import_as_ $u _object>](reader: &mut dyn BufRead, parameter_values: &ImporterParameterValues) -> Result<EbiObject>;
            }

            impl<T> [<To $t Object>] for T
            where
                T: Into<$t>,
            {
                fn [<to_ $u _object>](self) -> EbiObject {
                    EbiObject::$t(self.into())
                }
            }

            impl<T: Importable> [<ImportAs $t Object>] for T
            where T: Into<$t>{
                fn [<import_as_ $u _object>](reader: &mut dyn BufRead, parameter_values: &ImporterParameterValues) -> Result<EbiObject> {
                    let x = Self::import(reader, parameter_values)?.into();
                    Ok(EbiObject::$t(x))
                }
            }

            pub trait [<TryTo $t Object>] {
                /// A TryTo will first import, and then try to convert. The latter conversion may fail.
                fn [<try_import_as_ $u _object>](reader: &mut dyn BufRead, parameter_values: &ImporterParameterValues) -> Result<EbiObject>;
            }

            impl<T: Importable> [<TryTo $t Object>] for T
            where
                T: TryInto<$t, Error = anyhow::Error>,
            {
                fn [<try_import_as_ $u _object>](reader: &mut dyn BufRead, parameter_values: &ImporterParameterValues) -> Result<EbiObject> {
                    let x = Self::import(reader, parameter_values)?.try_into()?;
                    Ok(EbiObject::$t(x))
                }
            }
        }
    };
}

import_as_object!(EventLog, event_log);
import_as_object!(EventLogXes, event_log_xes);
import_as_object!(EventLogCsv, event_log_csv);
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
import_as_object!(
    StochasticNondeterministicFiniteAutomaton,
    stochastic_nondeterministic_finite_automaton
);
import_as_object!(FiniteLanguage, finite_language);
import_as_object!(FiniteStochasticLanguage, finite_stochastic_language);
import_as_object!(DeterministicFiniteAutomaton, deterministic_finite_automaton);
import_as_object!(ProcessTree, process_tree);
import_as_object!(StochasticProcessTree, stochastic_process_tree);
import_as_object!(LanguageOfAlignments, language_of_alignments);
import_as_object!(
    StochasticLanguageOfAlignments,
    stochastic_language_of_alignments
);
