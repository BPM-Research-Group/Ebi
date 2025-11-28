use crate::{
    ebi_framework::ebi_trait_object::EbiTraitObject,
    ebi_traits::{
        ebi_trait_activities::EbiTraitActivities, ebi_trait_event_log::EbiTraitEventLog,
        ebi_trait_event_log_trace_attributes::EbiTraitEventLogTraceAttributes,
        ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_graphable::EbiTraitGraphable,
        ebi_trait_iterable_language::EbiTraitIterableLanguage,
        ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
        ebi_trait_semantics::EbiTraitSemantics,
        ebi_trait_stochastic_deterministic_semantics::EbiTraitStochasticDeterministicSemantics,
        ebi_trait_stochastic_semantics::EbiTraitStochasticSemantics,
    },
};
use anyhow::Result;
use ebi_objects::{Importable, traits::importable::ImporterParameterValues};
use pastey::paste;
use std::io::BufRead;

macro_rules! import_as_trait_dyn {
    ($t:ident, $u:expr) => {
        paste! {
            pub trait [<ImportAs $t Trait>]: Importable + [<To $t Trait>] {
                fn [<import_as_ $u _trait>] (
                    reader: &mut dyn BufRead,
                    parameter_values: &ImporterParameterValues,
                ) -> Result<Box<dyn [<EbiTrait $t>]>>
                where
                    Self: Sized,
                {
                    Ok(Self::import(reader, parameter_values)?.[<to_ $u _trait>]())
                }
            }

            impl <T> [<ImportAs $t Trait>] for T where T: Importable + [<To $t Trait>] {}

            pub trait [<To $t Trait>] {
                fn [<to_ $u _trait>](self) -> Box<dyn [<EbiTrait $t >]>;

                fn [<to_ $u _ebi_trait_object>](self) -> EbiTraitObject
                where
                    Self: Sized,
                {
                    EbiTraitObject::[< $t >](self.[<to_ $u _trait>]())
                }
            }
        }
    };
}

macro_rules! import_as_trait {
    ($t:ident, $u:expr) => {
        paste! {
            pub trait [<ImportAs $t Trait>]: Importable + [<To $t Trait>] {
                fn [<import_as_ $u _trait>] (
                    reader: &mut dyn BufRead,
                    parameter_values: &ImporterParameterValues,
                ) -> Result<[<EbiTrait $t>]>
                where
                    Self: Sized,
                {
                    Ok(Self::import(reader, parameter_values)?.[<to_ $u _trait>]())
                }
            }

            impl <T> [<ImportAs $t Trait>] for T where T: Importable + [<To $t Trait>] {}

            pub trait [<To $t Trait>] {
                fn [<to_ $u _trait>](self) -> [<EbiTrait $t >];

                fn [<to_ $u _ebi_trait_object>](self) -> EbiTraitObject
                where
                    Self: Sized,
                {
                    EbiTraitObject::[< $t >](self.[<to_ $u _trait>]())
                }
            }
        }
    };
}

import_as_trait_dyn!(Activities, activities);
import_as_trait_dyn!(IterableLanguage, iterable_language);
import_as_trait_dyn!(IterableStochasticLanguage, iterable_stochastic_language);
import_as_trait_dyn!(QueriableStochasticLanguage, queriable_stochastic_language);
import_as_trait_dyn!(Graphable, graphable);
import_as_trait_dyn!(FiniteLanguage, finite_language);
import_as_trait_dyn!(FiniteStochasticLanguage, finite_stochastic_language);
import_as_trait_dyn!(EventLog, event_log);
import_as_trait_dyn!(EventLogTraceAttributes, event_log_trace_attributes);
import_as_trait!(Semantics, semantics);
import_as_trait!(StochasticSemantics, stochastic_semantics);
import_as_trait!(
    StochasticDeterministicSemantics,
    stochastic_deterministic_semantics
);
