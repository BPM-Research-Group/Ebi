use crate::{
    ebi_framework::ebi_trait::EbiTrait,
    ebi_traits::{
        ebi_trait_activities::EbiTraitActivities, ebi_trait_event_log::EbiTraitEventLog,
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

pub enum EbiTraitObject {
    EventLog(Box<dyn EbiTraitEventLog>),
    IterableLanguage(Box<dyn EbiTraitIterableLanguage>),
    FiniteLanguage(Box<dyn EbiTraitFiniteLanguage>),
    FiniteStochasticLanguage(Box<dyn EbiTraitFiniteStochasticLanguage>),
    IterableStochasticLanguage(Box<dyn EbiTraitIterableStochasticLanguage>),
    QueriableStochasticLanguage(Box<dyn EbiTraitQueriableStochasticLanguage>),
    StochasticDeterministicSemantics(EbiTraitStochasticDeterministicSemantics),
    StochasticSemantics(EbiTraitStochasticSemantics),
    Semantics(EbiTraitSemantics),
    Graphable(Box<dyn EbiTraitGraphable>),
    Activities(Box<dyn EbiTraitActivities>),
}

impl EbiTraitObject {
    pub fn get_trait(&self) -> EbiTrait {
        match self {
            EbiTraitObject::EventLog(_) => EbiTrait::EventLog,
            EbiTraitObject::IterableLanguage(_) => EbiTrait::IterableLanguage,
            EbiTraitObject::FiniteLanguage(_) => EbiTrait::FiniteLanguage,
            EbiTraitObject::FiniteStochasticLanguage(_) => EbiTrait::FiniteStochasticLanguage,
            EbiTraitObject::IterableStochasticLanguage(_) => EbiTrait::IterableStochasticLanguage,
            EbiTraitObject::QueriableStochasticLanguage(_) => EbiTrait::QueriableStochasticLanguage,
            EbiTraitObject::StochasticDeterministicSemantics(_) => {
                EbiTrait::StochasticDeterministicSemantics
            }
            EbiTraitObject::StochasticSemantics(_) => EbiTrait::StochasticSemantics,
            EbiTraitObject::Semantics(_) => EbiTrait::Semantics,
            EbiTraitObject::Graphable(_) => EbiTrait::Graphable,
            EbiTraitObject::Activities(_) => EbiTrait::Activities,
        }
    }
}

#[cfg(test)]
mod tests {

    use ebi_objects::EbiObjectType;
    use strum::IntoEnumIterator;

    use crate::ebi_framework::{
        ebi_file_handler::get_file_handlers,
        ebi_input::{EbiInput, TEST_INPUT_TYPE_STRING},
        ebi_trait::FromEbiTraitObject,
        prom_link::{
            get_java_object_handlers_that_can_export, get_java_object_handlers_that_can_import,
        },
    };

    #[test]
    fn object_types() {
        for object_type in EbiObjectType::iter() {
            object_type.get_article();
            get_file_handlers(&object_type);
            get_java_object_handlers_that_can_export(&object_type);
            get_java_object_handlers_that_can_import(&object_type);
        }

        let _ =
            String::from_trait_object(EbiInput::String("xyz".to_string(), &TEST_INPUT_TYPE_STRING));
    }

    #[test]
    fn objects() {
        for (input, _, _, _) in crate::tests::get_all_test_files() {
            if let EbiInput::Object(object, _) = input {
                object.get_type();
                object.to_string();
            } else if let EbiInput::Trait(object, _) = input {
                object.get_trait();
            }
        }
    }
}
