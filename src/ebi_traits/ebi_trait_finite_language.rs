use std::{collections::hash_set::Iter, fmt::Display, io::BufRead};

use anyhow::{anyhow, Result};

use crate::{activity_key::{Activity, ActivityKey}, ebi_input_output::EbiInput, ebi_objects::ebi_object::EbiTraitObject, import::Importable, ActivityTrace, Trace};

use super::{ebi_trait::FromEbiTraitObject, ebi_trait_event_log::IndexTrace, ebi_trait_iterable_language::EbiTraitIterableLanguage};

pub trait EbiTraitFiniteLanguage : IndexTrace + Display + EbiTraitIterableLanguage + Sync {
    
}

impl dyn EbiTraitFiniteLanguage {
    fn get_trace_string(&self, trace_index: usize) -> Option<Vec<&str>> {
        Some(self.get_activity_key().deprocess_trace(self.get_trace(trace_index)?))
    }
}

impl FromEbiTraitObject for dyn EbiTraitFiniteLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::FiniteLanguage(e), _) => Ok(e),
            _ => Err(anyhow!("cannot read {} {} as a finite language", object.get_type().get_article(), object.get_type()))
        }
    }
}

pub fn import<X: 'static + Importable + EbiTraitFiniteLanguage> (reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitFiniteLanguage>> {
    match X::import(reader) {
        Ok(x) => Ok(Box::new(x)),
        Err(x) => Err(x),
    }
}