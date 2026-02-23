use super::ebi_trait_finite_language::EbiTraitFiniteLanguage;
use crate::{
    ebi_framework::{
        ebi_command::EbiCommand, ebi_input::EbiInput, ebi_trait::FromEbiTraitObject,
        ebi_trait_object::EbiTraitObject, trait_importers::ToQueriableStochasticLanguageTrait,
    },
    follower_semantics::FollowerSemantics,
};
use anyhow::{Context, Error, Result, anyhow};
use ebi_objects::{
    ActivityKeyTranslator, CompressedEventLog, DirectlyFollowsGraph, EventLog, EventLogPython,
    EventLogTraceAttributes, EventLogXes, FiniteStochasticLanguage, HasActivityKey, Importable,
    StochasticDirectlyFollowsModel, StochasticLabelledPetriNet,
    ebi_arithmetic::{Fraction, Zero},
    ebi_objects::{
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
        event_log_csv::EventLogCsv,
    },
};
use std::sync::{Arc, Mutex};

pub trait EbiTraitQueriableStochasticLanguage: HasActivityKey + Sync {
    /**
     * Compute the probability that self produces a trace that is accepted by the follower.
     * Note that the follower must use the same activity key.
     */
    fn get_probability(&self, follower: &FollowerSemantics) -> Result<Fraction>;

    /**
     * Compute the probability that self produces a trace that is accepted by the language.
     */
    fn get_probability_language(
        &mut self,
        log: Box<dyn EbiTraitFiniteLanguage>,
    ) -> Result<Fraction> {
        let translator = ActivityKeyTranslator::new(log.activity_key(), self.activity_key_mut());

        let progress_bar = EbiCommand::get_progress_bar_ticks(log.number_of_traces());
        let error: Arc<Mutex<Option<Error>>> = Arc::new(Mutex::new(None));

        let sum = log
            .iter_traces()
            .filter_map(|log_trace| {
                let trace = translator.translate_trace(log_trace);
                let c = self
                    .get_probability(&FollowerSemantics::Trace(&trace))
                    .with_context(|| format!("cannot compute probability of trace {:?}", trace));
                progress_bar.inc(1);
                match c {
                    Result::Ok(c) => Some(c),
                    Err(err) => {
                        let error = Arc::clone(&error);
                        *error.lock().unwrap() = Some(err);
                        None
                    }
                }
            })
            .sum();

        progress_bar.finish_and_clear();

        //see whether an error was reported
        if let Result::Ok(mutex) = Arc::try_unwrap(error) {
            if let Result::Ok(err) = mutex.into_inner() {
                if let Some(err) = err {
                    return Err(err);
                }
            }
        }

        Ok(sum)
    }
}

impl FromEbiTraitObject for dyn EbiTraitQueriableStochasticLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::QueriableStochasticLanguage(e), _) => Ok(e),
            _ => Err(anyhow!(
                "cannot read {} {} as a queriable stochastic language",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl EbiTraitQueriableStochasticLanguage for FiniteStochasticLanguage {
    fn get_probability(&self, follower: &FollowerSemantics) -> Result<Fraction> {
        match follower {
            FollowerSemantics::Trace(trace) => {
                return match self.traces.get(*trace) {
                    Some(x) => Ok(x.clone()),
                    None => Ok(Fraction::zero()),
                };
            }
        }
    }
}

impl<T> ToQueriableStochasticLanguageTrait for T
where
    T: EbiTraitQueriableStochasticLanguage + Importable + 'static,
{
    fn to_queriable_stochastic_language_trait(
        self,
    ) -> Box<dyn EbiTraitQueriableStochasticLanguage> {
        Box::new(self)
    }
}

macro_rules! queriable_via_slpn {
    ($t:ident) => {
        impl ToQueriableStochasticLanguageTrait for $t {
            fn to_queriable_stochastic_language_trait(
                self,
            ) -> Box<dyn EbiTraitQueriableStochasticLanguage> {
                let lpn: StochasticLabelledPetriNet = self.into();
                Box::new(lpn)
            }
        }
    };
}

macro_rules! queriable_via_slang {
    ($t:ident) => {
        impl ToQueriableStochasticLanguageTrait for $t {
            fn to_queriable_stochastic_language_trait(
                self,
            ) -> Box<dyn EbiTraitQueriableStochasticLanguage> {
                let lpn: FiniteStochasticLanguage = self.into();
                Box::new(lpn)
            }
        }
    };
}

queriable_via_slpn!(DirectlyFollowsGraph);
queriable_via_slpn!(StochasticDirectlyFollowsModel);
queriable_via_slang!(EventLog);
queriable_via_slang!(EventLogTraceAttributes);
queriable_via_slang!(CompressedEventLog);
queriable_via_slang!(CompressedEventLogTraceAttributes);
queriable_via_slang!(EventLogXes);
queriable_via_slang!(EventLogCsv);
queriable_via_slang!(EventLogPython);

#[cfg(test)]
mod tests {
    use crate::ebi_traits::ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage;
    use ebi_objects::{
        FiniteLanguage, StochasticLabelledPetriNet,
        ebi_arithmetic::{Fraction, Zero},
    };
    use std::fs;

    #[test]
    fn emsc() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba_ali.slpn").unwrap();
        let mut slpn: StochasticLabelledPetriNet =
            fin1.parse::<StochasticLabelledPetriNet>().unwrap();

        let fin2 = fs::read_to_string("testfiles/aa.lang").unwrap();
        let slang2 = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let probability = slpn.get_probability_language(slang2).unwrap();

        assert_eq!(probability, Fraction::zero());
    }
}
