use std::{io::BufRead, sync::{Arc, Mutex}};
use anyhow::{anyhow, Context, Error, Result};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{ebi_framework::{activity_key::{ActivityKeyTranslator, HasActivityKey}, ebi_command::EbiCommand, ebi_input::EbiInput, ebi_object::EbiTraitObject, ebi_trait::FromEbiTraitObject, importable::Importable}, follower_semantics::FollowerSemantics, math::fraction::Fraction};

use super::ebi_trait_finite_language::EbiTraitFiniteLanguage;

pub trait EbiTraitQueriableStochasticLanguage: HasActivityKey + Sync {
    /**
     * Compute the probability that self produces a trace that is accepted by the follower.
     * Note that the follower must use the same activity key.
     */
    fn get_probability(&self, follower: &FollowerSemantics) -> Result<Fraction>;

    /**
     * Compute the probability that self produces a trace that is accepted by the language.
     */
    fn get_probability_language(&mut self, log: Box<dyn EbiTraitFiniteLanguage>) -> Result<Fraction> {
        let translator = ActivityKeyTranslator::new(log.get_activity_key(), self.get_activity_key_mut());
        
        let progress_bar = EbiCommand::get_progress_bar_ticks(log.len());
        let error: Arc<Mutex<Option<Error>>> = Arc::new(Mutex::new(None));

        let sum = (0..log.len()).into_par_iter().filter_map(|trace_index| {
            let trace = translator.translate_trace(log.get_trace(trace_index).unwrap());
            let c = self.get_probability(&FollowerSemantics::Trace(&trace)).with_context(|| format!("cannot compute probability of trace {:?}", trace));
            progress_bar.inc(1);
            match c {
                Result::Ok(c) => Some(c),
                Err(err) => {
                    let error = Arc::clone(&error);
                    *error.lock().unwrap() = Some(err);
                    None
                }
            }
        }).sum();

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
            _ => Err(anyhow!("cannot read {} {} as a queriable stochastic language", object.get_type().get_article(), object.get_type()))
        }
    }
}

pub fn import<X: 'static + Importable + EbiTraitQueriableStochasticLanguage> (reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitQueriableStochasticLanguage>> {
    match X::import(reader) {
        Ok(x) => Ok(Box::new(x)),
        Err(x) => Err(x),
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{ebi_objects::{finite_language::FiniteLanguage, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage, math::{fraction::Fraction, traits::Zero}};

    #[test]
    fn emsc() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba_ali.slpn").unwrap();
        let mut slpn: StochasticLabelledPetriNet = fin1.parse::<StochasticLabelledPetriNet>().unwrap();

        let fin2 = fs::read_to_string("testfiles/aa.lang").unwrap();
        let slang2 = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let probability = slpn.get_probability_language(slang2).unwrap();

        assert_eq!(probability, Fraction::zero());
    }
}