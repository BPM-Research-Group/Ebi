use super::{
    ebi_trait_finite_language::EbiTraitFiniteLanguage,
    ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage,
};
use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
    trait_importers::ToFiniteStochasticLanguageTrait,
};
use ebi_objects::{
    anyhow::{anyhow, Result},
    ebi_arithmetic::{Fraction, Zero},
    ebi_objects::finite_stochastic_language::FiniteStochasticLanguage,
    Activity, Importable, TranslateActivityKey,
};
use std::fmt::Debug;

/// Allows for iteration over traces and their probabilities.
///
/// For documentation on Ebi traits and mathematical definitions, please refer to the [manual](https://github.com/BPM-Research-Group/Ebi/releases/latest/download/manual.pdf).
pub trait EbiTraitFiniteStochasticLanguage:
    EbiTraitIterableStochasticLanguage + EbiTraitFiniteLanguage + Sync + Debug + TranslateActivityKey
{
    fn get_probability_sum(&self) -> Fraction;

    /// Creates a finite stochastic language.
    /// This clones all traces, so if you have a struct, it is probably cheaper to call .into() on it.
    fn to_finite_stochastic_language(&self) -> FiniteStochasticLanguage;

    /**
     * Remove traces for which the function returns false.
     * Normalisation is applied afterwards, that is, after the call, the sum of probabilities is 1, unless the language is empty.
     *
     * Note to callers: please put the closure definition inside the Box::new in the call of retain_traces.
     * Otherwise, Rust may give weird compile errors.
     */
    fn retain_traces_probabilities<'a>(
        &'a mut self,
        f: Box<dyn Fn(&Vec<Activity>, &Fraction) -> bool + 'static>,
    );

    /**
     * Remove traces for which the function returns false.
     * It is the responsibility of the caller to ensure normalisation. That is, probabilities will not be re-scaled.
     *
     * Note to callers: please put the closure definition inside the Box::new in the call of retain_traces.
     * Otherwise, Rust may give weird compile errors.
     */
    fn retain_traces_probabilities_mut<'a>(
        &'a mut self,
        f: Box<dyn FnMut(&Vec<Activity>, &mut Fraction) -> bool + 'static>,
    );

    /// Remove all traces.
    fn clear(&mut self);

    /// Make the sum of probabilities one, unless there are no traces.
    fn normalise(&mut self);

    fn trait_explanation() -> &'static str
    where
        Self: Sized,
    {
        "Iterating over a finite stochastic language will yield each trace variant once, as well as how likely that trace variant is in the language.
            The probabilities are positive and their sum must be smaller than or equal to one.
            The ``missing'' probability means ``no trace'' or ``livelock''.
            \\\\
            Definition: let $\\Sigma$ be an alphabet of activities.
            Then, a \\emph{trace} $\\sigma \\in \\Sigma^*$ is a finite sequence of activities, and 
            a \\emph{finite stochastic language} $S \\subseteq \\Sigma^* \\mapsto \\mathcal{R}^+$ is a finite set of trace-probability tuples, such that $1 \\geq \\sum_{\\sigma \\in \\Sigma^*} S(\\sigma) $."
    }
}

impl FromEbiTraitObject for dyn EbiTraitFiniteStochasticLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(e), _) => Ok(e),
            _ => Err(anyhow!(
                "cannot read {} {} as a finite stochastic language",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl EbiTraitFiniteStochasticLanguage for FiniteStochasticLanguage {
    fn to_finite_stochastic_language(&self) -> FiniteStochasticLanguage {
        self.clone()
    }

    fn get_probability_sum(&self) -> Fraction {
        self.traces.values().fold(Fraction::zero(), |mut x, y| {
            x += y;
            x
        })
    }

    fn retain_traces_probabilities<'a>(
        &'a mut self,
        f: Box<dyn Fn(&Vec<Activity>, &Fraction) -> bool + 'static>,
    ) {
        self.traces
            .retain(|trace, probability| f(trace, probability));
        self.normalise();
    }

    fn retain_traces_probabilities_mut<'a>(
        &'a mut self,
        f: Box<dyn FnMut(&Vec<Activity>, &mut Fraction) -> bool + 'static>,
    ) {
        self.traces.retain(f);
    }

    fn clear(&mut self) {
        self.traces.clear();
    }

    fn normalise(&mut self) {
        self.normalise();
    }
}

impl<T> ToFiniteStochasticLanguageTrait for T
where
    T: Into<FiniteStochasticLanguage> + Importable,
{
    fn to_finite_stochastic_language_trait(self) -> Box<dyn EbiTraitFiniteStochasticLanguage> {
        Box::new(Into::<FiniteStochasticLanguage>::into(self))
    }
}
