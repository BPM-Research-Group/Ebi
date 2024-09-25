use anyhow::Result;
use crate::{ebi_framework::activity_key::ActivityKeyTranslator, ebi_objects::{alignments::Move, labelled_petri_net::LabelledPetriNet, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, math::fraction::Fraction};

use super::align::Align;

pub trait AlignmentMiner {
    fn mine_stochastic_alignment(self, language: Box<dyn EbiTraitFiniteStochasticLanguage>) -> Result<StochasticLabelledPetriNet>;
}

impl AlignmentMiner for LabelledPetriNet {
    fn mine_stochastic_alignment(mut self, language: Box<dyn EbiTraitFiniteStochasticLanguage>) -> Result<StochasticLabelledPetriNet> {
        let mut weights: Vec<Fraction> = vec![Fraction::zero(); self.get_number_of_transitions()];
    
        let translator = ActivityKeyTranslator::new(language.get_activity_key(), self.get_activity_key_mut());
    
        for (trace, probability) in language.iter_trace_probability() {
            let translated_trace = translator.translate_trace(trace);
    
            let (moves, _) = self.align_trace(&translated_trace)?;
            for movee in moves {
                match movee {
                    Move::LogMove(_) => {},
                    Move::ModelMove(_, transition) |  Move::SynchronousMove(_, transition) | Move::SilentMove(transition) => {
                        weights[transition] += probability;
                    },
                }
            }
        }
    
        Ok((self, weights).into())
    }
}