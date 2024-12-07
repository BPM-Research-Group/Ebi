use anyhow::{anyhow, Result};
use crate::{ebi_objects::{language_of_alignments::Move, labelled_petri_net::LabelledPetriNet, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_semantics::Semantics}, math::fraction::Fraction};

use super::align::Align;

pub trait AlignmentMiner {
    fn mine_stochastic_alignment(self, language: Box<dyn EbiTraitFiniteStochasticLanguage>) -> Result<StochasticLabelledPetriNet>;
}

impl AlignmentMiner for LabelledPetriNet {
    fn mine_stochastic_alignment(mut self, language: Box<dyn EbiTraitFiniteStochasticLanguage>) -> Result<StochasticLabelledPetriNet> {
        let mut weights: Vec<Fraction> = vec![Fraction::zero(); self.get_number_of_transitions()];

        let alignments = self.align_stochastic_language(language)?;
        for index in 0..alignments.len() {
            let weight = alignments.get_weight(index).ok_or_else(|| anyhow!("should not happen"))?;
            
            for movee in alignments.get(index).ok_or_else(|| anyhow!("should not happen"))? {
                match movee {
                    Move::LogMove(_) => {},
                    Move::ModelMove(_, transition) |  Move::SynchronousMove(_, transition) | Move::SilentMove(transition) => {
                        weights[*transition] += weight;
                    },
                }
            }
        }
    
        Ok((self, weights).into())
    }
}