use std::{collections::HashMap, rc::Rc};

use anyhow::Result;
use fraction::{One, Zero};
use crate::{activity_key::{Activity, ActivityKeyTranslator}, ebi_objects::{alignments::Move, labelled_petri_net::LabelledPetriNet, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage, ebi_trait_labelled_petri_net::EbiTraitLabelledPetriNet, ebi_trait_stochastic_semantics::TransitionIndex}, math::fraction::Fraction};

use super::align::Align;

pub trait AlignmentMiner {
    fn mine_stochastic_alignment(self: Box<Self>, language: Box<dyn EbiTraitFiniteStochasticLanguage>) -> Result<StochasticLabelledPetriNet>;
}

impl AlignmentMiner for dyn EbiTraitLabelledPetriNet { //use the dyn syntax as we need to consume the input net
    fn mine_stochastic_alignment(self: Box<Self>, language: Box<dyn EbiTraitFiniteStochasticLanguage>) -> Result<StochasticLabelledPetriNet> {
        let mut weights: Vec<Fraction> = vec![Fraction::zero(); self.get_number_of_transitions()];

        let mut net = self.clone();
    
        let translator = ActivityKeyTranslator::new(language.get_activity_key(), net.get_activity_key_mut());
        let semantics = self.get_semantics();
    
        for (trace, probability) in language.iter_trace_probability() {
            let translated_trace = translator.translate_trace(trace);
    
            let (moves, _) = semantics.align_trace(&translated_trace)?;
            for movee in moves {
                match movee {
                    Move::LogMove(_) => {},
                    Move::ModelMove(_, transition) |  Move::SynchronousMove(_, transition) | Move::SilentMove(transition) => {
                        weights[transition] += probability;
                    },
                }
            }
    
        }
    
        Ok((net, weights).into())
    }
}