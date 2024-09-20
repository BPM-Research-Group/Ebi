use fraction::One;
use anyhow::Result;

use crate::{ebi_objects::{labelled_petri_net::LabelledPetriNet, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage}, math::fraction::Fraction};

pub trait UniformStochasticMiner {
    fn mine_uniform_stochastic(self) -> StochasticLabelledPetriNet;
}

impl UniformStochasticMiner for LabelledPetriNet {
    fn mine_uniform_stochastic(self) -> StochasticLabelledPetriNet {
        let len = self.get_number_of_transitions();
        (self, vec![Fraction::one(); len]).into()
    }
}