use crate::{ebi_objects::{labelled_petri_net::LabelledPetriNet, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::ebi_trait_semantics::Semantics, math::fraction::Fraction};

pub trait UniformStochasticMiner {
    fn mine_uniform_stochastic(self) -> StochasticLabelledPetriNet;
}

impl UniformStochasticMiner for LabelledPetriNet {
    fn mine_uniform_stochastic(self) -> StochasticLabelledPetriNet {
        let len = self.get_number_of_transitions();
        (self, vec![Fraction::one(); len]).into()
    }
}