use fraction::One;
use anyhow::Result;

use crate::{ebi_objects::{labelled_petri_net::LabelledPetriNet, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_labelled_petri_net::EbiTraitLabelledPetriNet}, math::fraction::Fraction};

pub trait UniformStochasticMiner {
    fn mine_uniform_stochastic(&self) -> StochasticLabelledPetriNet;
}

//impl UniformStochasticMiner for dyn EbiTraitLabelledPetriNet { //use the dyn syntax as we need to consume the input net
impl <T> UniformStochasticMiner for T where T: EbiTraitLabelledPetriNet + ?Sized {
    fn mine_uniform_stochastic(&self) -> StochasticLabelledPetriNet {
        let len = self.get_transitions().len();
        StochasticLabelledPetriNet::from_lpn(self, vec![Fraction::one(); len])
    }
}