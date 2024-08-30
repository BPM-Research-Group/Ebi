use fraction::One;

use crate::{ebi_objects::{labelled_petri_net::LabelledPetriNet, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::ebi_trait_labelled_petri_net::EbiTraitLabelledPetriNet, math::fraction::Fraction};

pub fn uniform_stochastic_miner(net: Box<dyn EbiTraitLabelledPetriNet>) -> StochasticLabelledPetriNet {
    let len = net.get_transitions().len();
    (net, vec![Fraction::one(); len]).into()
}