use crate::{
    ebi_objects::{
        labelled_petri_net::LabelledPetriNet,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet,
    },
    ebi_traits::ebi_trait_semantics::Semantics,
    math::{fraction::Fraction, traits::One},
};

pub trait UniformStochasticMiner {
    fn mine_uniform_stochastic(self) -> StochasticLabelledPetriNet;
}

impl UniformStochasticMiner for LabelledPetriNet {
    fn mine_uniform_stochastic(self) -> StochasticLabelledPetriNet {
        let len = self.get_number_of_transitions();
        (self, vec![Fraction::one(); len]).into()
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{ebi_objects::labelled_petri_net::LabelledPetriNet, techniques::uniform_stochastic_miner::UniformStochasticMiner};

    #[test]
    fn lpn_uniform() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.lpn").unwrap();
        let lpn = Box::new(fin.parse::<LabelledPetriNet>().unwrap());
        let slpn = lpn.mine_uniform_stochastic();
        let fout = fs::read_to_string("testfiles/aa-ab-ba_uni.slpn").unwrap();
        assert_eq!(fout, slpn.to_string())
    }
}