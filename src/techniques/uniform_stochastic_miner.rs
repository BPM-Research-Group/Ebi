use crate::{
    ebi_objects::{
        labelled_petri_net::LabelledPetriNet, process_tree::ProcessTree,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet,
        stochastic_process_tree::StochasticProcessTree,
    },
    ebi_traits::ebi_trait_semantics::Semantics,
    math::{fraction::Fraction, traits::One},
};

pub trait UniformStochasticMinerLPN {
    fn mine_uniform_stochastic_lpn(self) -> StochasticLabelledPetriNet;
}

pub trait UniformStochasticMinerTree {
    fn mine_uniform_stochastic_tree(self) -> StochasticProcessTree;
}

impl UniformStochasticMinerLPN for LabelledPetriNet {
    fn mine_uniform_stochastic_lpn(self) -> StochasticLabelledPetriNet {
        let len = self.get_number_of_transitions();
        (self, vec![Fraction::one(); len]).into()
    }
}

impl UniformStochasticMinerTree for ProcessTree {
    fn mine_uniform_stochastic_tree(self) -> StochasticProcessTree {
        let len = self.get_number_of_transitions() - 1;
        (self, vec![Fraction::one(); len], Fraction::one()).into()
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{
        ebi_objects::{labelled_petri_net::LabelledPetriNet, process_tree::ProcessTree},
        techniques::uniform_stochastic_miner::{UniformStochasticMinerLPN, UniformStochasticMinerTree},
    };

    #[test]
    fn lpn_uniform() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.lpn").unwrap();
        let lpn = Box::new(fin.parse::<LabelledPetriNet>().unwrap());
        let slpn = lpn.mine_uniform_stochastic_lpn();
        let fout = fs::read_to_string("testfiles/aa-ab-ba_uni.slpn").unwrap();
        assert_eq!(fout, slpn.to_string())
    }

    #[test]
    fn tree_uniform() {
        let fin = fs::read_to_string("testfiles/seq(a-xor(b-c)).ptree").unwrap();
        let lpn = Box::new(fin.parse::<ProcessTree>().unwrap());
        let slpn = lpn.mine_uniform_stochastic_tree();
        let fout = fs::read_to_string("testfiles/seq(a-xor(b-c))-uniform.sptree").unwrap();
        assert_eq!(fout, slpn.to_string())
    }
}
