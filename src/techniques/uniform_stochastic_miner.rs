use ebi_objects::{
    LabelledPetriNet, ProcessTree, StochasticLabelledPetriNet, StochasticProcessTree,
    ebi_arithmetic::{Fraction, One},
    ebi_objects::process_tree::get_number_of_transitions,
};

pub trait UniformStochasticMinerLPN {
    fn mine_uniform_stochastic_lpn(self) -> StochasticLabelledPetriNet;
}

pub trait UniformStochasticMinerTree {
    fn mine_uniform_stochastic_tree(self) -> StochasticProcessTree;
}

impl UniformStochasticMinerLPN for LabelledPetriNet {
    fn mine_uniform_stochastic_lpn(self) -> StochasticLabelledPetriNet {
        let len = self.transition2input_places.len();
        (self, vec![Fraction::one(); len]).into()
    }
}

impl UniformStochasticMinerTree for ProcessTree {
    fn mine_uniform_stochastic_tree(self) -> StochasticProcessTree {
        let len = get_number_of_transitions(&self) - 1;
        (self, vec![Fraction::one(); len], Fraction::one())
            .try_into()
            .unwrap()
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::{LabelledPetriNet, ProcessTree};

    use crate::techniques::uniform_stochastic_miner::{
        UniformStochasticMinerLPN, UniformStochasticMinerTree,
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
