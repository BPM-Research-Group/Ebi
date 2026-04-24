use ebi_objects::{
    BusinessProcessModelAndNotation, LabelledPetriNet, ProcessTree,
    StochasticBusinessProcessModelAndNotation, StochasticLabelledPetriNet, StochasticProcessTree,
    anyhow::anyhow,
    ebi_arithmetic::{Fraction, One},
    ebi_objects::process_tree::get_number_of_transitions,
};
use ebi_optimisation::anyhow::Result;

pub trait UniformStochasticBusinessProcessModelAndNotation {
    fn mine_uniform_stochastic_business_process_model_and_notation(
        self,
    ) -> Result<StochasticBusinessProcessModelAndNotation>;
}

pub trait UniformStochasticMinerLPN {
    fn mine_uniform_stochastic_lpn(self) -> StochasticLabelledPetriNet;
}

pub trait UniformStochasticMinerTree {
    fn mine_uniform_stochastic_tree(self) -> StochasticProcessTree;
}

impl UniformStochasticBusinessProcessModelAndNotation for BusinessProcessModelAndNotation {
    fn mine_uniform_stochastic_business_process_model_and_notation(
        mut self,
    ) -> Result<StochasticBusinessProcessModelAndNotation> {
        let global_indices = self
            .sequence_flows()
            .iter()
            .map(|sequence_flow| sequence_flow.global_index())
            .collect::<Vec<_>>();

        for global_index in global_indices {
            self.global_index_2_sequence_flow_mut(global_index)
                .ok_or_else(|| anyhow!("sequence flow not found"))?
                .weight = Some(Fraction::one());
        }
        self.try_into()
    }
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
