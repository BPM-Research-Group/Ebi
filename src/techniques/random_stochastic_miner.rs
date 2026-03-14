use ebi_objects::{
    BusinessProcessModelAndNotation, LabelledPetriNet, ProcessTree,
    StochasticBusinessProcessModelAndNotation, StochasticLabelledPetriNet, StochasticProcessTree,
    ebi_arithmetic::{Fraction, Random},
    ebi_objects::process_tree::get_number_of_transitions,
    malachite::base::random::Seed,
};
use ebi_optimisation::anyhow::{Result, anyhow};

pub trait RandomMinerSBPMN {
    fn mine_random_stochastic_business_process_model_and_notation(
        self,
        seed: Seed,
    ) -> Result<StochasticBusinessProcessModelAndNotation>;
}

pub trait RandomMinerSLPN {
    fn mine_random_stochastic_labelled_petri_net(self, seed: Seed) -> StochasticLabelledPetriNet;
}

pub trait RandomMinerSTREE {
    fn mine_random_stochastic_process_tree(self, seed: Seed) -> StochasticProcessTree;
}

impl RandomMinerSBPMN for BusinessProcessModelAndNotation {
    fn mine_random_stochastic_business_process_model_and_notation(
        mut self,
        mut seed: Seed,
    ) -> Result<StochasticBusinessProcessModelAndNotation> {
        let global_indices = self
            .sequence_flows()
            .iter()
            .map(|sequence_flow| sequence_flow.global_index())
            .collect::<Vec<_>>();

        for global_index in global_indices {
            self.global_index_2_sequence_flow_mut(global_index)
                .ok_or_else(|| anyhow!("sequence flow not found"))?
                .weight = Some(Fraction::random_non_zero_probability(10, seed));
            seed = seed.next();
        }
        self.try_into()
    }
}

impl RandomMinerSLPN for LabelledPetriNet {
    fn mine_random_stochastic_labelled_petri_net(
        self,
        mut seed: Seed,
    ) -> StochasticLabelledPetriNet {
        let len = self.transition2input_places.len();
        let mut weights = Vec::with_capacity(len);
        for _ in 0..len {
            weights.push(Fraction::random_non_zero_probability(10, seed));
            seed = seed.next()
        }
        (self, weights).into()
    }
}

impl RandomMinerSTREE for ProcessTree {
    fn mine_random_stochastic_process_tree(self, mut seed: Seed) -> StochasticProcessTree {
        let termination = Fraction::random_non_zero_probability(10, Fraction::random_seed());

        let len = get_number_of_transitions(&self) - 1;
        let mut weights = Vec::with_capacity(len);
        for _ in 0..len {
            weights.push(Fraction::random_non_zero_probability(10, seed));
            seed = seed.next();
        }

        (self, weights, termination).try_into().unwrap()
    }
}
