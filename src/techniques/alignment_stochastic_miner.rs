use crate::ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage;
use ebi_objects::{
    BusinessProcessModelAndNotation, LabelledPetriNet, StochasticBusinessProcessModelAndNotation,
    StochasticLabelledPetriNet,
    anyhow::{Result, anyhow},
    ebi_arithmetic::{Fraction, Zero},
    ebi_objects::language_of_alignments::Move,
};
use super::align::Align;

pub trait AlignmentMiner {
    type T;

    fn mine_stochastic_alignment(
        self,
        language: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<Self::T>;
}

impl AlignmentMiner for BusinessProcessModelAndNotation {
    type T = StochasticBusinessProcessModelAndNotation;

    fn mine_stochastic_alignment(
        mut self,
        language: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<Self::T> {
        //reset the weights
        {
            let ids = self
                .sequence_flows()
                .iter()
                .map(|sequence_flow| sequence_flow.global_index())
                .collect::<Vec<_>>();
            for sequence_flow_global_index in ids {
                self.global_index_2_sequence_flow_mut(sequence_flow_global_index)
                    .ok_or_else(|| anyhow!("sequence flow not found"))?
                    .weight = Some(Fraction::zero());
            }
        }

        let alignments = self.align_stochastic_language(language)?;
        for index in 0..alignments.len() {
            let probability = alignments
                .get_probability(index)
                .ok_or_else(|| anyhow!("should not happen"))?;

            let mut marking = self.get_initial_marking()?;

            for movee in alignments
                .get(index)
                .ok_or_else(|| anyhow!("should not happen"))?
            {
                match movee {
                    Move::LogMove(_) => {}
                    Move::ModelMove(_, transition_index)
                    | Move::SynchronousMove(_, transition_index)
                    | Move::SilentMove(transition_index) => {
                        for sequence_flow_index in self
                            .transition_2_marked_sequence_flows(*transition_index, &marking)
                            .ok_or_else(|| anyhow!("this should not happen"))?
                        {
                            let sequence_flow = self
                                .global_index_2_sequence_flow_mut(sequence_flow_index)
                                .ok_or_else(|| anyhow!("sequence flow not found"))?;

                            *sequence_flow.weight.get_or_insert_with(|| Fraction::zero()) +=
                                probability;
                        }

                        self.execute_transition(&mut marking, *transition_index)?;
                    }
                }
            }
        }

        self.try_into()
    }
}

impl AlignmentMiner for LabelledPetriNet {
    type T = StochasticLabelledPetriNet;

    fn mine_stochastic_alignment(
        mut self,
        language: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<StochasticLabelledPetriNet> {
        let mut probabilities: Vec<Fraction> =
            vec![Fraction::zero(); self.get_number_of_transitions()];

        let alignments = self.align_stochastic_language(language)?;
        for index in 0..alignments.len() {
            let probability = alignments
                .get_probability(index)
                .ok_or_else(|| anyhow!("should not happen"))?;

            for movee in alignments
                .get(index)
                .ok_or_else(|| anyhow!("should not happen"))?
            {
                match movee {
                    Move::LogMove(_) => {}
                    Move::ModelMove(_, transition)
                    | Move::SynchronousMove(_, transition)
                    | Move::SilentMove(transition) => {
                        probabilities[*transition] += probability;
                    }
                }
            }
        }

        Ok((self, probabilities).into())
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::{FiniteStochasticLanguage, LabelledPetriNet};

    use crate::techniques::alignment_stochastic_miner::AlignmentMiner;

    #[test]
    fn lpn_uniform() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.lpn").unwrap();
        let lpn = Box::new(fin.parse::<LabelledPetriNet>().unwrap());

        let fin2 = fs::read_to_string("testfiles/ba.slang").unwrap();
        let slang: Box<FiniteStochasticLanguage> =
            Box::new(fin2.parse::<FiniteStochasticLanguage>().unwrap());

        let slpn = lpn.mine_stochastic_alignment(slang).unwrap();
        let fout = fs::read_to_string("testfiles/aa-ab-ba_ali.slpn").unwrap();

        assert_eq!(fout, slpn.to_string())
    }
}
