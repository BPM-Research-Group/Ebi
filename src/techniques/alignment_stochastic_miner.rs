use crate::{ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, semantics::semantics::Semantics};
use anyhow::{Result, anyhow};
use ebi_arithmetic::{Fraction, Zero};
use ebi_objects::{
    LabelledPetriNet, StochasticLabelledPetriNet, ebi_objects::language_of_alignments::Move,
};

use super::align::Align;

pub trait AlignmentMiner {
    fn mine_stochastic_alignment(
        self,
        language: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<StochasticLabelledPetriNet>;
}

impl AlignmentMiner for LabelledPetriNet {
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
