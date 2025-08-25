use ebi_arithmetic::{ebi_number::Zero, fraction::Fraction};

use crate::ebi_objects::{
    language_of_alignments::Move, stochastic_language_of_alignments::StochasticLanguageOfAlignments,
};

pub trait Fitness {
    fn trace_fitness(self) -> Fraction;
}

impl Fitness for StochasticLanguageOfAlignments {
    fn trace_fitness(self) -> Fraction {
        let mut sum = Fraction::zero();
        for (alignment, mut probability) in self {
            let mut count_synchronous = 0usize;
            let mut count_moves = 0usize;
            for movee in alignment {
                match movee {
                    Move::LogMove(_) => count_moves += 1,
                    Move::ModelMove(_, _) => count_moves += 1,
                    Move::SynchronousMove(_, _) => {
                        count_moves += 1;
                        count_synchronous += 1;
                    }
                    Move::SilentMove(_) => {}
                }
            }
            probability *= Fraction::from((count_synchronous, count_moves));
            sum += probability;
        }
        sum
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_arithmetic::fraction::Fraction;

    use crate::{
        ebi_objects::stochastic_language_of_alignments::StochasticLanguageOfAlignments,
        techniques::fitness::Fitness,
    };

    #[test]
    fn trace_fitness_test() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.sali").unwrap();
        let sali = fin.parse::<StochasticLanguageOfAlignments>().unwrap();

        assert_eq!(sali.trace_fitness(), Fraction::from((7, 15)));
    }
}
