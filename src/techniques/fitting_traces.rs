use ebi_objects::{
    StochasticLanguageOfAlignments,
    ebi_arithmetic::{Fraction, Zero},
    ebi_objects::language_of_alignments::Move,
};

pub trait FittingTraces {
    fn fitting_traces(self) -> Fraction;
}

impl FittingTraces for StochasticLanguageOfAlignments {
    fn fitting_traces(self) -> Fraction {
        let mut count_fitting = Fraction::zero();
        let mut count_total = Fraction::zero();

        for (alignment, probability) in self.into_iter() {
            count_total += &probability;
            if alignment.into_iter().all(|movee| match movee {
                Move::LogMove { .. } => false,
                Move::ModelMove { .. } => false,
                Move::SynchronousMove { .. } => true,
                Move::SilentMove { .. } => true,
            }) {
                count_fitting += probability;
            }
        }
        count_fitting / count_total
    }
}
