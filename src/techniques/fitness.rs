use anyhow::Result;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{ebi_objects::{alignments::Move, stochastic_language_of_alignments::StochasticLanguageOfAlignments}, math::fraction::Fraction};

pub trait Fitness {
    fn fitness(&self) -> Result<Fraction>;
}

impl Fitness for StochasticLanguageOfAlignments {
    fn fitness(&self) -> Result<Fraction> {
        Ok((0..self.len()).into_par_iter().map(|i| { //iterate over traces concurrently
         
            let (sync, total) = self.get(i).unwrap().iter().map(|amove| { //iterate over moves to count number of sync moves vs. total numver of non-tau moves
                match amove {
                    Move::LogMove(_) => (0, 1),
                    Move::ModelMove(_, _) => (0, 1),
                    Move::SynchronousMove(_, _) => (1, 1),
                    Move::SilentMove(_) => (0, 0),
                }
            }).fold((0usize, 0usize), |(sync, total), (a, b)| (sync + a, total + b));
            
            let mut weight = self.get_weight(i).unwrap().clone();
            if total != 0 {
                weight *= Fraction::from((sync, total))
            }
            weight
        }).sum::<Fraction>())
    }
}