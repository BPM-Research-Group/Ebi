use std::{fmt::{Debug, Display}, hash::Hash};
use anyhow::Result;

use crate::{ebi_framework::activity_key::Activity, ebi_objects::alignments::Alignments, ebi_traits::ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, StochasticSemantics}, math::fraction::Fraction};

pub trait ExplainTrace {
    fn explain_trace(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments>;
}

impl ExplainTrace for EbiTraitStochasticSemantics {
    fn explain_trace(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments> {
        match self {
            EbiTraitStochasticSemantics::Usize(sem) => sem.explain_trace(trace, balance),
            EbiTraitStochasticSemantics::Marking(sem) => sem.explain_trace(trace, balance),
        }
    }
}

impl <T, FS> ExplainTrace for T where T: StochasticSemantics<State = FS> + ?Sized, FS: Hash + Display + Debug + Clone + Eq {

    fn explain_trace(&self, _trace: &Vec<Activity>, _balance: &Fraction) -> Result<Alignments> {
        todo!()
    }

}