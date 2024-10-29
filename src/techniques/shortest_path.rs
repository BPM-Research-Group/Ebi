use std::{collections::{HashMap, VecDeque}, fmt::{Debug, Display}, usize};
use core::hash::Hash;
use anyhow::{anyhow, Result};
use crate::{ebi_framework::activity_key::Activity, ebi_objects::finite_stochastic_language::FiniteStochasticLanguage, ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_semantics::{EbiTraitSemantics, Semantics}}, math::fraction::Fraction};

pub trait ShortestPath {
     /**
     * Find the shortest path in the model.
     */
    fn get_shortest_path_len(&self) -> Result<usize>;
}

impl ShortestPath for EbiTraitSemantics{
    fn get_shortest_path_len(&self) -> Result<usize> {
        match self {
            EbiTraitSemantics::Marking(semantics)=> semantics.get_shortest_path_len(),
            EbiTraitSemantics::Usize(semantics) => semantics.get_shortest_path_len(),
        }
    }
}

impl ShortestPath for dyn EbiTraitFiniteStochasticLanguage {
    fn get_shortest_path_len(&self) -> Result<usize> {
        let mut result = usize::MAX;

        let mut min_trace_len = usize::MAX;
        for (trace, probability) in self.iter_trace_probability() {
            if trace.len() < min_trace_len{
                result = trace.len();
                min_trace_len = trace.len();
            }
        }
        Ok(result)
    }
}

impl <T, A> ShortestPath for T where T: Semantics<State = A> + ?Sized, A: Display + Clone + Hash + Eq + Debug{
    fn get_shortest_path_len(&self) -> Result<usize> {
        let mut result = usize::MAX;
        let mut queue = VecDeque::new();
        queue.push_back((0, self.get_initial_state()));

        while let Some(y) = queue.pop_front(){
            match y {
                (prefix_len, p_state) => {
                    if self.is_final_state(&p_state){
                        result = prefix_len;
                        break;
                    }
                    let enabled_transitions: Vec<usize> = self.get_enabled_transitions(&p_state);
                    for transition in &enabled_transitions {
                        let mut new_state = p_state.clone();
                        self.execute_transition(&mut new_state, *transition)?;
                        queue.push_back((prefix_len+1, new_state));
                    }
                }
            }
        }
        Ok(result)
    }
}


enum Y<FS: Hash + Display + Debug + Clone + Eq> {
    Prefix(Vec<Activity>, FS),
    Trace(Vec<Activity>)
}

impl <FS: Hash + Display + Debug + Clone + Eq> Eq for Y<FS> {}

impl <FS: Hash + Display + Debug + Clone + Eq> PartialEq for Y<FS> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Prefix(l0, _), Self::Prefix(r0, _)) => l0 == r0,
            (Self::Trace(l0), Self::Trace(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl <FS: Hash + Display + Debug + Clone + Eq> Hash for Y<FS> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Y::Prefix(t, _) => t.hash(state),
            Y::Trace(t) => t.hash(state),
        }
    }
}