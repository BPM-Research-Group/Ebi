use std::{collections::VecDeque, fmt::{Debug, Display}, usize};
use core::hash::Hash;
use anyhow::Result;
use crate::ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_semantics::{EbiTraitSemantics, Semantics}};

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
        for (trace, _) in self.iter_trace_probability() {
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