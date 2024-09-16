use std::{fmt::{Debug, Display}, hash::Hash};

use anyhow::{anyhow, Result};

use crate::{activity_key::{Activity, ActivityKeyTranslator}, ebi_objects::alignments::{Alignments, Move}, ebi_traits::{ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_semantics::Semantics}};

impl <FS: Display + Debug + Clone + Hash + Eq> dyn Semantics<State = FS> {
    
    pub fn align_log(&mut self, log: Box<dyn EbiTraitFiniteLanguage>) -> Result<Alignments> {
        let translator = ActivityKeyTranslator::new(log.get_activity_key(), self.get_activity_key_mut());

        let mut result = Alignments::new();
        for trace in log.iter() {
            let trace_translated = translator.translate_trace(trace);
            if let Some((path, cost)) = self.align_trace(&trace_translated) {
                result.push(self.transform_alignment(path));
            } else {
                return Err(anyhow!("The model has no way to terminate: it is impossible to reach a deadlock."));
            }
        }

        Ok(result)
    }

    pub fn transform_alignment(&self, path: Vec<(usize, FS)>) -> Vec<Move> {
        vec![]
    }

    pub fn align_trace(&self, trace: &Vec<Activity>) -> Option<(Vec<(usize, FS)>, usize)> {
        let start = (0, self.get_initial_state());
        let successors = |(trace_index, state) : &(usize, FS)| {

            let mut result = vec![];
            
            if trace_index < &trace.len() {
                //we can do a log move
                result.push(((trace_index + 1, state.clone()), 10000));
            }

            //walk through the enabled transitions in the model
            for transition in self.get_enabled_transitions(&state) {

                let mut new_state = state.clone();
                self.execute_transition(&mut new_state, transition);

                if self.is_transition_silent(transition) {
                    //silent move
                    result.push(((*trace_index, new_state), 1));
                } else {
                    //non-silent move
                    if let Some(activity) = trace.get(*trace_index) {
                        //synchronous move
                        result.push(((trace_index + 1, new_state), 0));
                    } else {
                        //model move
                        result.push(((*trace_index, new_state), 10000));
                    }
                }
            }

            log::debug!("successors of log {} model {} are {:?}", trace_index, state, result);

            result
        };
        let heuristic = |astate : &(usize, FS)| {
            0
        };
        let success = |(trace_index, state): &(usize, FS)| {
            let result = trace_index == &trace.len() && self.is_final_state(&state);
            log::debug!("log {} model {} final state: {}", trace_index, state, result);
            result
        };

        let result = pathfinding::prelude::astar(&start, successors, heuristic, success);

        result
    }
}