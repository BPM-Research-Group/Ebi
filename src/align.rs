use std::{fmt::{Debug, Display}, hash::Hash, mem::transmute};

use anyhow::{anyhow, Context, Result};

use crate::{activity_key::{Activity, ActivityKeyTranslator}, align, ebi_objects::alignments::{Alignments, Move}, ebi_traits::{ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_semantics::Semantics, ebi_trait_stochastic_semantics::TransitionIndex}};

impl <FS: Display + Debug + Clone + Hash + Eq> dyn Semantics<State = FS> {
    
    pub fn align_log(&mut self, log: Box<dyn EbiTraitFiniteLanguage>) -> Result<Alignments> {
        let translator = ActivityKeyTranslator::new(log.get_activity_key(), self.get_activity_key_mut());

        let mut result = Alignments::new(self.get_activity_key().clone());
        for trace in log.iter() {
            let trace_translated = translator.translate_trace(trace);
            if let Some((states, _)) = self.align_trace(&trace_translated) {
                result.push(self.transform_alignment(trace, states)?);
            } else {
                return Err(anyhow!("The model has no way to terminate: it is impossible to reach a deadlock."));
            }
        }

        Ok(result)
    }

    pub fn align_trace(&self, trace: &Vec<Activity>) -> Option<(Vec<(usize, FS)>, usize)> {
        // log::debug!("activity key {}", self.get_activity_key());
        // log::debug!("align trace {:?}", trace);

        let start = (0, self.get_initial_state());
        let successors = |(trace_index, state) : &(usize, FS)| {

            let mut result = vec![];

            // log::debug!("successors of log {} model {}", trace_index, state);
            
            if trace_index < &trace.len() {
                //we can do a log move
                // log::debug!("\tlog move {}", trace[*trace_index]);
                result.push(((trace_index + 1, state.clone()), 10000));
            }

            //walk through the enabled transitions in the model
            for transition in self.get_enabled_transitions(&state) {

                let mut new_state = state.clone();
                // log::debug!("\t\tnew state before {}", new_state);
                self.execute_transition(&mut new_state, transition);
                // log::debug!("\t\tnew state after {}", new_state);

                if let Some(activity) = self.get_transition_activity(transition) {
                    //non-silent model move
                    result.push(((*trace_index, new_state.clone()), 10000));
                    // log::debug!("\tmodel move t{} {} to {}", transition, activity, new_state);

                    //which may also be a synchronous move
                    if trace_index < &trace.len() && activity == trace[*trace_index] {
                        //synchronous move
                        // log::debug!("\tsynchronous move t{} {} to {}", transition, activity, new_state);
                        result.push(((trace_index + 1, new_state), 0));
                    }
                } else {
                    //silent move
                    result.push(((*trace_index, new_state), 1));
                }
            }

            // log::debug!("successors of {} {}: {:?}", trace_index, state, result);
            result
        };

        //function that returns a heuristic on how far we are still minimally from a final state
        let heuristic = |astate : &(usize, FS)| {
            0
        };

        //function that returns whether we are in a final synchronous product state
        let success = |(trace_index, state): &(usize, FS)| {
            trace_index == &trace.len() && self.is_final_state(&state)
        };

        pathfinding::prelude::astar(&start, successors, heuristic, success)
    }

    /**
     * The a* function we use returns a sequence of states, while we need a sequence of moves.
     * This function transforms the sequence of states into a sequence of moves.
     * 
     * This function assumes equal costs (of 10000) for the log and model mvoes, and 1 for the silent moves.
     */
    pub fn transform_alignment(&self, trace: &Vec<Activity>, states: Vec<(usize, FS)>) -> Result<Vec<Move>> {
        let mut alignment = vec![];

        let mut it = states.into_iter();
        
        let (mut previous_trace_index, mut previous_state) = it.next().unwrap();
        
        for (trace_index, state) in it {

            if trace_index != previous_trace_index {
                //we did a move on the log
                let activity = trace[previous_trace_index];

                if previous_state == state {
                    //we did not move on the model => log move
                    alignment.push(Move::LogMove(activity));
                } else {
                    //we moved on the model => synchronous move
                    let transition = self.find_transition_with_label(&previous_state, &state, activity).with_context(|| format!("Map synchronous move from {} {} to {} {} with label {}", previous_trace_index, previous_state, trace_index, state, activity))?;
                    alignment.push(Move::SynchronousMove(activity, transition));
                }

            } else {
                //we did not do a move on the log

                if let Some(transition) = self.is_there_a_silent_transition_enabled(&previous_state, &state) {
                    //there is a silent transition enabled, which is the cheapest
                    alignment.push(Move::SilentMove(transition));
                } else {
                    //otherwise, we take an arbitrary labelled model move
                    let transition = self.find_labelled_transition(&previous_state, &state)?;
                    alignment.push(Move::ModelMove(self.get_transition_activity(transition).unwrap(), transition));
                }
            }

            previous_trace_index = trace_index;
            previous_state = state;

            // log::debug!("prefix: {:?}", alignment);
        }

        Ok(alignment)
    }

    fn is_there_a_silent_transition_enabled(&self, from: &FS, to: &FS) -> Option<TransitionIndex> {
        for transition in self.get_enabled_transitions(from) {
            if self.is_transition_silent(transition) {
                let mut from = from.clone();
                self.execute_transition(&mut from, transition);
                if &from == to {
                    return Some(transition);
                }
            }
        }
        None
    }

    fn find_transition_with_label(&self, from: &FS, to: &FS, label: Activity) -> Result<TransitionIndex> {
        // log::debug!("find transition with label {}", label);
        for transition in self.get_enabled_transitions(from) {
            // log::debug!("transition {} is enabled", transition);
            if self.get_transition_activity(transition) == Some(label) {
                let mut from = from.clone();
                self.execute_transition(&mut from, transition);
                if &from == to {
                    return Ok(transition);
                }
            }
        }
        Err(anyhow!("There is no transition with activity {} that brings the model from {} to {}", label, from, to))
    }

    fn find_labelled_transition(&self, from: &FS, to: &FS) -> Result<TransitionIndex> {
        for transition in self.get_enabled_transitions(from) {
            if !self.is_transition_silent(transition) {
                let mut from = from.clone();
                self.execute_transition(&mut from, transition);
                if &from == to {
                    return Ok(transition);
                }
            }
        }
        Err(anyhow!("There is no transition with any activity enabled that brings the model from {} to {}", from, to))
    }
}