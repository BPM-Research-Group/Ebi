use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;

use anyhow::Context;
use fraction::One;
use crate::ebi_traits::ebi_trait_stochastic_semantics::StochasticSemantics;
use crate::cross_product::{CrossProductResultImpl, CrossProductResult};
use crate::follower_semantics::FollowerSemantics;
use crate::math::fraction::Fraction;

#[derive(Eq, PartialEq, Hash)]
struct ABState<A: Eq + Hash + Clone> {
    state_a: A,
    state_b: usize
}

struct Z<A: Eq + Hash + Clone> {
    seen: HashMap<Rc<ABState<A>>, usize>, 
    worklist: Vec<Rc<ABState<A>>>,
    state_counter: usize, 
}

#[derive(Debug)]
struct Y {
    outgoing_states: Vec<usize>,
    outgoing_state_probabilities: Vec<Fraction>
}

pub fn trace_probability_semantics<A: Eq + Hash + Clone + Display>(semantics_a: &dyn StochasticSemantics<State = A>, follower_b: &FollowerSemantics) -> anyhow::Result<Fraction> {
    let mut result = CrossProductResultImpl::new();
    let mut z: Z<A> = Z {
        seen: HashMap::<Rc<ABState<A>>, usize>::new(),
        worklist: Vec::<Rc<ABState<A>>>::new(),
        state_counter: 0,
    };

    // log::debug!("trace probability init");

    //initialise
    {
        let state = Rc::new(ABState::<A> { state_a: semantics_a.get_initial_state(), state_b: follower_b.get_initial_state() });
        z.worklist.push(state.clone());
        z.seen.insert(state, z.state_counter);
        result.report_initial_state(z.state_counter);
        z.state_counter += 1;
    }

    let dead_state_a = z.state_counter;
    z.state_counter += 1;
    result.report_dead_state(dead_state_a);

    while !z.worklist.is_empty() {
        let state_ab = z.worklist.pop().unwrap();
        let state_ab_index: usize = *z.seen.get(&state_ab).unwrap();
        let state_a = &state_ab.state_a;
        let state_b = &state_ab.state_b;
        if semantics_a.is_final_state(&state_a) {
            if follower_b.is_final_state(&state_b) {
                result.report_final_state(state_ab_index);
            } else {
                let next_states = vec![dead_state_a];
                let next_probabilities = vec![Fraction::one()];
                //B is not ready; report this as a dead end
                result.report_non_final_state(state_ab_index, next_states, next_probabilities);
            }
        } else {
            let enabled_transitions = semantics_a.get_enabled_transitions(&state_a);

            let total_weight = semantics_a.get_total_weight_of_enabled_transitions(&state_a).with_context(|| format!("{}", state_a))?;

            let mut y = Y {
                outgoing_states: vec![],
                outgoing_state_probabilities: vec![],
            };
            
            for transition in enabled_transitions {
                let mut new_state_a = state_a.clone();
                semantics_a.execute_transition(&mut new_state_a, transition).unwrap();
                if semantics_a.is_transition_silent(transition) {
                    //silent transition; only A takes a step
                    let new_state_b = state_ab.state_b.clone();
                    
                    process_new_state(semantics_a, &mut z, &mut y, &total_weight, transition, &state_a, new_state_a, new_state_b);
                } else {
                    //labelled transition; both A and B need to take steps
                    if follower_b.is_final_state(&state_ab.state_b) {
                        //B cannot take a further step, so this is a dead end
                        y.outgoing_states.push(dead_state_a);
                        y.outgoing_state_probabilities
                                .push(semantics_a.get_transition_weight(&state_a, transition) / &total_weight);
                    } else {
                        let new_state_b = follower_b.take_step(&state_ab.state_b, &semantics_a.get_transition_activity(transition).unwrap());
                        if new_state_b.is_some() {
                            process_new_state(semantics_a, &mut z, &mut y, &total_weight, transition, &state_a, new_state_a, new_state_b.unwrap());
                        } else {
                            //dead state
                            y.outgoing_states.push(dead_state_a);
                            y.outgoing_state_probabilities
                                    .push(semantics_a.get_transition_weight(&state_a, transition) / &total_weight);
                        }
                    }
                }
            }
            result.report_non_final_state(state_ab_index, y.outgoing_states, y.outgoing_state_probabilities);
        }
    }

    let trace_probability = result.solve()?;
    Ok(trace_probability)
}

fn process_new_state<A: Eq + Hash + Clone + Display>(
        semantics_a: &dyn StochasticSemantics<State = A>, 
        z: &mut Z<A>,
        y: &mut Y, 
        total_weight: &Fraction, 
        transition: usize, 
        state_a: &A,
        new_state_a: A, 
        new_state_b: usize) {
    let new_state_ab = ABState::<A>{state_a: new_state_a, state_b: new_state_b};
    let new_state_indexx = z.seen.get(&new_state_ab);
    let new_state_index: usize;
    if new_state_indexx.is_some() {
        new_state_index = *new_state_indexx.unwrap();
    } else {
        //newStateAB was not encountered before
        let new_state_ab_rc = Rc::new(new_state_ab);
        z.worklist.push(new_state_ab_rc.clone());
        z.seen.insert(new_state_ab_rc, z.state_counter);
        new_state_index = z.state_counter.clone();
        z.state_counter += 1;
    }

    y.outgoing_states.push(new_state_index);
    y.outgoing_state_probabilities.push(semantics_a.get_transition_weight(&state_a, transition) / total_weight);
}