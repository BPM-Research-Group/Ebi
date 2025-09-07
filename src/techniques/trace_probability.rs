use anyhow::{Context, Result, anyhow};
use ebi_arithmetic::{EbiMatrix, Fraction, FractionMatrix, GaussJordan, One, Zero};
use ebi_objects::{
    StochasticDeterministicFiniteAutomaton, StochasticLabelledPetriNet, StochasticProcessTree,
};
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Display},
    hash::Hash,
    ops::Add,
    rc::Rc,
};

use crate::{
    ebi_traits::ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
    follower_semantics::FollowerSemantics,
    semantics::{
        labelled_petri_net_semantics::LPNMarking, process_tree_semantics::NodeStates,
        semantics::Semantics,
    },
    stochastic_semantics::stochastic_semantics::StochasticSemantics,
};

//generic implementation
macro_rules! default_trace_probability {
    ($t:ident, $s:ident) => {
        impl EbiTraitQueriableStochasticLanguage for $t {
            fn get_probability(&self, follower_b: &FollowerSemantics) -> anyhow::Result<Fraction> {
                let mut result = CrossProductResultImpl::new();
                let mut z: Z<$s> = Z {
                    seen: HashMap::<Rc<ABState<$s>>, usize>::new(),
                    worklist: Vec::<Rc<ABState<$s>>>::new(),
                    state_counter: 0,
                };

                // log::debug!("trace probability init");

                let initial_state_a = if let Some(i) = self.get_initial_state() {
                    i
                } else {
                    return Ok(Fraction::zero()); //If the language has no states, then the probability of any trace is zero.
                };

                //initialise
                {
                    let state = Rc::new(ABState::<$s> {
                        state_a: initial_state_a,
                        state_b: follower_b.get_initial_state(),
                    });
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
                    if self.is_final_state(&state_a) {
                        if follower_b.is_final_state(&state_b) {
                            result.report_final_state(state_ab_index);
                        } else {
                            let next_states = vec![dead_state_a];
                            let next_probabilities = vec![Fraction::one()];
                            //B is not ready; report this as a dead end
                            result.report_non_final_state(
                                state_ab_index,
                                next_states,
                                next_probabilities,
                            );
                        }
                    } else {
                        let enabled_transitions = self.get_enabled_transitions(&state_a);

                        let total_weight = self
                            .get_total_weight_of_enabled_transitions(&state_a)
                            .with_context(|| format!("{}", state_a))?;

                        let mut y = Y {
                            outgoing_states: vec![],
                            outgoing_state_probabilities: vec![],
                        };

                        for transition in enabled_transitions {
                            let mut new_state_a = state_a.clone();
                            self.execute_transition(&mut new_state_a, transition)
                                .unwrap();
                            if self.is_transition_silent(transition) {
                                //silent transition; only A takes a step
                                let new_state_b = state_ab.state_b.clone();

                                process_new_state(
                                    self,
                                    &mut z,
                                    &mut y,
                                    &total_weight,
                                    transition,
                                    &state_a,
                                    new_state_a,
                                    new_state_b,
                                );
                            } else {
                                //labelled transition; both A and B need to take steps
                                if follower_b.is_final_state(&state_ab.state_b) {
                                    //B cannot take a further step, so this is a dead end
                                    y.outgoing_states.push(dead_state_a);
                                    y.outgoing_state_probabilities.push(
                                        <$t as StochasticSemantics>::get_transition_weight(
                                            self, &state_a, transition,
                                        ) / &total_weight,
                                    );
                                } else {
                                    let new_state_b = follower_b.take_step(
                                        &state_ab.state_b,
                                        &self.get_transition_activity(transition).unwrap(),
                                    );
                                    if new_state_b.is_some() {
                                        process_new_state(
                                            self,
                                            &mut z,
                                            &mut y,
                                            &total_weight,
                                            transition,
                                            &state_a,
                                            new_state_a,
                                            new_state_b.unwrap(),
                                        );
                                    } else {
                                        //dead state
                                        y.outgoing_states.push(dead_state_a);
                                        y.outgoing_state_probabilities.push(
                                            <$t as StochasticSemantics>::get_transition_weight(
                                                self, &state_a, transition,
                                            ) / &total_weight,
                                        );
                                    }
                                }
                            }
                        }
                        result.report_non_final_state(
                            state_ab_index,
                            y.outgoing_states,
                            y.outgoing_state_probabilities,
                        );
                    }
                }

                let trace_probability = result.solve()?;
                Ok(trace_probability)
            }
        }
    };
}

default_trace_probability!(StochasticLabelledPetriNet, LPNMarking);
default_trace_probability!(StochasticProcessTree, NodeStates);

impl EbiTraitQueriableStochasticLanguage for StochasticDeterministicFiniteAutomaton {
    fn get_probability(&self, follower: &FollowerSemantics) -> Result<Fraction> {
        match follower {
            FollowerSemantics::Trace(trace) => {
                if let Some(initial_state) = self.get_initial_state() {
                    let mut state = initial_state;
                    let mut result = Fraction::one();

                    for activity in trace.iter() {
                        let (found, pos) = self
                            .binary_search(state, self.activity_key.get_id_from_activity(activity));
                        if !found {
                            return Ok(Fraction::zero());
                        }
                        state = self.targets[pos];
                        result *= &self.probabilities[pos];
                    }

                    result *= &self.terminating_probabilities[state];

                    Ok(result)
                } else {
                    Ok(Fraction::zero())
                }
            }
        }
    }
}

fn process_new_state<T: StochasticSemantics<StoSemState = A>, A: Eq + Hash + Clone + Display>(
    semantics: &T,
    z: &mut Z<A>,
    y: &mut Y,
    total_weight: &Fraction,
    transition: usize,
    state_a: &A,
    new_state_a: A,
    new_state_b: usize,
) {
    let new_state_ab = ABState::<A> {
        state_a: new_state_a,
        state_b: new_state_b,
    };
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
    y.outgoing_state_probabilities
        .push(semantics.get_transition_weight(&state_a, transition) / total_weight);
}

pub trait CrossProductResult {
    /**
     * The initial state will be reported twice: once as initial state, and
     * again as a final or non-final state.
     *
     * @param stateIndex
     */
    fn report_initial_state(&mut self, state_index: usize);

    /**
     * A state will be reported as either final, non-final, or dead.
     *
     * @param stateIndex
     * @param nextStateIndices
     *            may contain duplicated values. List might be reused and
     *            changed after this call returns, and changes by the
     *            implementer will be overwritten.
     * @param nextStateProbabilities
     *            list might be reused and changed after this call returns, and
     *            changes by the implementer will be overwritten.
     */
    fn report_non_final_state(
        &mut self,
        state_index: usize,
        next_state_indices: Vec<usize>,
        next_state_probabilities: Vec<Fraction>,
    );

    /**
     * A state will be reported as either final, non-final, or dead. Multiple
     * states might be reported as final.
     *
     * @param stateIndex
     */
    fn report_final_state(&mut self, state_index: usize);

    /**
     * A state will be reported as either final, non-final, or dead.
     *
     * A dead state is a state in the cross product that indicates that A made a
     * move that was not supported by B. At most one state will be reported as
     * dead.
     *
     *
     * @param stateIndex
     */
    fn report_dead_state(&mut self, state_index: usize);
}

pub struct CrossProductResultImpl {
    initial_state: usize,
    dead_state: usize,
    final_states: HashSet<usize>,
    next_states: Vec<Vec<usize>>,
    next_state_probabilities: Vec<Vec<Fraction>>,
    de_duplication_cache: HashSet<usize>,
}

impl CrossProductResultImpl {
    pub fn new() -> Self {
        Self {
            initial_state: usize::MAX,
            dead_state: usize::MAX,
            final_states: HashSet::new(),
            next_states: vec![],
            next_state_probabilities: vec![],
            de_duplication_cache: HashSet::new(),
        }
    }
}

impl CrossProductResult for CrossProductResultImpl {
    fn report_initial_state(&mut self, state_index: usize) {
        self.initial_state = state_index;
        self.ensure_capacity(state_index);

        // debug!("report initial state {}", state_index);
    }

    fn report_non_final_state(
        &mut self,
        state_index: usize,
        mut next_state_indices: Vec<usize>,
        mut next_state_probabilities: Vec<Fraction>,
    ) {
        self.de_duplicate(&mut next_state_indices, &mut next_state_probabilities);
        self.ensure_capacity(state_index);

        self.next_states[state_index] = next_state_indices;
        self.next_state_probabilities[state_index] = next_state_probabilities;
    }

    fn report_final_state(&mut self, state_index: usize) {
        self.ensure_capacity(state_index);
        self.final_states.insert(state_index);

        // debug!("report final state {}", state_index);
    }

    fn report_dead_state(&mut self, state_index: usize) {
        self.dead_state = state_index;

        // debug!("report dead state {}", state_index);
    }
}

impl CrossProductResultImpl {
    fn de_duplicate(
        &mut self,
        next_state_indexes: &mut Vec<usize>,
        next_state_probabilities: &mut Vec<Fraction>,
    ) {
        self.de_duplication_cache.clear();
        let mut index_a: usize = 0;
        while index_a < next_state_indexes.len() {
            let node_a = next_state_indexes[index_a];
            if self.de_duplication_cache.contains(&node_a) {
                //look for the duplicate
                for index_b in 0..index_a {
                    let node_b = next_state_indexes[index_b];
                    if node_a == node_b {
                        next_state_probabilities[index_b] = next_state_probabilities
                            .get(index_b)
                            .unwrap()
                            .add(&next_state_probabilities[index_a]);
                        next_state_indexes.remove(index_a);
                        next_state_probabilities.remove(index_a);
                        index_a -= 1;
                        break;
                    }
                }
            }

            self.de_duplication_cache.insert(node_a);

            index_a += 1;
        }
    }
}

impl CrossProductResultImpl {
    fn number_of_states(&self) -> usize {
        self.next_states.len()
    }

    fn ensure_capacity(&mut self, state_index: usize) {
        while self.next_states.len() < state_index + 1 {
            self.next_states.push(Vec::new());
            self.next_state_probabilities.push(Vec::new());
        }
    }

    /**
     * Structure of the LP model:
     *
     * One row per state; one column per state.
     *
     * @return
     * @throws LpSolveException
     */
    pub fn solve(&self) -> Result<Fraction> {
        if self.final_states.is_empty() {
            return Ok(Fraction::zero());
        }

        let mut matrix = FractionMatrix::new(self.number_of_states(), self.number_of_states() + 1);

        for state_index in 0..self.number_of_states() {
            matrix.set(state_index, state_index, Fraction::one());

            if state_index == self.dead_state {
                //a dead state has a 0 probability to end up in a final state
            } else if self.final_states.contains(&state_index) {
                //a final state has a 1 probability to end up in a final state
                matrix.set(state_index, self.number_of_states(), Fraction::one());
            } else {
                //any other state has a probability equal to the weighted sum of its next states, to end up in a final state
                for i in 0..self.next_states[state_index].len() {
                    let state_index_2 = self.next_states[state_index][i];
                    let coeff = &self.next_state_probabilities[state_index][i];
                    matrix.set(state_index, state_index_2, -coeff);
                }
            }
        }

        matrix = matrix.gauss_jordan_reduced()?;

        matrix
            .get(self.initial_state, self.number_of_states())
            .ok_or_else(|| anyhow!("item not found"))
    }
}

impl Display for CrossProductResultImpl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "initial state {}", self.initial_state)?;
        writeln!(f, "final states  {:?}", self.final_states)?;
        for state in 0..self.next_states.len() {
            writeln!(f, "state {}", state)?;
            if self.initial_state == state {
                writeln!(f, "\tinitial state")?;
            }
            if self.final_states.contains(&state) {
                writeln!(f, "\tfinal state")?;
            }
            for i in 0..self.next_states[state].len() {
                writeln!(
                    f,
                    "\tnext state {} with probability {}",
                    self.next_states[state][i], self.next_state_probabilities[state][i]
                )?;
            }
        }

        write!(f, "")
    }
}

#[derive(Eq, PartialEq, Hash)]
struct ABState<A: Eq + Hash + Clone> {
    state_a: A,
    state_b: usize,
}

struct Z<A: Eq + Hash + Clone> {
    seen: HashMap<Rc<ABState<A>>, usize>,
    worklist: Vec<Rc<ABState<A>>>,
    state_counter: usize,
}

#[derive(Debug)]
struct Y {
    outgoing_states: Vec<usize>,
    outgoing_state_probabilities: Vec<Fraction>,
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_arithmetic::{Fraction, Zero};
    use ebi_objects::{
        FiniteLanguage, HasActivityKey, StochasticDeterministicFiniteAutomaton, StochasticLabelledPetriNet, StochasticProcessTree
    };

    use crate::{
        ebi_traits::ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
        follower_semantics::FollowerSemantics,
    };

    #[test]
    fn probability_sdfa_livelock_zeroweight() {
        let fin1 = fs::read_to_string("testfiles/a-livelock-zeroweight.sdfa").unwrap();
        let mut sdfa = fin1
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        //a ends in a livelock and has probability 0
        let strace = vec!["a".to_string()];
        let trace = sdfa.activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(
            sdfa.get_probability(&trace_follower).unwrap(),
            Fraction::zero()
        );

        //a, a ends in a livelock and has probability 0
        let strace = vec!["a".to_string(), "a".to_string()];
        let trace = sdfa.activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(
            sdfa.get_probability(&trace_follower).unwrap(),
            Fraction::zero()
        );

        //b has weight 0
        let strace = vec!["b".to_string()];
        let trace = sdfa.activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(
            sdfa.get_probability(&trace_follower).unwrap(),
            Fraction::zero()
        );
    }

    #[test]
    fn probability_slpn_livelock_zeroweight() {
        let fin1 = fs::read_to_string("testfiles/a-livelock-zeroweight.slpn").unwrap();
        let mut slpn = fin1.parse::<StochasticLabelledPetriNet>().unwrap();

        //a ends in a livelock and has probability 0
        let strace = vec!["a".to_string()];
        let trace = slpn.activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(
            slpn.get_probability(&trace_follower).unwrap(),
            Fraction::zero()
        );

        //a, a ends in a livelock and has probability 0
        let strace = vec!["a".to_string(), "a".to_string()];
        let trace = slpn.activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(
            slpn.get_probability(&trace_follower).unwrap(),
            Fraction::zero()
        );

        //b has weight 0
        let strace = vec!["b".to_string()];
        let trace = slpn.activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(
            slpn.get_probability(&trace_follower).unwrap(),
            Fraction::zero()
        );

        //c has weight 0
        let strace = vec!["c".to_string()];
        let trace = slpn.activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(
            slpn.get_probability(&trace_follower).unwrap(),
            Fraction::zero()
        );
    }

    #[test]
    fn sdfa_trace_prob_livelock() {
        let fin = fs::read_to_string("testfiles/a-b-c-livelock.sdfa").unwrap();
        let mut sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        //a
        {
            let strace = vec!["a".to_string()];
            let trace = sdfa.activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = sdfa.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "2/5".parse::<Fraction>().unwrap());
        }

        //b
        {
            let strace = vec!["b".to_string()];
            let trace = sdfa.activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = sdfa.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "1/5".parse::<Fraction>().unwrap());
        }

        //c (part of livelock trace)
        {
            let strace = vec!["c".to_string()];
            let trace = sdfa.activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = sdfa.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "0".parse::<Fraction>().unwrap());
        }

        //d (non-existing label)
        {
            let strace = vec!["d".to_string()];
            let trace = sdfa.activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = sdfa.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "0".parse::<Fraction>().unwrap());
        }
    }

    #[test]
    fn slpn_trace_prob_livelock() {
        let fin = fs::read_to_string("testfiles/a-b-c-livelock.slpn").unwrap();
        let mut slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        //a
        {
            let strace = vec!["a".to_string()];
            let trace = slpn.activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "2/5".parse::<Fraction>().unwrap());
        }

        //b
        {
            let strace = vec!["b".to_string()];
            let trace = slpn.activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "1/5".parse::<Fraction>().unwrap());
        }

        //c (part of livelock trace)
        {
            let strace = vec!["c".to_string()];
            let trace = slpn.activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "0".parse::<Fraction>().unwrap());
        }

        //d (non-existing label)
        {
            let strace = vec!["d".to_string()];
            let trace = slpn.activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "0".parse::<Fraction>().unwrap());
        }
    }

    #[test]
    fn slpn_trace_prob_tau_livelock() {
        let fin = fs::read_to_string("testfiles/a-b-tau-livelock.slpn").unwrap();
        let mut slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        //a
        {
            let strace = vec!["a".to_string()];
            let trace = slpn.activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "2/5".parse::<Fraction>().unwrap());
        }

        //b
        {
            let strace = vec!["b".to_string()];
            let trace = slpn.activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "1/5".parse::<Fraction>().unwrap());
        }

        //empty trace (part of livelock trace)
        {
            let strace = vec![];
            let trace = slpn.activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "0".parse::<Fraction>().unwrap());
        }

        //d (non-existing label)
        {
            let strace = vec!["d".to_string()];
            let trace = slpn.activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "0".parse::<Fraction>().unwrap());
        }
    }

    #[test]
    fn trace_probability_sptree() {
        let fin = fs::read_to_string("testfiles/seq(a-xor(b-c)).sptree").unwrap();
        let mut tree = fin.parse::<StochasticProcessTree>().unwrap();

        let strace = vec!["d".to_string()];
        let trace = tree.activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        let probability = tree.get_probability(&trace_follower).unwrap();
        assert_eq!(probability, Fraction::zero());

        let strace = vec!["a".to_string(), "c".to_string()];
        let trace = tree.activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        let probability = tree.get_probability(&trace_follower).unwrap();
        assert_eq!(probability, Fraction::from((2, 3)));
    }

    #[test]
    fn emsc() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba_ali.slpn").unwrap();
        let mut slpn: StochasticLabelledPetriNet =
            fin1.parse::<StochasticLabelledPetriNet>().unwrap();

        let fin2 = fs::read_to_string("testfiles/aa.lang").unwrap();
        let slang2 = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let probability = slpn.get_probability_language(slang2).unwrap();

        assert_eq!(probability, Fraction::zero());
    }
}
