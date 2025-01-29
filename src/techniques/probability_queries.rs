use std::{collections::{HashMap, HashSet}, fmt::{Debug, Display}};
use core::hash::Hash;
use anyhow::{anyhow, Result};
use fraction::One;
use priority_queue::PriorityQueue;

use crate::{ebi_framework::{activity_key::Activity, displayable::Displayable, ebi_command::EbiCommand}, ebi_objects::finite_stochastic_language::FiniteStochasticLanguage, ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_stochastic_deterministic_semantics::{EbiTraitStochasticDeterministicSemantics, StochasticDeterministicSemantics}}, math::fraction::Fraction};

pub trait ProbabilityQueries {
    /**
     * Find all traces that have a given minimum probability.
     */
    fn analyse_minimum_probability(&self, at_least: &Fraction) -> Result<FiniteStochasticLanguage>;

    /**
     * Find the traces with the highest probabilities.
     */
    fn analyse_most_likely_traces(&self, number_of_traces: &usize) -> Result<FiniteStochasticLanguage>;

    /**
     * Find the most likely traces that together have a sum probability.
     */
    fn analyse_probability_coverage(&self, coverage: &Fraction) -> Result<FiniteStochasticLanguage>;
}

impl ProbabilityQueries for EbiTraitStochasticDeterministicSemantics {
    fn analyse_minimum_probability(&self, at_least: &Fraction) -> Result<FiniteStochasticLanguage> {
        match self {
            EbiTraitStochasticDeterministicSemantics::Usize(sem) => sem.analyse_minimum_probability(at_least),
            EbiTraitStochasticDeterministicSemantics::PMarking(sem) => sem.analyse_minimum_probability(at_least),
        }
    }

    fn analyse_most_likely_traces(&self, number_of_traces: &usize) -> Result<FiniteStochasticLanguage> {
        match self {
            EbiTraitStochasticDeterministicSemantics::Usize(sem) => sem.analyse_most_likely_traces(number_of_traces),
            EbiTraitStochasticDeterministicSemantics::PMarking(sem) => sem.analyse_most_likely_traces(number_of_traces),
        }
    }

    fn analyse_probability_coverage(&self, coverage: &Fraction) -> Result<FiniteStochasticLanguage> {
        match self {
            EbiTraitStochasticDeterministicSemantics::Usize(sem) => sem.analyse_probability_coverage(coverage),
            EbiTraitStochasticDeterministicSemantics::PMarking(sem) => sem.analyse_probability_coverage(coverage),
        }
    }
}

impl ProbabilityQueries for dyn EbiTraitFiniteStochasticLanguage {
    fn analyse_most_likely_traces(&self, number_of_traces: &usize) -> Result<FiniteStochasticLanguage> {

        if self.len() == 0 {
            Ok((HashMap::new(), self.get_activity_key().clone()).into())
        } else if number_of_traces.is_one() {
            let mut result = HashMap::new();

            let (mut max_trace, mut max_probability) = self.iter_trace_probability().next().ok_or_else(|| anyhow!("Finite stochastic language is empty where it should not."))?;

            for (trace, probability) in self.iter_trace_probability() {
                if probability > max_probability {
                    max_trace = trace;
                    max_probability = probability;
                }
            }

            result.insert(max_trace.clone(), max_probability.clone());

            Ok((result, self.get_activity_key().clone()).into())
        } else {
            let mut result = vec![];
            for (trace, probability) in self.iter_trace_probability() {
                match result.binary_search_by(|&(_, cmp_probability): &(_, &Fraction)| cmp_probability.cmp(probability)) {
                    Ok(index) => if index < number_of_traces - 1 { result.insert(index, (trace, probability)) },
                    Err(index) => if index < number_of_traces - 1 { result.insert(index, (trace, probability)) },
                }

            }

            let mut result2 = HashMap::new();
            for (trace, probability) in result {
                result2.insert(trace.clone(), probability.clone());
            }

            Ok((result2, self.get_activity_key().clone()).into())
        }
    }

    fn analyse_minimum_probability(&self, at_least: &Fraction) -> Result<FiniteStochasticLanguage> {
        let mut result = vec![];
        for (trace, probability) in self.iter_trace_probability() {
            if probability >= at_least {
                result.push((trace, probability));
            }
        }

        let mut result2 = HashMap::new();
        for (trace, probability) in result {
            result2.insert(trace.clone(), probability.clone());
        }

        Ok((result2, self.get_activity_key().clone()).into())
    }

    fn analyse_probability_coverage(&self, coverage: &Fraction) -> Result<FiniteStochasticLanguage> {

        if coverage.is_zero() {
            return Ok((HashMap::new(), self.get_activity_key().clone()).into());
        } else if self.len() == 0 {
            return Err(anyhow!("A coverage of {:.4} is unattainable as the stochastic language is empty.", coverage)); 
        }

        //idea: keep a list of traces sorted by probability

        //insert the first trace
        let mut result = vec![(self.get_trace(0).unwrap(), self.get_probability(0).unwrap())];

        let mut sum = Fraction::zero();
        
        for (trace, probability) in self.iter_trace_probability().skip(1) {

            if &sum < coverage || probability > result[0].1 {
                //as the new trace has a higher probability than the trace with the lowest probability, we have to insert it
                match result.binary_search_by(|&(_, cmp_probability): &(_, &Fraction)| cmp_probability.cmp(probability)) {
                    Ok(index) | Err(index) => result.insert(index, (trace, probability))
                }
                sum += probability;

                //then, remove the first trace until the sum would drop below the coverage
                let mut new_sum = &sum - result[0].1;
                while &new_sum > coverage {
                    result.remove(0);
                    sum = new_sum;
                    
                    new_sum = &sum - result[0].1;
                }
            }

        }

        if &sum < coverage {
            return Err(anyhow!("A coverage of {:.4} is unattainable as the stochastic language has a sum probability of {:.4}.", coverage, sum)); 
        }

        let mut result2 = HashMap::new();
        for (trace, probability) in result {
            result2.insert(trace.clone(), probability.clone());
        }

        Ok((result2, self.get_activity_key().clone()).into())
    }
}

impl <DState: Displayable> ProbabilityQueries for dyn StochasticDeterministicSemantics<DetState = DState, LivState = DState> {
    
    fn analyse_minimum_probability(&self, at_least: &Fraction) -> Result<FiniteStochasticLanguage> {
        
        let error_at_loop = at_least.is_zero(); //If at_least is zero, we are asked to return all traces. If there is a loop, then this is impossible.
        
        let mut seen = HashSet::new();
        if error_at_loop {
            seen.insert(self.get_deterministic_initial_state()?);
        }

        let mut result = HashMap::new();

        let mut queue = vec![];
        queue.push(X{
            prefix: vec![],
            probability: Fraction::one(),
            p_state: self.get_deterministic_initial_state()?,
        });

        while let Some(x) = queue.pop() {
            // log::debug!("queue length {}, process p-state {:?}", queue.len(), x.p_state);

            let mut probability_terminate_in_this_state = self.get_deterministic_termination_probability(&x.p_state);
            // log::debug!("probability termination in this state {}", probability_terminate_in_this_state);
            probability_terminate_in_this_state *= &x.probability;

            if !probability_terminate_in_this_state.is_zero() && probability_terminate_in_this_state >= *at_least {
                result.insert(x.prefix.clone(), probability_terminate_in_this_state);
            }

            let mut iff = self.get_deterministic_termination_probability(&x.p_state).one_minus();
            iff *= &x.probability;
            if iff >= *at_least {
                for activity in self.get_deterministic_enabled_activities(&x.p_state) {
                    // log::info!("consider activity {}", activity);

                    let probability = self.get_deterministic_activity_probability(&x.p_state, activity);

                    // log::info!("activity has probability {}", probability);

                    let new_p_state = self.execute_deterministic_activity(&x.p_state, activity)?;

                    // log::info!("activity executed");

                    if error_at_loop {
                        if !seen.insert(new_p_state.clone()) {
                            return Err(anyhow!("As the language is not finite, Ebi cannot return all traces that have a minimum probability of 0."));
                        }
                    }

                    let mut new_prefix = x.prefix.clone();
                    new_prefix.push(activity);
                    let new_x = X {
                        prefix: new_prefix,
                        probability: &x.probability * &probability,
                        p_state: new_p_state,
                    };

                    if !self.is_non_decreasing_livelock(&mut new_x.p_state.clone())? {
                        //if not a livelock, continue the search
                        queue.push(new_x);
                    }
                }
            }
        }

        Ok((result, self.get_activity_key().clone()).into())
    }

    fn analyse_most_likely_traces(&self, number_of_traces: &usize) -> Result<FiniteStochasticLanguage> {
        // log::info!("Compute most-likely traces");
        let progress_bar = EbiCommand::get_progress_bar_ticks(*number_of_traces);

        let mut result = HashMap::new();

        let mut queue = PriorityQueue::new();
        queue.push(Y::Prefix(vec![], self.get_deterministic_initial_state()?, None), Fraction::one());

        while let Some((y, priority)) = queue.pop() {
            match y {
                Y::Prefix(prefix, p_state, probability) => {

                    let y_probability = probability.unwrap_or(priority); //take the value of the object on the queue, or the queue priority if None.

                    let termination_proability = self.get_deterministic_termination_probability(&p_state);
                    if termination_proability.is_positive() {
                        queue.push(Y::Trace(prefix.clone()), &y_probability * &termination_proability);
                    }

                    for activity in self.get_deterministic_enabled_activities(&p_state) {
                        let activity_probability = self.get_deterministic_activity_probability(&p_state, activity);
        
                        let new_p_state = self.execute_deterministic_activity(&p_state, activity)?;
                        let mut new_prefix = prefix.clone();
                        new_prefix.push(activity);
                        let new_probability = &y_probability * &activity_probability;

                        //compute the new priority for the priority queue, which is (probability of prefix) * (1-livelock probability)
                        let livelock_probability = self.get_deterministic_non_decreasing_livelock_probability(&mut new_p_state.clone())?;
                        let (new_probability, new_priority) = if livelock_probability.is_zero() {
                            (None, new_probability)
                        } else {
                            let mut new_priority = new_probability.clone();
                            new_priority *= livelock_probability.one_minus();
                            (Some(new_probability), new_priority)
                        };
        
                        queue.push(Y::Prefix(new_prefix, new_p_state, new_probability), new_priority);
                    }
                },
                Y::Trace(trace) => {
                    result.insert(trace, priority);
                    progress_bar.inc(1);
                    if result.len() >= *number_of_traces {
                        break;
                    }
                },
            }
        }

        Ok((result, self.get_activity_key().clone()).into())
    }
    
    fn analyse_probability_coverage(&self, coverage: &Fraction) -> Result<FiniteStochasticLanguage> {
        let progress_bar = EbiCommand::get_progress_bar_message("found 0 traces which cover 0.0000000".to_owned());

        if !coverage.is_positive() {
            return Ok(HashMap::new().into());
        }
        
        let initial_state = self.get_deterministic_initial_state()?;

        let mut seen = HashSet::new();
        seen.insert(initial_state.clone());

        let mut loop_detected = false;
        let mut non_livelock_probability = Fraction::one();

        let mut result = HashMap::new();
        let mut result_sum = Fraction::zero();

        let mut queue = PriorityQueue::new();
        queue.push(Y::Prefix(vec![], initial_state, None), Fraction::one());

        while let Some((y, priority)) = queue.pop() {
            match y {
                Y::Prefix(xprefix, xp_state, probability) => {
                    // log::debug!("queue length {}, process p-state {:?}", queue.len(), xp_state);

                    let xprobability = probability.unwrap_or_else(|| priority); //take the value of the object on the queue, or the queue priority if None.

                    let mut probability_terminate_in_this_state = self.get_deterministic_termination_probability(&xp_state);
                    // log::debug!("probability termination in this state {}", probability_terminate_in_this_state);
                    probability_terminate_in_this_state *= &xprobability;

                    non_livelock_probability -= self.get_deterministic_silent_livelock_probability(&xp_state);
                    
                    if probability_terminate_in_this_state.is_positive() {
                        //we have found a trace; add it to the queue to ensure it is encountered at the right time
                        queue.push(Y::Trace(xprefix.clone()), probability_terminate_in_this_state);
                        // log::debug!("add trace {}; coverage now {:.4}", result.len(), result_sum);
                        // println!("add trace {}; coverage now {:.4}", result.len(), result_sum);
                    }

                    for activity in self.get_deterministic_enabled_activities(&xp_state) {
                        // log::info!("consider activity {}", activity);

                        let probability = self.get_deterministic_activity_probability(&xp_state, activity);

                        // log::info!("activity has probability {}", probability);

                        let new_p_state = self.execute_deterministic_activity(&xp_state, activity)?;

                        // log::info!("activity executed");

                        if !loop_detected {
                            if !seen.insert(new_p_state.clone()) {
                                loop_detected = true;
                                seen.clear();
                            }
                        }

                        let mut new_prefix = xprefix.clone();
                        new_prefix.push(activity);
                        let new_probability = &xprobability * &probability;

                        if self.is_non_decreasing_livelock(&mut new_p_state.clone())? { //TODO: replace with a full livelock check
                            //if a livelock, then keep track of its likelihood
                            non_livelock_probability -= new_probability;
                        } else {
                            //if not a livelock, continue the search

                            //compute the new priority for the priority queue, which is (probability of prefix) * (1-livelock probability)
                            let livelock_probability = self.get_deterministic_non_decreasing_livelock_probability(&mut new_p_state.clone())?;
                            let (new_probability, new_priority) = if livelock_probability.is_zero() {
                                (None, new_probability)
                            } else {
                                let mut new_priority = new_probability.clone();
                                new_priority *= livelock_probability.one_minus();
                                (Some(new_probability), new_priority)
                            };

                            queue.push(Y::Prefix(new_prefix, new_p_state, new_probability), new_priority);
                        }
                    }
                },
                Y::Trace(xtrace) => {
                    // log::debug!("found trace {:?} with probability {}", xtrace, xprobability);
                    result_sum += &priority;
                    result.insert(xtrace, priority);
                    progress_bar.set_message(format!("found {} traces which cover {:.8}", result.len(), result_sum));

                    //check whether we are done
                    if &result_sum > coverage {
                        break;
                    } else if &non_livelock_probability < coverage {
                        return Err(anyhow!("A coverage of {:.4} is unattainable as the stochastic language of the model is at most {:.4} large.", coverage, non_livelock_probability));
                    } else if loop_detected && &non_livelock_probability == coverage {
                        return Err(anyhow!("A coverage of {:.4} is unattainable as the stochastic language of the model is at most {:.4} large and contains a loop.", coverage, non_livelock_probability));
                    }
                }
            }
        }

        progress_bar.finish();
        Ok((result, self.get_activity_key().clone()).into())
    }
}

struct X<FS: Hash + Display + Debug + Clone + Eq> {
    prefix: Vec<Activity>,
    probability: Fraction,
    p_state: FS
}

enum Y<FS: Hash + Display + Debug + Clone + Eq> {
    Prefix(Vec<Activity>, FS, Option<Fraction>), //last field: in case of no livelocks, the probability will be equivalent to the queueing priority (None). If this field has a value, it should be taken as the probability of the prefix.
    Trace(Vec<Activity>)
}

impl <FS: Hash + Display + Debug + Clone + Eq> Eq for Y<FS> {}

impl <FS: Hash + Display + Debug + Clone + Eq> PartialEq for Y<FS> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Prefix(l0, _, _), Self::Prefix(r0, _, _)) => l0 == r0,
            (Self::Trace(l0), Self::Trace(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl <FS: Hash + Display + Debug + Clone + Eq> Hash for Y<FS> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Y::Prefix(t, _, _) => t.hash(state),
            Y::Trace(t) => t.hash(state),
        }
    }
}