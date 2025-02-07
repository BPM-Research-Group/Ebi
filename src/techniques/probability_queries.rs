use anyhow::{anyhow, Result};
use num::{Signed, Zero};
use core::hash::Hash;
use fraction::One;
use priority_queue::PriorityQueue;
use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt::{Debug, Display},
    ops::{AddAssign, SubAssign},
};

use crate::{
    ebi_framework::{activity_key::Activity, displayable::Displayable, ebi_command::EbiCommand},
    ebi_objects::finite_stochastic_language::FiniteStochasticLanguage,
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_stochastic_deterministic_semantics::{
            EbiTraitStochasticDeterministicSemantics, StochasticDeterministicSemantics,
        },
    },
    math::fraction::Fraction,
};

pub trait ProbabilityQueries {
    /**
     * Find all traces that have a given minimum probability.
     */
    fn analyse_minimum_probability(&self, at_least: &Fraction) -> Result<FiniteStochasticLanguage>;

    /**
     * Find the traces with the highest probabilities.
     */
    fn analyse_most_likely_traces(
        &self,
        number_of_traces: &usize,
    ) -> Result<FiniteStochasticLanguage>;

    /**
     * Find the most likely traces that together have a sum probability.
     */
    fn analyse_probability_coverage(&self, coverage: &Fraction)
        -> Result<FiniteStochasticLanguage>;
}

impl ProbabilityQueries for EbiTraitStochasticDeterministicSemantics {
    fn analyse_minimum_probability(&self, at_least: &Fraction) -> Result<FiniteStochasticLanguage> {
        match self {
            EbiTraitStochasticDeterministicSemantics::Usize(sem) => {
                sem.analyse_minimum_probability(at_least)
            }
            EbiTraitStochasticDeterministicSemantics::PMarking(sem) => {
                sem.analyse_minimum_probability(at_least)
            }
        }
    }

    fn analyse_most_likely_traces(
        &self,
        number_of_traces: &usize,
    ) -> Result<FiniteStochasticLanguage> {
        match self {
            EbiTraitStochasticDeterministicSemantics::Usize(sem) => {
                sem.analyse_most_likely_traces(number_of_traces)
            }
            EbiTraitStochasticDeterministicSemantics::PMarking(sem) => {
                sem.analyse_most_likely_traces(number_of_traces)
            }
        }
    }

    fn analyse_probability_coverage(
        &self,
        coverage: &Fraction,
    ) -> Result<FiniteStochasticLanguage> {
        match self {
            EbiTraitStochasticDeterministicSemantics::Usize(sem) => {
                sem.analyse_probability_coverage(coverage)
            }
            EbiTraitStochasticDeterministicSemantics::PMarking(sem) => {
                sem.analyse_probability_coverage(coverage)
            }
        }
    }
}

impl ProbabilityQueries for dyn EbiTraitFiniteStochasticLanguage {
    fn analyse_most_likely_traces(
        &self,
        number_of_traces: &usize,
    ) -> Result<FiniteStochasticLanguage> {
        if self.len() == 0 {
            Ok((HashMap::new(), self.get_activity_key().clone()).into())
        } else if number_of_traces.is_one() {
            let mut result = HashMap::new();

            let (mut max_trace, mut max_probability) =
                self.iter_trace_probability().next().ok_or_else(|| {
                    anyhow!("Finite stochastic language is empty where it should not.")
                })?;

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
                match result.binary_search_by(|&(_, cmp_probability): &(_, &Fraction)| {
                    cmp_probability.cmp(probability)
                }) {
                    Ok(index) => {
                        if index < number_of_traces - 1 {
                            result.insert(index, (trace, probability))
                        }
                    }
                    Err(index) => {
                        if index < number_of_traces - 1 {
                            result.insert(index, (trace, probability))
                        }
                    }
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

    fn analyse_probability_coverage(
        &self,
        coverage: &Fraction,
    ) -> Result<FiniteStochasticLanguage> {
        if coverage.is_zero() {
            return Ok((HashMap::new(), self.get_activity_key().clone()).into());
        } else if self.len() == 0 {
            return Err(anyhow!(
                "A coverage of {:.4} is unattainable as the stochastic language is empty.",
                coverage
            ));
        }

        //idea: keep a list of traces sorted by probability

        //insert the first trace
        let mut result = vec![(self.get_trace(0).unwrap(), self.get_probability(0).unwrap())];

        let mut sum = Fraction::zero();

        for (trace, probability) in self.iter_trace_probability().skip(1) {
            if &sum < coverage || probability > result[0].1 {
                //as the new trace has a higher probability than the trace with the lowest probability, we have to insert it
                match result.binary_search_by(|&(_, cmp_probability): &(_, &Fraction)| {
                    cmp_probability.cmp(probability)
                }) {
                    Ok(index) | Err(index) => result.insert(index, (trace, probability)),
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

impl<DState: Displayable>
    dyn StochasticDeterministicSemantics<DetState = DState, LivState = DState>
{
    pub fn iterate_most_likely_traces<F>(
        &self,
        mut stop: F,
        mut sum: MaybeConstant,
        mut total_non_livelock_probability: MaybeConstant,
    ) -> Result<Vec<(Vec<Activity>, Fraction)>>
    where
        F: FnMut(
            &Vec<(Vec<Activity>, Fraction)>,
            &Fraction,
            &MaybeConstant,
            &MaybeConstant,
        ) -> Result<bool>,
    {
        let mut queue = PriorityQueue::new();
        queue.push(
            Z::Prefix(
                Fraction::one(),
                vec![],
                self.get_deterministic_initial_state()?,
            ),
            Fraction::one(),
        );
        let mut s = vec![];

        while let Some((z, priority)) = queue.pop() {

            match z {
                Z::Prefix(prefix_probability, prefix, q_state) => {
                    // log::debug!(
                    //     "queue length: {}, queue head: prefix {:?}, q-state {:?}, priority {:.4}",
                    //     queue.len(), prefix, q_state, priority
                    // );

                    //see whether we are done
                    if stop(&s, &prefix_probability, &sum, &total_non_livelock_probability)? {
                        return Ok(s);
                    }

                    //check whether we can terminate
                    let termination_probability =
                        self.get_deterministic_termination_probability(&q_state);
                    if termination_probability.is_positive() {
                        let mut trace_probability = termination_probability;
                        trace_probability *= &prefix_probability;
                        queue.push(Z::Trace(prefix.clone()), trace_probability);
                    }

                    //follow outgoing activities
                    let enabled_activities = self.get_deterministic_enabled_activities(&q_state);
                    // log::debug!("\tenabled activities: {}", enabled_activities.len());
                    for activity in enabled_activities {
                        // log::debug!(
                        //     "\t\tconsider activity {:?} {}",
                        //     activity,
                        //     self.get_activity_key().deprocess_activity(&activity)
                        // );

                        let new_q_state =
                            self.execute_deterministic_activity(&q_state, activity)?;
                        let livelock_probability = self
                            .get_deterministic_non_decreasing_livelock_probability(
                                &mut new_q_state.clone(),
                            )?;

                        if !livelock_probability.is_one() {
                            let probability_activity =
                                self.get_deterministic_activity_probability(&q_state, activity);

                            let mut new_probability = prefix_probability.clone();
                            new_probability *= probability_activity;

                            let mut new_prefix = prefix.clone();
                            new_prefix.push(activity);

                            let mut new_priority = new_probability.clone();
                            new_priority *= livelock_probability.one_minus();

                            queue.push(
                                Z::Prefix(new_probability, new_prefix, new_q_state),
                                new_priority,
                            );
                            // log::debug!("\t\t\tpush to queue, queue length {}\n{:?}", queue.len(), queue);
                        } else {
                            total_non_livelock_probability -= &prefix_probability;
                        }
                    }
                }
                Z::Trace(trace) => {

                    //see whether we are done
                    if stop(&s, &priority, &sum, &total_non_livelock_probability)? {
                        return Ok(s);
                    }

                    sum += &priority;
                    s.push((trace, priority));
                }
            }
        }

        Ok(s)
    }
}

#[derive(Debug)]
pub enum Z<FS: Hash + Display + Debug + Clone + Eq> {
    Prefix(Fraction, Vec<Activity>, FS),
    Trace(Vec<Activity>),
}

impl<FS: Hash + Display + Debug + Clone + Eq> Eq for Z<FS> {}

impl<FS: Hash + Display + Debug + Clone + Eq> PartialEq for Z<FS> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Prefix(_, l0, _), Self::Prefix(_, r0, _)) => l0 == r0,
            (Self::Trace(l0), Self::Trace(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<FS: Hash + Display + Debug + Clone + Eq> Hash for Z<FS> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Z::Prefix(_, t, _) => t.hash(state),
            Z::Trace(t) => t.hash(state),
        }
    }
}

pub enum MaybeConstant {
    Some(Fraction),
    None(Fraction),
}

impl MaybeConstant {
    pub fn fraction(&self) -> &Fraction {
        match &self {
            MaybeConstant::Some(fraction) => fraction,
            MaybeConstant::None(fraction) => fraction,
        }
    }

    pub fn some_zero() -> MaybeConstant {
        Self::Some(Fraction::zero())
    }

    pub fn some_one() -> MaybeConstant {
        Self::Some(Fraction::one())
    }

    pub fn none_zero() -> MaybeConstant {
        Self::None(Fraction::zero())
    }

    pub fn none_one() -> MaybeConstant {
        Self::None(Fraction::one())
    }
}

impl AddAssign<&Fraction> for MaybeConstant {
    fn add_assign(&mut self, rhs: &Fraction) {
        match self {
            MaybeConstant::Some(fraction) => *fraction += rhs,
            MaybeConstant::None(_) => {}
        }
    }
}

impl SubAssign<&Fraction> for MaybeConstant {
    fn sub_assign(&mut self, rhs: &Fraction) {
        match self {
            MaybeConstant::Some(fraction) => *fraction -= rhs,
            MaybeConstant::None(_) => {}
        }
    }
}

impl PartialEq<Fraction> for MaybeConstant {
    fn eq(&self, other: &Fraction) -> bool {
        match self {
            MaybeConstant::Some(fraction) => fraction.eq(other),
            MaybeConstant::None(fraction) => fraction.eq(other),
        }
    }
}

impl PartialOrd<Fraction> for MaybeConstant {
    fn partial_cmp(&self, other: &Fraction) -> Option<Ordering> {
        match self {
            MaybeConstant::Some(fraction) => fraction.partial_cmp(other),
            MaybeConstant::None(fraction) => fraction.partial_cmp(other),
        }
    }
}

impl<DState: Displayable> ProbabilityQueries
    for dyn StochasticDeterministicSemantics<DetState = DState, LivState = DState>
{
    fn analyse_minimum_probability(&self, at_least: &Fraction) -> Result<FiniteStochasticLanguage> {

        let progress_bar = EbiCommand::get_progress_bar_message(
            "found 0 traces; lowest considered prefix probability 0.0000000".to_owned(),
        );

        let s = self.iterate_most_likely_traces(
            |s, prefix_probability, _, _| {

                //update progress bar
                progress_bar.set_message(format!(
                    "found {} traces; lowest considered prefix probability {:.8}",
                    s.len(),
                    prefix_probability
                ));
                   
                Ok(prefix_probability < at_least)
            },
            MaybeConstant::none_zero(),
            MaybeConstant::none_one(),
        )?;
        progress_bar.finish_and_clear();

        let map: HashMap<_, _> = s.into_iter().collect();
        Ok((map, self.get_activity_key().clone()).into())
    }

    fn analyse_most_likely_traces(
        &self,
        number_of_traces: &usize,
    ) -> Result<FiniteStochasticLanguage> {
        let progress_bar = EbiCommand::get_progress_bar_ticks(*number_of_traces);

        let mut last_number_of_traces = 0;

        let s = self.iterate_most_likely_traces(
            |s, _, _, _| {
                //update progress bar
                if last_number_of_traces != s.len() {
                    progress_bar.set_position(s.len().try_into().unwrap());
                    last_number_of_traces = s.len();
                }

                Ok(s.len() >= *number_of_traces)
            },
            MaybeConstant::none_zero(),
            MaybeConstant::none_one(),
        )?;
        progress_bar.finish_and_clear();

        let map: HashMap<_, _> = s.into_iter().collect();
        Ok((map, self.get_activity_key().clone()).into())
    }

    fn analyse_probability_coverage(
        &self,
        coverage: &Fraction,
    ) -> Result<FiniteStochasticLanguage> {
        if !coverage.is_positive() {
            return Ok(HashMap::new().into());
        }
        if coverage > &Fraction::one() {
            return Err(anyhow!("A coverage of {} is unattainable.", coverage));
        }

        let progress_bar = EbiCommand::get_progress_bar_message(
            "found 0 traces, which cover 0.0000000".to_owned(),
        );

        let mut last_number_of_traces = 0;

        let s = self.iterate_most_likely_traces(
            |s, _, sum, total_non_livelock_probability| {

                //update progress bar
                if last_number_of_traces != s.len() {
                    progress_bar.set_message(format!(
                        "found {} traces, which cover {:.8}",
                        s.len(),
                        sum.fraction()
                    ));
                    last_number_of_traces = s.len();
                }

                if total_non_livelock_probability < coverage {
                    Err(anyhow!("A probability coverage of {} was requested, but only {} is available due to livelocks.", coverage, total_non_livelock_probability.fraction()))
                } else {
                    Ok(sum >= coverage)
                }
            },
            MaybeConstant::some_zero(),
            MaybeConstant::some_one(),
        )?;
        progress_bar.finish_and_clear();

        let map: HashMap<_, _> = s.into_iter().collect();
        Ok((map, self.get_activity_key().clone()).into())
    }
}