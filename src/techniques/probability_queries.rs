use anyhow::{Result, anyhow};
use core::hash::Hash;
use ebi_arithmetic::{Fraction, One, OneMinus, Signed, Zero};
use ebi_objects::{Activity, FiniteStochasticLanguage};
use priority_queue::PriorityQueue;
use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt::{Debug, Display},
    ops::{AddAssign, SubAssign},
};

use crate::{
    ebi_framework::{displayable::Displayable, ebi_command::EbiCommand},
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_stochastic_deterministic_semantics::{
            EbiTraitStochasticDeterministicSemantics, StochasticDeterministicSemantics,
        },
    },
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
            EbiTraitStochasticDeterministicSemantics::UsizeDistribution(sem) => {
                sem.analyse_minimum_probability(at_least)
            }
            EbiTraitStochasticDeterministicSemantics::LPNMarkingDistribution(sem) => {
                sem.analyse_minimum_probability(at_least)
            }
            EbiTraitStochasticDeterministicSemantics::NodeStatesDistribution(sem) => {
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
            EbiTraitStochasticDeterministicSemantics::UsizeDistribution(sem) => {
                sem.analyse_most_likely_traces(number_of_traces)
            }
            EbiTraitStochasticDeterministicSemantics::LPNMarkingDistribution(sem) => {
                sem.analyse_most_likely_traces(number_of_traces)
            }
            EbiTraitStochasticDeterministicSemantics::NodeStatesDistribution(sem) => {
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
            EbiTraitStochasticDeterministicSemantics::UsizeDistribution(sem) => {
                sem.analyse_probability_coverage(coverage)
            }
            EbiTraitStochasticDeterministicSemantics::LPNMarkingDistribution(sem) => {
                sem.analyse_probability_coverage(coverage)
            }
            EbiTraitStochasticDeterministicSemantics::NodeStatesDistribution(sem) => {
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
        if self.number_of_traces() == 0 {
            Ok((self.activity_key().clone(), HashMap::new()).into())
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

            Ok((self.activity_key().clone(), result).into())
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

            Ok((self.activity_key().clone(), result2).into())
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

        Ok((self.activity_key().clone(), result2).into())
    }

    fn analyse_probability_coverage(
        &self,
        coverage: &Fraction,
    ) -> Result<FiniteStochasticLanguage> {
        if coverage.is_zero() {
            return Ok((self.activity_key().clone(), HashMap::new()).into());
        } else if self.number_of_traces() == 0 {
            return Err(anyhow!(
                "A coverage of {:.4} is unattainable as the stochastic language is empty.",
                coverage
            ));
        }

        //idea: keep a list of traces sorted by probability

        //insert the first trace
        let mut result = vec![(
            self.get_trace(0).unwrap(),
            self.get_trace_probability(0).unwrap(),
        )];

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
            return Err(anyhow!(
                "A coverage of {:.4} is unattainable as the stochastic language has a sum probability of {:.4}.",
                coverage,
                sum
            ));
        }

        let mut result2 = HashMap::new();
        for (trace, probability) in result {
            result2.insert(trace.clone(), probability.clone());
        }

        Ok((self.activity_key().clone(), result2).into())
    }
}

impl<DState: Displayable, LState: Displayable> dyn StochasticDeterministicSemantics<DetState = DState, LivState = LState> {
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
                self.get_deterministic_initial_state()?
                    .ok_or_else(|| anyhow!("Cannot get deterministic initial state."))?,
            ),
            Fraction::one(),
        );
        let mut s = vec![];

        while let Some((z, priority)) = queue.pop() {
            match z {
                Z::Prefix(prefix_probability, prefix, q_state) => {
                    // log::debug!("queue length: {}", queue.len() + 1);
                    // log::debug!(
                    //     "queue head: prefix {:?}, q-state {:?}, priority {:.4}",
                    //     prefix,
                    //     q_state,
                    //     priority,
                    // );

                    //see whether we are done
                    if stop(
                        &s,
                        &prefix_probability,
                        &sum,
                        &total_non_livelock_probability,
                    )? {
                        return Ok(s);
                    }

                    //check whether we can terminate
                    let termination_probability =
                        self.get_deterministic_termination_probability(&q_state);
                    log::debug!("\ttermination probability {:.4}", termination_probability);
                    if termination_probability.is_positive() {
                        let mut trace_probability = termination_probability;
                        trace_probability *= &prefix_probability;
                        queue.push(Z::Trace(prefix.clone()), trace_probability);

                        log::debug!(
                            "\tpush trace of length {} to queue, queue length {}",
                            prefix.len(),
                            queue.len()
                        );
                    }

                    //follow outgoing activities
                    let enabled_activities = self.get_deterministic_enabled_activities(&q_state);
                    log::debug!("\tenabled activities: {}", enabled_activities.len());
                    for activity in enabled_activities {
                        log::debug!(
                            "\t\tconsider activity {:?} {}",
                            activity,
                            self.activity_key().deprocess_activity(&activity)
                        );

                        let new_q_state =
                            self.execute_deterministic_activity(&q_state, activity)?;

                        log::debug!("\t\tq-state after activity {:?}", new_q_state);

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
                            log::debug!("\t\t\tpush prefix to queue, queue length {}", queue.len());
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

impl<DState: Displayable, LState: Displayable> ProbabilityQueries
    for dyn StochasticDeterministicSemantics<DetState = DState, LivState = LState>
{
    fn analyse_minimum_probability(&self, at_least: &Fraction) -> Result<FiniteStochasticLanguage> {
        if !at_least.is_positive() && self.infinitely_many_traces()? {
            return Err(anyhow!(
                "all traces were requested but as the model has infinitely many traces, this is impossible"
            ));
        }

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
        Ok((self.activity_key().clone(), map).into())
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
        Ok((self.activity_key().clone(), map).into())
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
                    //for experiments
                    // log::debug!("trace {} found", s.len());

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
        Ok((self.activity_key().clone(), map).into())
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_arithmetic::{Fraction, One, Zero};
    use ebi_objects::{
        FiniteStochasticLanguage, HasActivityKey, IndexTrace, StochasticDeterministicFiniteAutomaton, StochasticLabelledPetriNet
    };

    use crate::{
        ebi_traits::{
            ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
            ebi_trait_stochastic_deterministic_semantics::{
                EbiTraitStochasticDeterministicSemantics, StochasticDeterministicSemantics,
                ToStochasticDeterministicSemantics,
            },
        },
        semantics::labelled_petri_net_semantics::LPNMarking,
        techniques::{
            deterministic_semantics_for_stochastic_semantics::PMarking,
            probability_queries::ProbabilityQueries,
        },
    };

    #[test]
    fn slpn_cover_prefix() {
        let fin = fs::read_to_string("testfiles/aa-aaa-bb.slpn").unwrap();
        let mut slpn: Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<LPNMarking>,
                    LivState = LPNMarking,
                >,
        > = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());

        let state1 = slpn.get_deterministic_initial_state().unwrap().unwrap();
        let enabled1 = slpn.get_deterministic_enabled_activities(&state1);
        assert_eq!(enabled1.len(), 2);

        let a = slpn.activity_key_mut().process_activity("a");

        let state2 = slpn.execute_deterministic_activity(&state1, a).unwrap();
        let enabled2 = slpn.get_deterministic_enabled_activities(&state2);
        assert_eq!(enabled2.len(), 1);

        let state3 = slpn.execute_deterministic_activity(&state2, a).unwrap();
        assert_eq!(state3.p_marking.len(), 2);

        //after doing <a, a>, the model should be able to terminate with probability 1/10
        assert_eq!(
            slpn.get_deterministic_termination_probability(&state3),
            Fraction::from((1, 10))
        );

        //all traces must be found
        assert_eq!(
            slpn.analyse_probability_coverage(&Fraction::from((1, 1)))
                .unwrap()
                .number_of_traces(),
            3
        );
    }

    #[test]
    fn slpn_cover_bs() {
        let fin = fs::read_to_string("testfiles/infinite_bs.slpn").unwrap();
        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<LPNMarking>,
                    LivState = LPNMarking,
                >,
        > = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());
        assert_eq!(
            slpn.analyse_probability_coverage(&Fraction::from((4, 10)))
                .unwrap()
                .number_of_traces(),
            1
        );
    }

    #[test]
    fn slpn_minprob_bs() {
        let fin = fs::read_to_string("testfiles/infinite_bs.slpn").unwrap();
        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<LPNMarking>,
                    LivState = LPNMarking,
                >,
        > = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());
        assert_eq!(
            slpn.analyse_minimum_probability(&Fraction::from((51, 100)))
                .unwrap()
                .number_of_traces(),
            0
        );
    }

    // #[test]
    // fn pdc_cover() {
    //     let fin = fs::read_to_string("testfiles/pdc2023_121111.xes.gz-IMf.lpn-alignments.slpn").unwrap();
    //     let slpn: Box<
    //         dyn StochasticDeterministicSemantics<
    //             DetState = PMarking<LPNMarking>,
    //             LivState = PMarking<LPNMarking>,
    //         >,
    //     > = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());

    //     slpn.analyse_probability_coverage(&Fraction::from((4, 1000))).unwrap();
    // }

    #[test]
    fn slpn_cover_silent_livelock() {
        let fin = fs::read_to_string("testfiles/empty_lang_multiple_silent.slpn").unwrap();
        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<LPNMarking>,
                    LivState = LPNMarking,
                >,
        > = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());

        let slang2 = slpn
            .analyse_probability_coverage(&Fraction::zero())
            .unwrap();

        assert_eq!(slang2.number_of_traces(), 0);
    }

    #[test]
    fn slpn_cover_empty_net() {
        let fin = fs::read_to_string("testfiles/empty_net.slpn").unwrap();

        let slpn = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());
        let state = slpn.get_deterministic_initial_state().unwrap().unwrap();
        assert_eq!(slpn.get_deterministic_enabled_activities(&state).len(), 0);
        // assert_eq!(
        //     slpn.get_deterministic_termination_probability(&state),
        //     Fraction::one()
        // );

        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<LPNMarking>,
                    LivState = LPNMarking,
                >,
        > = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());
        let slang2 = slpn
            .analyse_probability_coverage(&Fraction::zero())
            .unwrap();
        assert_eq!(slang2.number_of_traces(), 0);

        let fin2 = fs::read_to_string("testfiles/empty_trace.slang").unwrap();
        let slang3 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(
            slpn.analyse_probability_coverage(&Fraction::from((1, 2)))
                .unwrap(),
            slang3
        );
    }

    #[test]
    fn slpn_cover_infinite_bs() {
        let fin = fs::read_to_string("testfiles/infinite_bs.slpn").unwrap();
        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<LPNMarking>,
                    LivState = LPNMarking,
                >,
        > = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());

        let state1 = slpn.get_deterministic_initial_state().unwrap().unwrap();
        let enabled1 = slpn.get_deterministic_enabled_activities(&state1);

        assert_eq!(enabled1.len(), 2);
    }

    #[test]
    fn slpn_empty_lang_labelled_cover() {
        let fin = fs::read_to_string("testfiles/empty_lang_labelled.slpn").unwrap();

        let slpn = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());
        let state = slpn.get_deterministic_initial_state().unwrap().unwrap();
        assert_eq!(slpn.get_deterministic_enabled_activities(&state).len(), 1);
        assert_eq!(
            slpn.get_deterministic_termination_probability(&state),
            Fraction::zero()
        );

        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<LPNMarking>,
                    LivState = LPNMarking,
                >,
        > = slpn;
        let slang2 = slpn
            .analyse_probability_coverage(&Fraction::zero())
            .unwrap();
        assert_eq!(slang2.number_of_traces(), 0);

        let fin2 = fs::read_to_string("testfiles/empty.slang").unwrap();
        let slang3 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(
            slpn.analyse_probability_coverage(&Fraction::from((1, 2)))
                .unwrap(),
            slang3
        );
    }

    #[test]
    fn slpn_empty_lang_silent_cover() {
        let fin = fs::read_to_string("testfiles/empty_lang_silent.slpn").unwrap();

        let slpn = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());
        let state = slpn.get_deterministic_initial_state().unwrap().unwrap();
        assert_eq!(slpn.get_deterministic_enabled_activities(&state).len(), 0);
        assert_eq!(
            slpn.get_deterministic_termination_probability(&state),
            Fraction::zero()
        );

        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<LPNMarking>,
                    LivState = LPNMarking,
                >,
        > = slpn;
        let slang2 = slpn
            .analyse_probability_coverage(&Fraction::zero())
            .unwrap();
        assert_eq!(slang2.number_of_traces(), 0);

        let fin2 = fs::read_to_string("testfiles/empty.slang").unwrap();
        let slang3 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(
            slpn.analyse_probability_coverage(&Fraction::zero())
                .unwrap(),
            slang3
        );
    }

    #[test]
    fn slpn_cover_nothing() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba_ali.slpn").unwrap();
        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<LPNMarking>,
                    LivState = LPNMarking,
                >,
        > = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());

        let slang2 = slpn
            .analyse_probability_coverage(&Fraction::zero())
            .unwrap();

        assert_eq!(slang2.number_of_traces(), 0);
    }

    #[test]
    fn slang_cover_empty() {
        let fin = fs::read_to_string("testfiles/empty.slang").unwrap();
        let slang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(fin.parse::<FiniteStochasticLanguage>().unwrap());

        let slang2 = slang
            .analyse_probability_coverage(&Fraction::zero())
            .unwrap();
        assert_eq!(slang2.number_of_traces(), 0);

        let slang3 = slang.analyse_probability_coverage(&Fraction::one());
        assert!(slang3.is_err());

        assert!(
            slang
                .analyse_probability_coverage(&Fraction::from((1, 2)))
                .is_err()
        );
    }

    #[test]
    fn slpn_cover_empty() {
        let fin = fs::read_to_string("testfiles/empty_lang_labelled.slpn").unwrap();
        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<LPNMarking>,
                    LivState = LPNMarking,
                >,
        > = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());
        let slang2 = slpn
            .analyse_probability_coverage(&Fraction::zero())
            .unwrap();
        assert_eq!(slang2.number_of_traces(), 0);
    }

    #[test]
    fn slpn_minprob_livelock() {
        let fin = fs::read_to_string("testfiles/a-b-c-livelock.slpn").unwrap();
        let slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();
        let x: Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<LPNMarking>,
                    LivState = LPNMarking,
                >,
        > = Box::new(slpn);
        let slang1 = x
            .analyse_minimum_probability(&Fraction::from((1, 5)))
            .unwrap();

        let fin2 = fs::read_to_string("testfiles/a-b-c-livelock.slang").unwrap();
        let slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(slang1, slang2);
    }

    #[test]
    fn slang_minprob_one_deterministic() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let semantics = slang.to_stochastic_deterministic_semantics();

        assert_eq!(
            semantics
                .analyse_minimum_probability(&Fraction::one())
                .unwrap()
                .number_of_traces(),
            0
        );
    }

    #[test]
    fn slang_minprob_one() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();

        let slang2: &dyn EbiTraitFiniteStochasticLanguage = &slang;
        assert_eq!(
            slang2
                .analyse_minimum_probability(&Fraction::one())
                .unwrap()
                .number_of_traces(),
            0
        );
    }

    #[test]
    fn slang_minprob_zero() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let slang2: &dyn EbiTraitFiniteStochasticLanguage = &slang;

        //should return the same
        let slang3 = slang2
            .analyse_minimum_probability(&Fraction::zero())
            .unwrap();

        assert_eq!(slang, slang3)
    }

    #[test]
    fn slang_minprob_zero_through_sdfa() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        assert_eq!(slang.number_of_traces(), 3);
        let mut sdfa: StochasticDeterministicFiniteAutomaton = slang.clone().into();
        assert_eq!(sdfa.max_state, 5);
        assert_eq!(sdfa.get_number_of_transitions(), 5);

        //initial state
        let state = sdfa.get_deterministic_initial_state().unwrap().unwrap();
        assert_eq!(state, 0);
        assert_eq!(sdfa.get_deterministic_enabled_activities(&state).len(), 2);

        //take a b
        let b = sdfa.activity_key_mut().process_activity("a");
        sdfa.execute_deterministic_activity(&state, b).unwrap();

        let semantics = EbiTraitStochasticDeterministicSemantics::Usize(Box::new(sdfa));

        // let semantics = slang.clone().to_stochastic_deterministic_semantics();

        //should return the same
        let slang2 = semantics
            .analyse_minimum_probability(&Fraction::zero())
            .unwrap();

        assert_eq!(slang, slang2)
    }

    #[test]
    fn sdfa_minprob_one() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();
        let semantics = sdfa.to_stochastic_deterministic_semantics();
        assert_eq!(
            semantics
                .analyse_minimum_probability(&Fraction::one())
                .unwrap()
                .number_of_traces(),
            0
        );
    }

    #[test]
    fn sdfa_minprob_zero() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();
        let semantics = sdfa.to_stochastic_deterministic_semantics();

        let slang2 = semantics
            .analyse_minimum_probability(&Fraction::zero())
            .unwrap();

        let should_string = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let should_slang = should_string.parse::<FiniteStochasticLanguage>().unwrap();
        assert_eq!(should_slang, slang2)
    }

    #[test]
    fn mode() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba_uni.slpn").unwrap();
        let slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        let x: Box<
            dyn StochasticDeterministicSemantics<
                    DetState = PMarking<LPNMarking>,
                    LivState = LPNMarking,
                >,
        > = Box::new(slpn);
        let slang = x.analyse_most_likely_traces(&1).unwrap();

        // let slang = Box::new(slpn).analyse_most_likely_traces(&1).unwrap();
        let fout = fs::read_to_string("testfiles/ba.slang").unwrap();
        assert_eq!(fout, slang.to_string())
    }
}
