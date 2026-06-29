use crate::{
    ebi_traits::{
        ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    },
    techniques::{
        inductive_miner::{
            cut_finding::{Cut, find_default_cut},
            find_base_cases::{
                base_case_empty_log, base_case_single_activity, base_case_single_activity_filtering,
            },
            find_fall_throughs::{
                activity_concurrent, activity_once_per_trace, empty_traces, flower_model,
                strict_tau_loop, tau_loop,
            },
            log_info::{ComputeLogInfo, LogInfo},
            log_splitting::SplitLog,
        },
        reduce::ReduceLanguageEquivalently,
    },
};
use ebi_objects::{
    FiniteLanguage, FiniteStochasticLanguage, ProcessTree,
    ebi_arithmetic::{Fraction, One, Signed, Zero},
    ebi_objects::process_tree::{Node, Operator},
};
use intmap::IntMap;
use std::fmt::Display;

pub trait InductiveMiner {
    /// Apply the Inductive Miner algorithm.
    fn inductive_miner(&self) -> ProcessTree;
}

impl InductiveMiner for dyn EbiTraitFiniteLanguage {
    fn inductive_miner(&self) -> ProcessTree {
        self.to_finite_language().inductive_miner()
    }
}

impl InductiveMiner for FiniteLanguage {
    fn inductive_miner(&self) -> ProcessTree {
        //create a finite stochastic language
        let slang = FiniteStochasticLanguage {
            activity_key: self.activity_key.clone(),
            traces: self
                .traces
                .iter()
                .map(|trace| (trace.clone(), Fraction::one()))
                .collect(),
        };
        slang.inductive_miner()
    }
}

impl InductiveMiner for FiniteStochasticLanguage {
    fn inductive_miner(&self) -> ProcessTree {
        let parameters = InductiveMinerParameters::default();
        inductive_miner(self, &parameters)
    }
}

pub trait InductiveMinerInfrequent {
    /// Apply the Inductive Miner - infrequent algorithm. The noise filtering parameter is 0 (no noise filtering) to 1 (maximal noise filtering).
    fn inductive_miner_infrequent(&self, noise_filtering: &Fraction) -> ProcessTree;
}

macro_rules! imf {
    ($t:ty) => {
        impl InductiveMinerInfrequent for $t {
            fn inductive_miner_infrequent(&self, noise_filtering: &Fraction) -> ProcessTree {
                let mut parameters = InductiveMinerParameters::default();
                parameters.noise_filtering = noise_filtering.clone();
                parameters.base_cases = |_, info, parameters, recursion_depth| {
                    base_case_empty_log(info, recursion_depth).or_else(|| {
                        base_case_single_activity_filtering(info, parameters, recursion_depth)
                    })
                };
                parameters.cut_finders = |_, info, parameters, recursion_depth| {
                    find_default_cut(info).or_else(|| {
                        let info = filter_log_info(info, parameters, recursion_depth);
                        find_default_cut(&info)
                    })
                };
                inductive_miner(self, &parameters)
            }
        }
    };
}
imf!(dyn EbiTraitFiniteStochasticLanguage);
imf!(FiniteStochasticLanguage);

fn inductive_miner(
    log: &dyn EbiTraitFiniteStochasticLanguage,
    parameters: &InductiveMinerParameters,
) -> ProcessTree {
    let nodes = inductive_miner_recursion(log, parameters, 0);
    let mut tree: ProcessTree = (log.activity_key().clone(), nodes).into();
    if parameters.reduce {
        tree.reduce_language_equivalently();
    }
    tree
}

pub struct InductiveMinerParameters {
    reduce: bool,
    noise_filtering: Fraction,
    base_cases: fn(
        log: &dyn EbiTraitFiniteStochasticLanguage,
        info: &LogInfo,
        parameters: &InductiveMinerParameters,
        recursion_depth: usize,
    ) -> Option<Vec<Node>>,
    cut_finders: fn(
        log: &dyn EbiTraitFiniteStochasticLanguage,
        info: &LogInfo,
        parameters: &InductiveMinerParameters,
        recursion_depth: usize,
    ) -> Option<Cut>,
    fall_throughs: fn(
        log: &dyn EbiTraitFiniteStochasticLanguage,
        info: &LogInfo,
        parameters: &InductiveMinerParameters,
        recursion_depth: usize,
    ) -> Vec<Node>,
}

impl Default for InductiveMinerParameters {
    fn default() -> Self {
        Self {
            reduce: true,
            noise_filtering: Fraction::zero(),
            base_cases: |_, info, _, recursion_depth| {
                base_case_empty_log(info, recursion_depth)
                    .or_else(|| base_case_single_activity(info, recursion_depth))
            },
            cut_finders: |_, info, _, _| find_default_cut(info),
            fall_throughs: |log, info, parameters, recursion_depth| {
                empty_traces(log, info, parameters, recursion_depth)
                    .or_else(|| activity_once_per_trace(log, info, parameters, recursion_depth))
                    .or_else(|| activity_concurrent(log, info, parameters, recursion_depth))
                    .or_else(|| strict_tau_loop(log, info, parameters, recursion_depth))
                    .or_else(|| tau_loop(log, info, parameters, recursion_depth))
                    .unwrap_or_else(|| flower_model(log, info, recursion_depth))
            },
        }
    }
}

fn inductive_miner_recursion(
    log: &dyn EbiTraitFiniteStochasticLanguage,
    parameters: &InductiveMinerParameters,
    recursion_depth: usize,
) -> Vec<Node> {
    let info = log.compute_log_info();

    if let Some(tree) = (parameters.base_cases)(log, &info, parameters, recursion_depth) {
        return tree;
    }

    if info.dfg.empty_traces_weight.is_not_positive() {
        if let Some(cut) = (parameters.cut_finders)(log, &info, parameters, recursion_depth) {
            debug(format!("cut found {:?}", cut), recursion_depth);
            let sublogs = log.split_log(&cut, recursion_depth);
            let subtrees = sublogs
                .iter()
                .map(|s| inductive_miner_recursion(s, parameters, recursion_depth + 1))
                .collect();
            return cut.build_tree(subtrees);
        }
    }

    (parameters.fall_throughs)(log, &info, parameters, recursion_depth)
}

fn debug<S: AsRef<str> + Display>(message: S, recursion_depth: usize) {
    if cfg!(test) {
        println!("{}{}", "\t".repeat(recursion_depth), message);
    } else {
        log::info!("{}{}", "\t".repeat(recursion_depth), message);
    }
}

fn build_operator_node(operator: Operator, subtrees: Vec<Vec<Node>>) -> Vec<Node> {
    let mut nodes = vec![Node::Operator(operator, subtrees.len())];
    for subtree in subtrees {
        nodes.extend(subtree);
    }
    nodes
}

mod find_base_cases {
    use crate::techniques::inductive_miner::{InductiveMinerParameters, debug, log_info::LogInfo};
    use ebi_objects::{
        ebi_arithmetic::{Fraction, Signed, Zero, f},
        ebi_objects::process_tree::Node,
    };

    pub(super) fn base_case_empty_log(info: &LogInfo, recursion_depth: usize) -> Option<Vec<Node>> {
        if info.total_traces.is_zero() {
            debug("empty log", recursion_depth);
            return Some(vec![]);
        }
        None
    }

    pub(super) fn base_case_single_activity(
        info: &LogInfo,
        recursion_depth: usize,
    ) -> Option<Vec<Node>> {
        if info.activities.len() == 1
            && !info.dfg.empty_traces_weight.is_positive()
            && info.activity_instances == info.total_traces
        {
            let only_activity = *info.activities.iter().next()?;
            debug(
                format!("single activity {}", only_activity),
                recursion_depth,
            );
            return Some(vec![Node::Activity(only_activity)]);
        }
        None
    }

    pub(super) fn base_case_single_activity_filtering(
        info: &LogInfo,
        parameters: &InductiveMinerParameters,
        recursion_depth: usize,
    ) -> Option<Vec<Node>> {
        if info.activities.len() != 1 {
            return None;
        }
        let p_hat = &info.total_traces / &(&info.total_traces + &info.activity_instances);
        if (p_hat - f!(1, 2)).abs() < parameters.noise_filtering {
            let only_activity = *info.activities.iter().next()?;

            debug(
                format!("single activity filtering {}", only_activity),
                recursion_depth,
            );

            return Some(vec![Node::Activity(only_activity)]);
        }
        None
    }
}

mod log_splitting {
    use crate::{
        ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        techniques::inductive_miner::cut_finding::Cut,
    };
    use ebi_objects::{Activity, FiniteStochasticLanguage};
    use std::collections::{HashMap, HashSet};

    pub(super) trait SplitLog<T> {
        fn split_log(&self, cut: &Cut, recursion_depth: usize) -> Vec<T>;
    }

    macro_rules! split_log_imf {
        ($t:ty, $r:ty) => {
            impl SplitLog<$r> for $t {
                fn split_log(&self, cut: &Cut, recursion_depth: usize) -> Vec<$r> {
                    match cut {
                        Cut::ExclusiveChoice(parts) => split_log_exclusive(self, parts),
                        Cut::Sequence(parts) => split_log_sequence(self, parts, recursion_depth),
                        Cut::Concurrent(parts) => split_log_concurrent(self, parts),
                        Cut::Loop { body, redos } => split_log_loop(self, body, redos),
                    }
                }
            }
        };
    }
    split_log_imf!(
        dyn EbiTraitFiniteStochasticLanguage + '_,
        FiniteStochasticLanguage
    );
    split_log_imf!(FiniteStochasticLanguage, FiniteStochasticLanguage);

    fn split_log_exclusive(
        log: &dyn EbiTraitFiniteStochasticLanguage,
        parts: &[HashSet<Activity>],
    ) -> Vec<FiniteStochasticLanguage> {
        let mut sublogs: Vec<FiniteStochasticLanguage> = parts
            .iter()
            .map(|_| FiniteStochasticLanguage::new_with_activity_key(log.activity_key().clone()))
            .collect();

        //map activities to sigmas
        let mut eventclass2sigma_index = HashMap::new();
        let mut sigma_index2sigma = HashMap::new();
        {
            let mut p = 0;
            for sigma in parts {
                sigma_index2sigma.insert(p, sigma);
                for activity in sigma {
                    eventclass2sigma_index.insert(activity, p);
                }
                p += 1;
            }
        }

        for (trace, probability) in log.iter_traces_probabilities() {
            //walk through the events and count how many go in each sigma
            let mut sigma_event_counters = vec![0; parts.len()];
            let mut max_counter = 0;
            let mut max_sigma = None;
            for activity in trace {
                let sigma_index = eventclass2sigma_index.get(activity).unwrap();
                sigma_event_counters[*sigma_index] += 1;
                if sigma_event_counters[*sigma_index] > max_counter {
                    max_counter = sigma_event_counters[*sigma_index];
                    max_sigma = Some(sigma_index);
                }
            }

            //determine whether this trace should go in this sublog
            if let Some(sigma) = max_sigma {
                //put it in the right sigma, but remove events not from sigma
                let mut trace = trace.clone();
                trace.retain(|activity| eventclass2sigma_index.get(activity) == max_sigma);
                _ = sublogs[*sigma].push_raw(trace, probability);
            } else {
                /*
                 * We have no information which sigma could
                 * have produced an empty trace, so we keep it in all sublogs.
                 */
                for sublog in sublogs.iter_mut() {
                    _ = sublog.push_raw(vec![], probability);
                }
            }
        }

        //normalise sub-logs
        for sublog in sublogs.iter_mut() {
            sublog.normalise()
        }

        sublogs
    }

    fn split_log_sequence(
        log: &dyn EbiTraitFiniteStochasticLanguage,
        parts: &[HashSet<Activity>],
        _recursion_depth: usize,
    ) -> Vec<FiniteStochasticLanguage> {
        let mut sublogs: Vec<FiniteStochasticLanguage> = parts
            .iter()
            .map(|_| FiniteStochasticLanguage::new_with_activity_key(log.activity_key().clone()))
            .collect();

        //create map activity -> part
        let mut activity_2_part = vec![usize::MAX; log.activity_key().get_number_of_activities()];
        for (part_i, part) in parts.iter().enumerate() {
            for activity in part {
                activity_2_part[activity.id] = part_i;
            }
        }

        for (trace, probability) in log.iter_traces_probabilities() {
            let mut ignore = HashSet::new();
            let mut at_position = 0;

            for (sigma_i, sigma) in parts.iter().enumerate() {
                let subtrace_start = at_position;

                //find where this sigma's subtrace will end
                if sigma_i < parts.len() - 1 {
                    at_position = find_optimal_split(trace, sigma, at_position, &ignore);
                } else {
                    //if this is the last sigma, this sigma must finish the trace
                    at_position = trace.len();
                }
                ignore.extend(sigma);

                //walk over this subtrace, remove all events not from sigma
                let subtrace = trace[subtrace_start..at_position]
                    .iter()
                    .filter(|x| sigma.contains(x))
                    .copied()
                    .collect();

                sublogs[sigma_i].push_raw(subtrace, probability).unwrap();
            }
        }

        // debug(format!("logs {:?}", sublogs), recursion_depth);

        sublogs
    }

    fn find_optimal_split(
        trace: &Vec<Activity>,
        sigma: &HashSet<Activity>,
        start_position: usize,
        ignore: &HashSet<Activity>,
    ) -> usize {
        let mut position_least_cost = 0;
        let mut least_cost = 0;
        let mut cost = 0;
        let mut position = 0;

        let mut it = trace.iter().peekable();

        //debug("find optimal split in " + trace.toString() + " for " + sigma.toString());

        //move to the start position
        while position < start_position && it.peek().is_some() {
            position = position + 1;
            position_least_cost = position_least_cost + 1;
            it.next();
        }

        while let Some(activity) = it.next() {
            if ignore.contains(activity) {
                //skip
            } else if sigma.contains(activity) {
                cost -= 1;
            } else {
                cost += 1;
            }

            position += 1;

            if cost < least_cost {
                least_cost = cost;
                position_least_cost = position;
            }
        }

        return position_least_cost;
    }

    pub(super) fn split_log_concurrent(
        log: &dyn EbiTraitFiniteStochasticLanguage,
        parts: &[HashSet<Activity>],
    ) -> Vec<FiniteStochasticLanguage> {
        let mut sublogs: Vec<FiniteStochasticLanguage> = parts
            .iter()
            .map(|_| FiniteStochasticLanguage::new_with_activity_key(log.activity_key().clone()))
            .collect();

        for (trace, probability) in log.iter_traces_probabilities() {
            for (i, partition) in parts.iter().enumerate() {
                let subtrace: Vec<Activity> = trace
                    .iter()
                    .filter(|&&a| partition.contains(&a))
                    .cloned()
                    .collect();
                sublogs[i].push_raw(subtrace, probability).unwrap();
            }
        }
        sublogs
    }

    fn split_log_loop(
        log: &dyn EbiTraitFiniteStochasticLanguage,
        body: &HashSet<Activity>,
        redos: &[HashSet<Activity>],
    ) -> Vec<FiniteStochasticLanguage> {
        let mut sublogs: Vec<FiniteStochasticLanguage> = (0..redos.len() + 1)
            .map(|_| FiniteStochasticLanguage::new_with_activity_key(log.activity_key().clone()))
            .collect();

        let find_part = |a: Activity| -> usize {
            if body.contains(&a) {
                return 0;
            }
            redos
                .iter()
                .position(|r| r.contains(&a))
                .map_or(0, |i| i + 1)
        };

        for (trace, probability) in log.iter_traces_probabilities() {
            let mut current: Vec<Activity> = Vec::new();
            let mut current_part = 0;
            for &a in trace {
                let part = find_part(a);
                if part != current_part {
                    sublogs[current_part]
                        .push_raw(std::mem::take(&mut current), probability)
                        .unwrap();
                    current_part = part;
                }
                current.push(a);
            }
            sublogs[current_part]
                .push_raw(current, probability)
                .unwrap();
        }

        //normalise the sub-logs
        for sublog in sublogs.iter_mut() {
            sublog.normalise();
        }

        sublogs
    }
}

mod cut_finding {
    use crate::{
        math::components::Components,
        techniques::inductive_miner::{build_operator_node, log_info::LogInfo},
    };
    use ebi_objects::{
        Activity,
        ebi_arithmetic::Signed,
        ebi_objects::process_tree::{Node, Operator},
    };
    use std::collections::{HashMap, HashSet};

    #[derive(Debug)]
    pub(super) enum Cut {
        ExclusiveChoice(Vec<HashSet<Activity>>),
        Sequence(Vec<HashSet<Activity>>),
        Concurrent(Vec<HashSet<Activity>>),
        Loop {
            body: HashSet<Activity>,
            redos: Vec<HashSet<Activity>>,
        },
    }

    impl Cut {
        pub(super) fn build_tree(&self, subtrees: Vec<Vec<Node>>) -> Vec<Node> {
            match self {
                Cut::ExclusiveChoice(_) => build_operator_node(Operator::Xor, subtrees),
                Cut::Sequence(_) => build_operator_node(Operator::Sequence, subtrees),
                Cut::Concurrent(_) => build_operator_node(Operator::Concurrent, subtrees),
                Cut::Loop { .. } => build_operator_node(Operator::Loop, subtrees),
            }
        }
    }

    pub(super) fn find_default_cut(info: &LogInfo) -> Option<Cut> {
        xor_cut(info)
            .or_else(|| sequence_cut(info))
            .or_else(|| concurrent_cut(info))
            .or_else(|| loop_cut(info))
    }

    pub(super) fn xor_cut(info: &LogInfo) -> Option<Cut> {
        let mut components = Components::new(info.activities.iter().cloned().collect());
        for (src, tgt) in info.dfg.get_sources().zip(info.dfg.get_targets()) {
            components.merge_components(src, tgt);
        }
        let parts = components.into_components();
        if parts.len() > 1 {
            Some(Cut::ExclusiveChoice(parts))
        } else {
            None
        }
    }

    pub(super) fn sequence_cut(info: &LogInfo) -> Option<Cut> {
        let activities: Vec<Activity> = info.activities.iter().cloned().collect();
        let n = activities.len();
        if n == 0 {
            return None;
        }

        let act_to_idx: HashMap<Activity, usize> = activities
            .iter()
            .enumerate()
            .map(|(i, &a)| (a, i))
            .collect();

        let mut adj: Vec<Vec<usize>> = vec![vec![]; n];
        for (source, (target, _)) in info.dfg.edges() {
            if let (Some(&u), Some(&v)) = (act_to_idx.get(&source), act_to_idx.get(&target)) {
                if !adj[u].contains(&v) {
                    adj[u].push(v);
                }
            }
        }

        let mut reachable: Vec<HashSet<usize>> = vec![HashSet::new(); n];
        for start in 0..n {
            let mut stack = vec![start];
            while let Some(v) = stack.pop() {
                for &w in &adj[v] {
                    if reachable[start].insert(w) {
                        stack.push(w);
                    }
                }
            }
        }

        //merge pairwise reachable and pairwise unreachable pairs
        let mut components = Components::new(info.activities.iter().cloned().collect());
        for i in 0..n {
            for j in (i + 1)..n {
                let (a, b) = (activities[i], activities[j]);
                let a_to_b = reachable[i].contains(&j);
                let b_to_a = reachable[j].contains(&i);
                if (a_to_b && b_to_a) || (!a_to_b && !b_to_a) {
                    components.merge_components(a, b);
                }
            }
        }

        let mut parts = components.into_components();
        if parts.len() <= 1 {
            return None;
        }

        parts.sort_by(|pi, pj| {
            let i_to_j = pi.iter().any(|&a| {
                let ai = act_to_idx[&a];
                pj.iter().any(|&b| reachable[ai].contains(&act_to_idx[&b]))
            });
            let j_to_i = pj.iter().any(|&a| {
                let ai = act_to_idx[&a];
                pi.iter().any(|&b| reachable[ai].contains(&act_to_idx[&b]))
            });
            match (i_to_j, j_to_i) {
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
                _ => std::cmp::Ordering::Equal, // but not possible cause pairwise reach/unreachable is merged before
            }
        });

        Some(Cut::Sequence(parts))
    }

    pub(super) fn concurrent_cut(info: &LogInfo) -> Option<Cut> {
        if info.activities.len() < 2 {
            return None;
        }
        if info.dfg.start_activities().next().is_some()
            || info.dfg.end_activities().next().is_some()
        {
            return None;
        }

        let activities: Vec<Activity> = info.activities.iter().cloned().collect();
        let n = activities.len();
        let mut components = Components::new(info.activities.iter().cloned().collect());

        //merge pairs that aren't fully connected in both directions
        for i in 0..n {
            for j in (i + 1)..n {
                let (a, b) = (activities[i], activities[j]);
                if !components.same_component(a, b) {
                    let a_to_b = info.dfg.edge_weight_activities(a, b).is_positive();
                    let b_to_a = info.dfg.edge_weight_activities(b, a).is_positive();
                    if !(a_to_b && b_to_a) {
                        components.merge_components(a, b);
                    }
                }
            }
        }

        //Apply an extension to the IM algorithm to account for loops that have the same directly follows graph as a parallel operator would have
        //make sure that activities on the minimum-self-distance-path are not separated by a parallel operator.
        {
            for activity in &info.activities {
                for (activity2, _) in info.msd.get(*activity).unwrap() {
                    components.merge_components(*activity, activity2);
                }
            }
        }

        let raw = components.build_components();
        let mut with_start_end: Vec<HashSet<Activity>> = vec![];
        let mut with_start: Vec<HashSet<Activity>> = vec![];
        let mut with_end: Vec<HashSet<Activity>> = vec![];
        let mut with_nothing: Vec<HashSet<Activity>> = vec![];

        for part in raw.into_values() {
            let has_start = part
                .iter()
                .any(|a| info.dfg.start_activity_weight(*a).is_positive());
            let has_end = part
                .iter()
                .any(|a| info.dfg.end_activity_weight(*a).is_positive());
            match (has_start, has_end) {
                (true, true) => with_start_end.push(part),
                (true, false) => with_start.push(part),
                (false, true) => with_end.push(part),
                (false, false) => with_nothing.push(part),
            }
        }
        if with_start_end.is_empty() {
            return None;
        }

        let mut result = with_start_end;
        let pairs = with_start.len().min(with_end.len());
        for i in 0..pairs {
            let mut merged = with_start[i].clone();
            merged.extend(with_end[i].iter());
            result.push(merged);
        }
        for part in &with_start[pairs..] {
            result[0].extend(part.iter());
        }
        for part in &with_end[pairs..] {
            result[0].extend(part.iter());
        }
        for part in with_nothing {
            result[0].extend(part.iter());
        }

        if result.len() > 1 {
            Some(Cut::Concurrent(result))
        } else {
            None
        }
    }

    pub(super) fn loop_cut(info: &LogInfo) -> Option<Cut> {
        if info.activities.len() < 2 {
            return None;
        }
        if info.dfg.start_activities().next().is_some()
            || info.dfg.end_activities().next().is_some()
        {
            return None;
        }

        let mut components = Components::new(info.activities.iter().cloned().collect());

        //merge all start + end activities
        let mut it = info.dfg.start_activities();
        let pivot = it.next().unwrap();
        for a in it.chain(info.dfg.end_activities()) {
            components.merge_components(pivot, a);
        }

        for (src, tgt) in info.dfg.get_sources().zip(info.dfg.get_targets()) {
            let src_is_start = info.dfg.is_start_activity(src);
            let src_is_end = info.dfg.is_end_activity(src);
            let tgt_is_start = info.dfg.is_start_activity(tgt);
            if !src_is_start && !src_is_end && !tgt_is_start {
                components.merge_components(src, tgt);
            } else if src_is_start && !src_is_end {
                components.merge_components(src, tgt);
            }
        }

        let (sub_ends, sub_starts): (HashSet<Activity>, HashSet<Activity>) = info
            .dfg
            .get_sources()
            .zip(info.dfg.get_targets())
            .filter(|(src, tgt)| !components.same_component(*src, *tgt))
            .fold(
                (HashSet::new(), HashSet::new()),
                |(mut ends, mut starts), (src, tgt)| {
                    ends.insert(src);
                    starts.insert(tgt);
                    (ends, starts)
                },
            );

        // sub-end of redo connects to all start acts
        for &sub_end in &sub_ends {
            if components.same_component(sub_end, pivot) {
                continue;
            }
            if info.dfg.start_activities().any(|s| {
                info.dfg
                    .edge_weight_activities(sub_end, s)
                    .is_not_positive()
            }) {
                components.merge_components(sub_end, pivot);
            }
        }
        // sub-tart of redo reachable from all end acts
        for &sub_start in &sub_starts {
            if components.same_component(sub_start, pivot) {
                continue;
            }
            if info.dfg.end_activities().any(|e| {
                info.dfg
                    .edge_weight_activities(e, sub_start)
                    .is_not_positive()
            }) {
                components.merge_components(sub_start, pivot);
            }
        }

        let mut parts = components.into_components();
        if parts.len() <= 1 {
            return None;
        }
        let body_idx = parts.iter().position(|p| p.contains(&pivot))?;
        parts.swap(0, body_idx);
        let body = parts.remove(0);

        Some(Cut::Loop { body, redos: parts })
    }
}

mod find_fall_throughs {
    use crate::{
        ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        techniques::inductive_miner::{
            InductiveMinerParameters, build_operator_node, debug, find_default_cut,
            inductive_miner_recursion,
            log_info::{ComputeLogInfo, LogInfo},
            log_splitting::split_log_concurrent,
        },
    };
    use ebi_objects::{
        Activity, FiniteStochasticLanguage, IntoRefTraceIterator,
        ebi_arithmetic::Signed,
        ebi_objects::process_tree::{Node, Operator},
    };
    use std::collections::HashSet;

    pub(super) fn empty_traces(
        log: &dyn EbiTraitFiniteStochasticLanguage,
        info: &LogInfo,
        parameters: &InductiveMinerParameters,
        recursion_depth: usize,
    ) -> Option<Vec<Node>> {
        if !info.dfg.empty_traces_weight.is_positive() {
            return None;
        }

        debug("empty traces", recursion_depth);

        let mut filtered_log =
            FiniteStochasticLanguage::new_with_activity_key(log.activity_key().clone());
        for (trace, probability) in log.iter_traces_probabilities() {
            if !trace.is_empty() {
                filtered_log.push_raw(trace.to_vec(), probability).unwrap();
            }
        }

        if filtered_log.iter_traces().next().is_none() {
            return Some(vec![Node::Tau]);
        }

        let subtree = inductive_miner_recursion(&filtered_log, parameters, recursion_depth + 1);
        let tau_leaf = vec![Node::Tau];
        Some(build_operator_node(Operator::Xor, vec![tau_leaf, subtree]))
    }

    pub(super) fn activity_once_per_trace(
        log: &dyn EbiTraitFiniteStochasticLanguage,
        info: &LogInfo,
        parameters: &InductiveMinerParameters,
        recursion_depth: usize,
    ) -> Option<Vec<Node>> {
        let activity = *info.activities.iter().find(|&&a| {
            log.iter_traces()
                .all(|trace| trace.iter().filter(|&&x| x == a).count() == 1)
        })?;

        debug(
            format!("activity one per trace {}", activity),
            recursion_depth,
        );

        let mut filtered_log =
            FiniteStochasticLanguage::new_with_activity_key(log.activity_key().clone());
        for (trace, probability) in log.iter_traces_probabilities() {
            filtered_log
                .push_raw(
                    trace.iter().filter(|&&x| x != activity).cloned().collect(),
                    probability,
                )
                .unwrap();
        }

        let subtree = inductive_miner_recursion(&filtered_log, parameters, recursion_depth + 1);
        let a_leaf = vec![Node::Activity(activity)];
        Some(build_operator_node(
            Operator::Concurrent,
            vec![a_leaf, subtree],
        ))
    }

    pub(super) fn activity_concurrent(
        log: &dyn EbiTraitFiniteStochasticLanguage,
        info: &LogInfo,
        parameters: &InductiveMinerParameters,
        recursion_depth: usize,
    ) -> Option<Vec<Node>> {
        if info.activities.len() < 3 {
            return None;
        }

        for &activity in &info.activities {
            let mut parts_1 = HashSet::new();
            parts_1.insert(activity);
            let mut parts_2 = info.activities.clone();
            parts_2.remove(&activity);
            let sub_logs = split_log_concurrent(log, &[parts_1, parts_2]);
            let sub_log_1 = &sub_logs[0];
            let sub_log_2 = &sub_logs[1];

            //perform cut dection on empty traces
            let mut check_log = sub_log_2.clone();
            check_log.traces.retain(|trace, _| !trace.is_empty());
            check_log.normalise();

            if let Some(cut) = find_default_cut(&(check_log.compute_log_info())) {
                debug(
                    format!("activity concurrent {} with sub-cut {:?}", activity, cut),
                    recursion_depth,
                );
                let subtree_1 =
                    inductive_miner_recursion(sub_log_1, parameters, recursion_depth + 1);
                let subtree_2 =
                    inductive_miner_recursion(sub_log_2, parameters, recursion_depth + 1);
                return Some(build_operator_node(
                    Operator::Concurrent,
                    vec![subtree_1, subtree_2],
                ));
            }
        }

        None
    }

    pub(super) fn strict_tau_loop(
        log: &dyn EbiTraitFiniteStochasticLanguage,
        info: &LogInfo,
        parameters: &InductiveMinerParameters,
        recursion_depth: usize,
    ) -> Option<Vec<Node>> {
        if info.activities.len() <= 1 {
            return None;
        }

        let mut new_log =
            FiniteStochasticLanguage::new_with_activity_key(log.activity_key().clone());
        let mut any_split = false;

        for (trace, probability) in log.iter_traces_probabilities() {
            let mut current: Vec<Activity> = Vec::new();
            for &a in trace {
                if let Some(&last) = current.last() {
                    if info.dfg.is_end_activity(last) && info.dfg.is_start_activity(a) {
                        new_log
                            .push_raw(std::mem::take(&mut current), probability)
                            .unwrap();
                        any_split = true;
                    }
                }
                current.push(a);
            }
            new_log.push_raw(current, probability).unwrap();
        }

        if !any_split {
            return None;
        }

        debug("strict tau loop", recursion_depth);

        let subtree = inductive_miner_recursion(&new_log, parameters, recursion_depth + 1);
        let tau_leaf = vec![Node::Tau];
        Some(build_operator_node(Operator::Loop, vec![subtree, tau_leaf]))
    }

    pub(super) fn tau_loop(
        log: &dyn EbiTraitFiniteStochasticLanguage,
        info: &LogInfo,
        parameters: &InductiveMinerParameters,
        recursion_depth: usize,
    ) -> Option<Vec<Node>> {
        if info.activities.len() <= 1 {
            return None;
        }

        let mut new_log =
            FiniteStochasticLanguage::new_with_activity_key(log.activity_key().clone());
        let mut any_split = false;

        for (trace, probability) in log.iter_traces_probabilities() {
            let mut current: Vec<Activity> = Vec::new();
            for &a in trace {
                if info.dfg.is_start_activity(a) && !current.is_empty() {
                    new_log
                        .push_raw(std::mem::take(&mut current), probability)
                        .unwrap();
                    any_split = true;
                }
                current.push(a);
            }
            new_log.push_raw(current, probability).unwrap();
        }

        if !any_split {
            return None;
        }

        new_log.normalise();

        debug("tau loop", recursion_depth);

        let subtree = inductive_miner_recursion(&new_log, parameters, recursion_depth + 1);
        let tau_leaf = vec![Node::Tau];
        Some(build_operator_node(Operator::Loop, vec![subtree, tau_leaf]))
    }

    pub(super) fn flower_model(
        _log: &dyn EbiTraitFiniteStochasticLanguage,
        info: &LogInfo,
        recursion_depth: usize,
    ) -> Vec<Node> {
        if info.activities.len() == 1 {
            let a = *info.activities.iter().next().unwrap();
            let a_leaf = vec![Node::Activity(a)];
            let tau_leaf = vec![Node::Tau];
            return build_operator_node(Operator::Loop, vec![a_leaf, tau_leaf]);
        }

        let activity_trees: Vec<Vec<Node>> = info
            .activities
            .iter()
            .map(|&a| vec![Node::Activity(a)])
            .collect();
        let xor_body = build_operator_node(Operator::Xor, activity_trees);
        let tau_leaf = vec![Node::Tau];
        debug("flower model", recursion_depth);
        build_operator_node(Operator::Loop, vec![xor_body, tau_leaf])
    }
}

mod log_info {
    use crate::{
        ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        techniques::directly_follows_graph_abstractor::DirectlyFollowsAbstractor,
    };
    use ebi_objects::{
        Activity, DirectlyFollowsGraph, FiniteStochasticLanguage, HasActivityKey,
        IntoRefTraceIterator, IntoRefTraceProbabilityIterator, ebi_arithmetic::Fraction,
        ebi_arithmetic::Zero,
    };
    use intmap::IntMap;
    use std::collections::HashSet;

    #[derive(Clone)]
    pub struct LogInfo {
        pub dfg: DirectlyFollowsGraph,
        pub msd: IntMap<Activity, IntMap<Activity, usize>>,
        pub activities: HashSet<Activity>,
        pub total_traces: Fraction,
        pub activity_instances: Fraction,
    }

    pub trait ComputeLogInfo {
        fn compute_log_info(&self) -> LogInfo;
    }

    macro_rules! compute_log_info_imf {
        ($t:ty) => {
            impl ComputeLogInfo for $t {
                fn compute_log_info(&self) -> LogInfo {
                    let dfg = self.abstract_to_directly_follows_graph();
                    let mut activities = HashSet::new();
                    let mut total_traces = Fraction::zero();
                    let mut activity_instances = Fraction::zero();

                    //basic data
                    for (trace, probability) in self.iter_traces_probabilities() {
                        total_traces += probability;
                        for &activity in trace {
                            activities.insert(activity);
                            activity_instances += probability;
                        }
                    }

                    //msd
                    let mut minimum_self_distances_between: IntMap<
                        Activity,
                        IntMap<Activity, usize>,
                    > = IntMap::new();
                    {
                        let mut minimum_self_distances = IntMap::new();

                        //walk trough the log
                        for trace in self.iter_traces() {
                            let mut trace_size = 0;
                            let mut event_seen_at = IntMap::new();
                            let mut read_trace = vec![];

                            for to_event_class in trace {
                                read_trace.push(to_event_class);

                                if (event_seen_at.contains_key(*to_event_class)) {
                                    //we have detected an activity for the second time
                                    //check whether this is shorter than what we had already seen
                                    let old_distance = minimum_self_distances
                                        .get(*to_event_class)
                                        .unwrap_or(&usize::MAX);

                                    if !minimum_self_distances.contains_key(*to_event_class)
                                        || trace_size
                                            - event_seen_at.get(*to_event_class).unwrap_or(&0)
                                            <= *old_distance
                                    {
                                        //keep the new minimum self distance
                                        let new_distance = trace_size
                                            - event_seen_at.get(*to_event_class).unwrap_or(&0);
                                        if *old_distance > new_distance {
                                            //we found a shorter minimum self distance, record and restart with a new multiset
                                            minimum_self_distances
                                                .insert(*to_event_class, new_distance);

                                            minimum_self_distances_between
                                                .insert(*to_event_class, IntMap::new());
                                        }

                                        //store the minimum self-distance activities
                                        let mb: &mut IntMap<Activity, usize> =
                                            minimum_self_distances_between
                                                .get_mut(*to_event_class)
                                                .unwrap();
                                        for x in &read_trace[(event_seen_at
                                            .get(*to_event_class)
                                            .unwrap_or(&0)
                                            + 1)
                                            ..trace_size]
                                        {
                                            *mb.entry(**x).or_insert(0) += 1;
                                        }
                                    }
                                }
                                event_seen_at.insert(*to_event_class, trace_size);

                                trace_size += 1;
                            }
                        }
                    }

                    LogInfo {
                        dfg,
                        msd: minimum_self_distances_between,
                        activities,
                        total_traces,
                        activity_instances,
                    }
                }
            }
        };
    }
    compute_log_info_imf!(dyn EbiTraitFiniteStochasticLanguage + '_);
    compute_log_info_imf!(FiniteStochasticLanguage);
}

pub fn filter_log_info(
    info: &LogInfo,
    parameters: &InductiveMinerParameters,
    recursion_depth: usize,
) -> LogInfo {
    debug("filter log info", recursion_depth);
    let mut result = info.clone();

    //filter start activities
    {
        let sum: Fraction = result
            .dfg
            .start_states
            .iter()
            .map(|(_, weight)| weight)
            .sum();
        let threshold = &parameters.noise_filtering * &sum;
        result
            .dfg
            .start_states
            .retain(|_, weight| weight >= &threshold);
    }

    //filter edges
    {
        let mut activity_2_threshold = IntMap::new();
        for activity in &result.activities {
            activity_2_threshold.insert(
                *activity,
                &parameters.noise_filtering
                    * &(result.dfg.end_activity_weight(*activity)
                        + result
                            .dfg
                            .outgoing_edges(*activity)
                            .into_iter()
                            .map(|(_, weight)| weight)
                            .sum::<Fraction>()),
            );
        }

        //remove edges
        for (source, (_, weight)) in result.dfg.edges_mut() {
            if &*weight < activity_2_threshold.get(source).unwrap() {
                *weight = Fraction::zero();
            }
        }

        //remove end activities
        for activity in &result.activities {
            if &result.dfg.end_activity_weight(*activity)
                < activity_2_threshold.get(*activity).unwrap()
            {
                result.dfg.remove_end_activity(*activity);
            }
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use crate::techniques::inductive_miner::{InductiveMiner, InductiveMinerInfrequent};
    use ebi_objects::{
        ActivityKey, ActivityKeyTranslator, EventLog, FiniteLanguage, FiniteStochasticLanguage,
        ProcessTree, activity, con,
        ebi_arithmetic::Fraction,
        ebi_arithmetic::f,
        ebi_objects::process_tree::{Node, Operator},
        event_log, or, seq, tau, trace, xor,
    };

    #[test]
    fn im_l67() {
        let log = event_log!(
            trace!("a", "b", "c", "d", "e"),
            trace!("a", "d", "b", "e"),
            trace!("a", "e", "b"),
            trace!("a", "c", "b"),
            trace!("a", "b", "d", "e", "c")
        );

        let lang = FiniteLanguage::from(log);
        let tree = lang.inductive_miner();

        let should_tree = seq!(
            activity!("a"),
            con!(
                activity!("b"),
                xor!(
                    tau!(),
                    or!(
                        activity!("c"),
                        seq!(xor!(tau!(), activity!("d")), activity!("e"))
                    )
                )
            )
        );

        println!("{}", tree);

        assert_eq!(tree, should_tree);
    }

    #[test]
    fn im_l71() {
        let log = event_log!(
            trace!("c", "d", "e"),
            trace!("d", "e"),
            trace!("e"),
            trace!("c"),
            trace!("d", "e", "c")
        );

        let lang = FiniteLanguage::from(log);
        let tree = lang.inductive_miner();

        let should_tree = xor!(
            tau!(),
            or!(
                activity!("c"),
                seq!(xor!(tau!(), activity!("d")), activity!("e"))
            )
        );

        println!("{}", tree.activity_key);
        println!("result {}", tree);

        assert_eq!(tree, should_tree);
    }

    #[test]
    /// Differs from the example in the thesis: there are actually two optimal splits for the noisy trace in the sequence cut.
    fn imf_l92() {
        let log = event_log!(
            trace!("a", "b", "c", "d", "e"),
            trace!("a", "b", "c", "d", "e"),
            trace!("a", "b", "c", "d", "e"),
            trace!("a", "b", "c", "d", "e"),
            trace!("a", "b", "c", "d", "e"),
            trace!("a", "b", "c", "d", "e"),
            trace!("a", "b", "c", "d", "e"),
            trace!("a", "b", "c", "d", "e"),
            trace!("a", "b", "c", "d", "e"),
            trace!("a", "b", "c", "d", "e"),
            trace!("a", "d", "b", "e"),
            trace!("a", "d", "b", "e"),
            trace!("a", "d", "b", "e"),
            trace!("a", "d", "b", "e"),
            trace!("a", "d", "b", "e"),
            trace!("a", "d", "b", "e"),
            trace!("a", "d", "b", "e"),
            trace!("a", "d", "b", "e"),
            trace!("a", "d", "b", "e"),
            trace!("a", "d", "b", "e"),
            trace!("a", "e", "b"),
            trace!("a", "e", "b"),
            trace!("a", "e", "b"),
            trace!("a", "e", "b"),
            trace!("a", "e", "b"),
            trace!("a", "e", "b"),
            trace!("a", "e", "b"),
            trace!("a", "e", "b"),
            trace!("a", "e", "b"),
            trace!("a", "e", "b"),
            trace!("a", "c", "b"),
            trace!("a", "c", "b"),
            trace!("a", "c", "b"),
            trace!("a", "c", "b"),
            trace!("a", "c", "b"),
            trace!("a", "c", "b"),
            trace!("a", "c", "b"),
            trace!("a", "c", "b"),
            trace!("a", "c", "b"),
            trace!("a", "c", "b"),
            trace!("a", "b", "d", "e", "c"),
            trace!("a", "b", "d", "e", "c"),
            trace!("a", "b", "d", "e", "c"),
            trace!("a", "b", "d", "e", "c"),
            trace!("a", "b", "d", "e", "c"),
            trace!("a", "b", "d", "e", "c"),
            trace!("a", "b", "d", "e", "c"),
            trace!("a", "b", "d", "e", "c"),
            trace!("a", "b", "d", "e", "c"),
            trace!("a", "b", "d", "e", "c"),
            trace!("c", "a", "b")
        );

        let slang = FiniteStochasticLanguage::from(log);

        let tree = slang.inductive_miner_infrequent(&f!(15, 100));

        let should_tree = seq!(
            activity!("a"),
            con!(
                activity!("b"),
                activity!("c"),
                xor!(tau!(), seq!(activity!("d"), activity!("e")))
            )
        );

        println!("{}", tree);

        assert_eq!(tree, should_tree);
    }
}
