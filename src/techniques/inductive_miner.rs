use std::collections::{HashMap, HashSet};

use ebi_objects::{
    Activity, ActivityKey, DirectlyFollowsGraph, FiniteLanguage, IntoRefTraceIterator, ProcessTree,
    ebi_arithmetic::{Fraction, One},
    ebi_objects::process_tree::{Node, Operator},
};

use crate::ebi_traits::ebi_trait_finite_language::EbiTraitFiniteLanguage;

pub trait InductiveMinerTree {
    fn inductive_miner(&self) -> ProcessTree;
}

impl InductiveMinerTree for dyn EbiTraitFiniteLanguage {
    fn inductive_miner(&self) -> ProcessTree {
        inductive_miner(self)
    }
}

pub struct LogInfo {
    pub dfg: DirectlyFollowsGraph,
    pub start_activities: HashSet<Activity>,
    pub end_activities: HashSet<Activity>,
    pub has_empty_traces: bool,
    pub activities: HashSet<Activity>,
    pub total_traces: usize,
    pub activity_instances: usize,
}

/// Pre-computed log stats, DFG construction mirrors directly_follows_graph_abstractor.
impl LogInfo {
    pub fn log_calcs(log: &dyn EbiTraitFiniteLanguage) -> Self {
        let mut dfg = DirectlyFollowsGraph::new(log.activity_key().clone());
        let mut start_activities = HashSet::new();
        let mut end_activities = HashSet::new();
        let mut activities = HashSet::new();
        let mut has_empty_traces = false;
        let mut total_traces = 0;
        let mut activity_instances = 0;

        for trace in log.iter_traces() {
            total_traces += 1;
            let mut last_activity = None;
            for &activity in trace {
                activities.insert(activity);
                activity_instances += 1;
                match last_activity {
                    Some(prev) => dfg.add_edge(prev, activity, &Fraction::one()),
                    None => {
                        dfg.add_start_activity(activity, &Fraction::one());
                        start_activities.insert(activity);
                    }
                }
                last_activity = Some(activity);
            }
            match last_activity {
                Some(a) => {
                    dfg.add_end_activity(a, &Fraction::one());
                    end_activities.insert(a);
                }
                None => {
                    dfg.add_empty_trace(&Fraction::one());
                    has_empty_traces = true;
                }
            }
        }

        LogInfo {
            dfg,
            start_activities,
            end_activities,
            has_empty_traces,
            activities,
            total_traces,
            activity_instances,
        }
    }
}

/// Partitioning activities
struct Components {
    activities: Vec<Activity>,
    groups: Vec<usize>,
}

impl Components {
    fn new(activities: &HashSet<Activity>) -> Self {
        let activities: Vec<Activity> = activities.iter().cloned().collect();
        let n = activities.len();
        Self {
            activities,
            groups: (0..n).collect(),
        }
    }

    fn merge_components(&mut self, a: Activity, b: Activity) {
        let g_a = self
            .activities
            .iter()
            .position(|&x| x == a)
            .map(|i| self.groups[i]);
        let g_b = self
            .activities
            .iter()
            .position(|&x| x == b)
            .map(|i| self.groups[i]);
        if let (Some(group_a), Some(group_b)) = (g_a, g_b) {
            if group_a != group_b {
                for g in self.groups.iter_mut() {
                    if *g == group_a {
                        *g = group_b;
                    }
                }
            }
        }
    }

    fn build_components(&self) -> HashMap<usize, HashSet<Activity>> {
        let mut map: HashMap<usize, HashSet<Activity>> = HashMap::new();
        for (index, &activity) in self.activities.iter().enumerate() {
            map.entry(self.groups[index]).or_default().insert(activity);
        }
        map
    }

    fn into_components(self) -> Vec<HashSet<Activity>> {
        self.build_components().into_values().collect()
    }

    fn same_component(&self, a: Activity, b: Activity) -> bool {
        let ga = self
            .activities
            .iter()
            .position(|&x| x == a)
            .map(|i| self.groups[i]);
        let gb = self
            .activities
            .iter()
            .position(|&x| x == b)
            .map(|i| self.groups[i]);

        ga.is_some() && ga == gb
    }
}

enum Cut {
    ExclusiveChoice(Vec<HashSet<Activity>>),
    Sequence(Vec<HashSet<Activity>>),
    Concurrent(Vec<HashSet<Activity>>),
    Loop {
        body: HashSet<Activity>,
        redos: Vec<HashSet<Activity>>,
    },
}

impl Cut {
    fn split_log(&self, log: &dyn EbiTraitFiniteLanguage) -> Vec<Box<dyn EbiTraitFiniteLanguage>> {
        match self {
            Cut::ExclusiveChoice(parts) => xor_split(log, parts),
            Cut::Sequence(parts) => seq_conc_split(log, parts),
            Cut::Concurrent(parts) => seq_conc_split(log, parts),
            Cut::Loop { body, redos } => loop_split(log, body, redos),
        }
    }

    fn build_tree(&self, subtrees: Vec<ProcessTree>, activity_key: ActivityKey) -> ProcessTree {
        match self {
            Cut::ExclusiveChoice(_) => build_operator_node(Operator::Xor, subtrees, activity_key),
            Cut::Sequence(_) => build_operator_node(Operator::Sequence, subtrees, activity_key),
            Cut::Concurrent(_) => build_operator_node(Operator::Concurrent, subtrees, activity_key),
            Cut::Loop { .. } => build_operator_node(Operator::Loop, subtrees, activity_key),
        }
    }
}

fn inductive_miner(log: &dyn EbiTraitFiniteLanguage) -> ProcessTree {
    let info = LogInfo::log_calcs(log);

    if let Some(tree) = base_case(log, &info) {
        return tree;
    }

    if let Some(cut) = find_cut(&info) {
        let sublogs = cut.split_log(log);
        let subtrees = sublogs.iter().map(|s| inductive_miner(s.as_ref())).collect();
        return cut.build_tree(subtrees, log.activity_key().clone());
    }

    fall_throughs(log)
}

fn base_case(log: &dyn EbiTraitFiniteLanguage, info: &LogInfo) -> Option<ProcessTree> {
    let activity_key = log.activity_key().clone();
    //emptyLog
    if info.total_traces == 0 {
        return Some((activity_key, vec![]).into());
    }
    //singleActivity: no empty traces, every trace has exactly one event
    if info.activities.len() == 1
        && !info.has_empty_traces
        && info.activity_instances == info.total_traces
    {
        let only_activity = *info.activities.iter().next().unwrap();
        return Some((activity_key, vec![Node::Activity(only_activity)]).into());
    }

    None
}

fn find_cut(info: &LogInfo) -> Option<Cut> {
    if info.has_empty_traces {
        return None;
    }

    xor_cut(info)
        .or_else(|| sequence_cut(info))
        .or_else(|| concurrent_cut(info))
        .or_else(|| loop_cut(info))
}

fn xor_cut(info: &LogInfo) -> Option<Cut> {
    let mut components = Components::new(&info.activities);
    for (src, tgt) in info.dfg.sources.iter().zip(info.dfg.targets.iter()) {
        components.merge_components(*src, *tgt);
    }
    let parts = components.into_components();
    if parts.len() > 1 {
        Some(Cut::ExclusiveChoice(parts))
    } else {
        None
    }
}

fn sequence_cut(info: &LogInfo) -> Option<Cut> {
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
    for (&src, &tgt) in info.dfg.sources.iter().zip(info.dfg.targets.iter()) {
        if let (Some(&u), Some(&v)) = (act_to_idx.get(&src), act_to_idx.get(&tgt)) {
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
    let mut components = Components::new(&info.activities);
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

fn concurrent_cut(info: &LogInfo) -> Option<Cut> {
    if info.activities.len() < 2 {
        return None;
    }
    if info.start_activities.is_empty() || info.end_activities.is_empty() {
        return None;
    }

    let activities: Vec<Activity> = info.activities.iter().cloned().collect();
    let n = activities.len();
    let mut components = Components::new(&info.activities);

    //merge pairs that aren't fully connected in both directions
    for i in 0..n {
        for j in (i + 1)..n {
            let (a, b) = (activities[i], activities[j]);
            if !components.same_component(a, b) {
                let a_to_b = info.dfg.edge_weight(a, b).is_some();
                let b_to_a = info.dfg.edge_weight(b, a).is_some();
                if !(a_to_b && b_to_a) {
                    components.merge_components(a, b);
                }
            }
        }
    }

    let raw = components.build_components();
    let mut with_start_end: Vec<HashSet<Activity>> = vec![];
    let mut with_start: Vec<HashSet<Activity>> = vec![];
    let mut with_end: Vec<HashSet<Activity>> = vec![];
    let mut with_nothing: Vec<HashSet<Activity>> = vec![];

    for part in raw.into_values() {
        let has_start = part.iter().any(|a| info.start_activities.contains(a));
        let has_end = part.iter().any(|a| info.end_activities.contains(a));
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

fn loop_cut(info: &LogInfo) -> Option<Cut> {
    if info.activities.len() < 2 {
        return None;
    }
    if info.start_activities.is_empty() || info.end_activities.is_empty() {
        return None;
    }

    let mut components = Components::new(&info.activities);

    //merge all start + end activities
    let pivot = *info.start_activities.iter().next().unwrap();
    for &a in info.start_activities.iter().chain(&info.end_activities) {
        components.merge_components(pivot, a);
    }

    for (&src, &tgt) in info.dfg.sources.iter().zip(&info.dfg.targets) {
        let src_is_start = info.start_activities.contains(&src);
        let src_is_end = info.end_activities.contains(&src);
        let tgt_is_start = info.start_activities.contains(&tgt);
        if !src_is_start && !src_is_end && !tgt_is_start {
            components.merge_components(src, tgt);
        } else if src_is_start && !src_is_end {
            components.merge_components(src, tgt);
        }
    }

    let (sub_ends, sub_starts): (HashSet<Activity>, HashSet<Activity>) = info
        .dfg
        .sources
        .iter()
        .zip(&info.dfg.targets)
        .filter(|&(&src, &tgt)| !components.same_component(src, tgt))
        .fold(
            (HashSet::new(), HashSet::new()),
            |(mut ends, mut starts), (&src, &tgt)| {
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
        if info
            .start_activities
            .iter()
            .any(|&s| info.dfg.edge_weight(sub_end, s).is_none())
        {
            components.merge_components(sub_end, pivot);
        }
    }
    // sub-tart of redo reachable from all end acts
    for &sub_start in &sub_starts {
        if components.same_component(sub_start, pivot) {
            continue;
        }
        if info
            .end_activities
            .iter()
            .any(|&e| info.dfg.edge_weight(e, sub_start).is_none())
        {
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

fn new_sublog(log: &dyn EbiTraitFiniteLanguage) -> FiniteLanguage {
    FiniteLanguage {
        activity_key: log.activity_key().clone(),
        traces: FiniteLanguage::new_hashmap(),
    }
}

fn xor_split(
    log: &dyn EbiTraitFiniteLanguage,
    parts: &[HashSet<Activity>],
) -> Vec<Box<dyn EbiTraitFiniteLanguage>> {
    let mut sublogs: Vec<FiniteLanguage> = parts.iter().map(|_| new_sublog(log)).collect();

    'trace: for trace in log.iter_traces() {
        for (i, partition) in parts.iter().enumerate() {
            if trace.iter().any(|a| partition.contains(a)) {
                sublogs[i].push(trace.to_vec());
                continue 'trace;
            }
        }
    }

    sublogs
        .into_iter()
        .map(|s| Box::new(s) as Box<dyn EbiTraitFiniteLanguage>)
        .collect()
}

fn seq_conc_split(
    log: &dyn EbiTraitFiniteLanguage,
    parts: &[HashSet<Activity>],
) -> Vec<Box<dyn EbiTraitFiniteLanguage>> {
    let mut sublogs: Vec<FiniteLanguage> = parts.iter().map(|_| new_sublog(log)).collect();

    for trace in log.iter_traces() {
        for (i, partition) in parts.iter().enumerate() {
            let subtrace: Vec<Activity> = trace
                .iter()
                .filter(|&&a| partition.contains(&a))
                .cloned()
                .collect();
            sublogs[i].push(subtrace);
        }
    }

    sublogs
        .into_iter()
        .map(|s| Box::new(s) as Box<dyn EbiTraitFiniteLanguage>)
        .collect()
}

fn loop_split(
    log: &dyn EbiTraitFiniteLanguage,
    body: &HashSet<Activity>,
    redos: &[HashSet<Activity>],
) -> Vec<Box<dyn EbiTraitFiniteLanguage>> {
    let mut sublogs: Vec<FiniteLanguage> = (0..redos.len() + 1).map(|_| new_sublog(log)).collect();

    let find_part = |a: Activity| -> usize {
        if body.contains(&a) {
            return 0;
        }
        redos
            .iter()
            .position(|r| r.contains(&a))
            .map_or(0, |i| i + 1)
    };

    for trace in log.iter_traces() {
        let mut current: Vec<Activity> = Vec::new();
        let mut current_part = 0;
        for &a in trace {
            let part = find_part(a);
            if part != current_part {
                sublogs[current_part].push(std::mem::take(&mut current));
                current_part = part;
            }
            current.push(a);
        }
        sublogs[current_part].push(current);
    }

    sublogs
        .into_iter()
        .map(|s| Box::new(s) as Box<dyn EbiTraitFiniteLanguage>)
        .collect()
}

fn build_operator_node(
    operator: Operator,
    subtrees: Vec<ProcessTree>,
    activity_key: ActivityKey,
) -> ProcessTree {
    let n = subtrees.len();
    let mut nodes = vec![Node::Operator(operator, n)];

    for subtree in subtrees {
        nodes.extend(subtree.tree);
    }

    (activity_key, nodes).into()
}

fn fall_throughs(log: &dyn EbiTraitFiniteLanguage) -> ProcessTree {
    empty_traces(log)
        .or_else(|| activity_once_per_trace(log))
        .or_else(|| activity_concurrent(log))
        .or_else(|| strict_tau_loop(log))
        .or_else(|| tau_loop(log))
        .unwrap_or_else(|| flower_model(log))
}

fn empty_traces(log: &dyn EbiTraitFiniteLanguage) -> Option<ProcessTree> {
    let info = LogInfo::log_calcs(log);
    if !info.has_empty_traces {
        return None;
    }

    let mut filtered_log = new_sublog(log);
    for trace in log.iter_traces() {
        if !trace.is_empty() {
            filtered_log.push(trace.to_vec());
        }
    }

    let activity_key = log.activity_key().clone();

    if LogInfo::log_calcs(&filtered_log).total_traces == 0 {
        return Some((activity_key, vec![Node::Tau]).into());
    }

    let subtree = inductive_miner(&filtered_log);
    let tau_leaf: ProcessTree = (activity_key.clone(), vec![Node::Tau]).into();
    Some(build_operator_node(
        Operator::Xor,
        vec![tau_leaf, subtree],
        activity_key,
    ))
}

fn activity_once_per_trace(log: &dyn EbiTraitFiniteLanguage) -> Option<ProcessTree> {
    let info = LogInfo::log_calcs(log);

    let a = *info.activities.iter().find(|&&a| {
        log.iter_traces()
            .all(|trace| trace.iter().filter(|&&x| x == a).count() == 1)
    })?;

    let activity_key = log.activity_key().clone();
    let mut filtered_log = new_sublog(log);
    for trace in log.iter_traces() {
        filtered_log.push(trace.iter().filter(|&&x| x != a).cloned().collect());
    }

    let subtree = inductive_miner(&filtered_log);
    let a_leaf: ProcessTree = (activity_key.clone(), vec![Node::Activity(a)]).into();
    Some(build_operator_node(
        Operator::Concurrent,
        vec![a_leaf, subtree],
        activity_key,
    ))
}

fn activity_concurrent(log: &dyn EbiTraitFiniteLanguage) -> Option<ProcessTree> {
    let info = LogInfo::log_calcs(log);
    if info.activities.len() < 3 {
        return None;
    }

    for &a in &info.activities {
        let mut filtered_log = new_sublog(log);
        for trace in log.iter_traces() {
            filtered_log.push(trace.iter().filter(|&&x| x != a).cloned().collect());
        }

        let mut non_empty = new_sublog(log);
        for trace in filtered_log.iter_traces() {
            if !trace.is_empty() {
                non_empty.push(trace.to_vec());
            }
        }

        if find_cut(&LogInfo::log_calcs(&non_empty)).is_some() {
            let activity_key = log.activity_key().clone();
            let subtree = inductive_miner(&filtered_log);
            let a_leaf: ProcessTree = (activity_key.clone(), vec![Node::Activity(a)]).into();
            return Some(build_operator_node(
                Operator::Concurrent,
                vec![a_leaf, subtree],
                activity_key,
            ));
        }
    }

    None
}

fn strict_tau_loop(log: &dyn EbiTraitFiniteLanguage) -> Option<ProcessTree> {
    let info = LogInfo::log_calcs(log);
    if info.activities.len() <= 1 {
        return None;
    }

    let mut new_log = new_sublog(log);
    let mut any_split = false;

    for trace in log.iter_traces() {
        let mut current: Vec<Activity> = Vec::new();
        for &a in trace {
            if let Some(&last) = current.last() {
                if info.end_activities.contains(&last) && info.start_activities.contains(&a) {
                    new_log.push(std::mem::take(&mut current));
                    any_split = true;
                }
            }
            current.push(a);
        }
        new_log.push(current);
    }

    if !any_split {
        return None;
    }

    let activity_key = log.activity_key().clone();
    let subtree = inductive_miner(&new_log);
    let tau_leaf: ProcessTree = (activity_key.clone(), vec![Node::Tau]).into();
    Some(build_operator_node(
        Operator::Loop,
        vec![subtree, tau_leaf],
        activity_key,
    ))
}

fn tau_loop(log: &dyn EbiTraitFiniteLanguage) -> Option<ProcessTree> {
    let info = LogInfo::log_calcs(log);
    if info.activities.len() <= 1 {
        return None;
    }

    let mut new_log = new_sublog(log);
    let mut any_split = false;

    for trace in log.iter_traces() {
        let mut current: Vec<Activity> = Vec::new();
        for &a in trace {
            if info.start_activities.contains(&a) && !current.is_empty() {
                new_log.push(std::mem::take(&mut current));
                any_split = true;
            }
            current.push(a);
        }
        new_log.push(current);
    }

    if !any_split {
        return None;
    }

    let activity_key = log.activity_key().clone();
    let subtree = inductive_miner(&new_log);
    let tau_leaf: ProcessTree = (activity_key.clone(), vec![Node::Tau]).into();
    Some(build_operator_node(
        Operator::Loop,
        vec![subtree, tau_leaf],
        activity_key,
    ))
}

fn flower_model(log: &dyn EbiTraitFiniteLanguage) -> ProcessTree {
    let info = LogInfo::log_calcs(log);
    let activity_key = log.activity_key().clone();

    if info.activities.len() == 1 {
        let a = *info.activities.iter().next().unwrap();
        let a_leaf: ProcessTree = (activity_key.clone(), vec![Node::Activity(a)]).into();
        let tau_leaf: ProcessTree = (activity_key.clone(), vec![Node::Tau]).into();
        return build_operator_node(Operator::Loop, vec![a_leaf, tau_leaf], activity_key);
    }

    let activity_trees: Vec<ProcessTree> = info
        .activities
        .iter()
        .map(|&a| (activity_key.clone(), vec![Node::Activity(a)]).into())
        .collect();
    let xor_body = build_operator_node(Operator::Xor, activity_trees, activity_key.clone());
    let tau_leaf: ProcessTree = (activity_key.clone(), vec![Node::Tau]).into();
    build_operator_node(Operator::Loop, vec![xor_body, tau_leaf], activity_key)
}
