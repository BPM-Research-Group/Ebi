use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    techniques::directly_follows_graph_abstractor::DirectlyFollowsAbstractor,
};
use ebi_objects::{
    Activity, BusinessProcessModelAndNotation, DirectlyFollowsGraph,
    anyhow::Result,
    ebi_arithmetic::{Fraction, Signed, ToNative, Zero, f},
    ebi_bpmn::{
        BPMNCreator, Container, EndEventType, GatewayType, GlobalIndex, StartEventType,
        if_not::IfNot,
    },
};
use intmap::IntMap;
use itertools::Itertools;
use std::collections::{HashSet, VecDeque};

pub trait SplitMiner {
    fn split_miner(
        &self,
        parameters: &SplitMinerParameters,
    ) -> Result<BusinessProcessModelAndNotation>;

    fn split_miner_default(&self) -> Result<BusinessProcessModelAndNotation> {
        self.split_miner(&SplitMinerParameters::default())
    }
}

impl SplitMiner for dyn EbiTraitFiniteStochasticLanguage {
    fn split_miner(&self, parameters: &SplitMinerParameters) -> Result<BusinessProcessModelAndNotation> {
        split_miner(self, parameters)
    }
}

#[derive(Clone)]
pub struct SplitMinerParameters {
    /// Threshold for whether a pair of back-and-forth edges gets treated as concurrent or as infrequent behaviour (Parallelisms Threshold).
    pub epsilon_parallelism: Fraction,
    /// Threshold for filtering dfg edges (Percentile Frequency Threshold).
    pub eta_frequency: Fraction,
}

impl Default for SplitMinerParameters {
    fn default() -> Self {
        Self {
            epsilon_parallelism: f!(1, 10),
            eta_frequency: f!(4, 10),
        }
    }
}

pub fn split_miner(
    log: &dyn EbiTraitFiniteStochasticLanguage,
    parameters: &SplitMinerParameters,
) -> Result<BusinessProcessModelAndNotation> {
    //special case: empty log
    if log.number_of_traces() == 0 {
        //empty model
        return BPMNCreator::new().to_bpmn();
    }

    //special case: only empty traces
    if log.number_of_events() == 0 {
        //model with only an empty trace
        let mut bpmn_creator = BPMNCreator::new();
        let process = bpmn_creator.add_process(None);
        let start_event = bpmn_creator.add_start_event(process, StartEventType::None)?;
        let end_event = bpmn_creator.add_end_event(process, EndEventType::None)?;
        bpmn_creator.add_sequence_flow(start_event, end_event)?;
        return bpmn_creator.to_bpmn();
    }

    let mut filtered_dfg = step_1_dfg(parameters, log);

    println!("2 {}", filtered_dfg.dfg);

    algorithm_1_prune_dfg(parameters, &mut filtered_dfg)?;

    println!("3 {}", filtered_dfg.dfg);

    let mut initial_bpmn = algorithm_4_filtered_dfg_to_bpmn(filtered_dfg)?;

    process_loops(&mut initial_bpmn)?;

    println!("{}", initial_bpmn.bpmn_creator);

    initial_bpmn.bpmn_creator.to_bpmn()
}

fn step_1_dfg(
    parameters: &SplitMinerParameters,
    log: &dyn EbiTraitFiniteStochasticLanguage,
) -> FilteredDfg {
    //create dfg
    let mut dfg = log.abstract_to_directly_follows_graph();

    println!("1 {}", dfg);

    //detect self-loops
    let self_loops = dfg
        .edges()
        .filter_map(
            |(source, (target, _))| {
                if source == target { Some(source) } else { None }
            },
        )
        .collect::<Vec<_>>();

    //detect short-loops
    let mut short_loops = HashSet::new();
    for trace in log.iter_traces() {
        for window in trace.windows(3) {
            if window[0] == window[2] {
                short_loops.insert(PairSet::new(window[0], window[1]));
            }
        }
    }

    //detect concurrent activities
    let mut concurrent_activities = HashSet::new();
    for (source, (target, weight)) in dfg.edges() {
        if !self_loops.contains(&source)
            && !self_loops.contains(&target)
            && !short_loops.contains(&PairSet::new(source, target))
        {
            let weight_ab = weight;
            let weight_ba = dfg.edge_weight(target, source);
            if (weight_ab - weight_ba).abs() / (weight_ab + weight_ba) <= parameters.epsilon_parallelism
            //paper says "<", but example suggests "<="
            {
                //activities are concurrent
                concurrent_activities.insert(PairSet::new(source, target));
            }
        }
    }

    //filter the dfg
    let mut remove_edges = vec![];
    for (a, (b, weight)) in dfg.edges() {
        if concurrent_activities.contains(&PairSet::new(a, b))
            || (dfg.contains_edge(b, a) && weight < dfg.edge_weight(b, a))
        {
            remove_edges.push((a, b));
        }
    }

    for (a, b) in remove_edges {
        dfg.remove_edge(a, b);
    }

    FilteredDfg {
        dfg,
        self_loops,
        concurrent_activities,
    }
}

fn algorithm_1_prune_dfg(
    parameters: &SplitMinerParameters,
    filtered_dfg: &mut FilteredDfg,
) -> Result<()> {
    let FilteredDfg { dfg, .. } = filtered_dfg;

    //line 3
    let mut c_f = IntMap::new();
    let mut c_b = IntMap::new();

    //this is defined as a set in the paper, though intuition tells it could be a multiset or list.
    let mut f = HashSet::new();

    //line 6-7
    //we do not have explicit start and end

    //line 8
    for t in dfg.activities() {
        c_f.insert(t, Fraction::zero());
        c_b.insert(t, Fraction::zero());

        let f_i = dfg
            .incoming_edges(t)
            .map(|(_, weight)| weight)
            .chain(vec![dfg.start_activity_weight(t)])
            .max()
            .unwrap();
        let f_o = dfg
            .outgoing_edges(t)
            .iter()
            .map(|(_, weight)| *weight)
            .chain(vec![dfg.end_activity_weight(t)])
            .max()
            .unwrap();
        f.insert(f_i);
        f.insert(f_o);
    }

    //line 14
    // Here, we follow the implementation, as to match how ties are dealt with.
    let mut f = f.into_iter().collect::<Vec<_>>();
    f.sort();
    let mut i = (&f!(f.len()) * &parameters.eta_frequency).to_usize();
    if i == f.len() {
        i -= 1
    };
    let f_th = f[i].clone();

    //line 15
    let mut e_i = IntMap::new();
    let mut e_o = IntMap::new();

    //line 17
    algorithm_2_discover_best_incoming_edges(&dfg, &mut c_f, &mut e_i)?;
    algorithm_3_discover_best_outgoing_edges(&dfg, &mut c_b, &mut e_o)?;

    //line 19
    //here, the paper removes edges; we instead remove them
    for (source, (target, weight)) in dfg.edges_mut() {
        //line 21
        if !(e_i.values().contains(&(Some(source), target)))
            && !(e_o.values().contains(&(source, Some(target))))
            && !(*weight > f_th)
        {
            //remove edge
            weight.set_zero();
        }
    }

    Ok(())
}

fn algorithm_2_discover_best_incoming_edges(
    dfg: &DirectlyFollowsGraph,
    c_f: &mut IntMap<Activity, Fraction>,
    e_i: &mut IntMap<Activity, (Option<Activity>, Activity)>,
) -> Result<()> {
    let mut q = VecDeque::new();

    //line 2
    let mut u = dfg.activities().collect::<HashSet<_>>();

    //line 3
    //Change w.r.t. paper: we have an implicit start event
    //unfold the loop of line 7 for `i` manually
    for start_activity in dfg.start_activities() {
        let c_max = dfg.start_activity_weight(start_activity);
        if c_max
            > c_f
                .get(start_activity)
                .and_if_not("Start activity not found.")?
        {
            c_f.insert(start_activity, c_max.clone());
            e_i.insert(start_activity, (None, start_activity));
        }
        u.remove(&start_activity);
        q.push_back(start_activity);
    }

    //line 4
    while let Some(p) = q.pop_front() {
        //line 7
        for (n, f_e) in dfg.outgoing_edges(p) {
            //line 10
            let c_max = c_f.get(p).and_if_not("Activity not found.")?.min(f_e);

            //line 11
            if c_max > c_f.get(n).and_if_not("Activity not found.")? {
                //line 12
                c_f.insert(n, c_max.clone());

                //line 13
                e_i.insert(n, (Some(p), n));

                //line 14
                if !q.contains(&n) && !u.contains(&n) {
                    u.insert(n);
                }
            }

            //line 15
            if u.contains(&n) {
                u.remove(&n);
                q.push_back(n);
            }
        }
    }

    Ok(())
}

fn algorithm_3_discover_best_outgoing_edges(
    dfg: &DirectlyFollowsGraph,
    c_b: &mut IntMap<Activity, Fraction>,
    e_o: &mut IntMap<Activity, (Activity, Option<Activity>)>,
) -> Result<()> {
    let mut q = VecDeque::new();
    let mut u = dfg.activities().collect::<HashSet<_>>();

    //line 1
    //we do not have an explicit end activity
    //follow the loop manually for `o`,
    //then line 10 evaluates to false.
    //only line 14 is executed
    for p in dfg.end_activities() {
        let f_e = dfg.end_activity_weight(p);
        let c_max = f_e;
        if c_max > c_b.get(p).and_if_not("End activity not found")? {
            c_b.insert(p, c_max.clone());
            e_o.insert(p, (p, None));
        }
        u.remove(&p);
        q.push_back(p);
    }

    //line 3
    while let Some(n) = q.pop_front() {
        //line 6
        for (p, f_e) in dfg.incoming_edges(n) {
            let c_max = c_b.get(n).and_if_not("Activity not found")?.min(f_e);

            //line 10
            if c_max > c_b.get(p).and_if_not("Activity not found.")? {
                c_b.insert(p, c_max.clone());
                e_o.insert(p, (p, Some(n)));

                //line 13
                if !q.contains(&p) && !u.contains(&p) {
                    u.insert(p);
                }
            }

            //line 14
            if u.contains(&p) {
                u.remove(&p);
                q.push_back(p);
            }
        }
    }

    Ok(())
}

fn algorithm_4_filtered_dfg_to_bpmn(filtered_pruned_dfg: FilteredDfg) -> Result<InitialBPMN> {
    let FilteredDfg {
        dfg,
        self_loops,
        concurrent_activities,
    } = filtered_pruned_dfg;

    let mut bpmn_creator = BPMNCreator::new_with_activity_key(dfg.activity_key.clone());
    let process = bpmn_creator.add_process(None);

    let start_event = bpmn_creator.add_start_event(process, StartEventType::None)?;
    let end_event = bpmn_creator.add_end_event(process, EndEventType::None)?;

    //add tasks
    let mut activity_2_task = IntMap::new();
    for activity in dfg.activities() {
        let task = bpmn_creator.add_task(process, activity)?;
        activity_2_task.insert(activity, task);
    }

    //start flows
    for start_activity in dfg.start_activities() {
        bpmn_creator.add_sequence_flow(
            start_event,
            *activity_2_task
                .get(start_activity)
                .and_if_not("Start task not found.")?,
        )?;
    }

    //end flows
    for end_activity in dfg.end_activities() {
        bpmn_creator.add_sequence_flow(
            *activity_2_task
                .get(end_activity)
                .and_if_not("End task not found.")?,
            end_event,
        )?;
    }

    //edges
    for (source, (target, _)) in dfg.edges() {
        let source_task = activity_2_task.get(source).and_if_not("Task not found.")?;
        let target_task = activity_2_task.get(target).and_if_not("Task not found.")?;
        bpmn_creator.add_sequence_flow(*source_task, *target_task)?;
    }

    //transform the concurrent activities
    let concurrent_tasks = concurrent_activities
        .into_iter()
        .map(|pair| {
            Some(PairSet::new(
                *activity_2_task.get(pair.a)?,
                *activity_2_task.get(pair.b)?,
            ))
        })
        .collect::<Option<HashSet<_>>>()
        .and_if_not("Activity not found.")?;

    let mut initial_bpmn = InitialBPMN {
        self_loops,
        concurrent_tasks,
        activity_2_task,
        bpmn_creator,
        process,
    };

    algorithm_5_discover_splits(&mut initial_bpmn)?;

    algorithm_8_discover_joins(&mut initial_bpmn)?;

    algorithm_9_replace_ors(&mut initial_bpmn)?;

    Ok(initial_bpmn)
}

/// Algorithm 5
fn algorithm_5_discover_splits(initial_bpmn: &mut InitialBPMN) -> Result<()> {
    let InitialBPMN {
        concurrent_tasks,
        activity_2_task,
        bpmn_creator,
        process,
        ..
    } = initial_bpmn;

    for (activity, task) in activity_2_task.iter() {
        let t_dot = bpmn_creator
            .outgoing_sequence_flows_of_element(*task)?
            .collect::<Vec<_>>();
        if t_dot.len() > 1 {
            //compute d-successors
            let mut s_set = t_dot
                .iter()
                .map(|flow| bpmn_creator.target_of_sequence_flow(*flow))
                .collect::<Option<HashSet<_>>>()
                .and_if_not("Target not found.")?;

            let mut cover = IntMap::new();
            let mut future = IntMap::new();

            //line 6
            for s_1 in s_set.iter().copied() {
                //line 7
                let mut s = HashSet::new();
                s.insert(s_1);
                cover.insert(s_1, s);

                //line 8
                future.insert(s_1, HashSet::new());

                //line 9
                for s_2 in s_set.iter().copied() {
                    //line 10
                    if s_1 != s_2 && concurrent_tasks.contains(&PairSet::new(s_1, s_2)) {
                        future
                            .get_mut(s_1)
                            .and_if_not("successor not found")?
                            .insert(s_2);
                    }
                }
            }

            //line 11
            for flow in t_dot {
                bpmn_creator.remove_sequence_flow(flow)?;
            }

            //line 12
            while s_set.len() > 1 {
                //line 13
                algorithm_6_discover_xor_splits(
                    bpmn_creator,
                    *process,
                    &mut s_set,
                    &mut cover,
                    &mut future,
                )?;

                //line 14
                algorithm_7_discover_and_splits(
                    bpmn_creator,
                    *process,
                    &mut s_set,
                    &mut cover,
                    &mut future,
                )?;
            }

            //line 15
            let s = s_set.iter().next().unwrap();

            //line 16
            let source_task = activity_2_task
                .get(activity)
                .and_if_not("Task not found.")?;
            bpmn_creator.add_sequence_flow(*source_task, *s)?;
        }
    }
    Ok(())
}

/// Algorithm 6
fn algorithm_6_discover_xor_splits(
    bpmn_creator: &mut BPMNCreator,
    process: Container,
    s_set: &mut HashSet<GlobalIndex>,
    cover: &mut IntMap<GlobalIndex, HashSet<GlobalIndex>>,
    future: &mut IntMap<GlobalIndex, HashSet<GlobalIndex>>,
) -> Result<()> {
    loop {
        //line 2
        let mut x = HashSet::new();

        //line 3
        for s_1 in s_set.iter().copied() {
            //line 4
            let mut c_u = cover.get(s_1).and_if_not("Activity not found.")?.clone();

            //line 5
            for s_2 in s_set.iter().copied() {
                //line 6
                if s_1 != s_2 && future.get(s_1) == future.get(s_2) {
                    //line 7
                    x.insert(s_2);
                    //line 8
                    c_u.extend(cover.get(s_2).and_if_not("Activity not found.")?);
                }
            }

            //line 9
            if !x.is_empty() {
                //line 10
                x.insert(s_1);

                //here, the algorithm in the paper takes variables outside of the scope of the loop.
                //rather, we take lines 12-20 inside and break;

                //line 13 & 14
                let g = bpmn_creator.add_gateway(process, GatewayType::Exclusive)?;
                //line 15
                for s in x.iter().copied() {
                    //line 16
                    bpmn_creator.add_sequence_flow(g, s)?;
                    //line 17
                    s_set.remove(&s);
                }

                //line 18
                s_set.insert(g);

                //line 19
                future.insert(g, future.get(s_1).and_if_not("Element not found")?.clone());

                //line 20
                cover.insert(g, c_u);

                //line 11
                break;
            }
        }

        if x.is_empty() {
            return Ok(());
        }
    }
}

/// Algorithm 7
fn algorithm_7_discover_and_splits(
    bpmn_creator: &mut BPMNCreator,
    process: Container,
    s_set: &mut HashSet<GlobalIndex>,
    cover: &mut IntMap<GlobalIndex, HashSet<GlobalIndex>>,
    future: &mut IntMap<GlobalIndex, HashSet<GlobalIndex>>,
) -> Result<()> {
    loop {
        let mut a = HashSet::new();
        for s_1 in s_set.iter().copied() {
            let mut c_u = cover.get(s_1).and_if_not("Element not found")?.clone();

            let mut f_i = future.get(s_1).and_if_not("Element not found.")?.clone();

            let mut cfs1 = c_u.clone();
            cfs1.extend(f_i.clone());

            //line 7
            for s_2 in s_set.iter().copied() {
                if s_1 != s_2 {
                    let mut cfs2 = cover.get(s_1).and_if_not("Element not found")?.clone();
                    cfs2.extend(future.get(s_1).and_if_not("Element not found.")?.clone());

                    if cfs1 == cfs2 {
                        //line 10
                        a.insert(s_2);

                        //line 11
                        let c_s_2 = cover.get(s_2).and_if_not("Element not found")?.clone();
                        c_u.extend(c_s_2);

                        //line 12
                        let f_s_2 = future.get(s_2).and_if_not("Element not found.")?.clone();
                        f_i.retain(|x| f_s_2.contains(x));
                    }
                }
            }

            //line 13
            if !a.is_empty() {
                //line 14
                a.insert(s_1);

                //here, the algorithm in the paper breaks and takes the variables outside the loop
                //rather, we put lines 16-24 here

                //line 17 & 18
                let g = bpmn_creator.add_gateway(process, GatewayType::Parallel)?;

                //line 19
                for s in a.iter().copied() {
                    bpmn_creator.add_sequence_flow(g, s)?;
                    s_set.remove(&s);
                }

                //line 22
                s_set.insert(g);

                //line 23
                cover.insert(g, c_u);

                //line 24
                future.insert(g, f_i);

                //line 15
                break;
            }
        }

        if a.is_empty() {
            return Ok(());
        }
    }
}

fn algorithm_8_discover_joins(initial_bpmn: &mut InitialBPMN) -> Result<()> {
    let InitialBPMN {
        activity_2_task,
        bpmn_creator,
        process,
        ..
    } = initial_bpmn;

    //todo: implement the actual algorithm 8 of the paper, which uses SESE fragments.
    //for now, replace every implicit join with an OR gateway

    for (_activity, task) in activity_2_task {
        let incoming_flows = bpmn_creator
            .incoming_sequence_flows_of_element(*task)?
            .collect::<Vec<_>>();

        if incoming_flows.len() > 1 {
            //add an OR gateway
            let or = bpmn_creator.add_gateway(*process, GatewayType::Inclusive)?;

            //add a sequence flow from the gateway to the task
            bpmn_creator.add_sequence_flow(or, *task)?;

            for sequence_flow in incoming_flows {
                let source = bpmn_creator
                    .source_of_sequence_flow(sequence_flow)
                    .and_if_not("Source not found.")?;

                //remove the old sequence flow
                bpmn_creator.remove_sequence_flow(sequence_flow)?;

                //add a new sequence flow
                bpmn_creator.add_sequence_flow(source, or)?;
            }
        }
    }

    Ok(())
}

fn algorithm_9_replace_ors(_initial_bpmn: &mut InitialBPMN) -> Result<()> {
    Ok(())
}

/// This is a guess as it is not in the paper, but: transforms each self-loop task into an `a+` loop.
fn process_loops(initial_bpmn: &mut InitialBPMN) -> Result<()> {
    let InitialBPMN {
        self_loops,
        activity_2_task,
        bpmn_creator,
        process,
        ..
    } = initial_bpmn;

    for activity in self_loops {
        let task = activity_2_task
            .get(*activity)
            .and_if_not("Activity not found.")?;

        let pre_gateway = bpmn_creator.add_gateway(*process, GatewayType::Exclusive)?;
        bpmn_creator.swap_incoming_sequence_flows(*task, pre_gateway)?;
        bpmn_creator.add_sequence_flow(pre_gateway, *task)?;

        let post_gateway = bpmn_creator.add_gateway(*process, GatewayType::Exclusive)?;
        bpmn_creator.swap_outgoing_sequence_flows(*task, post_gateway)?;
        bpmn_creator.add_sequence_flow(*task, post_gateway)?;
    }

    Ok(())
}

struct FilteredDfg {
    dfg: DirectlyFollowsGraph,
    self_loops: Vec<Activity>,
    concurrent_activities: HashSet<PairSet<Activity>>,
}

#[derive(Hash, Eq, PartialEq)]
struct PairSet<T> {
    a: T,
    b: T,
}

impl<T: Ord> PairSet<T> {
    fn new(a: T, b: T) -> Self {
        if a > b {
            Self { a, b }
        } else {
            Self { a: b, b: a }
        }
    }
}

struct InitialBPMN {
    self_loops: Vec<Activity>,
    concurrent_tasks: HashSet<PairSet<GlobalIndex>>,
    activity_2_task: IntMap<Activity, GlobalIndex>,
    bpmn_creator: BPMNCreator,
    process: Container,
}

#[cfg(test)]
mod tests {
    use crate::{
        ebi_framework::trait_importers::ToFiniteStochasticLanguageTrait,
        techniques::split_miner::SplitMiner,
    };
    use ebi_objects::FiniteStochasticLanguage;
    use std::fs;

    #[test]
    fn ab() {
        let fin = fs::read_to_string("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap();
        let log = fin.parse::<FiniteStochasticLanguage>().unwrap();

        let x = log.to_finite_stochastic_language_trait();

        println!("{}", x);

        x.split_miner_default().unwrap();
    }

    #[test]
    fn empty() {
        let fin = fs::read_to_string("./testfiles/empty.slang").unwrap();
        let log = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let x = log.to_finite_stochastic_language_trait();

        println!("{}", x);

        x.split_miner_default().unwrap();
    }

    #[test]
    fn empty_traces() {
        let fin = fs::read_to_string("./testfiles/empty_trace.slang").unwrap();
        let log = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let x = log.to_finite_stochastic_language_trait();

        x.split_miner_default().unwrap();
    }
}
