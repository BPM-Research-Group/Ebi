use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    techniques::directly_follows_graph_abstractor::DirectlyFollowsAbstractor,
};
use ebi_objects::{
    Activity, BusinessProcessModelAndNotation, DirectlyFollowsGraph, HasActivityKey,
    anyhow::Result,
    ebi_arithmetic::{Fraction, Signed, Zero, f},
    ebi_bpmn::{
        BPMNCreator, Container, EndEventType, GatewayType, GlobalIndex, StartEventType,
        if_not::IfNot,
    },
};
use intmap::IntMap;
use std::collections::HashSet;

pub trait SplitMiner {
    fn split_miner(&self) -> Result<BusinessProcessModelAndNotation>;
}

impl SplitMiner for dyn EbiTraitFiniteStochasticLanguage {
    fn split_miner(&self) -> Result<BusinessProcessModelAndNotation> {
        split_miner(self, &SplitMinerParameters::default())
    }
}

#[derive(Clone)]
pub struct SplitMinerParameters {
    parallelisms_threshold: Fraction,
}

impl Default for SplitMinerParameters {
    fn default() -> Self {
        Self {
            parallelisms_threshold: f!(1, 10),
        }
    }
}

pub fn split_miner(
    log: &dyn EbiTraitFiniteStochasticLanguage,
    parameters: &SplitMinerParameters,
) -> Result<BusinessProcessModelAndNotation> {
    let filtered_dfg = step_1_dfg_and_loop_discovery(log);

    let pruned_dfg = step_2_concurrency_discovery(parameters, filtered_dfg);

    let filtered_pruned_dfg = algorithm_1_generate_filtered_dfg(pruned_dfg);

    let initial_bpmn = algorithm_4_filtered_dfg_to_bpmn(filtered_pruned_dfg)?;

    println!("{}", initial_bpmn.bpmn_creator);

    initial_bpmn.bpmn_creator.to_bpmn()
}

fn step_1_dfg_and_loop_discovery(log: &dyn EbiTraitFiniteStochasticLanguage) -> FilteredDfg {
    let mut dfg = log.abstract_to_directly_follows_graph();

    //detect and remove self-loops
    let self_loops = dfg
        .edges_mut()
        .filter_map(|(source, (target, weight))| {
            if source == target {
                weight.set_zero();
                Some(source)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    //detect short-loops
    let mut short_loops = vec![];
    for trace in log.iter_traces() {
        for window in trace.windows(3) {
            if window[0] == window[2]
                && !short_loops.contains(&(window[0], window[1]))
                && !short_loops.contains(&(window[1], window[0]))
            {
                short_loops.push((window[0], window[1]));
            }
        }
    }
    for (a, b) in short_loops.iter() {
        dfg.remove_edge(*a, *b);
        dfg.remove_edge(*b, *a);
    }

    FilteredDfg {
        dfg,
        self_loops,
        short_loops,
    }
}

fn step_2_concurrency_discovery(
    parameters: &SplitMinerParameters,
    filtered_dfg: FilteredDfg,
) -> PrunedDfg {
    let FilteredDfg {
        mut dfg,
        self_loops,
        short_loops,
    } = filtered_dfg;

    let mut remove_edges = vec![];
    let mut concurrent_activities = HashSet::new();
    for (source, (target, weight)) in dfg.edges() {
        if !self_loops.contains(&source) && !self_loops.contains(&target) {
            let weight_ab = weight;
            let weight_ba = dfg.edge_weight(target, source);
            if (weight_ab - &weight_ba).abs() / (weight_ab + &weight_ba)
                <= parameters.parallelisms_threshold
            //paper says "<", but example suggests "<="
            {
                //activities are concurrent
                remove_edges.push((source, target));
                remove_edges.push((target, source));

                concurrent_activities.insert(ConcurrentPair::new(source, target));
            } else if weight_ab < &weight_ba {
                remove_edges.push((source, target));
            } else {
                remove_edges.push((target, source));
            }
        }
    }

    for (source, target) in remove_edges {
        dfg.remove_edge(source, target);
    }

    PrunedDfg {
        dfg,
        self_loops,
        short_loops,
        concurrent_activities,
    }
}

fn algorithm_1_generate_filtered_dfg(pruned_dfg: PrunedDfg) -> FilteredPrunedDdg {
    let PrunedDfg {
        dfg,
        self_loops,
        short_loops,
        concurrent_activities,
    } = pruned_dfg;

    // let mut c_f = IntMap::new();
    // let mut c_b = IntMap::new();
    // let mut f = HashSet::new();

    // //line 8
    // for t in dfg.activities() {
    //     c_f.insert(t, Fraction::zero());
    //     c_b.insert(t, Fraction::zero());

    //     f_i = dfg.incom
    // }


    FilteredPrunedDdg {
        dfg,
        self_loops,
        short_loops,
        concurrent_activities,
    }
}

fn algorithm_4_filtered_dfg_to_bpmn(filtered_pruned_dfg: FilteredPrunedDdg) -> Result<InitialBPMN> {
    let FilteredPrunedDdg {
        dfg,
        self_loops,
        short_loops,
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
            Some(ConcurrentPair::new(
                *activity_2_task.get(pair.a)?,
                *activity_2_task.get(pair.b)?,
            ))
        })
        .collect::<Option<HashSet<_>>>()
        .and_if_not("Activity not found.")?;

    let mut initial_bpmn = InitialBPMN {
        self_loops,
        short_loops,
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
        self_loops,
        short_loops,
        concurrent_tasks: concurrent_activities,
        activity_2_task,
        bpmn_creator,
        process,
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
                    if s_1 != s_2 && concurrent_activities.contains(&ConcurrentPair::new(s_1, s_2))
                    {
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
        self_loops,
        short_loops,
        concurrent_tasks: concurrent_activities,
        activity_2_task,
        bpmn_creator,
        process,
    } = initial_bpmn;

    //todo: implement the actual algorithm 8 of the paper, which uses SESE fragments.
    //for now, replace every implicit join with an OR gateway

    for (activity, task) in activity_2_task {
        let incoming_flows = bpmn_creator
            .incoming_sequence_flows_of_element(*task)?
            .collect::<Vec<_>>();

        if incoming_flows.len() > 1 {
            println!(
                "add OR for task {}",
                bpmn_creator.activity_key().deprocess_activity(&activity)
            );
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
        } else {
            println!(
                "do not add OR for task {}",
                bpmn_creator.activity_key().deprocess_activity(&activity)
            );
        }
    }

    Ok(())
}

fn algorithm_9_replace_ors(initial_bpmn: &mut InitialBPMN) -> Result<()> {
    let InitialBPMN {
        self_loops,
        short_loops,
        concurrent_tasks: concurrent_activities,
        activity_2_task,
        bpmn_creator,
        process,
    } = initial_bpmn;

    Ok(())
}

struct FilteredDfg {
    dfg: DirectlyFollowsGraph,
    self_loops: Vec<Activity>,
    short_loops: Vec<(Activity, Activity)>,
}

struct PrunedDfg {
    dfg: DirectlyFollowsGraph,
    self_loops: Vec<Activity>,
    short_loops: Vec<(Activity, Activity)>,
    concurrent_activities: HashSet<ConcurrentPair<Activity>>,
}

#[derive(Hash, Eq, PartialEq)]
struct ConcurrentPair<T> {
    a: T,
    b: T,
}

impl<T: Ord> ConcurrentPair<T> {
    fn new(a: T, b: T) -> Self {
        if a > b {
            Self { a, b }
        } else {
            Self { a: b, b: a }
        }
    }
}

struct FilteredPrunedDdg {
    dfg: DirectlyFollowsGraph,
    self_loops: Vec<Activity>,
    short_loops: Vec<(Activity, Activity)>,
    concurrent_activities: HashSet<ConcurrentPair<Activity>>,
}

struct InitialBPMN {
    self_loops: Vec<Activity>,
    short_loops: Vec<(Activity, Activity)>,
    concurrent_tasks: HashSet<ConcurrentPair<GlobalIndex>>,
    activity_2_task: IntMap<Activity, GlobalIndex>,
    bpmn_creator: BPMNCreator,
    process: Container,
}
