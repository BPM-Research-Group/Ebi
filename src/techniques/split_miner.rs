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
use std::collections::{HashMap, HashSet};

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
    // In contrast to the paper, we do not add the task-task edges,
    // as Algorithm 5 removes them.

    let mut initial_bpmn = InitialBPMN {
        dfg,
        self_loops,
        short_loops,
        concurrent_activities,
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
        dfg,
        self_loops,
        short_loops,
        concurrent_activities,
        activity_2_task,
        bpmn_creator,
        process,
    } = initial_bpmn;

    for (activity, _task) in activity_2_task.iter() {
        let outgoing_edges = dfg.outgoing_edges(activity);
        if outgoing_edges.len() > 1 {
            //compute d-successors
            let d_successors = outgoing_edges
                .into_iter()
                .map(|(target, _)| target)
                .collect::<Vec<_>>();

            let mut cover = IntMap::new();
            let mut future = IntMap::new();

            //line 6
            for successor in d_successors.iter().copied() {
                //line 7
                let mut s = HashSet::new();
                s.insert(successor);
                cover.insert(successor, s);

                //line 8
                future.insert(successor, HashSet::new());

                //line 9
                for successor_b in d_successors.iter().copied() {
                    //line 10
                    if successor != successor_b
                        && concurrent_activities
                            .contains(&ConcurrentPair::new(successor, successor_b))
                    {
                        future
                            .get_mut(successor)
                            .and_if_not("successor not found")?
                            .insert(successor_b);
                    }
                }
            }

            //line 11
            //we did not add any edges, so we don't have to remove them here

            //change to algorithm: we switch to BPMN elements in set S (d_successors up till now).
            let mut s_set = d_successors
                .into_iter()
                .map(|activity| {
                    activity_2_task
                        .get(activity)
                        .and_if_not("Activity not found")
                        .copied()
                })
                .collect::<Result<HashSet<_>>>()?;

            let mut cover = cover
                .into_iter()
                .map(|(activity, map)| {
                    Ok((
                        activity_2_task
                            .get(activity)
                            .and_if_not("Activity not found")
                            .copied()?,
                        map,
                    ))
                })
                .collect::<Result<HashMap<_, _>>>()?;

            let mut f_set = future
                .into_iter()
                .map(|(activity, map)| {
                    Ok((
                        activity_2_task
                            .get(activity)
                            .and_if_not("Activity not found")
                            .copied()?,
                        map,
                    ))
                })
                .collect::<Result<HashMap<_, _>>>()?;

            //line 12
            while s_set.len() > 1 {
                //line 13
                algorithm_6_discover_xor_splits(
                    bpmn_creator,
                    *process,
                    &mut s_set,
                    &mut cover,
                    &mut f_set,
                )?;

                //line 14
                algorithm_7_discover_and_splits(
                    bpmn_creator,
                    *process,
                    &mut s_set,
                    &mut cover,
                    &mut f_set,
                )?;
            }

            //line 15
            let s = s_set.iter().next().unwrap();

            //line 16
            let source_task = activity_2_task
                .get(activity)
                .and_if_not("Task not found.")?;
            bpmn_creator.add_sequence_flow(*source_task, *s)?;
        } else if outgoing_edges.len() == 1 {
            // optimisation of the algorithm: add an edge if there is one
            let (target, _) = outgoing_edges[0];
            let source_task = activity_2_task
                .get(activity)
                .and_if_not("Task not found.")?;
            let target_task = activity_2_task.get(target).and_if_not("Task not found.")?;
            bpmn_creator.add_sequence_flow(*source_task, *target_task)?;
        }
    }
    Ok(())
}

/// Algorithm 6
fn algorithm_6_discover_xor_splits(
    bpmn_creator: &mut BPMNCreator,
    process: Container,
    s_set: &mut HashSet<GlobalIndex>,
    cover: &mut HashMap<GlobalIndex, HashSet<Activity>>,
    future: &mut HashMap<GlobalIndex, HashSet<Activity>>,
) -> Result<()> {
    loop {
        //line 2
        let mut x = HashSet::new();

        //line 3
        for s_1 in s_set.iter().copied() {
            //line 4
            let mut c_u = cover.get(&s_1).and_if_not("Activity not found.")?.clone();

            //line 5
            for s_2 in s_set.iter().copied() {
                //line 6
                if s_1 != s_2 && future.get(&s_1) == future.get(&s_2) {
                    //line 7
                    x.insert(s_2);
                    //line 8
                    c_u.extend(cover.get(&s_2).and_if_not("Activity not found.")?);
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
                future.insert(g, future.get(&s_1).and_if_not("Element not found")?.clone());

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
    cover: &mut HashMap<GlobalIndex, HashSet<Activity>>,
    future: &mut HashMap<GlobalIndex, HashSet<Activity>>,
) -> Result<()> {
    loop {
        let mut a = HashSet::new();
        for s_1 in s_set.iter().copied() {
            let mut c_u = cover.get(&s_1).and_if_not("Element not found")?.clone();

            let mut f_i = future.get(&s_1).and_if_not("Element not found.")?.clone();

            let mut cfs1 = c_u.clone();
            cfs1.extend(f_i.clone());

            //line 7
            for s_2 in s_set.iter().copied() {
                if s_1 != s_2 {
                    let mut cfs2 = cover.get(&s_1).and_if_not("Element not found")?.clone();
                    cfs2.extend(future.get(&s_1).and_if_not("Element not found.")?.clone());

                    if cfs1 == cfs2 {
                        //line 10
                        a.insert(s_2);

                        //line 11
                        let c_s_2 = cover.get(&s_2).and_if_not("Element not found")?.clone();
                        c_u.extend(c_s_2);

                        //line 12
                        let f_s_2 = future.get(&s_2).and_if_not("Element not found.")?.clone();
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
        dfg,
        self_loops,
        short_loops,
        concurrent_activities,
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
                dfg.activity_key().deprocess_activity(&activity)
            );
            //add an OR gateway
            let or = bpmn_creator.add_gateway(*process, GatewayType::Inclusive)?;

            //add a sequence flow from the gateway to the task
            bpmn_creator.add_sequence_flow(*task, or)?;

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
                dfg.activity_key().deprocess_activity(&activity)
            );
        }
    }

    Ok(())
}

fn algorithm_9_replace_ors(initial_bpmn: &mut InitialBPMN) -> Result<()> {
    let InitialBPMN {
        dfg,
        self_loops,
        short_loops,
        concurrent_activities,
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
    concurrent_activities: HashSet<ConcurrentPair>,
}

#[derive(Hash, Eq, PartialEq)]
struct ConcurrentPair {
    a: Activity,
    b: Activity,
}

impl ConcurrentPair {
    fn new(a: Activity, b: Activity) -> Self {
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
    concurrent_activities: HashSet<ConcurrentPair>,
}

struct InitialBPMN {
    dfg: DirectlyFollowsGraph,
    self_loops: Vec<Activity>,
    short_loops: Vec<(Activity, Activity)>,
    concurrent_activities: HashSet<ConcurrentPair>,
    activity_2_task: IntMap<Activity, GlobalIndex>,
    bpmn_creator: BPMNCreator,
    process: Container,
}
