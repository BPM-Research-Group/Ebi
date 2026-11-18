use crate::{
    ebi_framework::ebi_command::EbiCommand,
    ebi_traits::{
        ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    },
    techniques::align::Align,
};
use ebi_objects::{
    anyhow::{anyhow, ensure, Context, Result},
    ebi_objects::{
        labelled_petri_net::TransitionIndex,
        language_of_alignments::Move,
        process_tree::{Node, Operator},
    },
    Activity, ActivityKey, ActivityKeyTranslator, LanguageOfAlignments, ProcessTree,
    StochasticLanguageOfAlignments,
};
use fnv::FnvBuildHasher;
use rayon::iter::ParallelIterator;
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::Arc,
};

pub struct ProcessTreeAligner<'a> {
    tree: &'a ProcessTree,
    node_metadata: Vec<NodeMetadata>,
}

#[derive(Default)]
struct NodeMetadata {
    child_nodes: Vec<usize>,
    alphabet: HashSet<Activity>,
    activity_to_child_rank: HashMap<Activity, usize, FnvBuildHasher>,
    transition_index: Option<TransitionIndex>,
    redo_is_silent: bool,
    is_unary_loop: bool,
}

impl<'a> ProcessTreeAligner<'a> {
    pub fn new(tree: &'a ProcessTree) -> Result<Self> {
        ensure!(
            !tree.tree.is_empty(),
            "Cannot align an empty process tree: its language is empty."
        );
        let mut aligner = Self {
            tree,
            node_metadata: (0..tree.tree.len())
                .map(|_| NodeMetadata::default())
                .collect(),
        };
        let mut next_node_index = 0;
        let mut next_transition_index = 0;
        aligner.prepare_node(&mut next_node_index, &mut next_transition_index)?;
        ensure!(
            next_node_index == tree.tree.len()
                && next_transition_index == tree.transition2node.len(),
            "Invalid process tree structure or transition mapping."
        );
        Ok(aligner)
    }

    fn prepare_node(
        &mut self,
        next_node_index: &mut usize,
        next_transition_index: &mut usize,
    ) -> Result<usize> {
        let node_index = *next_node_index;
        let Some(node) = self.tree.tree.get(node_index) else {
            return Err(anyhow!("Incomplete process tree."));
        };
        *next_node_index += 1;
        match node {
            Node::Activity(_) | Node::Tau => {
                ensure!(
                    self.tree.transition2node.get(*next_transition_index) == Some(&node_index),
                    "Invalid transition mapping at node {node_index}."
                );
                self.node_metadata[node_index].transition_index = Some(*next_transition_index);
                *next_transition_index += 1;
                if let Node::Activity(activity) = node {
                    self.node_metadata[node_index].alphabet.insert(*activity);
                }
            }
            Node::Operator(operator, child_count) => {
                ensure!(
                    *child_count > 0,
                    "Operator at node {node_index} has no children."
                );
                let mut metadata = NodeMetadata::default();
                for child_rank in 0..*child_count {
                    let child_node = self.prepare_node(next_node_index, next_transition_index)?;
                    metadata.child_nodes.push(child_node);
                    for &activity in &self.node_metadata[child_node].alphabet {
                        ensure!(
                            metadata.alphabet.insert(activity),
                            "Dynamic alignments require unique activity labels; label {:?} occurs in multiple children of node {node_index}.",
                            self.tree.activity_key.get_activity_label(&activity)
                        );
                        metadata.activity_to_child_rank.insert(activity, child_rank);
                    }
                }
                metadata.redo_is_silent = *operator == Operator::Loop
                    && metadata.child_nodes.len() > 1
                    && metadata.child_nodes[1..]
                        .iter()
                        .all(|&child| self.node_metadata[child].alphabet.is_empty());
                metadata.is_unary_loop =
                    *operator == Operator::Loop && metadata.child_nodes.len() == 1;
                self.node_metadata[node_index] = metadata;
            }
        }
        Ok(node_index)
    }
}

impl Align for ProcessTreeAligner<'_> {
    fn align_language(
        &mut self,
        log: Box<dyn EbiTraitFiniteLanguage>,
    ) -> Result<LanguageOfAlignments> {
        align_language_with(&self.tree.activity_key, log, |trace| {
            self.align_trace(trace)
        })
    }

    fn align_stochastic_language(
        &mut self,
        log: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<StochasticLanguageOfAlignments> {
        align_stochastic_language_with(&self.tree.activity_key, log, |trace| {
            self.align_trace(trace)
        })
    }

    fn align_trace(&self, trace: &Vec<Activity>) -> Result<(Vec<Move>, usize)> {
        let mut solver = DynamicProgramming {
            tree: self.tree,
            node_metadata: &self.node_metadata,
            solution_cache: vec![HashMap::default(); self.node_metadata.len()],
            solutions: Vec::new(),
        };
        let root_solution = solver.solve(self.tree.root(), trace);
        let cost = solver.solutions[root_solution].cost + 1;
        let mut moves = Vec::new();
        solver.reconstruct(root_solution, &mut moves);
        moves.push(Move::SilentMove {
            transition: self.tree.transition2node.len(),
        });
        Ok((moves, cost))
    }
}

type SolutionIndex = usize;

struct Solution {
    node_index: usize,
    trace: Rc<[Activity]>,
    cost: usize,
    decision: Decision,
}

struct ChildAlignment {
    alignment: SolutionIndex,
    event_positions: Vec<usize>,
}

enum Decision {
    Leaf,
    Concatenate(Vec<SolutionIndex>),
    OrderedChildren(Vec<ChildAlignment>),
    Shuffle {
        child_alignments: Vec<Option<SolutionIndex>>,
        event_owners: Vec<Option<usize>>,
    },
}

struct DynamicProgramming<'a> {
    tree: &'a ProcessTree,
    node_metadata: &'a [NodeMetadata],
    solution_cache: Vec<HashMap<Rc<[Activity]>, SolutionIndex, FnvBuildHasher>>,
    solutions: Vec<Solution>,
}

#[derive(Clone)]
struct SequenceCandidate {
    event_positions: Vec<Vec<usize>>,
    log_skips: usize,
}

#[derive(Clone, Copy)]
enum LoopPhase {
    Body,
    Redo,
}

#[derive(Clone, Copy)]
struct LoopPrefix {
    cost: usize,
    last_link: Option<usize>,
}

struct LoopLink {
    previous_link: Option<usize>,
    solution: SolutionIndex,
}

struct InterleavedPlan {
    cost: usize,
    step: Option<InterleavedStep>,
}

struct InterleavedStep {
    child_rank: usize,
    alignment: SolutionIndex,
    projected_range: std::ops::Range<usize>,
    next_plan: usize,
}

struct InterleavedSearch<'a> {
    trace: &'a [Activity],
    child_nodes: &'a [usize],
    projected_traces: Vec<Vec<Activity>>,
    event_positions: Vec<Vec<usize>>,
    empty_alignments: Vec<SolutionIndex>,
    candidate_children: Vec<usize>,
    inactive_children_cost: usize,
    cached_plans: HashMap<(Vec<u64>, usize), usize, FnvBuildHasher>,
    plans: Vec<InterleavedPlan>,
}

impl DynamicProgramming<'_> {
    fn solve(&mut self, node_index: usize, trace: &[Activity]) -> SolutionIndex {
        if let Some(&result) = self.solution_cache[node_index].get(trace) {
            return result;
        }
        let (cost, decision) = match self.tree.tree[node_index] {
            Node::Tau => (10_000 * trace.len() + 1, Decision::Leaf),
            Node::Activity(activity) => (
                if trace.contains(&activity) {
                    10_000 * (trace.len() - 1)
                } else {
                    10_000 * (trace.len() + 1)
                },
                Decision::Leaf,
            ),
            Node::Operator(Operator::Sequence, _) => self.align_sequence(node_index, trace),
            Node::Operator(Operator::Xor, _) => {
                let node_metadata = self.node_metadata;
                let best = self.solve_choice(&node_metadata[node_index].child_nodes, trace);
                (self.solutions[best].cost, Decision::Concatenate(vec![best]))
            }
            Node::Operator(Operator::Concurrent, _) => self.align_shuffle(node_index, trace, false),
            Node::Operator(Operator::Or, _) => self.align_shuffle(node_index, trace, true),
            Node::Operator(Operator::Loop, _) => self.align_loop(node_index, trace),
            Node::Operator(Operator::Interleaved, _) => self.align_interleaved(node_index, trace),
        };
        let trace: Rc<[Activity]> = Rc::from(trace);
        let index = self.solutions.len();
        self.solutions.push(Solution {
            node_index,
            trace: Rc::clone(&trace),
            cost,
            decision,
        });
        self.solution_cache[node_index].insert(trace, index);
        index
    }

    fn solve_choice(&mut self, child_nodes: &[usize], trace: &[Activity]) -> SolutionIndex {
        let mut best = self.solve(child_nodes[0], trace);
        for &child_node in &child_nodes[1..] {
            if self.solutions[best].cost == 0 {
                break;
            }
            let candidate = self.solve(child_node, trace);
            if self.solutions[candidate].cost < self.solutions[best].cost {
                best = candidate;
            }
        }
        best
    }

    fn align_sequence(&mut self, node_index: usize, trace: &[Activity]) -> (usize, Decision) {
        let metadata = &self.node_metadata[node_index];
        let child_count = metadata.child_nodes.len();
        let mut candidates_by_child: Vec<Vec<SequenceCandidate>> = vec![Vec::new(); child_count];
        let mut last_event_by_child: Vec<Option<usize>> = vec![None; child_count];
        for (event_position, event) in trace.iter().enumerate() {
            let Some(&child_rank) = metadata.activity_to_child_rank.get(event) else {
                continue;
            };
            let mut extended_existing_candidate = false;
            let mut reference_position = 0;
            if let Some(previous_position) = last_event_by_child[child_rank] {
                reference_position = previous_position;
                last_event_by_child[child_rank] = Some(event_position);
                for candidate in &mut candidates_by_child[child_rank] {
                    candidate
                        .event_positions
                        .last_mut()
                        .unwrap()
                        .push(event_position);
                    candidate.log_skips += event_position - reference_position - 1;
                }
                extended_existing_candidate = true;
            }
            for previous_child_rank in (0..child_rank).rev() {
                if let Some(last_position) = last_event_by_child[previous_child_rank]
                    .filter(|&last_position| last_position >= reference_position)
                {
                    reference_position = last_position;
                    last_event_by_child[child_rank] = Some(event_position);
                    let extensions: Vec<_> = candidates_by_child[previous_child_rank]
                        .iter()
                        .map(|previous| {
                            let mut next = previous.clone();
                            next.event_positions.resize(child_rank, Vec::new());
                            next.event_positions.push(vec![event_position]);
                            next.log_skips += event_position - reference_position - 1;
                            next
                        })
                        .collect();
                    candidates_by_child[child_rank].extend(extensions);
                    extended_existing_candidate = true;
                }
            }
            if !extended_existing_candidate {
                let mut event_positions = vec![Vec::new(); child_rank];
                event_positions.push(vec![event_position]);
                candidates_by_child[child_rank] = vec![SequenceCandidate {
                    event_positions,
                    log_skips: event_position,
                }];
                last_event_by_child[child_rank] = Some(event_position);
            }
        }
        let mut complete_candidates = Vec::new();
        let mut reference_position = 0;
        for child_rank in (0..child_count).rev() {
            if let Some(last_position) = last_event_by_child[child_rank]
                .filter(|&last_position| last_position >= reference_position)
            {
                for mut candidate in candidates_by_child[child_rank].drain(..) {
                    candidate.event_positions.resize(child_count, Vec::new());
                    candidate.log_skips += trace.len() - last_position - 1;
                    complete_candidates.push(candidate);
                }
                reference_position = last_position;
            }
        }
        if complete_candidates.is_empty() {
            complete_candidates.push(SequenceCandidate {
                event_positions: vec![Vec::new(); child_count],
                log_skips: trace.len(),
            });
        }
        let node_metadata = self.node_metadata;
        let mut best_cost = usize::MAX;
        let mut best_child_alignments = Vec::new();
        for candidate in complete_candidates {
            let mut cost = 10_000 * candidate.log_skips;
            let mut child_alignments = Vec::with_capacity(child_count);
            for (&child, event_positions) in node_metadata[node_index]
                .child_nodes
                .iter()
                .zip(candidate.event_positions)
            {
                let child_trace: Vec<_> = event_positions
                    .iter()
                    .map(|&event_position| trace[event_position])
                    .collect();
                let child_solution = self.solve(child, &child_trace);
                cost += self.solutions[child_solution].cost;
                child_alignments.push(ChildAlignment {
                    alignment: child_solution,
                    event_positions,
                });
            }
            if cost < best_cost {
                best_cost = cost;
                best_child_alignments = child_alignments;
                if cost == 0 {
                    break;
                }
            }
        }
        (best_cost, Decision::OrderedChildren(best_child_alignments))
    }

    fn align_interleaved(&mut self, node_index: usize, trace: &[Activity]) -> (usize, Decision) {
        let node_metadata = self.node_metadata;
        let metadata = &node_metadata[node_index];
        let child_count = metadata.child_nodes.len();
        let mut event_positions = vec![Vec::new(); child_count];
        let mut projected_traces = vec![Vec::new(); child_count];
        let mut execution_order = Vec::new();
        let mut seen_children = vec![false; child_count];
        let mut previous_child_rank = None;
        let mut children_form_contiguous_blocks = true;
        let mut unmatched_event_count = 0;
        for (position, activity) in trace.iter().enumerate() {
            if let Some(&child_rank) = metadata.activity_to_child_rank.get(activity) {
                event_positions[child_rank].push(position);
                projected_traces[child_rank].push(*activity);
                if previous_child_rank != Some(child_rank) {
                    children_form_contiguous_blocks &= !seen_children[child_rank];
                    if !seen_children[child_rank] {
                        execution_order.push(child_rank);
                        seen_children[child_rank] = true;
                    }
                    previous_child_rank = Some(child_rank);
                }
            } else {
                unmatched_event_count += 1;
            }
        }
        let projected_alignments: Vec<_> = metadata
            .child_nodes
            .iter()
            .zip(&projected_traces)
            .map(|(&child_node, projection)| self.solve(child_node, projection))
            .collect();
        if children_form_contiguous_blocks {
            execution_order
                .extend((0..child_count).filter(|&child_rank| !seen_children[child_rank]));
            let cost = 10_000 * unmatched_event_count
                + projected_alignments
                    .iter()
                    .map(|&result| self.solutions[result].cost)
                    .sum::<usize>();
            let child_alignments = execution_order
                .into_iter()
                .map(|child_rank| ChildAlignment {
                    alignment: projected_alignments[child_rank],
                    event_positions: std::mem::take(&mut event_positions[child_rank]),
                })
                .collect();
            return (cost, Decision::OrderedChildren(child_alignments));
        }
        let empty_alignments: Vec<_> = metadata
            .child_nodes
            .iter()
            .map(|&child_node| self.solve(child_node, &[]))
            .collect();
        let mut candidate_children = Vec::new();
        let mut inactive_children_cost = 0;
        for child_rank in 0..child_count {
            let empty_cost = self.solutions[empty_alignments[child_rank]].cost;
            if self.solutions[projected_alignments[child_rank]].cost
                < 10_000 * (projected_traces[child_rank].len()) + empty_cost
            {
                candidate_children.push(child_rank);
            } else {
                inactive_children_cost += empty_cost;
            }
        }
        let mut used_children = vec![0u64; candidate_children.len().div_ceil(64)];
        let mut search = InterleavedSearch {
            trace,
            child_nodes: &metadata.child_nodes,
            projected_traces,
            event_positions,
            empty_alignments,
            candidate_children,
            inactive_children_cost,
            cached_plans: HashMap::default(),
            plans: Vec::new(),
        };
        let mut plan_index = search.solve_suffix(self, &mut used_children, 0);
        let cost = search.plans[plan_index].cost;
        let mut executed_children = vec![false; child_count];
        let mut child_alignments = Vec::with_capacity(child_count);
        while let Some(step) = &search.plans[plan_index].step {
            child_alignments.push(ChildAlignment {
                alignment: step.alignment,
                event_positions: search.event_positions[step.child_rank]
                    [step.projected_range.clone()]
                .to_vec(),
            });
            executed_children[step.child_rank] = true;
            plan_index = step.next_plan;
        }
        for (child_rank, &was_executed) in executed_children.iter().enumerate() {
            if !was_executed {
                child_alignments.push(ChildAlignment {
                    alignment: search.empty_alignments[child_rank],
                    event_positions: Vec::new(),
                });
            }
        }
        (cost, Decision::OrderedChildren(child_alignments))
    }

    fn align_shuffle(
        &mut self,
        node_index: usize,
        trace: &[Activity],
        allow_skipping_children: bool,
    ) -> (usize, Decision) {
        let node_metadata = self.node_metadata;
        let metadata = &node_metadata[node_index];
        let mut projected_traces = vec![Vec::new(); metadata.child_nodes.len()];
        let event_owners: Vec<_> = trace
            .iter()
            .map(|activity| {
                let owner = metadata.activity_to_child_rank.get(activity).copied();
                if let Some(i) = owner {
                    projected_traces[i].push(*activity);
                }
                owner
            })
            .collect();
        let child_solutions: Vec<_> = metadata
            .child_nodes
            .iter()
            .zip(&projected_traces)
            .map(|(&child, trace)| self.solve(child, trace))
            .collect();
        let mut selected = vec![true; child_solutions.len()];
        if allow_skipping_children {
            for (i, &result) in child_solutions.iter().enumerate() {
                selected[i] = self.solutions[result].cost < 10_000 * (projected_traces[i].len());
            }
            if !selected.iter().any(|&take| take) {
                let best = (0..child_solutions.len())
                    .min_by_key(|&i| {
                        self.solutions[child_solutions[i]].cost
                            - 10_000 * (projected_traces[i].len())
                    })
                    .unwrap();
                selected[best] = true;
            }
        }
        let mut cost = 10_000 * trace.len();
        let mut child_alignments = vec![None; child_solutions.len()];
        for (i, &result) in child_solutions.iter().enumerate() {
            if selected[i] {
                cost -= 10_000 * (projected_traces[i].len());
                cost += self.solutions[result].cost;
                child_alignments[i] = Some(result);
            }
        }
        let event_owners = event_owners
            .into_iter()
            .map(|owner| owner.filter(|&i| selected[i]))
            .collect();
        (
            cost,
            Decision::Shuffle {
                child_alignments,
                event_owners,
            },
        )
    }

    fn solve_loop_phase(
        &mut self,
        node_index: usize,
        trace: &[Activity],
        trace_start: usize,
        trace_end: usize,
        phase: LoopPhase,
    ) -> SolutionIndex {
        let node_metadata = self.node_metadata;
        let child_nodes = &node_metadata[node_index].child_nodes;
        match phase {
            LoopPhase::Body => self.solve(child_nodes[0], &trace[trace_start..trace_end]),
            LoopPhase::Redo => self.solve_choice(&child_nodes[1..], &trace[trace_start..trace_end]),
        }
    }

    fn align_loop(&mut self, node_index: usize, trace: &[Activity]) -> (usize, Decision) {
        let node_metadata = self.node_metadata;
        if node_metadata[node_index].is_unary_loop {
            let body = self.solve(node_metadata[node_index].child_nodes[0], trace);
            return (self.solutions[body].cost, Decision::Concatenate(vec![body]));
        }
        let whole_body = self.solve_loop_phase(node_index, trace, 0, trace.len(), LoopPhase::Body);
        let upper_bound = self.solutions[whole_body].cost;
        if upper_bound == 0 || trace.is_empty() {
            return (upper_bound, Decision::Concatenate(vec![whole_body]));
        }
        let empty_redo = self.solve_loop_phase(node_index, trace, 0, 0, LoopPhase::Redo);
        let empty_redo_cost = self.solutions[empty_redo].cost;
        let redo_is_silent = node_metadata[node_index].redo_is_silent;

        let mut links = Vec::<LoopLink>::new();
        let mut after_body = Vec::<LoopPrefix>::with_capacity(trace.len() + 1);
        let mut after_redo = vec![LoopPrefix {
            cost: 0,
            last_link: None,
        }];
        for prefix_end in 0..=trace.len() {
            let mut best = LoopPrefix {
                cost: upper_bound + 1,
                last_link: None,
            };
            let mut best_link = None;
            for (phase_start, prefix) in after_redo.iter().enumerate() {
                if prefix.cost < best.cost {
                    let solution = self.solve_loop_phase(
                        node_index,
                        trace,
                        phase_start,
                        prefix_end,
                        LoopPhase::Body,
                    );
                    let cost = prefix.cost + self.solutions[solution].cost;
                    if cost < best.cost {
                        best.cost = cost;
                        best_link = Some(LoopLink {
                            previous_link: prefix.last_link,
                            solution,
                        });
                        if cost == 0 {
                            break;
                        }
                    }
                }
            }
            if let Some(link) = best_link {
                best.last_link = Some(links.len());
                links.push(link);
            }
            after_body.push(best);
            if prefix_end == trace.len() {
                break;
            }
            if best.cost + empty_redo_cost < after_redo[prefix_end].cost {
                after_redo[prefix_end] = LoopPrefix {
                    cost: best.cost + empty_redo_cost,
                    last_link: Some(links.len()),
                };
                links.push(LoopLink {
                    previous_link: best.last_link,
                    solution: empty_redo,
                });
            }
            let mut best = LoopPrefix {
                cost: upper_bound + 1,
                last_link: None,
            };
            let mut best_link = None;
            let mut best_silent_redo_start = None;
            for (phase_start, prefix) in after_body.iter().enumerate() {
                if prefix.cost < best.cost {
                    let solution = if redo_is_silent {
                        None
                    } else {
                        Some(self.solve_loop_phase(
                            node_index,
                            trace,
                            phase_start,
                            prefix_end + 1,
                            LoopPhase::Redo,
                        ))
                    };
                    let cost = prefix.cost
                        + solution.map_or_else(
                            || 10_000 * (prefix_end + 1 - phase_start) + empty_redo_cost,
                            |s| self.solutions[s].cost,
                        );
                    if cost < best.cost {
                        best.cost = cost;
                        if let Some(solution) = solution {
                            best_link = Some(LoopLink {
                                previous_link: prefix.last_link,
                                solution,
                            });
                        } else {
                            best_silent_redo_start = Some(phase_start);
                        }
                        if cost == 0 {
                            break;
                        }
                    }
                }
            }
            if let Some(phase_start) = best_silent_redo_start {
                let solution = self.solve_loop_phase(
                    node_index,
                    trace,
                    phase_start,
                    prefix_end + 1,
                    LoopPhase::Redo,
                );
                best_link = Some(LoopLink {
                    previous_link: after_body[phase_start].last_link,
                    solution,
                });
            }
            if let Some(link) = best_link {
                best.last_link = Some(links.len());
                links.push(link);
            }
            after_redo.push(best);
        }
        let best = after_body[trace.len()];
        debug_assert!(best.cost <= upper_bound);
        let mut phases = Vec::new();
        let mut last_link = best.last_link;
        while let Some(link_index) = last_link {
            phases.push(links[link_index].solution);
            last_link = links[link_index].previous_link;
        }
        phases.reverse();
        (best.cost, Decision::Concatenate(phases))
    }

    fn reconstruct(&self, solution_index: SolutionIndex, moves: &mut Vec<Move>) {
        let solution = &self.solutions[solution_index];
        match &solution.decision {
            Decision::Leaf => self.reconstruct_leaf(solution, moves),
            Decision::Concatenate(parts) => {
                for &part in parts {
                    self.reconstruct(part, moves);
                }
            }
            Decision::OrderedChildren(children) => {
                self.reconstruct_ordered_children(&solution.trace, children, moves);
            }
            Decision::Shuffle {
                child_alignments,
                event_owners,
            } => {
                self.reconstruct_shuffle(&solution.trace, child_alignments, event_owners, moves);
            }
        }
    }

    fn reconstruct_leaf(&self, solution: &Solution, moves: &mut Vec<Move>) {
        let trace = &solution.trace;
        let transition = self.node_metadata[solution.node_index]
            .transition_index
            .unwrap();
        let activity = match self.tree.tree[solution.node_index] {
            Node::Activity(activity) => Some(activity),
            Node::Tau => None,
            _ => unreachable!(),
        };
        let matched_position =
            activity.and_then(|activity| trace.iter().position(|&event| event == activity));
        for (position, &event) in trace.iter().enumerate() {
            moves.push(if matched_position == Some(position) {
                Move::SynchronousMove {
                    activity: event,
                    transition,
                }
            } else {
                Move::LogMove { activity: event }
            });
        }
        match (activity, matched_position) {
            (None, _) => moves.push(Move::SilentMove { transition }),
            (Some(activity), None) => moves.push(Move::ModelMove {
                activity,
                transition,
            }),
            _ => (),
        }
    }

    fn reconstruct_ordered_children(
        &self,
        trace: &[Activity],
        children: &[ChildAlignment],
        moves: &mut Vec<Move>,
    ) {
        let mut trace_position = 0;
        for child in children {
            let mut child_moves = Vec::new();
            self.reconstruct(child.alignment, &mut child_moves);
            let mut child_moves = child_moves.into_iter();
            for &event_position in &child.event_positions {
                while trace_position < event_position {
                    moves.push(Move::LogMove {
                        activity: trace[trace_position],
                    });
                    trace_position += 1;
                }
                Self::append_until_consumed(&mut child_moves, moves);
                trace_position += 1;
            }
            moves.extend(child_moves);
        }
        moves.extend(
            trace[trace_position..]
                .iter()
                .map(|&activity| Move::LogMove { activity }),
        );
    }

    fn reconstruct_shuffle(
        &self,
        trace: &[Activity],
        child_alignments: &[Option<SolutionIndex>],
        event_owners: &[Option<usize>],
        moves: &mut Vec<Move>,
    ) {
        let mut child_moves: Vec<_> = child_alignments
            .iter()
            .map(|alignment| {
                let mut branch = Vec::new();
                if let Some(alignment) = alignment {
                    self.reconstruct(*alignment, &mut branch);
                }
                branch.into_iter()
            })
            .collect();
        for (&activity, &owner) in trace.iter().zip(event_owners) {
            if let Some(child_rank) = owner {
                Self::append_until_consumed(&mut child_moves[child_rank], moves);
            } else {
                moves.push(Move::LogMove { activity });
            }
        }
        for branch in child_moves {
            moves.extend(branch);
        }
    }

    fn append_until_consumed(branch: &mut impl Iterator<Item = Move>, moves: &mut Vec<Move>) {
        loop {
            let next = branch
                .next()
                .expect("one consuming move per assigned event");
            let consumes = matches!(next, Move::LogMove { .. } | Move::SynchronousMove { .. });
            moves.push(next);
            if consumes {
                break;
            }
        }
    }
}

impl InterleavedSearch<'_> {
    fn solve_suffix(
        &mut self,
        solver: &mut DynamicProgramming<'_>,
        used_children: &mut [u64],
        trace_start: usize,
    ) -> usize {
        let key = (used_children.to_vec(), trace_start);
        if let Some(&plan_index) = self.cached_plans.get(&key) {
            return plan_index;
        }
        let mut best = InterleavedPlan {
            cost: 10_000 * (self.trace.len() - trace_start) + self.inactive_children_cost,
            step: None,
        };
        let mut lower_bound = best.cost;
        let mut candidates = Vec::new();
        for (candidate_slot, &child_rank) in self.candidate_children.iter().enumerate() {
            if used_children[candidate_slot / 64] & (1u64 << (candidate_slot % 64)) != 0 {
                continue;
            }
            let empty_cost = solver.solutions[self.empty_alignments[child_rank]].cost;
            best.cost += empty_cost;
            let projection_start = self.event_positions[child_rank]
                .partition_point(|&position| position < trace_start);
            let projected_trace = &self.projected_traces[child_rank][projection_start..];
            let full = solver.solve(self.child_nodes[child_rank], projected_trace);
            let full_cost = solver.solutions[full].cost;
            lower_bound -= 10_000 * projected_trace.len();
            lower_bound += full_cost;
            if full_cost < 10_000 * projected_trace.len() + empty_cost {
                candidates.push((candidate_slot, child_rank, projection_start));
            }
        }
        if best.cost > lower_bound {
            candidates.sort_by_key(|&(_, child_rank, projection_start)| {
                self.event_positions[child_rank][projection_start]
            });
            for (candidate_slot, child_rank, projection_start) in candidates {
                used_children[candidate_slot / 64] |= 1u64 << (candidate_slot % 64);
                for projection_end in
                    (projection_start + 1..=self.projected_traces[child_rank].len()).rev()
                {
                    let next_trace_start = self.event_positions[child_rank][projection_end - 1] + 1;
                    let alignment = solver.solve(
                        self.child_nodes[child_rank],
                        &self.projected_traces[child_rank][projection_start..projection_end],
                    );
                    let block_cost = 10_000
                        * (next_trace_start - trace_start - (projection_end - projection_start))
                        + solver.solutions[alignment].cost;
                    if block_cost >= best.cost {
                        continue;
                    }
                    let next_plan = self.solve_suffix(solver, used_children, next_trace_start);
                    let cost = block_cost + self.plans[next_plan].cost;
                    if cost < best.cost {
                        best = InterleavedPlan {
                            cost,
                            step: Some(InterleavedStep {
                                child_rank,
                                alignment,
                                projected_range: projection_start..projection_end,
                                next_plan,
                            }),
                        };
                        if cost == lower_bound {
                            break;
                        }
                    }
                }
                used_children[candidate_slot / 64] &= !(1u64 << (candidate_slot % 64));
                if best.cost == lower_bound {
                    break;
                }
            }
        }
        debug_assert!(best.cost >= lower_bound);
        let plan_index = self.plans.len();
        self.plans.push(best);
        self.cached_plans.insert(key, plan_index);
        plan_index
    }
}

fn align_language_with<F>(
    model_activity_key: &ActivityKey,
    log: Box<dyn EbiTraitFiniteLanguage>,
    align_trace: F,
) -> Result<LanguageOfAlignments>
where
    F: Fn(&Vec<Activity>) -> Result<(Vec<Move>, usize)> + Sync,
{
    let mut activity_key = model_activity_key.clone();
    let translator = Arc::new(ActivityKeyTranslator::new(
        log.activity_key(),
        &mut activity_key,
    ));

    let progress_bar = EbiCommand::get_progress_bar_ticks(log.number_of_traces());

    let mut aligned_traces = log
        .par_iter_traces()
        .map(|trace| {
            let translator = Arc::clone(&translator);

            let trace_translated = translator.translate_trace(trace);

            let result = align_trace(&trace_translated)
                .with_context(|| format!("Aligning trace {:?}", trace))?;
            progress_bar.inc(1);
            Ok(result.0)
        })
        .collect::<Result<Vec<_>>>()
        .with_context(|| anyhow!("Computing alignments."))?;

    progress_bar.finish_and_clear();

    let mut result = LanguageOfAlignments::new(activity_key);
    result.append(&mut aligned_traces);
    Ok(result)
}

fn align_stochastic_language_with<F>(
    model_activity_key: &ActivityKey,
    log: Box<dyn EbiTraitFiniteStochasticLanguage>,
    align_trace: F,
) -> Result<StochasticLanguageOfAlignments>
where
    F: Fn(&Vec<Activity>) -> Result<(Vec<Move>, usize)> + Sync,
{
    let mut activity_key = model_activity_key.clone();
    let translator = Arc::new(ActivityKeyTranslator::new(
        log.activity_key(),
        &mut activity_key,
    ));

    let progress_bar = EbiCommand::get_progress_bar_ticks(log.number_of_traces());

    let aligned_traces = log
        .par_iter_traces_probabilities()
        .map(|(trace, probability)| {
            let translator = Arc::clone(&translator);

            let trace_translated = translator.translate_trace(&trace);

            let result = align_trace(&trace_translated)
                .with_context(|| format!("Aligning trace {:?}", trace))?;
            progress_bar.inc(1);

            Ok((result.0, probability.clone()))
        })
        .collect::<Result<Vec<_>>>()?;

    progress_bar.finish_and_clear();

    let mut result = StochasticLanguageOfAlignments::new(activity_key);
    result.append(aligned_traces);
    Ok(result)
}
