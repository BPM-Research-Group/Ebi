use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    techniques::stochastic_markovian_abstraction::MarkovianAbstraction,
};
use ebi_objects::{
    Activity, BusinessProcessModelAndNotation, FiniteStochasticLanguage, HasActivityKey,
    IntoRefTraceIterator, IntoRefTraceProbabilityIterator,
    ebi_arithmetic::{Fraction, Signed, Zero, f},
};
use intmap::IntMap;
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
};
use uuid::Uuid;

pub trait SplitMiner {
    fn split_miner(&self) -> BusinessProcessModelAndNotation;
}

impl SplitMiner for dyn EbiTraitFiniteStochasticLanguage {
    fn split_miner(&self) -> BusinessProcessModelAndNotation {
        split_miner(self, &SplitMinerParameters::default())
    }
}

#[derive(Clone)]
pub struct SplitMinerParameters {
    replaceIORs: bool,
    removeLoopActivities: bool,
    percentileFrequencyThreshold: Fraction,
    parallelismsThreshold: Fraction,
    parallelismsFirst: bool,
    structuringTime: StructuringTime,
    filterType: FilterType,
}

impl Default for SplitMinerParameters {
    fn default() -> Self {
        Self {
            replaceIORs: true,
            removeLoopActivities: false,
            percentileFrequencyThreshold: f!(4, 10),
            parallelismsThreshold: f!(1, 10),
            parallelismsFirst: false,
            structuringTime: StructuringTime::None,
            filterType: FilterType::FWG,
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
pub enum StructuringTime {
    None,
    Post,
    Pre,
}

#[derive(Clone)]
pub enum FilterType {
    STD,
    NOF,
    FWG,
    WTH,
}

pub fn split_miner(
    log: &dyn EbiTraitFiniteStochasticLanguage,
    parameters: &SplitMinerParameters,
) -> BusinessProcessModelAndNotation {
    let mut slang = log.to_finite_stochastic_language();

    //add start and end activities to each trace
    let (start_activity, end_activity) =
        MarkovianAbstraction::create_start_end(slang.activity_key_mut());

    let dfgp = generateDFGP(&slang, parameters, start_activity, end_activity);
    let mut bpmnDiagram = transformDFGPintoBPMN(dfgp);
    if parameters.structuringTime == StructuringTime::Post {
        bpmnDiagram = structure(bpmnDiagram);
    }
    bpmnDiagram
}

fn generateDFGP<'a>(
    log: &'a FiniteStochasticLanguage,
    parameters: &'a SplitMinerParameters,
    start_activity: Activity,
    end_activity: Activity,
) -> DirectlyFollowGraphPlus<'a> {
    let mut dfgp = DirectlyFollowGraphPlus::new(log, parameters, start_activity, end_activity);
    dfgp.oracle = !parameters.parallelismsFirst;
    dfgp.buildDFGP();
    dfgp
}

struct DirectlyFollowGraphPlus<'a> {
    log: &'a FiniteStochasticLanguage,
    startcode: Activity,
    endcode: Activity,

    edges: HashSet<DFGEdge>,
    edge_2_frequency: HashMap<Uuid, Fraction>,
    nodes: IntMap<Activity, DFGNode>,
    node_2_frequency: HashMap<Uuid, Fraction>,
    outgoings: IntMap<Activity, HashSet<DFGEdge>>,
    incomings: IntMap<Activity, HashSet<DFGEdge>>,
    dfgp: IntMap<Activity, IntMap<Activity, DFGEdge>>,

    loopsL1: HashSet<Activity>,
    loopsL1Freq: IntMap<Activity, Fraction>,
    loopsL2: HashSet<DFGEdge>,
    parallelisms: IntMap<Activity, HashSet<Activity>>,
    concurrencyMatrix: Option<Vec<Fraction>>,
    bestEdges: HashSet<DFGEdge>,
    untouchableEdges: Option<HashSet<DFGEdge>>,
    potentialConcurrency: HashSet<DFGEdge>,
    relations: Option<HashMap<(usize, usize), Gate>>,

    parameters: SplitMinerParameters,

    filterThreshold: Option<usize>,

    oracle: bool,

    zero: Fraction,
}

impl<'a> DirectlyFollowGraphPlus<'a> {
    fn new(
        log: &'a FiniteStochasticLanguage,
        parameters: &SplitMinerParameters,
        start_activity: Activity,
        end_activity: Activity,
    ) -> Self {
        Self {
            log,
            startcode: start_activity,
            endcode: end_activity,
            edges: HashSet::new(),
            edge_2_frequency: HashMap::new(),
            nodes: IntMap::new(),
            node_2_frequency: HashMap::new(),
            outgoings: IntMap::new(),
            incomings: IntMap::new(),
            dfgp: IntMap::new(),
            loopsL1: HashSet::new(),
            loopsL1Freq: IntMap::new(),
            loopsL2: HashSet::new(),
            parallelisms: IntMap::new(),
            concurrencyMatrix: None,
            bestEdges: HashSet::new(),
            untouchableEdges: None,
            potentialConcurrency: HashSet::new(),
            relations: None,
            parameters: parameters.clone(),
            filterThreshold: None,
            oracle: false,
            zero: Fraction::zero(),
        }
    }

    fn buildDFGP(&mut self) {
        self.untouchableEdges = None;

        self.buildDirectlyFollowsGraph();
        self.detectLoops(); //depends on buildDirectlyFollowsGraph()
        if self.oracle {
            detectRelationsOnLog()
        } else {
            self.detectParallelismsOnDFG()
        } //depends on detectLoops()

        match self.parameters.filterType {
            FilterType::STD => {
                standardFilter();
                exploreAndRemove();
            }
            FilterType::NOF => {}
            FilterType::FWG => {
                filterWithGuarantees();
            }
            FilterType::WTH => {
                filterWithThreshold();
                exploreAndRemove();
            }
        }
    }

    fn buildDirectlyFollowsGraph(&mut self) {
        let autogen_start = DFGNode::new(self.startcode);
        self.addNode(autogen_start.clone());
        //        while parsing the simple log we will always skip the start event,
        //        so we set now the maximum frequency because it is an artificial start event
        self.increase_node_frequency(autogen_start.id, &self.log.get_probability_sum());

        let autogen_end = DFGNode::new(self.endcode);
        self.addNode(autogen_end);

        for (trace, trace_frequency) in self.log.iter_traces_probabilities() {
            let mut trace_it = trace.iter();
            //            consuming the start event that is always 0
            trace_it.next();

            let mut prevEvent = self.startcode;
            let mut prevNode = autogen_start.clone();

            while let Some(event) = trace_it.next() {
                //                we read the next event of the trace until it is finished

                if prevEvent == *event {
                    if self.loopsL1.contains(event) {
                        *self.loopsL1Freq.get_mut(*event).unwrap() += trace_frequency;
                    } else {
                        self.loopsL1.insert(*event);
                        self.loopsL1Freq.insert(*event, trace_frequency.clone());
                    }
                    continue;
                }

                let node;
                if !self.nodes.contains_key(*event) {
                    node = DFGNode::new(*event);
                    self.addNode(node.clone());
                } else {
                    node = self.nodes.get(*event).unwrap().clone();
                }

                //                  increasing frequency of this event occurrence
                self.increase_node_frequency(node.id, trace_frequency);

                if !self.dfgp.contains_key(prevEvent)
                    || !self.dfgp.get(prevEvent).unwrap().contains_key(*event)
                {
                    let edge = DFGEdge::new(prevNode, node.clone());
                    self.addEdge(edge, trace_frequency.clone());
                }

                //                  increasing frequency of this directly following relationship
                let edge_id = self.dfgp.get(prevEvent).unwrap().get(*event).unwrap().id;
                self.increase_edge_frequency(edge_id, trace_frequency);

                prevEvent = *event;
                prevNode = node;
            }
        }
    }

    fn detectLoops(&mut self) {
        self.loopsL2.clear();
        for e1 in &self.edges {
            let src = e1.getSourceCode();
            let tgt = e1.getTargetCode();

            //            if src OR tgt are length 1 loops, we do not evaluate length 2 loops for this edge,
            //            because a length 1 loop in parallel with something else
            //            can generate pattern of the type [src :: tgt :: src] OR [tgt :: src :: tgt]
            if !self.loopsL2.contains(e1)
                && self.dfgp.get(tgt).unwrap().contains_key(src)
                && !self.loopsL1.contains(&src)
                && !self.loopsL1.contains(&tgt)
            {
                let e2 = self.dfgp.get(tgt).unwrap().get(src).unwrap();

                let src2tgt_loop2_pattern = vec![src, tgt, src];
                let tgt2src_loop2_pattern = vec![tgt, src, tgt];
                let mut src2tgt_loop2_frequency = 0;
                let mut tgt2src_loop2_frequency = 0;

                for trace in self.log.iter_traces() {
                    src2tgt_loop2_frequency += trace
                        .windows(3)
                        .filter(|x| *x == src2tgt_loop2_pattern)
                        .count();
                    tgt2src_loop2_frequency += trace
                        .windows(3)
                        .filter(|x: &&[Activity]| *x == tgt2src_loop2_pattern)
                        .count();
                }

                let loop2score = src2tgt_loop2_frequency + tgt2src_loop2_frequency;

                //                if the loop2score is not zero, it means we found patterns of the type:
                //                [src :: tgt :: src] OR [tgt :: src :: tgt], so we set both edges as short-loops
                if loop2score != 0 {
                    self.loopsL2.insert(e1.clone());
                    self.loopsL2.insert(e2.clone());
                }
            }
        }
    }

    fn detectParallelismsOnDFG(&mut self) {
        let mut removable_edges = HashSet::new();

        if self.parameters.parallelismsThreshold.is_zero() {
            return;
        }

        for e1 in &self.edges {
            let src = e1.getSourceCode();
            let tgt = e1.getTargetCode();

            let priority_check = if self.parameters.parallelismsFirst {
                !self.loopsL2.contains(&e1)
            } else {
                !self.loopsL2.contains(&e1)
                    && !self.loopsL1.contains(&src)
                    && !self.loopsL1.contains(&tgt)
            };

            if self.dfgp.get(tgt).unwrap().contains_key(src)
                && priority_check
                && !removable_edges.contains(&e1)
            {
                //                this means: src || tgt is candidate parallelism
                let e2 = self.dfgp.get(tgt).unwrap().get(src).unwrap();

                let src2tgt_frequency = self.get_edge_frequency(&e1);
                let tgt2src_frequency = self.get_edge_frequency(&e2);
                let parallelism_score = (src2tgt_frequency - tgt2src_frequency)
                    / (src2tgt_frequency + tgt2src_frequency);

                if parallelism_score.clone().abs() < self.parameters.parallelismsThreshold {
                    //                    if parallelismScore is less than the threshold epslon,
                    //                    we set src || tgt and vice-versa, and we remove e1 and e2
                    if !self.parallelisms.contains_key(src) {
                        self.parallelisms.insert(src, HashSet::new());
                    }
                    self.parallelisms.get_mut(src).unwrap().insert(tgt);
                    if !self.parallelisms.contains_key(tgt) {
                        self.parallelisms.insert(tgt, HashSet::new());
                    }
                    self.parallelisms.get_mut(tgt).unwrap().insert(src);
                    removable_edges.insert(e1);
                    removable_edges.insert(e2);
                //                        totalParallelisms+=2;
                } else {
                    //                    otherwise we remove the least frequent edge, e1 or e2
                    if parallelism_score.is_positive() {
                        removable_edges.insert(e2);
                    } else {
                        removable_edges.insert(e1);
                    }
                    //                        notParallel++;
                }
            }
        }

        let mut orderedRemovableEdges = removable_edges.into_iter().cloned().collect::<Vec<_>>();
        orderedRemovableEdges.sort();
        for re in orderedRemovableEdges {
            if !self.removeEdge(&re, true) {
                //                System.out.println("DEBUG - impossible remove: " + re.print());
                let src = re.getSourceCode();
                let tgt = re.getTargetCode();
                if self.parallelisms.contains_key(src) {
                    self.parallelisms.get_mut(src).unwrap().remove(&tgt);
                }
                if self.parallelisms.contains_key(tgt) {
                    self.parallelisms.get_mut(tgt).unwrap().remove(&src);
                }
                if let Some(e) = self.dfgp.get(tgt).unwrap().get(src)
                    && *e == re
                {
                    self.removeEdge(&re, true);
                }
            }
            //            else { confirmedParallelisms++; }
        }
    }

    fn addNode(&mut self, n: DFGNode) {
        let code = n.code;

        self.nodes.insert(code, n.clone());
        if !self.incomings.contains_key(code) {
            self.incomings.insert(code, HashSet::new());
        }
        if !self.outgoings.contains_key(code) {
            self.outgoings.insert(code, HashSet::new());
        }
        if !self.dfgp.contains_key(code) {
            self.dfgp.insert(code, IntMap::new());
        }
        self.node_2_frequency.insert(n.id, Fraction::zero());
    }

    fn addEdge(&mut self, e: DFGEdge, frequency: Fraction) {
        let src = e.getSourceCode();
        let tgt = e.getTargetCode();

        self.edges.insert(e.clone());
        self.incomings.get_mut(tgt).unwrap().insert(e.clone());
        self.outgoings.get_mut(src).unwrap().insert(e.clone());
        self.dfgp.get_mut(src).unwrap().insert(tgt, e.clone());
        self.edge_2_frequency.insert(e.id, frequency);
    }

    fn removeEdge(&mut self, e: &DFGEdge, safe: bool) -> bool {
        let src = e.getSourceCode();
        let tgt = e.getTargetCode();
        if let Some(untouchableEdges) = &self.untouchableEdges
            && untouchableEdges.contains(&e)
        {
            // System.out.println("DEBUG - this edge ensures connectedness! not removable!");
            return false;
        }
        if safe && (self.incomings.get(tgt).unwrap().len() == 1)
            || (self.outgoings.get(src).unwrap().len() == 1)
        {
            return false;
        }
        self.incomings.get_mut(tgt).unwrap().remove(&e);
        self.outgoings.get_mut(src).unwrap().remove(&e);
        self.dfgp.get_mut(src).unwrap().remove(tgt);
        self.edges.remove(&e);
        return true;
        //        System.out.println("DEBUG - removed edge: " + src + " -> " + tgt);
    }

    fn increase_edge_frequency(&mut self, e: Uuid, frequency: &Fraction) {
        *self.edge_2_frequency.get_mut(&e).unwrap() += frequency;
    }

    fn increase_node_frequency(&mut self, n: Uuid, frequency: &Fraction) {
        *self.node_2_frequency.get_mut(&n).unwrap() += frequency;
    }

    fn get_edge_frequency(&self, edge: &DFGEdge) -> &Fraction {
        self.edge_2_frequency.get(&edge.id).unwrap_or(&self.zero)
    }
}

#[derive(Clone)]
struct DFGEdge {
    isLoop: bool,
    id: Uuid,
    label: Option<String>,
    source: DFGNode,
    target: DFGNode,
}

impl DFGEdge {
    fn new(source: DFGNode, target: DFGNode) -> Self {
        Self {
            isLoop: false,
            id: uuid::Uuid::new_v4(),
            label: None,
            source,
            target,
        }
    }

    fn getSourceCode(&self) -> Activity {
        return self.source.code;
    }

    fn getTargetCode(&self) -> Activity {
        return self.target.code;
    }
}

impl PartialEq for DFGEdge {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for DFGEdge {}

impl Hash for DFGEdge {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl Ord for DFGEdge {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl PartialOrd for DFGEdge {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

#[derive(Clone)]
struct DFGNode {
    id: Uuid,
    code: Activity,
}

impl DFGNode {
    fn new(code: Activity) -> Self {
        Self {
            id: Uuid::new_v4(),
            code,
        }
    }
}

enum Gate {
    SAND,
    AND,
    OR,
    XOR,
}

fn transformDFGPintoBPMN(dfgp: DirectlyFollowGraphPlus) -> BusinessProcessModelAndNotation {}

fn structure(bpmnDiagram: BusinessProcessModelAndNotation) -> BusinessProcessModelAndNotation {}
