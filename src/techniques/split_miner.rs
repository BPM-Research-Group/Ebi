use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    techniques::stochastic_markovian_abstraction::MarkovianAbstraction,
};
use ebi_objects::{
    Activity, BusinessProcessModelAndNotation, FiniteStochasticLanguage, HasActivityKey, ebi_arithmetic::{Fraction, Zero, f},
};
use intmap::IntMap;
use std::{
    collections::{HashMap, HashSet},
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

    let dfgp = generateDFGP(log, parameters, start_activity, end_activity);
    let mut bpmnDiagram = transformDFGPintoBPMN(dfgp);
    if parameters.structuringTime == StructuringTime::Post {
        bpmnDiagram = structure(bpmnDiagram);
    }
    bpmnDiagram
}

fn generateDFGP<'a>(
    log: &'a dyn EbiTraitFiniteStochasticLanguage,
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
    parallelisms: IntMap<usize, HashSet<usize>>,
    concurrencyMatrix: Vec<Fraction>,
    bestEdges: HashSet<DFGEdge>,
    untouchableEdges: Option<HashSet<DFGEdge>>,
    potentialConcurrency: HashSet<DFGEdge>,
    relations: HashMap<(usize, usize), Gate>,

    parameters: SplitMinerParameters,

    filterThreshold: Option<usize>,

    oracle: bool,
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
            outgoings: IntMap::new(),
            incomings: IntMap::new(),
            dfgp: IntMap::new(),
            loopsL1: HashSet::new(),
            loopsL1Freq: IntMap::new(),
            loopsL2: (),
            parallelisms: (),
            concurrencyMatrix: (),
            bestEdges: (),
            untouchableEdges: (),
            potentialConcurrency: (),
            relations: (),
            parameters: parameters.clone(),
            filterThreshold: (),
            oracle: (),
        }
    }

    fn buildDFGP(&mut self) {
        self.untouchableEdges = None;

        self.buildDirectlyFollowsGraph();
        detectLoops(); //depends on buildDirectlyFollowsGraph()
        if self.oracle {
            detectRelationsOnLog()
        } else {
            detectParallelismsOnDFG()
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
        let autogenStart = DFGNode::new(self.startcode);
        self.addNode(autogenStart.clone());
        //        while parsing the simple log we will always skip the start event,
        //        so we set now the maximum frequency because it is an artificial start event
        self.increase_node_frequency(autogenStart.id, &self.log.get_probability_sum());
        autogenStart.increaseFrequency(&f!(self.log.number_of_traces()));

        let autogenEnd = DFGNode::new(self.endcode);
        self.addNode(autogenEnd);

        for (trace, traceFrequency) in self.log.iter_traces_probabilities() {
            let mut trace_it = trace.iter();
            //            consuming the start event that is always 0
            trace_it.next();

            let prevEvent = self.startcode;
            let prevNode = autogenStart;

            while let Some(event) = trace_it.next() {
                //                we read the next event of the trace until it is finished

                if prevEvent == *event {
                    if self.loopsL1.contains(event) {
                        *self.loopsL1Freq.get_mut(*event).unwrap() += traceFrequency;
                    } else {
                        self.loopsL1.insert(*event);
                        self.loopsL1Freq.insert(*event, traceFrequency.clone());
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
                self.increase_node_frequency(node.id, traceFrequency);

                if !self.dfgp.contains_key(prevEvent)
                    || !self.dfgp.get(prevEvent).unwrap().contains_key(*event)
                {
                    let edge = DFGEdge::new(prevNode, node);
                    self.addEdge(edge, traceFrequency.clone());
                }

                //                  increasing frequency of this directly following relationship
                let edge_id = self.dfgp.get(prevEvent).unwrap().get(*event).unwrap().id;
                self.increase_edge_frequency(edge_id, traceFrequency);

                prevEvent = *event;
                prevNode = node;
            }
        }
    }

    fn addNode(&mut self, n: DFGNode) {
        let code = n.code;

        self.nodes.insert(code, n);
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
        self.edge_2_frequency.insert(e, frequency);
    }

    fn increase_edge_frequency(&mut self, e: Uuid, frequency: &Fraction) {
        *self.edge_2_frequency.get_mut(&e).unwrap() += frequency;
    }

    fn increase_node_frequency(&mut self, n: Uuid, frequency: &Fraction) {
        *self.node_2_frequency.get_mut(&n).unwrap() += frequency;
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

#[derive(Clone)]
struct DFGNode {
    id: Uuid,
    code: Activity,

    frequency: Fraction,
    startFrequency: usize,
    endFrequency: usize,
}

impl DFGNode {
    fn new(code: Activity) -> Self {
        id: Uuid::new_v4(),
        code
    }

    fn increaseFrequency(&mut self, amount: &Fraction) {
        self.frequency += amount;
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
