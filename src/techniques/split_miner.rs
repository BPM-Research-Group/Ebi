use std::collections::{HashMap, HashSet};

use ebi_objects::{
    Activity, BusinessProcessModelAndNotation, HasActivityKey, LabelledPetriNet,
    ebi_arithmetic::{Fraction, Zero, f},
    ebi_bpmn::elements::process::BPMNProcess,
};
use intmap::IntMap;
use uuid::Uuid;

use crate::{
    ebi_traits::{
        ebi_trait_event_log::EbiTraitEventLog,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    },
    techniques::stochastic_markovian_abstraction::MarkovianAbstraction,
};

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
    log: &'a dyn EbiTraitFiniteStochasticLanguage,
    startcode: Activity,
    endcode: Activity,

    edges: HashSet<DFGEdge>,
    nodes: IntMap<Activity, DFGNode>,
    outgoings: IntMap<usize, HashSet<DFGEdge>>,
    incomings: IntMap<usize, HashSet<DFGEdge>>,
    dfgp: IntMap<Activity, IntMap<usize, DFGEdge>>,

    loopsL1: HashSet<Activity>,
    loopsL1Freq: IntMap<Activity, usize>,
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
        log: &'a dyn EbiTraitFiniteStochasticLanguage,
        parameters: &SplitMinerParameters,
        start_activity: Activity,
        end_activity: Activity,
    ) -> Self {
        Self {
            log,
            startcode: start_activity,
            endcode: end_activity,
            edges: HashSet::new(),
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
        let autogenStart = DFGNode::new(events.get(startcode), self.startcode);
        self.addNode(autogenStart);
        //        while parsing the simple log we will always skip the start event,
        //        so we set now the maximum frequency because it is an artificial start event
        autogenStart.increaseFrequency(&f!(self.log.number_of_traces()));

        let autogenEnd = DFGNode::new(events.get(endcode), self.endcode);
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
                        *self.loopsL1Freq.get_mut(*event).unwrap() += 1;
                    } else {
                        self.loopsL1.insert(*event);
                        self.loopsL1Freq.insert(*event, 1);
                    }
                    continue;
                }

                let node;
                if !self.nodes.contains_key(*event) {
                    node = DFGNode::new(events.get(event), event);
                    self.addNode(node);
                } else {
                    node = nodes.get(event);
                }

                //                  increasing frequency of this event occurrence
                node.increaseFrequency(traceFrequency);

                if (!dfgp.containsKey(prevEvent)
                    || !self.dfgp.get(prevEvent).unwrap().contains_key(event))
                {
                    edge = DFGEdge::new(prevNode, node);
                    self.addEdge(edge);
                }

                //                  increasing frequency of this directly following relationship
                self.dfgp
                    .get(prevEvent)
                    .get(event)
                    .increaseFrequency(traceFrequency);

                prevEvent = *event;
                prevNode = node;
            }
        }
    }
}

struct DFGEdge {
    frequency: Fraction,
    isLoop: bool,
    id: Uuid,
    label: Option<String>,
    source: DFGNode,
    target: DFGNode,
}

impl DFGEdge {
    fn new(source: DFGNode, target: DFGNode) -> Self {
        Self {
            frequency: Fraction::zero(),
            isLoop: false,
            id: uuid::Uuid::new_v4(),
            label: None,
            source,
            target,
        }
    }
}

struct DFGNode {
    id: String,
    label: String,
    code: Activity,

    frequency: Fraction,
    startFrequency: usize,
    endFrequency: usize,
}

impl DFGNode {
    fn new(label: String, code: Activity) -> Self {}

    fn increaseFrequency(&mut self, amount: &Fraction) {
        *self.frequency += amount;
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
