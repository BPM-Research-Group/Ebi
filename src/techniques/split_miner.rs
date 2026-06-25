use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    techniques::stochastic_markovian_abstraction::MarkovianAbstraction,
};
use bitset::BitSet;
use ebi_objects::{
    Activity, BusinessProcessModelAndNotation, FiniteStochasticLanguage, HasActivityKey,
    IntoRefTraceIterator, IntoRefTraceProbabilityIterator,
    ebi_arithmetic::{Fraction, Round, Signed, ToNative, Zero, f},
    malachite::base::num::conversion::traits::RoundingInto,
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
    relations: HashMap<(Activity, Activity), Gate>,

    parameters: SplitMinerParameters,

    filterThreshold: Fraction,

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
            relations: HashMap::new(),
            parameters: parameters.clone(),
            filterThreshold: Fraction::zero(),
            oracle: false,
            zero: Fraction::zero(),
        }
    }

    fn buildDFGP(&mut self) {
        self.untouchableEdges = None;

        self.buildDirectlyFollowsGraph();
        self.detectLoops(); //depends on buildDirectlyFollowsGraph()
        if self.oracle {
            self.detectRelationsOnLog()
        } else {
            self.detectParallelismsOnDFG()
        } //depends on detectLoops()

        match self.parameters.filterType {
            FilterType::FWG => {
                self.filterWithGuarantees();
            }
            _ => todo!(), //as the choice of filtertype is disabled in the gui, these implementations are omitted.
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
                if let Some(e) = self.dfgp.get(tgt).unwrap().get(src) {
                    self.removeEdge(&e.clone(), true);
                }
            }
            //            else { confirmedParallelisms++; }
        }
    }

    fn detectRelationsOnLog(&mut self) {
        let mut confirmedParallelisms = 0;

        self.relations.clear();
        self.potentialConcurrency.clear();
        self.parallelisms.clear();
        self.generateBitmatrixSplits();

        for ((s1, s2), gate) in self.relations.iter() {
            if *gate == Gate::AND || *gate == Gate::OR {
                if let Some(e) = self.dfgp.get(*s1).unwrap().get(*s2) {
                    self.potentialConcurrency.insert(e.clone());
                }

                if let Some(e) = self.dfgp.get(*s2).unwrap().get(*s1) {
                    self.potentialConcurrency.insert(e.clone());
                }
            }
        }

        // System.out.println("DEBUG - total potential concurrencies: " + potentialConcurrency.size());

        let mut orderedRemovableEdges = self
            .potentialConcurrency
            .iter()
            .cloned()
            .collect::<Vec<_>>();
        orderedRemovableEdges.sort();
        while let Some(re) = orderedRemovableEdges.pop() {
            let src = re.getSourceCode();
            let tgt = re.getTargetCode();
            if !self.removeEdge(&re, true) {
                //                System.out.println("DEBUG - impossible remove: " + re.print());
                if self.parallelisms.contains_key(src) {
                    self.parallelisms.get_mut(src).unwrap().remove(&tgt);
                }
                if self.parallelisms.contains_key(tgt) {
                    self.parallelisms.get_mut(tgt).unwrap().remove(&src);
                }
                if let Some(e) = self.dfgp.get(tgt).unwrap().get(src) {
                    self.removeEdge(&re, true);
                }
            } else {
                confirmedParallelisms += 1;
                if (!self.parallelisms.contains_key(src)) {
                    self.parallelisms.insert(src, HashSet::new());
                }
                self.parallelisms.get_mut(src).unwrap().insert(tgt);
                if !self.parallelisms.contains_key(tgt) {
                    self.parallelisms.insert(tgt, HashSet::new());
                }
                self.parallelisms.get_mut(tgt).unwrap().insert(src);
            }
        }

        // System.out.println("DEBUG - removed parallelism edges: " + confirmedParallelisms);
    }

    fn generateBitmatrixSplits(&mut self) {
        //      METHOD-DEPENDENT DATA STRUCTURES

        //        for each split task, we have a matrix of bits, each column being a combination of successors tasks that are executed
        let mut splitMaps = IntMap::new();
        //        for each split task, we have an array storing the successors ids
        //        we query the array to know the index of the successor to update accordingly the row of a matrix
        let mut successors = IntMap::new();

        //        these two structures keep track of the mapping between integer successors codes and the BPMN nodes
        //        as well as the incoming edge of each successor, this is fundamental to edit the BPMN diagram later
        let mut successorsToNodes = IntMap::new();
        let mut successorsToEdges = IntMap::new();

        //      TRACE-DEPENDENT DATA STRUCTURES

        //        while parsing each trace, we have to keep track of all the split tasks that we encountered
        //        then for each of them, if we see one of their successors, we update their bit set
        let mut splitTasksInTrace = IntMap::new();
        //        for each encountered split task, we remember how far in time we encountered it
        //        we can set a max distance after which we do not update anymore the bitarray for that split task
        let mut distances = IntMap::new();

        let MAXD = usize::MAX;
        let skipcounter = 0;

        for TID in self.nodes.keys() {
            let size = self.outgoings.get(TID).unwrap().len();
            if size > 1 {
                splitMaps.insert(TID, Matrix::new(size));

                let mut tmpSuccessors = Vec::with_capacity(size);
                for e in self.outgoings.get(TID).unwrap() {
                    let SID = e.getTargetCode();
                    tmpSuccessors.push(SID);
                    splitMaps.get(TID).addSuccessor(SID);
                }
                successors.insert(TID, tmpSuccessors);
            }
        }

        for (trace, trace_frequency) in self.log.iter_traces_probabilities() {
            splitTasksInTrace.clear();

            //            consuming the start event that is always 0
            //            we assume that the start event is not a successor of any split or a split itself
            let mut trace_it = trace.iter();
            trace_it.next();

            while let Some(event) = trace_it.next() {
                if splitMaps.contains_key(*event) {
                    distances.insert(*event, 0); // not sure we need this, for the moment we keep it
                    if !splitTasksInTrace.contains_key(*event) {
                        splitTasksInTrace.insert(*event, BitSet::new());
                    }
                }

                for sti in splitTasksInTrace.keys() {
                    //                    we now scan all the split tasks that we encountered so far
                    //                    however, split task executed too long ago or same of the current event are not taken into account
                    //                    distances.put(sti, distances.get(sti)+1);
                    if *distances.get(sti).unwrap() > MAXD || *event == sti {
                        skipcounter += 1;
                        continue;
                    }
                    //                    if the current event is a successor for one or more of them (in which case the event is also a join)
                    //                    we update the observation bitset of the split task for this trace
                    if let Some(i) = successors.get(sti).unwrap().iter().position(|e| e == event) {
                        splitTasksInTrace.get(sti).unwrap().set(i, true);
                    }
                }
            }

            //            once the whole trace has been parsed, we add the bitset of each split task to its matrix of bitsets
            for sti in splitTasksInTrace.keys() {
                splitMaps
                    .get(sti)
                    .addBitset(splitTasksInTrace.get(sti), trace_frequency);
            }
        }

        for sti in splitMaps.keys() {
            generateSplitsHierarchyFromObservationMatrix(sti, splitMaps.get(sti));
        }

        // System.out.println("DEBUG - skipcounter = " + skipcounter);
    }

    fn generateSplitsHierarchyFromObservationMatrix(&mut self, split: usize, matrix: Matrix) {
        let print = false;

        // System.out.println("DEBUG - split task: " + log.getEvents().get(split) + " (" + split + ")");
        //        matrix.print();
        matrix.prune(self.parameters.parallelismsThreshold);

        let mut transposedMatrix: IntMap<Activity, BitSet> = IntMap::new();
        let rows = matrix.rows();

        //      first we transpose the matrix
        for i in 0..matrix.totalSuccessors() {
            transposedMatrix.insert(matrix.successors[i], BitSet::with_capacity(rows));
        }

        let r = 0; // row = bitset
        for bs in matrix.matrix.keys() {
            let sl = 0; // location of the successor in the current biset
            for sl in 0..matrix.totalSuccessors() {
                transposedMatrix
                    .get_mut(matrix.successors[sl])
                    .unwrap()
                    .set(r, bs.get(sl));
            }
            r += 1;
        }
        //      transposition is over

        if print {
            print!("DEBUG - printing transposed matrix:");
            for s in transposedMatrix.keys() {
                print!("S: {}: ", s);
                for si in 0..rows {
                    if transposedMatrix.get(s).get(si) {
                        print!("1");
                    } else {
                        print!("0");
                    }
                }
                println!();
            }
        }

        //        we compare the successors observations in the same trace
        let mut ANDs = HashSet::new();
        let mut SANDs = HashSet::new();
        let mut XORs = HashSet::new();
        let mut ORs = HashSet::new();
        let mut skips = HashSet::new();
        let mut removableSuccessors = HashSet::new();
        let mut analysed = HashSet::new();

        // System.out.println("DEBUG - discovering relations");
        for s1 in transposedMatrix.keys() {
            let bs1 = transposedMatrix.get(s1);
            analysed.insert(s1);
            for s2 in transposedMatrix.keys() {
                if analysed.contains(s2) {
                    continue;
                }
                let bs2 = transposedMatrix.get(s2);

                match determineRelation(s1, s2, bs1, bs2, rows, skips) {
                    Gate::SAND => SANDs.insert((s1, s2)),
                    Gate::AND => ANDs.insert((s1, s2)),
                    Gate::XOR => XORs.insert((s1, s2)),
                    Gate::OR => ORs.insert((s1, s2)),
                };
            }
        }

        //            first we check if we found ANDs
        for p in ANDs {
            //                if(removableSuccessors.contains(p.getLeft()) || removableSuccessors.contains(p.getRight())) continue;
            //                when we find an AND we can remove one of the two without any issues
            let bs1 = transposedMatrix.get(p.0);
            removableSuccessors.insert(p.0);
            removableSuccessors.insert(p.1);
            let bs0 = BitSet::new();
            for i in 0..rows {
                bs0.set(i, bs1.get(i));
            }
            //                transposedMatrix.put(p.getLeft()*100, bs0);
            //            System.out.println("DEBUG - AND("+ p.getLeft() + "," + p.getRight() + ")");
            // if(relations.contains_key(p) && self.relations.get(p) != Gate::AND) { System.out.println("WARNING - double relation for: ("+ p.getLeft() + "," + p.getRight() + ")");}
            self.relations.insert(p, Gate::AND);
        }

        //            then we check for XORs
        for p in XORs {
            //                if(removableSuccessors.contains(p.getLeft()) || removableSuccessors.contains(p.getRight())) continue;
            //            System.out.println("DEBUG - XOR("+ p.getLeft() + "," + p.getRight() + ")");
            //                when we find an XOR....
            let bs1 = transposedMatrix.get(p.0);
            let bs2 = transposedMatrix.get(p.1);
            removableSuccessors.add(p.0);
            removableSuccessors.add(p.1);
            let bs0 = BitSet::new();
            for i in 0..rows {
                bs0.set(i, bs1.get(i) || bs2.get(i));
            }
            //                transposedMatrix.put(p.getLeft()*100, bs0);
            // if(relations.containsKey(p) && relations.get(p) != Gate.XOR) System.out.println("WARNING - double relation for: ("+ p.getLeft() + "," + p.getRight() + ")");
            self.relations.insert(p, Gate::XOR);
        }

        //            then we consider ORs
        for p in ORs {
            //                if(removableSuccessors.contains(p.getLeft()) || removableSuccessors.contains(p.getRight())) continue;
            //            System.out.println("DEBUG - OR("+ p.getLeft() + "," + p.getRight() + ")");
            //                when we find an OR...
            let bs1 = transposedMatrix.get(p.0);
            let bs2 = transposedMatrix.get(p.1);
            removableSuccessors.add(p.0);
            removableSuccessors.add(p.1);
            let bs0 = BitSet::new();
            for i in 0..rows {
                bs0.set(i, bs1.get(i) || bs2.get(i));
            }
            //                transposedMatrix.put(p.getLeft()*100, bs0);
            // if(relations.containsKey(p) && relations.get(p) != Gate.OR) System.out.println("WARNING - double relation for: ("+ p.getLeft() + "," + p.getRight() + ")");
            self.relations.insert(p, Gate::OR);
        }

        /*      NOT USED ATM                 finally we consider ANDs with skips
                for (Pair<Integer, Integer> p : SANDs) {
        //                if(removableSuccessors.contains(p.getLeft()) || removableSuccessors.contains(p.getRight())) continue;
                    bs1 = transposedMatrix.get(p.getLeft());
                    bs2 = transposedMatrix.get(p.getRight());
                    removableSuccessors.add(p.getLeft());
                    removableSuccessors.add(p.getRight());
                    bs0 = new BitSet();

                    if( skips.contains(p.getLeft()) ) {
                        System.out.println("DEBUG - AND(SKIP(" + p.getLeft() + ")," + p.getRight() + ")");
                        for (int i = 0; i < ROWS; i++) bs0.set(i, bs2.get(i));
                    } else {
                        System.out.println("DEBUG - AND("+ p.getLeft() + ",SKIP(" + p.getRight() + "))");
                        for(int i = 0; i<ROWS; i++) bs0.set(i, bs1.get(i));
                    }
        //                transposedMatrix.put(p.getLeft()*100, bs0);
                }
        */

        for rs in removableSuccessors {
            transposedMatrix.remove(rs);
        }
    }

    fn filterWithGuarantees(&mut self) {
        self.bestEdgesOnMaxFrequencies();
        self.computeFilterThreshold();

        self.bestEdgesOnMaxCapacities();
        for e in self.edges.clone() {
            if !self.bestEdges.contains(&e)
                && !(self.get_edge_frequency(&e) >= &self.filterThreshold)
            {
                self.removeEdge(&e, false);
            }
        }
    }

    fn bestEdgesOnMaxFrequencies(&mut self) {
        self.bestEdges.clear();

        for (node, _) in &self.nodes {
            if node != self.endcode {
                //                System.out.println("DEBUG - node (max, " + endcode + " ): " + node);
                self.bestEdges.insert(
                    self.outgoings
                        .get(node)
                        .unwrap()
                        .iter()
                        .max()
                        .unwrap()
                        .clone(),
                );
            }
            if node != self.startcode {
                self.bestEdges.insert(
                    self.incomings
                        .get(node)
                        .unwrap()
                        .iter()
                        .max()
                        .unwrap()
                        .clone(),
                );
            }
        }
    }

    fn computeFilterThreshold(&mut self) {
        let mut frequencyOrderedEdges = self.bestEdges.iter().collect::<Vec<_>>();
        frequencyOrderedEdges.sort();

        let mut i = (&self.parameters.percentileFrequencyThreshold
            * &f!(frequencyOrderedEdges.len()))
            .to_usize();
        if i == frequencyOrderedEdges.len() {
            i -= 1;
        };
        self.filterThreshold = self
            .get_edge_frequency(frequencyOrderedEdges.get(i).unwrap())
            .clone();
    }

    fn bestEdgesOnMaxCapacities(&mut self) {
        let mut tiebreak;
        let use_tiebreaker = false;

        let mut toVisit = VecDeque::new();
        let mut unvisited = HashSet::new();

        let mut bestPredecessorFromSource: IntMap<Activity, &DFGEdge> = IntMap::new();
        let mut bestSuccessorToSink: IntMap<Activity, &DFGEdge> = IntMap::new();

        let mut maxCapacitiesFromSource = IntMap::new();
        let mut maxCapacitiesToSink = IntMap::new();

        for (n, _) in self.nodes.iter() {
            maxCapacitiesFromSource.insert(n, Fraction::zero());
            maxCapacitiesToSink.insert(n, Fraction::zero());
        }

        maxCapacitiesFromSource.insert(self.startcode, f!(usize::MAX));
        maxCapacitiesToSink.insert(self.endcode, f!(usize::MAX));

        //      forward exploration
        toVisit.push_back(self.startcode);
        unvisited.extend(self.nodes.keys());
        unvisited.remove(&self.startcode);

        while let Some(src) = toVisit.pop_front() {
            let cap = maxCapacitiesFromSource.get(src).unwrap().clone();
            for oe in self.outgoings.get(src).unwrap() {
                let tgt = oe.getTargetCode();
                let max_cap = if cap > *self.get_edge_frequency(oe) {
                    self.get_edge_frequency(oe)
                } else {
                    &cap
                };
                tiebreak = (max_cap == maxCapacitiesFromSource.get(tgt).unwrap())
                    && bestPredecessorFromSource.get(tgt).unwrap().isLoop
                    && use_tiebreaker;
                if (max_cap > maxCapacitiesFromSource.get(tgt).unwrap()) || tiebreak {
                    maxCapacitiesFromSource.insert(tgt, max_cap.clone());
                    bestPredecessorFromSource.insert(tgt, oe);
                    if !toVisit.contains(&tgt) {
                        unvisited.insert(tgt);
                    }
                }
                if unvisited.contains(&tgt) {
                    toVisit.push_back(tgt);
                    unvisited.remove(&tgt);
                }
            }
        }

        //      backward exploration
        toVisit.push_back(self.endcode);
        unvisited.clear();
        unvisited.extend(self.nodes.keys());
        unvisited.remove(&self.endcode);

        while let Some(tgt) = toVisit.pop_front() {
            let cap = maxCapacitiesToSink.get(tgt).unwrap().clone();
            for ie in self.incomings.get(tgt).unwrap() {
                let src = ie.getSourceCode();
                let max_cap = if cap > *self.get_edge_frequency(ie) {
                    self.get_edge_frequency(ie)
                } else {
                    &cap
                };
                tiebreak = (max_cap == maxCapacitiesToSink.get(src).unwrap())
                    && bestSuccessorToSink.get(src).unwrap().isLoop
                    && use_tiebreaker;
                if (max_cap > maxCapacitiesToSink.get(src).unwrap()) || tiebreak {
                    maxCapacitiesToSink.insert(src, max_cap.clone());
                    bestSuccessorToSink.insert(src, ie);
                    if !toVisit.contains(&src) {
                        unvisited.insert(src);
                    }
                }
                if unvisited.contains(&src) {
                    toVisit.push_back(src);
                    unvisited.remove(&src);
                }
            }
        }

        self.bestEdges.clear();
        for n in self.nodes.keys() {
            if let Some(x) = bestPredecessorFromSource.get(n) {
                self.bestEdges.insert((*x).clone());
            }
            if let Some(x) = bestSuccessorToSink.get(n) {
                self.bestEdges.insert((*x).clone());
            }
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
        if let Some(untouchable_edges) = &self.untouchableEdges
            && untouchable_edges.contains(&e)
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

#[derive(Eq, PartialEq)]
enum Gate {
    SAND,
    AND,
    OR,
    XOR,
}

fn transformDFGPintoBPMN(dfgp: DirectlyFollowGraphPlus) -> BusinessProcessModelAndNotation {}

fn structure(bpmnDiagram: BusinessProcessModelAndNotation) -> BusinessProcessModelAndNotation {}
