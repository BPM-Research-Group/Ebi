use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    techniques::{
        split_miner::bpmn::{BPMNNode, structure, transformDFGPintoBPMN},
        stochastic_markovian_abstraction::MarkovianAbstraction,
    },
};
use bit_set::BitSet;
use ebi_objects::{
    Activity, BusinessProcessModelAndNotation, FiniteStochasticLanguage, HasActivityKey,
    IntoRefTraceIterator, IntoRefTraceProbabilityIterator,
    anyhow::{Result, anyhow},
    ebi_arithmetic::{Fraction, Signed, ToNative, Zero, f, f0},
    ebi_bpmn::{BPMNCreator, Container, EndEventType, StartEventType},
};
use intmap::IntMap;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
};
use uuid::Uuid;

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
) -> Result<BusinessProcessModelAndNotation> {
    let mut slang = log.to_finite_stochastic_language();

    //add start and end activities to each trace
    let (start_activity, end_activity) =
        MarkovianAbstraction::create_start_end(slang.activity_key_mut());

    let dfgp = generateDFGP(&slang, parameters, start_activity, end_activity);
    let mut bpmnDiagram = transformDFGPintoBPMN(dfgp, parameters)?;
    if parameters.structuringTime == StructuringTime::Post {
        bpmnDiagram = structure(bpmnDiagram);
    }
    bpmnDiagram.to_bpmn()
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
        let successorsToNodes: IntMap<(), ()> = IntMap::new();
        let successorsToEdges: IntMap<(), ()> = IntMap::new();

        //      TRACE-DEPENDENT DATA STRUCTURES

        //        while parsing each trace, we have to keep track of all the split tasks that we encountered
        //        then for each of them, if we see one of their successors, we update their bit set
        let mut splitTasksInTrace = IntMap::new();
        //        for each encountered split task, we remember how far in time we encountered it
        //        we can set a max distance after which we do not update anymore the bitarray for that split task
        let mut distances = IntMap::new();

        let MAXD = usize::MAX;
        let mut skipcounter = 0;

        for TID in self.nodes.keys() {
            let size = self.outgoings.get(TID).unwrap().len();
            if size > 1 {
                splitMaps.insert(TID, Matrix::new(size));

                let mut tmpSuccessors = Vec::with_capacity(size);
                for e in self.outgoings.get(TID).unwrap() {
                    let SID = e.getTargetCode();
                    tmpSuccessors.push(SID);
                    splitMaps.get_mut(TID).unwrap().addSuccessor(SID);
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

                for (sti, value) in splitTasksInTrace.iter_mut() {
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
                        value.insert(i);
                    }
                }
            }

            //            once the whole trace has been parsed, we add the bitset of each split task to its matrix of bitsets
            for sti in splitTasksInTrace.keys() {
                splitMaps.get_mut(sti).expect("sti not found").addBitset(
                    splitTasksInTrace.get(sti).expect("sti not found").clone(),
                    trace_frequency.clone(),
                );
            }
        }

        for (sti, value) in splitMaps.iter_mut() {
            self.generateSplitsHierarchyFromObservationMatrix(sti, value);
        }

        // System.out.println("DEBUG - skipcounter = " + skipcounter);
    }

    fn generateSplitsHierarchyFromObservationMatrix(
        &mut self,
        split: Activity,
        matrix: &mut Matrix,
    ) {
        let print = false;

        // System.out.println("DEBUG - split task: " + log.getEvents().get(split) + " (" + split + ")");
        //        matrix.print();
        matrix.prune(&self.parameters.parallelismsThreshold);

        let mut transposedMatrix: IntMap<Activity, BitSet> = IntMap::new();
        let rows = matrix.rows();

        //      first we transpose the matrix
        for i in 0..matrix.totalSuccessors() {
            transposedMatrix.insert(matrix.successors[i], BitSet::with_capacity(rows));
        }

        let mut r = 0; // row = bitset
        for bs in matrix.matrix.keys() {
            let sl = 0; // location of the successor in the current biset
            for sl in 0..matrix.totalSuccessors() {
                if bs.contains(sl) {
                    transposedMatrix
                        .get_mut(matrix.successors[sl])
                        .unwrap()
                        .insert(r);
                }
            }
            r += 1;
        }
        //      transposition is over

        if print {
            print!("DEBUG - printing transposed matrix:");
            for s in transposedMatrix.keys() {
                print!("S: {}: ", s);
                for si in 0..rows {
                    if transposedMatrix.get(s).unwrap().contains(si) {
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
            let bs1 = transposedMatrix.get(s1).expect("bs1 not found");
            analysed.insert(s1);
            for s2 in transposedMatrix.keys() {
                if analysed.contains(&s2) {
                    continue;
                }
                let bs2 = transposedMatrix.get(s2).expect("bs2 not found");

                match self.determineRelation(s1, s2, bs1, bs2, rows, &mut skips) {
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
            let bs1 = transposedMatrix.get(p.0).expect("bs1 not found");
            removableSuccessors.insert(p.0);
            removableSuccessors.insert(p.1);
            let mut bs0 = BitSet::new();
            for i in 0..rows {
                bs0.set(i, bs1.contains(i));
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
            let bs1 = transposedMatrix.get(p.0).expect("bs1 not found");
            let bs2 = transposedMatrix.get(p.1).expect("bs2 not found");
            removableSuccessors.insert(p.0);
            removableSuccessors.insert(p.1);
            let mut bs0 = BitSet::new();
            for i in 0..rows {
                bs0.set(i, bs1.contains(i) || bs2.contains(i));
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
            let bs1 = transposedMatrix.get(p.0).expect("bs1 not found");
            let bs2 = transposedMatrix.get(p.1).expect("bs2 not found");
            removableSuccessors.insert(p.0);
            removableSuccessors.insert(p.1);
            let mut bs0 = BitSet::new();
            for i in 0..rows {
                bs0.set(i, bs1.contains(i) || bs2.contains(i));
            }
            //                transposedMatrix.put(p.getLeft()*100, bs0);
            // if(relations.containsKey(p) && relations.get(p) != Gate.OR) System.out.println("WARNING - double relation for: ("+ p.getLeft() + "," + p.getRight() + ")");
            self.relations.insert(p, Gate::OR);
        }
        for rs in removableSuccessors {
            transposedMatrix.remove(rs);
        }
    }

    fn determineRelation(
        &self,
        s1: Activity,
        s2: Activity,
        bs1: &BitSet,
        bs2: &BitSet,
        size: usize,
        skips: &mut HashSet<Activity>,
    ) -> Gate {
        let typee = Gate::OR;
        let safe = true;
        let mut mismatch = 0;
        let mut matchh = 0;

        if self
            .loopsL2
            .contains(self.dfgp.get(s1).unwrap().get(s2).unwrap())
        {
            // System.out.println("DEBUG - pair ("+ s1 + "," + s2 + ") - shortloop");
            return Gate::XOR;
        }

        let mut observations = HashSet::new();
        for i in 0..size {
            observations.insert((bs1.contains(i), bs2.contains(i)));
        }

        //        System.out.println("DEBUG - obs for ("+ s1 + "," + s2 + "): " + observations.size());

        let mut size = observations.len();
        if observations.remove(&(false, false)) {
            size -= 1;
        }
        if observations.len() >= 3 {
            return Gate::OR;
        }

        //        if all the observations match, we have an AND
        for p in &observations {
            if p.0 == p.1 {
                matchh += 1;
            } else {
                mismatch += 1;
            };
        }

        if matchh == size {
            return Gate::AND;
        }
        if safe && (s1 < 0) || (s2 < 0) {
            return Gate::XOR; // this is to play safe with successor-joins
        }

        //        otherwise we could have a XOR or OR
        let skipL = (false, true);
        let skipR = (true, false);
        let skipNone = (true, true);
        //        Pair<Boolean, Boolean> skipBoth = new ImmutablePair<>(false, false);

        if observations.contains(&skipL) && observations.contains(&skipNone) {
            skips.insert(s1);
            return Gate::XOR;
        }

        if observations.contains(&skipR) && observations.contains(&skipNone) {
            skips.insert(s2);
            return Gate::XOR;
        }

        if observations.contains(&skipL) && observations.contains(&skipR) {
            return Gate::XOR;
        } else {
            return Gate::OR;
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

    fn convertIntoBPMNDiagram(&self) -> Result<(BPMNCreator, Container, BPMNNode, BPMNNode)> {
        let mut creator = BPMNCreator::new();
        let process = creator.add_process(Some("eDFGP-diagram".to_string()));
        let mut mapping = HashMap::new();

        let mut start_event = None;
        let mut end_event = None;

        for event in self.nodes.keys() {
            let label = self.log.activity_key().deprocess_activity(&event);

            let node = match (event == self.startcode, event == self.endcode) {
                (true, false) => {
                    let e = creator.add_start_event(process, StartEventType::None)?;
                    start_event = Some(e);
                    e
                }
                (false, true) => {
                    let e = creator.add_end_event(process, EndEventType::None)?;
                    end_event = Some(e);
                    e
                }
                _ => creator.add_task(process, event)?,
            };
            mapping.insert(event, node);
        }

        for edge in &self.edges {
            let src = mapping.get(&edge.getSourceCode()).expect("src not found");
            let tgt = mapping.get(&edge.getTargetCode()).expect("tgt not found");
            creator.add_sequence_flow(process, *src, *tgt)?;
        }

        return Ok((
            creator,
            process,
            start_event.ok_or_else(|| anyhow!("no start event"))?,
            end_event.ok_or_else(|| anyhow!("no end event"))?,
        ));
    }
}

mod bpmn {
    use crate::techniques::split_miner::{
        DirectlyFollowGraphPlus, SplitMinerParameters,
        oracle::{Oracle, OracleItem},
    };
    use ebi_objects::{
        BusinessProcessModelAndNotation,
        anyhow::Result,
        ebi_bpmn::{BPMNCreator, Container},
    };
    use std::collections::{HashMap, HashSet, VecDeque};

    pub(super) type BPMNNode = (usize, ());

    pub(super) fn transformDFGPintoBPMN(
        dfgp: DirectlyFollowGraphPlus,
        parameters: &SplitMinerParameters,
    ) -> Result<BPMNCreator> {
        //        System.out.println("SplitMiner - generating bpmn diagram");

        let gateCounter = 0;

        //        we retrieve the starting BPMN diagram from the DFGP,
        //        it is a DFGP with start and end events, but no gateways
        let (bpmnDiagram, process, entry, exit) = dfgp.convertIntoBPMNDiagram()?;
        let candidateJoins = HashMap::new();

        //        we start the transformation of the DFGP into BPMN by generating the splits
        //        generateBitmatrixSplits();
        generateSplits(&dfgp, bpmnDiagram, process, entry, exit);

        //        after generating the split hierarchy we should have only SPLITs,
        //        however, it may happen that some JOINs are generated as well (due to shared future)
        //        it is important that we do not leave any gateway that is both a SPLIT and a JOIN
        helper.removeJoinSplit(bpmnDiagram);

        //        at this point, all the splits were generated, along with just a few joins
        //        now we focus only on the joins. we use the RPST in order to place INCLUSIVE joins
        //        which will be turned into AND or XOR joins later
        //        System.out.println("SplitMiner - generating SESE joins ...");
        let bondsEntries = HashSet::new();
        let rigidsEntries = HashSet::new();
        while (generateSESEjoins()) {}

        //        this second method adds the remaining joins, which were no entry neither exits of any RPST node
        //        System.out.println("SplitMiner - generating inner joins ...");
        generateInnerJoins();

        //        if( structuringTime == SplitMinerUIResult.StructuringTime.PRE ) structure();
        //        helper.removeEmptyParallelFlows(bpmnDiagram);
        helper.fixSoundness(bpmnDiagram);

        //        finally, we turn all the inclusive joins placed, into proper joins: ANDs or XORs
        //        System.out.println("SplitMiner - turning inclusive joins ...");
        replaceIORs(helper);

        updateLabels(dfgp.log.getEvents());

        //            helper.collapseSplitGateways(bpmnDiagram);
        //            helper.collapseJoinGateways(bpmnDiagram);

        if parameters.removeLoopActivities {
            helper.removeLoopActivityMarkers(bpmnDiagram);
        }

        Ok(bpmnDiagram)

        //        System.out.println("SplitMiner - bpmn diagram generated successfully");
    }

    fn generateSplits(
        dfgp: &DirectlyFollowGraphPlus,
        bpmn_diagram: BPMNCreator,
        process: Container,
        entry: BPMNNode,
        exit: BPMNNode,
    ) -> Result<()> {
        let mut mapping = HashMap::new();
        let mut toVisit = VecDeque::new();
        let mut visited = HashSet::new();

        let oracle = Oracle::new();

        //        we perform a breadth-first exploration of the DFGP-diagram
        //        every time we find a node with multiple outgoing edges we stop
        //        and we generate the corresponding hierarchy of gateways

        toVisit.push_back(entry);
        while let Some(entry) = toVisit.pop_front() {
            visited.insert(entry);
            //            System.out.println("DEBUG - visiting: " + entry.getLabel());

            if entry == exit {
                continue;
            }

            if bpmn_diagram
                .outgoing_sequence_flows_of_element(process, entry)?
                .count()
                > 1
            {
                //                entry is a node with multiple outgoing edges

                let mut successors = HashSet::new();
                let mut removableEdges = HashSet::new();
                for oe in bpmn_diagram.outgoing_sequence_flows_of_element(process, entry)? {
                    let tgt = bpmn_diagram
                        .target_of_sequence_flow(oe)
                        .expect("target not found");
                    //                    we remove all the outgoing edges, because we will restore them with the split gateways
                    removableEdges.insert(oe);
                    successors.insert(tgt.0);
                    mapping.insert(tgt.0, tgt);
                    if !toVisit.contains(&tgt) && !visited.contains(&tgt) {
                        toVisit.push_back(tgt);
                    }
                }

                for e in removableEdges {
                    bpmn_diagram.remove_sequence_flow(process, e);
                }

                //                to decide the hierarchy of the gateways we use an Oracle item
                //                an Oracle item is a string of the type past|future
                //                more info about this object in its own class
                let mut oracleItems = HashSet::new();
                for a in successors {
                    //                    we generate one Oracle item for each successor of the entry
                    //                    the successor will be the past
                    let oracleItem = OracleItem::new();
                    oracleItem.fillPast(a);

                    //                    then we fill its future with all the successors which are in a concurrency relationship with it
                    //                    if a successor is not concurrent, it means it will we exclusive or directly follow
                    //                    if exclusive we do not have to care about it
                    //                    if directly follow, it will be processed later
                    for b in successors {
                        if (a != b) && (dfgp.areConcurrent(a, b)) {
                            oracleItem.fillFuture(b);
                        }
                    }

                    oracleItem.engrave();
                    oracleItems.insert(oracleItem);
                }

                let finalOracleItem = oracle.getFinalOracleItem(oracleItems);

                //                the finalOracleItem is a matryoshka containing the info about the gateway hierarchy
                //                the following method will explore inside-out this matryoshka and will place the gateways accordingly
                //                the entry will be the last node to be linked to the outer gateway of the hierarchy
                generateSplitsHierarchy(entry, finalOracleItem, mapping);
            } else {
                //                we save the only successor of the src
                let tgt = bpmn_diagram
                    .outgoing_sequence_flows_of_element(process, entry)?
                    .iter()
                    .next()
                    .unwrap()
                    .getTarget();
                if !toVisit.contains(tgt) && !visited.contains(tgt) {
                    toVisit.push_back(tgt);
                }
            }
        }
        Ok(())
    }

    pub(super) fn structure(bpmnDiagram: BPMNCreator) -> BPMNCreator {
        todo!()
    }
}

mod oracle {
    use std::{collections::HashSet, hash::Hash};

    use ebi_objects::Activity;

    pub(super) struct Oracle;

    impl Oracle {
        pub(super) fn new() -> Self {
            Self
        }

        fn getFinalOracleItem(oracleItems: HashSet<OracleItem>) -> Option<OracleItem> {
            let mut matryoshka = None;
            let mut forced = true;
            let mut counter = 0;
            let mut toMerge = HashSet::new();

            while oracleItems.len() != 1 {
                //            System.out.println("DEBUG - oracle items: " + oracleItems.size());
                //            for( OracleItem oi : oracleItems ) System.out.println("DEBUG - oracle item: " + oi);
                let mut merged = false;

                //            firstly we try to merge XORs,
                //            because XORs have priority on ANDs for the merging technique
                loop {
                    let mut toMerge = HashSet::new();
                    for oi in &oracleItems {
                        //                    if we found oracle items to be merged we add the Oracle item we were analysing
                        //                    to the toMerge set and we proceed with the merging
                        for oii in &oracleItems {
                            if oi != oii && oi.isXOR(oii) {
                                toMerge.insert(oii);
                            }
                        }

                        //                    if we found some Oracle item that can be merged we add also the one we used for the comparisons
                        if toMerge.len() != 0 {
                            toMerge.insert(oi);
                            break;
                        }
                    }

                    //                merging time: XORs
                    if toMerge.len() != 0 {
                        merged = true;
                        matryoshka = Some(OracleItem::mergeXORs(toMerge));
                        //                    System.out.println("DEBUG - merging XORs ...");
                        oracleItems.removeAll(toMerge);
                        oracleItems.insert(matryoshka);
                    } else {
                        break;
                    }
                }

                //            after we merged all the possible XORs Oracle items,
                //            we try to merge the ANDs Oracle items, using the same technique
                for oi in &oracleItems {
                    toMerge = HashSet::new();
                    for oii in &oracleItems {
                        if (oi != oii) && oi.isAND(oii) {
                            toMerge.insert(oii);
                        }
                    }
                    if toMerge.len() != 0 {
                        toMerge.insert(oi);
                        break;
                    }
                }

                //            merging time: ANDs
                if toMerge.len() != 0 {
                    merged = true;
                    matryoshka = Some(OracleItem::mergeANDs(toMerge));
                    //                System.out.println("DEBUG - merging ANDs ...");
                    oracleItems.removeAll(toMerge);
                    oracleItems.insert(matryoshka);
                }

                //            it can happens that we did not merge anything, but there are still Oracle items
                //            this can happen because the concurrency relationships are not complete
                //            in such case, we have to force the merging
                if !merged {
                    let mut minDistance = usize::MAX;
                    //                System.out.println("WARNING - impossible merging oracle items, extending the concurrency relationships");

                    //                if we have to force a merging, we try to merge the minimum distance couple of Oracle items
                    //                that is: we are trying to introduce the less possible changes in the concurrency relationships
                    //                we prefer to assume that a concurrency relationship was missing
                    //                rather than a concurrency relationship was an error
                    for oi in &oracleItems {
                        for oii in &oracleItems {
                            if (oi == oii) {
                                continue;
                            }
                            let tmpDistance = oi.getANDDistance(oii);
                            if tmpDistance < minDistance {
                                minDistance = tmpDistance;
                                toMerge = HashSet::new();
                                toMerge.add(oi);
                                toMerge.add(oii);
                            }
                        }
                    }

                    if forced {
                        matryoshka = OracleItem::forcedMergeANDs(toMerge);
                    } else {
                        matryoshka = OracleItem::mergeANDs(toMerge);
                    }

                    //                System.out.println("WARNING - forcing AND merge ...");
                    counter += 1;
                    oracleItems.removeAll(toMerge);
                    oracleItems.insert(matryoshka);
                }
            }

            // if( counter != 0 ) System.out.println("DEBUG - forced AND merging: " + counter);
            //        System.out.println("DEBUG - matryoshka: " + matryoshka);
            return matryoshka;
        }
    }

    #[derive(Clone)]
    pub(super) struct OracleItem {
        past: HashSet<Activity>,
        future: HashSet<Activity>,

        oracle: Vec<Activity>,
        oraclePast: Vec<Activity>,
        oracleFuture: Vec<Activity>,

        xorBrothers: HashSet<OracleItem>,
        andBrothers: HashSet<OracleItem>,
    }

    impl OracleItem {
        pub(super) fn new() -> Self {
            Self {
                past: HashSet::new(),
                future: HashSet::new(),
                xorBrothers: HashSet::new(),
                andBrothers: HashSet::new(),
                oracle: vec![],
                oracleFuture: vec![],
                oraclePast: vec![],
            }
        }

        pub(super) fn isAND(&self, o_item: &OracleItem) -> bool {
            o_item.oracle == self.oracle
        }

        pub(super) fn isXOR(&self, oItem: &OracleItem) -> bool {
            oItem.oracleFuture == self.oracleFuture
        }

        pub(super) fn mergeANDs(and_brothers: HashSet<OracleItem>) -> OracleItem {
            /*
             * merging two or more AND oracle items means:
             * 1. create a new oracle item that contains all the oracle items to be merged as andBrothers
             * 2. its future will be only the part of shared future of all the andBrothers in input
             * 3. its past will be the union of the pasts of all the andBrothers in input
             *
             * eg: inputs = { (:A:|:B:C:D:), (:B:|:A:C:D:) } output = (:A:B:|:C:D:)
             */

            let mut oiUnion = OracleItem::new();
            oiUnion.andBrothers.extend(and_brothers.clone());

            for and in &and_brothers {
                oiUnion.future.extend(and.future.clone());
            }
            for and in &and_brothers {
                oiUnion.future.extend(and.future.clone());
            }

            for and in &and_brothers {
                oiUnion.past.extend(and.past.clone());
            }

            oiUnion.engrave();
            return oiUnion;
        }

        pub(super) fn mergeXORs(xorBrothers: &HashSet<OracleItem>) -> OracleItem {
            /*
             * merging two or more XOR oracle items means:
             * 1. create a new oracle item that contains all the oracle items to be merged as xorBrothers
             * 2. its future will be the same shared future of all the xorBrothers in input (see also isXOR)
             * 3. its past will be the union of the pasts of all the xorBrothers in input
             *
             * eg: inputs = { (:A:|:C:D:), (:B:|:C:D:) } output = (:A:B:|:C:D:)
             */

            let mut oiUnion = OracleItem::new();
            oiUnion.xorBrothers.extend(xorBrothers.clone());

            for xor in xorBrothers {
                oiUnion.future.extend(xor.future.clone());
                break;
            }

            for xor in xorBrothers {
                oiUnion.past.extend(xor.past.clone());
            }

            oiUnion.engrave();
            return oiUnion;
        }

        pub(super) fn engrave(&mut self) {
            //        this method should be called when we want to finalize an Oracle item
            //        that means, it is ready to be successively used
            //        it transform the Oracle item into its final string of type:
            //        past|future where past = :x:y:z: and future = :j:k:l:
            //        therefore: Oracle item = :x:y:z:|:j:k:l:

            let mut past = self.past.iter().cloned().collect::<Vec<_>>();
            let mut future = self.future.iter().cloned().collect::<Vec<_>>();

            let mut present = vec![];
            present.extend(&self.past);
            present.extend(&self.future);

            let i = 0;
            past.sort();
            self.oraclePast = past.clone();

            let i = 0;
            future.sort();
            self.oracleFuture = future.clone();

            //        this is an extra string used to merge AND Oracle items
            //        it contains all the ordered elements of the past and the future
            let i = 0;
            present.sort();
            self.oracle = present.clone();
        }
    }

    impl Eq for OracleItem {}

    impl PartialEq for OracleItem {
        fn eq(&self, other: &Self) -> bool {
            self.oraclePast == other.oraclePast && self.oracleFuture == self.oracleFuture
        }
    }

    impl Hash for OracleItem {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.oraclePast.hash(state);
            self.oracleFuture.hash(state);
        }
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

struct Matrix {
    successors: Vec<Activity>,
    is: usize,
    matrix: HashMap<BitSet, Fraction>,
    totalFrequency: Fraction,
}

impl Matrix {
    fn new(successors: usize) -> Self {
        Self {
            successors: Vec::with_capacity(successors),
            matrix: HashMap::new(),
            is: 0,
            totalFrequency: f0!(),
        }
    }

    fn addSuccessor(&mut self, s: Activity) {
        self.successors.push(s);
        self.is += 1;
    }

    fn addBitset(&mut self, combo: BitSet, frequency: Fraction) {
        self.totalFrequency += &frequency;
        if !self.matrix.contains_key(&combo) {
            self.matrix.insert(combo, frequency);
        } else {
            let f = self.matrix.get(&combo).expect("combo not find") + &frequency;
            self.matrix.insert(combo, f);
        }
    }

    fn prune(&mut self, threshold: &Fraction) {
        let mut lowFrequency = HashSet::new();
        let avgFrequency = &self.totalFrequency / &f!(self.matrix.len());

        for bs in self.matrix.keys() {
            if &(self.matrix.get(bs).expect("bs not found") / &avgFrequency) < threshold {
                lowFrequency.insert(bs.clone());
            }
        }
        // System.out.println("DEBUG - removing " + lowFrequency.size() + " low frequency observations");

        for bs in lowFrequency {
            self.matrix.remove(&bs);
        }
    }

    fn totalSuccessors(&self) -> usize {
        self.is
    }

    fn rows(&self) -> usize {
        self.matrix.len()
    }

    fn getObservations(&self) -> impl Iterator<Item = &BitSet> {
        self.matrix.keys()
    }

    fn print(&self) {
        println!("DEBUG - # successors: {}", self.is);
        println!("DEBUG - matrix size: {}", self.matrix.len());
        println!(
            "DEBUG - matrix full @: {}",
            2u32.pow(self.is.try_into().unwrap()) - 1
        );
        for combo in self.matrix.keys() {
            for s in 0..self.is {
                if combo.contains(s) {
                    print!("1");
                } else {
                    print!("0");
                }
            }
            println!();
        }
    }
}

trait Set {
    fn set(&mut self, element: usize, value: bool);
}

impl Set for BitSet {
    fn set(&mut self, element: usize, value: bool) {
        if value {
            self.insert(element);
        } else {
            self.remove(element);
        }
    }
}
