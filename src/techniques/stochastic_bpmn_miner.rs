use crate::ebi_objects::business_process_model_and_notation::{
    BusinessProcessModelAndNotation, BPMNNode, BPMNGateway, GatewayDirection
};
use crate::ebi_objects::stochastic_business_process_model_and_notation::{
    StochasticBusinessProcessModelAndNotation, StochasticSequenceFlow, StochasticTask
};
use crate::ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage;
//use crate::optimisation_algorithms::microlp::microlp::{ComparisonOp, OptimizationDirection, Problem};

use ebi_arithmetic::fraction::Fraction;
use ebi_arithmetic::exact::MaybeExact;

use ebi_optimisation::linear_programming::{OptimisationDirection, ComparisonOp, Problem};

use std::collections::{HashMap, HashSet, VecDeque};
//use crate::math::fraction::MaybeExact;
//use crate::math::fraction::Fraction;
//use crate::math::traits::{Zero};

fn is_diverging_gateway(node: &BPMNGateway) -> bool {
    node.direction == GatewayDirection::Diverging
}

/// Detects pairs of activity names that are concurrent in the BPMN model.
/// For each diverging AND/OR gateway, find the nearest converging join of the same type
/// that is reachable from all branches, then take activities up to (but not beyond) the join
/// as concurrent across branches. Returns pairs of activity names to match estimators.
pub fn detect_concurrent_pairs(_log: &dyn EbiTraitFiniteStochasticLanguage, bpmn: &BusinessProcessModelAndNotation) -> HashSet<(String, String)> {
    // Adjacency
    let mut succ: HashMap<String, Vec<String>> = HashMap::new();
    for f in &bpmn.sequence_flows { succ.entry(f.source_id.clone()).or_default().push(f.target_id.clone()); }

    // Sets and counts
    let activity_ids: HashSet<String> = bpmn.nodes.iter().map(|n| n.id.clone()).collect();
    let and_ids: HashSet<String> = bpmn.and_gateways.iter().map(|g| g.id.clone()).collect();
    let or_ids: HashSet<String> = bpmn.or_gateways.iter().map(|g| g.id.clone()).collect();
    let mut in_deg: HashMap<String, usize> = HashMap::new();
    for f in &bpmn.sequence_flows { *in_deg.entry(f.target_id.clone()).or_default() += 1; in_deg.entry(f.source_id.clone()).or_default(); }

    // BFS that halts expansion at stop nodes (but includes them in visited)
    let mut bfs_until = |start: &str, stop: &HashSet<String>| -> (HashSet<String>, HashMap<String, usize>) {
        let mut vis = HashSet::new();
        let mut dist: HashMap<String, usize> = HashMap::new();
        let mut q = VecDeque::new();
        q.push_back(start.to_string());
        dist.insert(start.to_string(), 0);
        while let Some(u) = q.pop_front() {
            if !vis.insert(u.clone()) { continue; }
            if stop.contains(&u) { continue; }
            if let Some(ns) = succ.get(&u) {
                let du = dist.get(&u).cloned().unwrap_or(0) + 1;
                for v in ns {
                    if !dist.contains_key(v) { dist.insert(v.clone(), du); }
                    if !vis.contains(v) { q.push_back(v.clone()); }
                }
            }
        }
        (vis, dist)
    };

    let mut pairs: HashSet<(String, String)> = HashSet::new();

    // Helper to process a diverging gateway set (AND or OR)
    let mut process_diverging = |gateways: &Vec<BPMNGateway>, is_and: bool| {
        for gw in gateways {
            if !is_diverging_gateway(gw) || gw.outgoing.len() <= 1 { continue; }

            // Gather per-branch reachable nodes and distances (no stop initially)
            let mut branch_joins: Vec<HashSet<String>> = Vec::new();
            let mut branch_dist: Vec<HashMap<String, usize>> = Vec::new();
            for of in &gw.outgoing {
                let empty: HashSet<String> = HashSet::new();
                let (vis, dist) = bfs_until(&of.target_id, &empty);
                let joins: HashSet<String> = vis.into_iter()
                    .filter(|nid| if is_and { and_ids.contains(nid) } else { or_ids.contains(nid) })
                    .filter(|nid| in_deg.get(nid).cloned().unwrap_or(0) > 1)
                    .collect();
                branch_joins.push(joins);
                branch_dist.push(dist);
            }

            if branch_joins.is_empty() { continue; }
            // Common joins across all branches
            let mut common = branch_joins[0].clone();
            for s in &branch_joins[1..] { common = common.intersection(s).cloned().collect(); }
            if common.is_empty() { continue; }

            // Pick nearest join by minimizing max distance over branches
            let mut best: Option<(String, usize)> = None;
            for jid in common {
                let mut ok = true; let mut maxd = 0usize;
                for dist in &branch_dist {
                    match dist.get(&jid) { Some(d) => { if *d > maxd { maxd = *d; } }, None => { ok = false; break; } }
                }
                if ok { match best { Some((_, cur)) if cur <= maxd => {}, _ => best = Some((jid.clone(), maxd)) } }
            }
            let join_id = if let Some((jid, _)) = best { jid } else { continue; };

            // Collect activities up to the join for each branch
            let stop: HashSet<String> = vec![join_id.clone()].into_iter().collect();
            let mut branch_acts: Vec<HashSet<String>> = Vec::new();
            for of in &gw.outgoing {
                let (vis, _d) = bfs_until(&of.target_id, &stop);
                branch_acts.push(vis.into_iter().filter(|nid| activity_ids.contains(nid)).collect());
            }

            // Add cross-branch activity-name pairs
            for i in 0..branch_acts.len() {
                for j in (i+1)..branch_acts.len() {
                    for a in &branch_acts[i] {
                        for b in &branch_acts[j] {
                            if a == b { continue; }
                            if let (Some(na), Some(nb)) = (bpmn.label.get(a), bpmn.label.get(b)) {
                                pairs.insert((na.clone(), nb.clone()));
                            }
                        }
                    }
                }
            }
        }
    };

    process_diverging(&bpmn.and_gateways, true);
    process_diverging(&bpmn.or_gateways, false);

    println!("Concurrent pairs: {:#?}", pairs);
    pairs
}

// Estimates the frequency of each activity in the log and maps it to node IDs using fractions.
pub fn frequency_estimator(log: &dyn EbiTraitFiniteStochasticLanguage, bpmn: &BusinessProcessModelAndNotation) -> HashMap<String, Fraction> {
    let mut count: HashMap<String, Fraction> = HashMap::new();
    let activity_key = log.get_activity_key();
    
    // Count activity occurrences weighted by trace probabilities
    let mut activity_counts: HashMap<String, Fraction> = HashMap::new();
    
    for (trace, probability) in log.iter_trace_probability() {
        if let Ok(exact_fraction) = probability.extract_exact() {
            // Count each activity in the trace, weighted by trace probability
            for activity in trace {
                let activity_name = activity_key.get_activity_label(activity).to_string();
                let current_count = activity_counts.get(&activity_name).cloned().unwrap_or_else(|| Fraction::from((0, 1)));
                let fraction_to_add = match exact_fraction {
                    fraction::GenericFraction::Rational(sign, ratio) => {
                        Fraction::Exact(fraction::GenericFraction::Rational(
                            sign.clone(),
                            ratio.clone()
                        ))
                    }
                    _ => Fraction::from(0)
                };
                activity_counts.insert(activity_name, &current_count + &fraction_to_add);
            }
        }
    }
    
    // Map activity names to node IDs
    for (activity_name, frequency) in activity_counts {
        if let Some(node_id) = bpmn.label.iter()
            .find(|(_id, label)| **label == activity_name)
            .map(|(id, _label)| id) {
            count.insert(node_id.clone(), frequency);
        }
    }
    count
}

/// Estimates left-hand pair frequencies, excluding concurrent pairs, for a BPMN model using fractions.
pub fn lhpair_estimator(log: &dyn EbiTraitFiniteStochasticLanguage, bpmn: &BusinessProcessModelAndNotation) -> HashMap<String, Fraction> {
    let mut counts: HashMap<String, Fraction> = HashMap::new();
    let concurrent: HashSet<(String, String)> = detect_concurrent_pairs(log, bpmn);
    let activity_key = log.get_activity_key();
    
    for (trace, probability) in log.iter_trace_probability() {
        if probability.is_exact() {
            if let Ok(exact_fraction) = probability.extract_exact() {
                let fraction_to_add = match exact_fraction {
                    fraction::GenericFraction::Rational(sign, ratio) => {
                        Fraction::Exact(fraction::GenericFraction::Rational(
                            sign.clone(),
                            ratio.clone()
                        ))
                    }
                    _ => Fraction::from(0)
                };
                
                // Count first activity
                if let Some(first) = trace.first() {
                    let first_str = activity_key.get_activity_label(first).to_string();
                    let current_count = counts.get(&first_str).cloned().unwrap_or_else(|| Fraction::from(0));
                    counts.insert(first_str, &current_count + &fraction_to_add);
                }
                
                // Count left-hand pairs
                for window in trace.windows(2) {
                    let current_str = activity_key.get_activity_label(&window[0]).to_string();
                    let next_str = activity_key.get_activity_label(&window[1]).to_string();
                    // Skip if the model indicates they can be concurrent or are the same
                    if concurrent.contains(&(current_str.clone(), next_str.clone())) || next_str == current_str || concurrent.contains(&(next_str.clone(), current_str.clone())) {
                        continue;
                    }
                    let current_count = counts.get(&next_str).cloned().unwrap_or_else(|| Fraction::from(0));
                    counts.insert(next_str.clone(), &current_count + &fraction_to_add);
                }
            }
        }
    }
    
    // Map activity names to node IDs
    let mut result: HashMap<String, Fraction> = HashMap::new();
    for (activity_name, frequency) in counts {
        if let Some(node_id) = bpmn.label.iter()
            .find(|(_id, label)| **label == activity_name)
            .map(|(id, _label)| id) {
            result.insert(node_id.clone(), frequency);
        } else {
            // Fallback: assume activity name is the node ID
            result.insert(activity_name, frequency);
        }
    }
    result
}

/// Estimates right-hand pair frequencies, excluding concurrent pairs, for a BPMN model using fractions.
pub fn rhpair_estimator(log: &dyn EbiTraitFiniteStochasticLanguage, bpmn: &BusinessProcessModelAndNotation) -> HashMap<String, Fraction> {
    let mut counts: HashMap<String, Fraction> = HashMap::new();
    let concurrent = detect_concurrent_pairs(log, bpmn);
    let activity_key = log.get_activity_key();
    
    for (trace, probability) in log.iter_trace_probability() {
        if probability.is_exact() {
            if let Ok(exact_fraction) = probability.extract_exact() {
                let fraction_to_add = match exact_fraction {
                    fraction::GenericFraction::Rational(sign, ratio) => {
                        Fraction::Exact(fraction::GenericFraction::Rational(
                            sign.clone(),
                            ratio.clone()
                        ))
                    }
                    _ => Fraction::from(0)
                };
                let len = trace.len();
                if len == 0 {
                    continue;
                }

                // Count last activity
                let last_str = activity_key.get_activity_label(&trace[len - 1]).to_string();
                let current_count = counts.get(&last_str).cloned().unwrap_or_else(|| Fraction::from(0));
                counts.insert(last_str, &current_count + &fraction_to_add);
                
                // Count right-hand pairs
                for window in trace.windows(2) {
                    let current_str = activity_key.get_activity_label(&window[0]).to_string();
                    let next_str = activity_key.get_activity_label(&window[1]).to_string();
                    // Skip if the model indicates they can be concurrent or are the same
                    if concurrent.contains(&(current_str.clone(), next_str.clone())) || next_str == current_str {
                        continue;
                    }
                    let current_count = counts.get(&current_str).cloned().unwrap_or_else(|| Fraction::from(0));
                    counts.insert(current_str.clone(), &current_count + &fraction_to_add);
                }
            }
        }
    }
    
    // Map activity names to node IDs
    let mut result: HashMap<String, Fraction> = HashMap::new();
    for (activity_name, frequency) in counts {
        if let Some(node_id) = bpmn.label.iter()
            .find(|(_id, label)| **label == activity_name)
            .map(|(id, _label)| id) {
            result.insert(node_id.clone(), frequency);
        } else {
            // Fallback: assume activity name is the node ID
            result.insert(activity_name, frequency);
        }
    }
    result
}

/// Estimates scaled right-hand pair frequencies for a BPMN model using fractions.
pub fn pairscale_estimator(log: &dyn EbiTraitFiniteStochasticLanguage, bpmn: &BusinessProcessModelAndNotation) -> HashMap<String, Fraction> {
    let counts: HashMap<String, Fraction> = rhpair_estimator(log, bpmn);
    
    // Calculate total trace length as fraction
    let mut trace_total = Fraction::from((0, 1));
    let mut trace_count = 0;
    
    for (trace, probability) in log.iter_trace_probability() {
        if probability.is_exact() {
            if let Ok(exact_fraction) = probability.extract_exact() {
                let trace_length = Fraction::from((trace.len() as i64, 1));
                let fraction_to_add = match exact_fraction {
                    fraction::GenericFraction::Rational(sign, ratio) => {
                        Fraction::Exact(fraction::GenericFraction::Rational(
                            sign.clone(),
                            ratio.clone()
                        ))
                    }
                    _ => Fraction::from(0)
                };
                trace_total = &trace_total + &(&trace_length * &fraction_to_add);
                trace_count += 1;
            }
        }
    }
    
    let mut result: HashMap<String, Fraction> = HashMap::new();
    if trace_count > 0 {
        let transition_freq = if counts.len() > 0 {
            &trace_total / &Fraction::from((counts.len() as i64, 1))
        } else {
            Fraction::from((1, 1))
        };
        
        for (node_id, count) in counts {
            result.insert(node_id, &count / &transition_freq);
        }
    }
    
    result
}

pub fn weight_propagtion_micro_lp(bpmn: &BusinessProcessModelAndNotation, weights: &HashMap<String, Fraction>) -> anyhow::Result<StochasticBusinessProcessModelAndNotation>{
    let n_flows = bpmn.sequence_flows.len();

    //List of tasks without start, end
    let mut tasks: Vec<&BPMNNode> = vec![];
    for task in &bpmn.nodes {
        if !bpmn.start_event.id.eq(&task.id) && !bpmn.end_events.iter().any(|end| end.id == task.id) {
            tasks.push(task);
        }
    }

    if n_flows == 0 {
        // No flows, return empty model
        let tasks: Vec<StochasticTask> = bpmn.nodes.iter().map(|node| {
            let w = weights.get(&node.id).cloned().unwrap_or(Fraction::from(0));
            StochasticTask { id: node.id.clone(), weight: w }
        }).collect();
        
        return Ok(StochasticBusinessProcessModelAndNotation {
            tasks,
            start_event: bpmn.start_event.id.clone(),
            xor_gateways: bpmn.xor_gateways.iter().map(|g| g.id.clone()).collect(),
            and_gateways: bpmn.and_gateways.iter().map(|g| g.id.clone()).collect(),
            or_gateways: bpmn.or_gateways.iter().map(|g| g.id.clone()).collect(),
            end_events: bpmn.end_events.len(),
            sequence_flows: vec![],
        });
    }

    //map incoming, outgoing sequence flows to tasks/gateways
    // Build incoming/outgoing maps for each node
    let mut incoming: HashMap<String, Vec<String>> = HashMap::new();
    let mut outgoing: HashMap<String, Vec<String>> = HashMap::new();
    for flow in &bpmn.sequence_flows {
        let flow_id = format!("flow_{}_{}", flow.source_id, flow.target_id);
        outgoing.entry(flow.source_id.clone()).or_default().push(flow_id.clone());
        incoming.entry(flow.target_id.clone()).or_default().push(flow_id);
    }

    //Setup Problem: Minimize sum of absolute slacks, ignore flow weights in optimisation
    let mut problem = Problem::new(OptimisationDirection::Minimise);

    //Setup variable per sequence flow
    let mut flow_vars = HashMap::new();
    for (_i, flow) in bpmn.sequence_flows.iter().enumerate(){
        let flow_id = format!("flow_{}_{}", flow.source_id, flow.target_id);
        // 0 <= v <= infinity, coefficient = 0
        let v = problem.add_var(Fraction::from(0), (Fraction::from(0), Fraction::infinity()));
        flow_vars.insert(flow_id, v);
    }

    // Add constraints based on nodetype

    //Soft task constraints:
    //Incoming: f - slack <= w and f + slack >= w
    //Outgoing: sum of f - slack <= w and sum of f + slack >= w
    for task in &tasks {
        let in_flows = incoming.get(task.id.as_str()).cloned().unwrap_or_default();
        let out_flows = outgoing.get(task.id.as_str()).cloned().unwrap_or_default();

        if let Some(task_weight) = weights.get(&task.id){
            //Inflows, add one constraint per flow
            for f in &in_flows{
                if let Some(&flow_var) = flow_vars.get(f)  {
                    let slack = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
                    problem.add_constraint([(flow_var, Fraction::from(1)), (slack, -Fraction::from(1))], ComparisonOp::Le, Fraction::from(task_weight));
                    problem.add_constraint([(flow_var, Fraction::from(1)), (slack, Fraction::from(1))], ComparisonOp::Ge, Fraction::from(task_weight));
                }
            }
            //Outflows, add one constraint for all flows
            let slack = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
            let mut sum_vars = vec![];
            for f in &out_flows{
                if let Some(&flow_var) = flow_vars.get(f){
                    sum_vars.push((flow_var, Fraction::from(1)));
                }
            }

            let mut con_le = sum_vars.clone();
            con_le.push((slack, -Fraction::from(1))); 
            let mut con_ge = sum_vars.clone();
            con_ge.push((slack, Fraction::from(1)));
            
            problem.add_constraint(con_le, ComparisonOp::Le, Fraction::from(task_weight));
            problem.add_constraint(con_ge, ComparisonOp::Ge, Fraction::from(task_weight));
        }
    }

    //Soft And constraints:
    // For all incoming, outgoing:
    // flow - first - slack <= 0
    // flow - first + slack >= 0
    for gateway in &bpmn.and_gateways{
        let in_flows = incoming.get(gateway.id.as_str()).cloned().unwrap_or_default();
        let out_flows = outgoing.get(gateway.id.as_str()).cloned().unwrap_or_default();
        let mut all_flows = in_flows.clone();
        all_flows.extend(out_flows.clone());
        if let Some(&first_var) = flow_vars.get(&all_flows[0]){
            for f in &all_flows[1..]{
                if let Some(&flow_var) = flow_vars.get(f){
                    let slack = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
                    let mut con_le = [(first_var, -Fraction::from(1)), (flow_var, Fraction::from(1)), (slack, -Fraction::from(1))];
                    con_le.sort_by_key(|(var, _)| var.idx());
                    let mut con_re = [(first_var, -Fraction::from(1)), (flow_var, Fraction::from(1)), (slack, Fraction::from(1))];
                    con_re.sort_by_key(|(var, _)| var.idx());

                    problem.add_constraint(con_le, ComparisonOp::Le, Fraction::from(0));
                    problem.add_constraint(con_re, ComparisonOp::Ge, Fraction::from(0));
                }
            }
        }
    }

    //Soft XOR constraints:
    // Sum of incoming - Sum of outgoing - slack <= 0
    // Sum of incoming - Sum of outgoing + slack >= 0
    for gateway in &bpmn.xor_gateways{
        let in_flows = incoming.get(gateway.id.as_str()).cloned().unwrap_or_default();
        let out_flows = outgoing.get(gateway.id.as_str()).cloned().unwrap_or_default();
        let mut sum_vars = vec![];
        for f in &in_flows{
            if let Some(&flow_var) = flow_vars.get(f){
                sum_vars.push((flow_var, Fraction::from(1)));
            }
        }
        for f in &out_flows{
            if let Some(&flow_var) = flow_vars.get(f){
                sum_vars.push((flow_var, -Fraction::from(1)));
            }
        }
        let slack = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));

        let mut con_le = sum_vars.clone();
        con_le.push((slack, -Fraction::from(1)));
        con_le.sort_by_key(|(var, _)| var.idx());
        let mut con_ge = sum_vars.clone();
        con_ge.push((slack, Fraction::from(1)));
        con_ge.sort_by_key(|(var, _)| var.idx());
        problem.add_constraint(con_le, ComparisonOp::Le, Fraction::from(0));
        problem.add_constraint(con_ge, ComparisonOp::Ge, Fraction::from(0));
    }

    //Soft OR constraints
    //Diverging: incoming - each outgoing + slack >= 1 (implies  incoming - max(outgoing) + slack >= 1), incoming - sum of outgoing - slack <= -1
    //Converging: outgoing - each incoming + slack >= 1 (implies outgoing . max(incoming) + slack >= 1), outgoing - sum of incoming - slack <= -1
    for gateway in &bpmn.or_gateways{
        let in_flows = incoming.get(gateway.id.as_str()).cloned().unwrap_or_default();
        let out_flows = outgoing.get(gateway.id.as_str()).cloned().unwrap_or_default();
        if gateway.direction == GatewayDirection::Diverging{
            if let Some(&in_var) = flow_vars.get(&in_flows[0]){
                let mut sum_vars = vec![];
                for out_flow in out_flows{
                    if let Some(&out_var) = flow_vars.get(&out_flow){
                        let slack = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
                        let mut con = [(in_var, Fraction::from(1)), (out_var, -Fraction::from(1)), (slack, Fraction::from(1))];
                        con.sort_by_key(|(var, _)| var.idx());
                        problem.add_constraint(con, ComparisonOp::Ge, Fraction::from(1));
                        sum_vars.push((out_var, -Fraction::from(1)))
                    }
                }
                let slack = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
                sum_vars.push((in_var, Fraction::from(1)));
                sum_vars.push((slack, -Fraction::from(1)));
                sum_vars.sort_by_key(|(var, _)| var.idx());
                problem.add_constraint(sum_vars, ComparisonOp::Le, -Fraction::from(1));
            }
        }else{
            if let Some(&out_var) = flow_vars.get(&out_flows[0]){
                let mut sum_vars = vec![];
                for in_flow in in_flows{
                    if let Some(&in_var) = flow_vars.get(&in_flow){
                        let slack = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
                        let mut con = [(out_var, Fraction::from(1)), (in_var, -Fraction::from(1)), (slack, Fraction::from(1))];
                        con.sort_by_key(|(var, _)| var.idx());
                        problem.add_constraint(con, ComparisonOp::Ge, Fraction::from(1));
                        sum_vars.push((in_var, -Fraction::from(1)))
                    }
                }
                let slack = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
                sum_vars.push((out_var, Fraction::from(1)));
                sum_vars.push((slack, -Fraction::from(1)));
                sum_vars.sort_by_key(|(var, _)| var.idx());
                problem.add_constraint(sum_vars, ComparisonOp::Le, -Fraction::from(1));
            }
            }
        }
    let solution = problem.solve().unwrap();
    
    //Retreive result
    let mut flow_weights: HashMap<String, Fraction> = HashMap::new();
    for flow in &bpmn.sequence_flows{
        let flow_id = format!("flow_{}_{}", flow.source_id, flow.target_id);
        if let Some(&flow_var) = flow_vars.get(&flow_id){
            flow_weights.insert(flow_id, solution[flow_var].clone());
        }
     }


    //Scale flow weights
    // All the outgoing flows of a gateway/task are scaled
    // For XOR the sum is scaled to 1
    // For And the max is scaled to 1
    // For OR if max > 0, max is scaled to 1
    // For Tasks the max is scaled to 1

    let mut scaled_flows = HashSet::new();
    for gateway in &bpmn.xor_gateways{
        let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
        let sum: Fraction = out_flows.iter()
            .filter_map(|flow_id| flow_weights.get(flow_id))
            .sum();
        let factor = &Fraction::from(1) / &sum;
        for flow_id in &out_flows{
            if let Some(weight) = flow_weights.get_mut(flow_id){
                *weight *= factor.clone();
                scaled_flows.insert(flow_id.clone());
            }
        }
    }

    for gateway in &bpmn.and_gateways{
        let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
        let max_weight = out_flows.iter()
            .filter_map(|flow_id| flow_weights.get(flow_id))
            .fold(Fraction::from(0), |acc, w| acc.max(w.clone()));
        let factor = &Fraction::from(1) / &max_weight;
        for flow_id in &out_flows{
            if let Some(weight) = flow_weights.get_mut(flow_id){
                *weight *= factor.clone();
                scaled_flows.insert(flow_id.clone());
            }
        }
    }

    for gateway in &bpmn.or_gateways{
                let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
        let max_weight = out_flows.iter()
            .filter_map(|flow_id| flow_weights.get(flow_id))
            .fold(Fraction::from(0), |acc, w| acc.max(w.clone()));
        if max_weight > Fraction::from(1){
            let factor = &Fraction::from(1) / &max_weight;
            for flow_id in &out_flows{
                if let Some(weight) = flow_weights.get_mut(flow_id){
                    *weight *= factor.clone();
                    scaled_flows.insert(flow_id.clone());
                }
            }
        }
    }

    for task in &tasks{
        let out_flows = outgoing.get(&task.id).cloned().unwrap_or_default();
            let max_weight = out_flows.iter()
                .filter_map(|flow_id| flow_weights.get(flow_id))
                .fold(Fraction::from(0), |acc, w| acc.max(w.clone()));
            let factor = &Fraction::from(1) / &max_weight;
            for flow_id in &out_flows{
                if let Some(weight) = flow_weights.get_mut(flow_id){
                    *weight *= factor.clone();
                    scaled_flows.insert(flow_id.clone());
                }
            }
    }

    let mut sequence_flows = Vec::new();
    for flow in &bpmn.sequence_flows {
        let flow_id = format!("flow_{}_{}", flow.source_id, flow.target_id);
        let weight = flow_weights.get(&flow_id).cloned().unwrap_or(Fraction::from(1));
        
        sequence_flows.push(StochasticSequenceFlow {
            source_id: flow.source_id.clone(),
            target_id: flow.target_id.clone(),
            weight,
        });
    }

    let mut stoch_tasks = Vec::new();
    for node in &bpmn.nodes {
        let weight = weights.get(&node.id).cloned().unwrap_or(Fraction::from(0));
        stoch_tasks.push(StochasticTask {
            id: node.id.clone(),
            weight,
        });
    }

    return Ok(StochasticBusinessProcessModelAndNotation {
        tasks: stoch_tasks,
        start_event: bpmn.start_event.id.clone(),
        xor_gateways: bpmn.xor_gateways.iter().map(|g| g.id.clone()).collect(),
        and_gateways: bpmn.and_gateways.iter().map(|g| g.id.clone()).collect(),
        or_gateways: bpmn.or_gateways.iter().map(|g| g.id.clone()).collect(),
        end_events: bpmn.end_events.len(),
        sequence_flows: sequence_flows,
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use std::io::BufReader;
    use std::fs::File;
    use crate::ebi_objects::event_log::EventLog;
    use crate::ebi_objects::finite_stochastic_language::FiniteStochasticLanguage;
    use crate::ebi_framework::importable::Importable;
    use std::time::Instant;


    #[test]
    fn test_basic_compilation() {
        // Simple test to verify the functions compile correctly
        let weights: HashMap<String, u64> = HashMap::new();
        assert!(weights.is_empty());
        
        // Test that our main functions exist and have the right signatures
        let _f = frequency_estimator as fn(&dyn EbiTraitFiniteStochasticLanguage, &BusinessProcessModelAndNotation) -> HashMap<String, Fraction>;
        //let _h = weight_propagation as fn(&BusinessProcessModelAndNotation, &HashMap<String, Fraction>) -> anyhow::Result<StochasticBusinessProcessModelAndNotation>;
        
        println!("All functions compile correctly");
    }

    #[test]
    fn test_gateway_direction_helper() {
        // Test the helper function
        let diverging_gateway = BPMNGateway {
            id: "test".to_string(),
            direction: GatewayDirection::Diverging,
            incoming: vec![],
            outgoing: vec![],
        };
        
        let converging_gateway = BPMNGateway {
            id: "test2".to_string(),
            direction: GatewayDirection::Converging,
            incoming: vec![],
            outgoing: vec![],
        };
        
        assert!(is_diverging_gateway(&diverging_gateway));
        assert!(!is_diverging_gateway(&converging_gateway));
    }

    #[test]
    #[ignore]
    fn test_weight_propagation_from_files() {
        use crate::ebi_traits::ebi_trait_event_log::IndexTrace;
        
        // File paths
        let  bpmn_file = r"C:\Users\larso\OneDrive\Dokumente\RWTH\SS25\Thesis\evaluation\Road_Traffic_Fine_Management_Process.bpmn";
        let log_file = r"C:\Users\larso\OneDrive\Dokumente\RWTH\SS25\Thesis\evaluation\Road_Traffic_Fine_Management_Process.xes";
        
        
        println!("=== Loading files for weight propagation test ===");
        
        let mut start = Instant::now();
        // Load XES eventlog, transform to FIniteStochasticLanguage
        println!("Loading event log from: {}", log_file);
        let log_result = File::open(log_file)
            .map_err(|e| anyhow::anyhow!("Failed to open log file: {}", e))
            .and_then(|file| {
                let mut reader = BufReader::new(file);
                // Step 1: Import as EventLog (handles XES format)
                EventLog::import(&mut reader)
                    .map_err(|e| anyhow::anyhow!("Failed to parse XES: {}", e))
            })
            .map(|event_log| {
                // Step 2: Convert EventLog to FiniteStochasticLanguage
                FiniteStochasticLanguage::from(event_log)
            });
        
        let event_log = match log_result {
            Ok(log) => {
                println!(" Event log loaded successfully");
                println!(" Number of traces: {}", log.len());
                log
            }
            Err(e) => {
                println!(" Failed to load event log: {:?}", e);
                println!(" Please ensure the file exists and is in the correct format");
                return;
            }
        };


        // Load BPMN model using the Importable trait
        println!("Loading BPMN model from: {}", bpmn_file);
        let bpmn_result = File::open(bpmn_file)
            .map_err(|e| anyhow::anyhow!("Failed to open BPMN file: {}", e))
            .and_then(|file| {
                let mut reader = BufReader::new(file);
                BusinessProcessModelAndNotation::import(&mut reader)
            });
        
        let bpmn_model = match bpmn_result {
            Ok(model) => {
                println!(" BPMN model loaded successfully");
                println!(" Number of nodes: {}", model.nodes.len());
                println!(" Number of sequence flows: {}", model.sequence_flows.len());
                println!(" Number of AND gateways: {}", model.and_gateways.len());
                println!(" Number of XOR gateways: {}", model.xor_gateways.len());
                println!(" Number of OR gateways: {}", model.or_gateways.len());
                
                // Display the loaded model
                println!("\n--- Loaded BPMN Model ---");
                println!("{}", model);
                
                model
            }
            Err(e) => {
                println!(" Failed to load BPMN model: {:?}", e);
                println!(" Please ensure the file exists and is in the correct format");
                return;
            }
        };
        let duration = start.elapsed();
        println!("Time conversion: {:?}", duration);

        start = Instant::now();
        // Estimate frequencies from the event log
        let frequencies = lhpair_estimator(&event_log as &dyn EbiTraitFiniteStochasticLanguage, &bpmn_model);

        /*for (node_id, fraction) in &frequencies {
            // Try to get the activity name from the label mapping
            let activity_name = bpmn_model.label.get(node_id).unwrap_or(node_id);
            if let Ok(exact_fraction) = fraction.extract_exact() {
                match exact_fraction {
                    fraction::GenericFraction::Rational(_, ratio) => {
                        let num = ratio.numer();
                        let den = ratio.denom();
                        println!("   {} ({}): {}/{} = {:.3}", activity_name, node_id, num, den, 
                            num.to_string().parse::<f64>().unwrap_or(0.0) / den.to_string().parse::<f64>().unwrap_or(1.0));
                    }
                    _ => {
                        println!("   {} ({}): {}", activity_name, node_id, fraction);
                    }
                }
            } else {
                println!("   {} ({}): {}", activity_name, node_id, fraction);
            }
        }*/
        
        // Use frequencies directly as weights (already mapped to node IDs)
        let weights: HashMap<String, Fraction> = frequencies;
        
        // Apply weight propagation
        let result: Result<StochasticBusinessProcessModelAndNotation, anyhow::Error> = weight_propagtion_micro_lp(&bpmn_model, &weights);
        let duration = start.elapsed();
        println!("Estimator + Propagation time: {:?}", duration);

        match result {
            Ok(stochastic_bpmn) => {
                println!("Weight propagation completed successfully!");
                println!("SBPMN: {}", stochastic_bpmn);
                println!("\n--- Results ---");
                println!("Stochastic BPMN model created with:");
                println!("  - {} tasks", stochastic_bpmn.tasks.len());
                println!("  - {} sequence flows", stochastic_bpmn.sequence_flows.len());
                println!("  - {} AND gateways", stochastic_bpmn.and_gateways.len());
                println!("  - {} XOR gateways", stochastic_bpmn.xor_gateways.len());
                println!("  - {} OR gateways", stochastic_bpmn.or_gateways.len());
                
                // Create a mapping from node ID to node name for better display
                let mut id_to_name: HashMap<String, String> = HashMap::new();
                for (node_id, label) in &bpmn_model.label {
                    id_to_name.insert(node_id.clone(), label.clone());
                }
                
                // Add gateway type mappings
                for gw in &bpmn_model.and_gateways {
                    id_to_name.insert(gw.id.clone(), "and_gateway".to_string());
                }
                for gw in &bpmn_model.xor_gateways {
                    id_to_name.insert(gw.id.clone(), "xor_gateway".to_string());
                }
                for gw in &bpmn_model.or_gateways {
                    id_to_name.insert(gw.id.clone(), "or_gateway".to_string());
                }
                
                println!("\n--- Stochastic BPMN Model (with readable names) ---");
                println!("Tasks:");
                for task in &stochastic_bpmn.tasks {
                    let name = id_to_name.get(&task.id).unwrap_or(&task.id);
                    println!("  - {}: weight {:.2}", name, task.weight);
                }
                
                println!("\nSequence Flows:");
                for flow in &stochastic_bpmn.sequence_flows {
                    let source_name = id_to_name.get(&flow.source_id).unwrap_or(&flow.source_id);
                    let target_name = id_to_name.get(&flow.target_id).unwrap_or(&flow.target_id);
                    println!("  - {} --> {}: weight {:.2}", source_name, target_name, flow.weight);
                }
                
                println!("\nGateways:");
                for gw_id in &stochastic_bpmn.and_gateways {
                    println!("  - and_gateway ({})", gw_id);
                }
                for gw_id in &stochastic_bpmn.xor_gateways {
                    println!("  - xor_gateway ({})", gw_id);
                }
                for gw_id in &stochastic_bpmn.or_gateways {
                    println!("  - or_gateway ({})", gw_id);
                }
                
                println!("\n Test completed successfully!");
                println!("Note: The model includes {} gateways total", 
                    stochastic_bpmn.and_gateways.len() + 
                    stochastic_bpmn.xor_gateways.len() + 
                    stochastic_bpmn.or_gateways.len());
            }
            Err(e) => {
                println!("Weight propagation failed: {:?}", e);
                panic!("Weight propagation failed: {:?}", e);
            }
        }
    }
    
    #[test]
    #[ignore]
    fn evaluation() {        
        // File paths
        let  bpmn_file = r"C:\Users\larso\OneDrive\Dokumente\RWTH\SS25\Thesis\test_models\airline.bpmn";
        let log_file = r"C:\Users\larso\OneDrive\Dokumente\RWTH\SS25\Thesis\eventlogs\airline.xes";
        
        // Load XES eventlog, transform to FIniteStochasticLanguage
        let log_result = File::open(log_file)
            .map_err(|e| anyhow::anyhow!("Failed to open log file: {}", e))
            .and_then(|file| {
                let mut reader = BufReader::new(file);
                // Step 1: Import as EventLog (handles XES format)
                EventLog::import(&mut reader)
                    .map_err(|e| anyhow::anyhow!("Failed to parse XES: {}", e))
            })
            .map(|event_log| {
                // Step 2: Convert EventLog to FiniteStochasticLanguage
                FiniteStochasticLanguage::from(event_log)
            });
        
        let event_log = match log_result {
            Ok(log) => {
                log
            }
            Err(e) => {
                return;
            }
        };


        // Load BPMN model using the Importable trait
        let bpmn_result = File::open(bpmn_file)
            .map_err(|e| anyhow::anyhow!("Failed to open BPMN file: {}", e))
            .and_then(|file| {
                let mut reader = BufReader::new(file);
                BusinessProcessModelAndNotation::import(&mut reader)
            });
        
        let bpmn_model = match bpmn_result {
            Ok(model) => {
                model
            }
            Err(e) => {
                return;
            }
        };


        let start = Instant::now();
        // Estimate frequencies from the event log
        let frequencies = lhpair_estimator(&event_log as &dyn EbiTraitFiniteStochasticLanguage, &bpmn_model);
        let duration = start.elapsed();
        println!("Estimator: {:?}", duration);

        let start = Instant::now();
        // Use frequencies directly as weights (already mapped to node IDs)
        let weights: HashMap<String, Fraction> = frequencies;
        
        // Apply weight propagation
        let result: Result<StochasticBusinessProcessModelAndNotation, anyhow::Error> = weight_propagtion_micro_lp(&bpmn_model, &weights);
        let duration = start.elapsed();
        println!("Propagation time: {:?}", duration);

        match result {
            Ok(stochastic_bpmn) => {
                println!("Weight propagation completed successfully!");
                println!("SBPMN: {}", stochastic_bpmn);
                println!("\n--- Results ---");
                println!("Stochastic BPMN model created with:");
                println!("  - {} tasks", stochastic_bpmn.tasks.len());
                println!("  - {} sequence flows", stochastic_bpmn.sequence_flows.len());
                println!("  - {} AND gateways", stochastic_bpmn.and_gateways.len());
                println!("  - {} XOR gateways", stochastic_bpmn.xor_gateways.len());
                println!("  - {} OR gateways", stochastic_bpmn.or_gateways.len());
                
                // Create a mapping from node ID to node name for better display
                let mut id_to_name: HashMap<String, String> = HashMap::new();
                for (node_id, label) in &bpmn_model.label {
                    id_to_name.insert(node_id.clone(), label.clone());
                }
                
                // Add gateway type mappings
                for gw in &bpmn_model.and_gateways {
                    id_to_name.insert(gw.id.clone(), "and_gateway".to_string());
                }
                for gw in &bpmn_model.xor_gateways {
                    id_to_name.insert(gw.id.clone(), "xor_gateway".to_string());
                }
                for gw in &bpmn_model.or_gateways {
                    id_to_name.insert(gw.id.clone(), "or_gateway".to_string());
                }
                
                println!("\n--- Stochastic BPMN Model (with readable names) ---");
                println!("Tasks:");
                for task in &stochastic_bpmn.tasks {
                    let name = id_to_name.get(&task.id).unwrap_or(&task.id);
                    println!("  - {}: weight {:.2}", name, task.weight);
                }
                
                println!("\nSequence Flows:");
                for flow in &stochastic_bpmn.sequence_flows {
                    let source_name = id_to_name.get(&flow.source_id).unwrap_or(&flow.source_id);
                    let target_name = id_to_name.get(&flow.target_id).unwrap_or(&flow.target_id);
                    println!("  - {} --> {}: weight {:.2}", source_name, target_name, flow.weight);
                }
                
                println!("\nGateways:");
                for gw_id in &stochastic_bpmn.and_gateways {
                    println!("  - and_gateway ({})", gw_id);
                }
                for gw_id in &stochastic_bpmn.xor_gateways {
                    println!("  - xor_gateway ({})", gw_id);
                }
                for gw_id in &stochastic_bpmn.or_gateways {
                    println!("  - or_gateway ({})", gw_id);
                }
                
                println!("\n Test completed successfully!");
                println!("Note: The model includes {} gateways total", 
                    stochastic_bpmn.and_gateways.len() + 
                    stochastic_bpmn.xor_gateways.len() + 
                    stochastic_bpmn.or_gateways.len());
            }
            Err(e) => {
                println!("Weight propagation failed: {:?}", e);
                panic!("Weight propagation failed: {:?}", e);
            }
        }
    }
}