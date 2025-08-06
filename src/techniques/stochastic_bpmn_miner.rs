use crate::ebi_objects::business_process_model_and_notation::{
    BusinessProcessModelAndNotation, BPMNNode, BPMNGateway, GatewayDirection
};
use crate::ebi_objects::stochastic_business_process_model_and_notation::{
    StochasticBusinessProcessModelAndNotation, StochasticSequenceFlow, StochasticTask
};
use crate::ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage;
use crate::optimisation_algorithms::microlp::microlp::{ComparisonOp, OptimizationDirection, Problem };
use std::collections::{HashMap, HashSet, VecDeque};
use crate::math::fraction::MaybeExact;
use crate::math::fraction::Fraction;
use crate::math::traits::{Zero};

fn is_diverging_gateway(node: &BPMNGateway) -> bool {
    node.direction == GatewayDirection::Diverging
}

fn get_gateway_by_id<'a>(gateways: &'a [BPMNGateway], id: &str) -> Option<&'a BPMNGateway> {
    gateways.iter().find(|g| g.id == id)
}

/// Detects pairs of node IDs that are concurrent in the BPMN model.
pub fn detect_concurrent_pairs(_log: &dyn EbiTraitFiniteStochasticLanguage, bpmn: &BusinessProcessModelAndNotation) -> HashSet<(String, String)> {
    let mut id_to_node: HashMap<String, &BPMNNode> = HashMap::new();
    let mut source_to_targets: HashMap<String, Vec<String>> = HashMap::new();
    for node in &bpmn.nodes {
        id_to_node.insert(node.id.clone(), node);
    }
    for flow in &bpmn.sequence_flows {
        source_to_targets.entry(flow.source_id.clone()).or_default().push(flow.target_id.clone());
    }
    let mut concurrent_pairs = HashSet::new();
    for gateway in bpmn.and_gateways.iter().chain(bpmn.or_gateways.iter()) {
        if is_diverging_gateway(gateway) && gateway.outgoing.len() > 1 {
            let mut branches: Vec<HashSet<String>> = Vec::with_capacity(gateway.outgoing.len());
            for out_flow in &gateway.outgoing {
                let mut visited = HashSet::new();
                let mut queue = VecDeque::new();
                queue.push_back(out_flow.target_id.clone());
                while let Some(current_id) = queue.pop_front() {
                    if !visited.insert(current_id.clone()) {
                        continue;
                    }
                    // Stop BFS at join (parallel or inclusive) gateway
                    if let Some(_current_node) = id_to_node.get(&current_id) {
                        if let Some(join_gateway) = get_gateway_by_id(&bpmn.and_gateways, &current_id) {
                            if !is_diverging_gateway(join_gateway) && join_gateway.incoming.len() > 1 {
                                continue;
                            }
                        }
                        if let Some(join_gateway) = get_gateway_by_id(&bpmn.or_gateways, &current_id) {
                            if !is_diverging_gateway(join_gateway) && join_gateway.incoming.len() > 1 {
                                continue;
                            }
                        }
                    }
                    if let Some(next_targets) = source_to_targets.get(&current_id) {
                        for next_id in next_targets {
                            queue.push_back(next_id.clone());
                        }
                    }
                }
                branches.push(visited);
            }
            //Collect unicque pairs between branches
            for (i, branch_i) in branches.iter().enumerate() {
                for branch_j in branches.iter().skip(i + 1) {
                    for a in branch_i {
                        for b in branch_j {
                            if a != b {
                                concurrent_pairs.insert((a.clone(), b.clone()));
                            }
                        }
                    }
                }
            }
        }
    }
    concurrent_pairs
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
                    _ => Fraction::zero()
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
    let concurrent = detect_concurrent_pairs(log, bpmn);
    
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
                    _ => Fraction::zero()
                };
                
                // Count first activity
                if let Some(first) = trace.first() {
                    let first_str = first.to_string();
                    let current_count = counts.get(&first_str).cloned().unwrap_or_else(|| Fraction::from((0, 1)));
                    counts.insert(first_str, &current_count + &fraction_to_add);
                }
                
                // Count left-hand pairs
                for window in trace.windows(2) {
                    let current_str = &window[0].to_string();
                    let next_str = &window[1].to_string();
                    // Skip if the model indicates they can be concurrent or are the same
                    if concurrent.contains(&(current_str.clone(), next_str.clone())) || next_str == current_str || concurrent.contains(&(next_str.clone(), current_str.clone())) {
                        continue;
                    }
                    let current_count = counts.get(next_str).cloned().unwrap_or_else(|| Fraction::from((0, 1)));
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
                    _ => Fraction::zero()
                };
                let len = trace.len();
                if len == 0 {
                    continue;
                }

                // Count last activity
                let last_str = trace[len - 1].to_string();
                let current_count = counts.get(&last_str).cloned().unwrap_or_else(|| Fraction::from((0, 1)));
                counts.insert(last_str, &current_count + &fraction_to_add);
                
                // Count right-hand pairs
                for window in trace.windows(2) {
                    let current_str = &window[0].to_string();
                    let next_str = &window[1].to_string();
                    // Skip if the model indicates they can be concurrent or are the same
                    if concurrent.contains(&(current_str.clone(), next_str.clone())) || next_str == current_str {
                        continue;
                    }
                    let current_count = counts.get(current_str).cloned().unwrap_or_else(|| Fraction::from((0, 1)));
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
                    _ => Fraction::zero()
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


/*pub fn weight_propagation(bpmn: &BusinessProcessModelAndNotation, weights: &HashMap<String, Fraction>) -> anyhow::Result<StochasticBusinessProcessModelAndNotation> {
    let n_flows = bpmn.sequence_flows.len();

    let mut tasks: Vec<&BPMNNode> = vec![];
    for task in &bpmn.nodes {
        if !bpmn.start_event.id.eq(&task.id) && !bpmn.end_events.iter().any(|end| end.id == task.id) {
            tasks.push(task);
        }
    }

    if n_flows == 0 {
        // No flows, return empty model
        let tasks: Vec<StochasticTask> = bpmn.nodes.iter().map(|node| {
            let w = weights.get(&node.id).and_then(|f| f.extract_approx().ok()).unwrap_or(0.0);
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
    let mut incoming: HashMap<&str, Vec<usize>> = HashMap::new();
    let mut outgoing: HashMap<&str, Vec<usize>> = HashMap::new();
    for (idx, flow) in bpmn.sequence_flows.iter().enumerate() {
        outgoing.entry(flow.source_id.as_str()).or_default().push(idx);
        incoming.entry(flow.target_id.as_str()).or_default().push(idx);
    }

    let mut total_slack: usize = 0;

    for task in &tasks {
        let _out_flows = outgoing.get(task.id.as_str()).cloned().unwrap_or_default();
        // Incoming implicit XOR, outgoing implicit AND
        total_slack += 1 + _out_flows.len();
    }

    for gateway in &bpmn.and_gateways {
        let _in_flows = incoming.get(gateway.id.as_str()).cloned().unwrap_or_default();
        let _out_flows = outgoing.get(gateway.id.as_str()).cloned().unwrap_or_default();
        // One constraint per incoming/outgoing flow
        total_slack += _in_flows.len() + _out_flows.len();
    }

    for _gateway in &bpmn.xor_gateways {
        // One constraint per gate
        total_slack += 1;
    }
    for gateway in &bpmn.or_gateways {
        let _in_flows = incoming.get(gateway.id.as_str()).cloned().unwrap_or_default();
        let _out_flows = outgoing.get(gateway.id.as_str()).cloned().unwrap_or_default();
        if gateway.direction == GatewayDirection::Diverging {
            total_slack += _out_flows.len() + 1;
        } else {
            total_slack += _in_flows.len() + 1;
        }
    }
    
    let total_vars = n_flows + total_slack;
    let mut constraints: Vec<SimplexConstraint> = vec![];
    // Create objective: minimize slack (flow vars weight 0, slack vars weight 1)
    let mut objective = vec![Fraction::from(1); total_vars];
    for f in 0..n_flows {
        objective[f] = Fraction::from(0);
    }

    let mut slack_count = 0;
    
    // Soft Task constraints:
    //Incoming: f- slack <= w and -f-slack <= -w
    //Outgoing: sum of f - slack <= w and -sum of f - slack <= -w
    for task in &tasks {
        let _in_flows = incoming.get(task.id.as_str()).cloned().unwrap_or_default();
        let _out_flows = outgoing.get(task.id.as_str()).cloned().unwrap_or_default();
        if let Some(task_weight) = weights.get(&task.id) {
            let mut coeffs_pos = vec![Fraction::from(0); total_vars];
            let mut coeffs_neg = vec![Fraction::from(0); total_vars];
            for in_flow in _in_flows{
                coeffs_pos[in_flow] = Fraction::from(1);
                coeffs_neg[in_flow] = -Fraction::from(1);
            }
            coeffs_pos[n_flows + slack_count] = -Fraction::from(1);
            coeffs_neg[n_flows + slack_count] = -Fraction::from(1);
            slack_count += 1;
            constraints.push(SimplexConstraint::LessThan(coeffs_pos, Fraction::from(task_weight.clone())));
            constraints.push(SimplexConstraint::LessThan(coeffs_neg, -Fraction::from(task_weight.clone())));
            for out_flow in _out_flows{
                let mut coeffs_pos = vec![Fraction::from(0); total_vars];
                let mut coeffs_neg = vec![Fraction::from(0); total_vars];
                coeffs_pos[out_flow] = Fraction::from(1);
                coeffs_neg[out_flow] = -Fraction::from(1);
                coeffs_pos[n_flows + slack_count] = -Fraction::from(1);
                coeffs_neg[n_flows + slack_count] = -Fraction::from(1);
                slack_count += 1;
                constraints.push(SimplexConstraint::LessThan(coeffs_pos, Fraction::from(task_weight.clone())));
                constraints.push(SimplexConstraint::LessThan(coeffs_neg, -Fraction::from(task_weight.clone())));
            }
        }
    }

    // Soft AND Gateway constraints
    // Constraint: For all incoming outgoing: |f-first| - slack <= 0
    for gateway in &bpmn.and_gateways {
        let _in_flows = incoming.get(gateway.id.as_str()).cloned().unwrap_or_default();
        let _out_flows = outgoing.get(gateway.id.as_str()).cloned().unwrap_or_default();
        let all_flows = _in_flows.iter().chain(_out_flows.iter()).cloned().collect::<Vec<_>>();
        let first_flow = all_flows[0];
        for flow in all_flows {
            let mut coeffs_pos = vec![Fraction::from(0); total_vars];
            let mut coeffs_neg = vec![Fraction::from(0); total_vars];
            coeffs_pos[flow] = Fraction::from(1);
            coeffs_neg[flow] = -Fraction::from(1);
            coeffs_pos[first_flow] = -Fraction::from(1);
            coeffs_neg[first_flow] = Fraction::from(1);
            coeffs_pos[n_flows + slack_count] = -Fraction::from(1);
            coeffs_neg[n_flows + slack_count] = -Fraction::from(1);
            slack_count += 1;
            constraints.push(SimplexConstraint::LessThan(coeffs_pos, Fraction::from(0)));
            constraints.push(SimplexConstraint::LessThan(coeffs_neg, Fraction::from(0)))
        }
    }

    // Soft XOR Gateway constraints
    // Sum of incoming - sum of outgoing - slack <= 0
    for gateway in &bpmn.xor_gateways {
        let _in_flows = incoming.get(gateway.id.as_str()).cloned().unwrap_or_default();
        let _out_flows = outgoing.get(gateway.id.as_str()).cloned().unwrap_or_default();
        let mut coeffs_pos = vec![Fraction::from(0); total_vars];
        let mut coeffs_neg = vec![Fraction::from(0); total_vars];
        for in_flow in _in_flows {
            coeffs_pos[in_flow] = Fraction::from(1);
            coeffs_neg[in_flow] = -Fraction::from(1);
        }
        for out_flow in _out_flows {
            coeffs_pos[out_flow] = -Fraction::from(1);
            coeffs_neg[out_flow] = Fraction::from(1);
        }
        coeffs_pos[n_flows + slack_count] = -Fraction::from(1);
        coeffs_neg[n_flows + slack_count] = -Fraction::from(1);
        slack_count += 1;
        constraints.push(SimplexConstraint::LessThan(coeffs_pos, Fraction::from(0)));
        constraints.push(SimplexConstraint::LessThan(coeffs_neg, Fraction::from(0)));
    }

    // Soft Or Gateway constraints
    for gateway in &bpmn.or_gateways {
    let _in_flows = incoming.get(gateway.id.as_str()).cloned().unwrap_or_default();
    let _out_flows = outgoing.get(gateway.id.as_str()).cloned().unwrap_or_default();

    let mut coeffs_pos = vec![Fraction::from(0); total_vars];
    let mut coeffs_neg = vec![Fraction::from(0); total_vars];

    if gateway.direction == GatewayDirection::Diverging {
        // Diverging OR: in_sum >= max(outgoing) + 1, in_sum <= sum(outgoing) - 1
        // in_sum - max(outgoing) - 1 >= -slack
        for out_flow in _out_flows.clone() {
            coeffs_pos = vec![Fraction::from(0); total_vars];
            for in_flow in _in_flows.clone() {
                coeffs_pos[in_flow] = Fraction::from(1);
            }
            coeffs_pos[out_flow] = -Fraction::from(1);
            coeffs_pos[n_flows + slack_count] = Fraction::from(-1);
            slack_count += 1;
            constraints.push(SimplexConstraint::GreaterThan(coeffs_pos, Fraction::from(1)));
        }
        // in_sum - sum(outgoing) + 1 <= slack
        coeffs_neg = vec![Fraction::from(0); total_vars];
        for in_flow in _in_flows {
            coeffs_neg[in_flow] = Fraction::from(1);
        }
        for out_flow in _out_flows {
            coeffs_neg[out_flow] = -Fraction::from(1);
        }
        coeffs_neg[n_flows + slack_count] = Fraction::from(1);
        slack_count += 1;
        constraints.push(SimplexConstraint::LessThan(coeffs_neg, Fraction::from(-1)));
    } else {
        // Converging OR: outgoing in (max(incoming)+1, sum(incoming)-1)

        // out_flow - max(incoming) - 1 >= -slack
        for in_flow in _in_flows.clone() {
            coeffs_pos = vec![Fraction::from(0); total_vars];
            for out_flow in _out_flows.clone() {
                coeffs_pos[out_flow] = Fraction::from(1);
            }
            coeffs_pos[in_flow] = -Fraction::from(1);
            coeffs_pos[n_flows + slack_count] = Fraction::from(-1);
            slack_count += 1;
            constraints.push(SimplexConstraint::GreaterThan(coeffs_pos, Fraction::from(1)));
        }
        // out_sum - sum(incoming) + 1 <= slack
        coeffs_neg = vec![Fraction::from(0); total_vars];
        for out_flow in _out_flows {
            coeffs_neg[out_flow] = Fraction::from(1);
        }
        for in_flow in _in_flows {
            coeffs_neg[in_flow] = -Fraction::from(1);
        }
        coeffs_neg[n_flows + slack_count] = Fraction::from(1);
        slack_count += 1;
        constraints.push(SimplexConstraint::LessThan(coeffs_neg, Fraction::from(-1)));
    }
}

    /*for var in 0..(total_vars - total_slack) {
        let mut coeffs = vec![Fraction::from(0); total_vars];
        coeffs[var] = Fraction::from(1);
        constraints.push(SimplexConstraint::GreaterThan(coeffs, Fraction::from(0)));
    }*/

    // Debug output
    println!("Slack count: {}", slack_count);
    println!("Slack + n_flows: {}", total_slack + n_flows);
    println!("Simplex setup with split slack variables:");    println!("  Flow variables: {}", n_flows);
    println!("  Total slack variables: {}", total_slack);
    println!("  Total variables: {}", total_vars);
    println!("  Constraints: {}", constraints.len());
    println!("All constraints {:?}", constraints);
    println!("Objective: {:?}", objective.clone());
    // Solve
    let simplex_result = Simplex::minimize(objective).with(constraints);
    let mut simplex = match simplex_result {
        Ok(s) => s,
        Err(e) => return Err(anyhow::anyhow!("Simplex setup failed: {}", e)),
    };
    


    let solve_result = simplex.solve();
    println!("Solve result: {:?}", solve_result);
    for var in 0..total_vars{
        println!("Var {}: {:?}", var, simplex.get_var(var));
    }
    let solve_two = simplex.solve();
    println!("Second solve: {:?}", solve_two);
    for var in 0..total_vars{
        println!("Var {}: {:?}", var, simplex.get_var(var));
    }

    // Extract flow weights 
    let mut sequence_flows = Vec::new();
    for (idx, flow) in bpmn.sequence_flows.iter().enumerate() {
        let var_id = idx + 1; // Simplex uses 1-based indexing
        let val = simplex.get_var(var_id).unwrap_or_else(|| Fraction::from(0));
        let weight = val.extract_approx().unwrap_or(0.0).max(0.0);
        
        sequence_flows.push(StochasticSequenceFlow {
            source_id: flow.source_id.clone(),
            target_id: flow.target_id.clone(),
            weight,
        });
    }
    
    // Build tasks
    let tasks: Vec<StochasticTask> = bpmn.nodes.iter().map(|node| {
        let w = weights.get(&node.id).and_then(|f| f.extract_approx().ok()).unwrap_or(0.0);
        StochasticTask { id: node.id.clone(), weight: w }
    }).collect();
    
    Ok(StochasticBusinessProcessModelAndNotation {
        tasks,
        start_event: bpmn.start_event.id.clone(),
        xor_gateways: bpmn.xor_gateways.iter().map(|g| g.id.clone()).collect(),
        and_gateways: bpmn.and_gateways.iter().map(|g| g.id.clone()).collect(),
        or_gateways: bpmn.or_gateways.iter().map(|g| g.id.clone()).collect(),
        end_events: bpmn.end_events.len(),
        sequence_flows,
    })
} */


/*pub fn weight_propagation_good_lp(bpmn: &BusinessProcessModelAndNotation, weights: &HashMap<String, Fraction>) -> anyhow::Result<StochasticBusinessProcessModelAndNotation> {
    
    // Convert task weights to node weights (convert fractions to f64 for LP solving)
    let node_weights_by_id: HashMap<String, f64> = weights.iter()
        .map(|(id, fraction)| {
            let weight = if let Ok(exact_fraction) = fraction.extract_exact() {
                match exact_fraction {
                    fraction::GenericFraction::Rational(_, ratio) => {
                        let num_val: f64 = ratio.numer().to_string().parse().unwrap_or(0.0);
                        let den_val: f64 = ratio.denom().to_string().parse().unwrap_or(1.0);
                        if den_val != 0.0 { num_val / den_val } else { 0.0 }
                    }
                    _ => 0.0
                }
            } else if let Ok(approx_val) = fraction.extract_approx() {
                approx_val
            } else {
                0.0
            };
            (id.clone(), weight)
        })
        .collect();

    // Setup variables for each sequence flow
    variables! { vars: }
    let mut flow_vars = HashMap::new();
    for (i, flow) in bpmn.sequence_flows.iter().enumerate() {
        let flow_id = format!("flow_{}_{}", flow.source_id, flow.target_id);
        let v = variable().min(0.0).name(&format!("flow_{}", i));
        flow_vars.insert(flow_id, vars.add(v));
    }

    // Slack variable setup for soft constraints
    #[derive(Debug, Clone)]
    enum SlackKind {
        TaskIn(String),
        TaskOut(String), 
        AndGateway(String),
        XorGateway(String),
        OrGateway(String),
    }
    
    #[derive(Debug, Clone)]
    struct SlackRequest {
        kind: SlackKind,
    }
    
    let mut slack_requests = Vec::new();

    // Build incoming/outgoing maps for each node
    let mut incoming: HashMap<String, Vec<String>> = HashMap::new();
    let mut outgoing: HashMap<String, Vec<String>> = HashMap::new();
    for flow in &bpmn.sequence_flows {
        let flow_id = format!("flow_{}_{}", flow.source_id, flow.target_id);
        outgoing.entry(flow.source_id.clone()).or_default().push(flow_id.clone());
        incoming.entry(flow.target_id.clone()).or_default().push(flow_id);
    }

    // Add Slack per Task constraint
    for node in &bpmn.nodes {
        let in_flows = incoming.get(&node.id).cloned().unwrap_or_default();
        let out_flows = outgoing.get(&node.id).cloned().unwrap_or_default();
        
        if node_weights_by_id.contains_key(&node.id) {
            for f in &in_flows {
                slack_requests.push(SlackRequest {
                    kind: SlackKind::TaskIn(f.clone()),
                });
            }
            if !out_flows.is_empty() {
                slack_requests.push(SlackRequest {
                    kind: SlackKind::TaskOut("sum".to_string()),
                });
            }
        }
    }

    // Add Slack per AND Gateway constraint
    for gateway in &bpmn.and_gateways {
        let in_flows = incoming.get(&gateway.id).cloned().unwrap_or_default();
        let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
        let mut all_flows = in_flows.clone();
        all_flows.extend(out_flows);
        
        if all_flows.len() > 1 {
            // Need (total_flows - 1) slack variables for equality constraints
            for f in &all_flows[1..] {
                slack_requests.push(SlackRequest {
                    kind: SlackKind::AndGateway(f.clone()),
                });
            }
        }
    }

    // Add Slack per XOR Gateway constraint
    for gateway in &bpmn.xor_gateways {
        let in_flows = incoming.get(&gateway.id).cloned().unwrap_or_default();
        let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
        if !in_flows.is_empty() && !out_flows.is_empty() {
            slack_requests.push(SlackRequest {
                kind: SlackKind::XorGateway(gateway.id.clone()),
            });
        }
    }

    // Add Slack per OR Gateway constraint
    for gateway in &bpmn.or_gateways {
        let in_flows = incoming.get(&gateway.id).cloned().unwrap_or_default();
        let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
        if !in_flows.is_empty() && !out_flows.is_empty() {
            slack_requests.push(SlackRequest {
                kind: SlackKind::OrGateway(gateway.id.clone()),
            });
        }
    }

    // Add all slack variables to vars, and store in a Vec in the same order
    let mut slack_vars = Vec::new();
    let mut or_slack_indices = Vec::new();
    let mut other_slack_indices = Vec::new();
    
    for (idx, req) in slack_requests.iter().enumerate() {
        let name = match &req.kind {
            SlackKind::TaskIn(fid) => format!("slack_task_in_{}", fid),
            SlackKind::TaskOut(fid) => format!("slack_task_out_{}", fid),
            SlackKind::AndGateway(fid) => format!("slack_and_{}", fid),
            SlackKind::XorGateway(fid) => format!("slack_xor_{}", fid),
            SlackKind::OrGateway(fid) => format!("slack_or_{}", fid),
        };
        slack_vars.push(vars.add(variable().min(0.0).name(&name)));
        match &req.kind {
            SlackKind::OrGateway(_) => or_slack_indices.push(idx),
            _ => other_slack_indices.push(idx),
        }
    }

    // All slack variables have the same weight in the objective
    // Prioritize slack variables
    let mut objective = Expression::from(0.0);
    // Keep or gateways more flexible: lower penalty
    for &idx in &or_slack_indices {
        objective = objective + slack_vars[idx] * 1.0;
    }
    // Keep other slack more stiff with higher penalty
    for &idx in &other_slack_indices {
        objective = objective + slack_vars[idx] * 10.0;
    }


    let mut model = vars.minimise(objective).using(default_solver);

    let mut slack_iter = slack_vars.iter();
    
    // Add task constraints with slack variables
    for node in &bpmn.nodes {
        let in_flows = incoming.get(&node.id).cloned().unwrap_or_default();
        let out_flows = outgoing.get(&node.id).cloned().unwrap_or_default();

        if let Some(&w) = node_weights_by_id.get(&node.id) {
            // Soft constraint: Each incoming flow = task weight (implicit XOR Gateway)
            for f in &in_flows {
                if let Some(&flow_var) = flow_vars.get(f) {
                    let slack = *slack_iter.next().unwrap();
                    model = model
                        .with(constraint!(flow_var - w <= slack))
                        .with(constraint!(w - flow_var <= slack));
                }
            }
            
            // Soft constraint: Sum of outgoing flows = task weight (implicit AND Gateway)
            if !out_flows.is_empty() {
                let slack = *slack_iter.next().unwrap();
                let mut sum_expr = Expression::from(0.0);
                for f in &out_flows {
                    if let Some(&flow_var) = flow_vars.get(f) {
                        sum_expr = sum_expr + flow_var;
                    }
                }
                model = model
                    .with(constraint!(sum_expr.clone() - w <= slack))
                    .with(constraint!(w - sum_expr <= slack));
            }
        }
    }

    // Add AND gateway constraints with slack variables
    for gateway in &bpmn.and_gateways {
        let in_flows = incoming.get(&gateway.id).cloned().unwrap_or_default();
        let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
        let mut all_flows = in_flows.clone();
        all_flows.extend(out_flows.clone());
        if all_flows.len() > 1 {
            if let Some(&first_var) = flow_vars.get(&all_flows[0]) {
                for f in &all_flows[1..] {
                    if let Some(&flow_var) = flow_vars.get(f) {
                        let slack = *slack_iter.next().unwrap();
                        model = model
                            .with(constraint!(flow_var - first_var <= slack))
                            .with(constraint!(first_var - flow_var <= slack));
                    }
                }
            }
        }
    }

    // Add XOR gateway constraints with slack variables
    for gateway in &bpmn.xor_gateways {
        let in_flows = incoming.get(&gateway.id).cloned().unwrap_or_default();
        let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
        
        if !in_flows.is_empty() && !out_flows.is_empty() {
            // Soft constraint: sum(incoming) = sum(outgoing)
            let slack = *slack_iter.next().unwrap();
            
            let in_sum: Expression = in_flows.iter().filter_map(|f| flow_vars.get(f).copied()).fold(Expression::from(0.0), |acc, v| acc + v);
            let out_sum: Expression = out_flows.iter().filter_map(|f| flow_vars.get(f).copied()).fold(Expression::from(0.0), |acc, v| acc + v);
            
            model = model
                .with(constraint!(in_sum.clone() - out_sum.clone() <= slack))
                .with(constraint!(out_sum - in_sum <= slack));
        }
    }

    // Add OR gateway constraints with slack variables
    for gateway in &bpmn.or_gateways {
        let in_flows = incoming.get(&gateway.id).cloned().unwrap_or_default();
        let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
        
        if !in_flows.is_empty() && !out_flows.is_empty() {
            let slack = *slack_iter.next().unwrap();
            
            if gateway.direction == GatewayDirection::Diverging {
                // For diverging OR: incoming = [max(outgoing)+1, sum(outgoing)-1]
                let out_vars: Vec<_> = out_flows.iter().filter_map(|f| flow_vars.get(f).copied()).collect();
                let out_sum: Expression = out_vars.iter().fold(Expression::from(0.0), |acc, &v| acc + v);
                let in_sum: Expression = in_flows.iter().filter_map(|f| flow_vars.get(f).copied()).fold(Expression::from(0.0), |acc, v| acc + v);

                // in_sum >= max(outgoing) + 1
                for &v in &out_vars {
                    model = model.with(constraint!(in_sum.clone() - (v + 1.0) >= -slack));
                }
                // in_sum <= sum(outgoing) - 1
                model = model.with(constraint!(in_sum.clone() - (out_sum.clone() - 1.0) <= slack));
            } else {
                // For converging OR:
                // For outgoing weight x, all incoming weights in (0, x)
                // sum of all incoming weights >= outgoing weight
                // outgoing weight in (max(incoming)+1, sum(incoming)-1)

                if !out_flows.is_empty() {
                    if let Some(&x) = flow_vars.get(&out_flows[0]) {
                        let in_vars: Vec<_> = in_flows.iter().filter_map(|f| flow_vars.get(f).copied()).collect();
                        let in_sum: Expression = in_vars.iter().fold(Expression::from(0.0), |acc, &v| acc + v);

                        // Each incoming in (0, x)
                        for &v in &in_vars {
                            model = model
                                .with(constraint!(v >= 0.0 - slack))
                                .with(constraint!(v <= x + slack));
                        }
                        // sum of all incoming >= outgoing
                        model = model.with(constraint!(in_sum.clone() - x >= -slack));

                        // outgoing >= max(incoming) + 1
                        for &v in &in_vars {
                            model = model.with(constraint!(x - (v + 1.0) >= -slack));
                        }
                        // outgoing <= sum(incoming) - 1
                        model = model.with(constraint!(x - (in_sum.clone() - 1.0) <= slack));
                    }
                }
            }
        }
    }

    // Solve and update sequence_flows
    let solution = model.solve().map_err(|e| anyhow::anyhow!("LP solve failed: {:?}", e))?;
    
    // First pass: collect raw weights from solution
    let mut flow_weights: HashMap<String, f64> = HashMap::new();
    for flow in &bpmn.sequence_flows {
        let flow_id = format!("flow_{}_{}", flow.source_id, flow.target_id);
        let weight = if let Some(&flow_var) = flow_vars.get(&flow_id) {
            solution.value(flow_var).max(0.0)
        } else {
            1.0 // Fallback
        };
        flow_weights.insert(flow_id, weight);
    }
    
    // Second pass: apply scaling for all sequence flows
    println!("Applying post-processing scaling for all sequence flows...");
    
    // Create a set to track which flows have been scaled
    let mut scaled_flows = HashSet::new();
    
    // Scale diverging XOR gateways: sum of outgoing flows = 1.0
    for gateway in &bpmn.xor_gateways {
        if gateway.direction == GatewayDirection::Diverging {
            let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
            if out_flows.len() > 1 {
                // Calculate current sum of outgoing flows
                let current_sum: f64 = out_flows.iter()
                    .filter_map(|flow_id| flow_weights.get(flow_id))
                    .sum();
                
                if current_sum > 0.0 {
                    // Scale all outgoing flows so they sum to 1.0
                    let scale_factor = 1.0 / current_sum;
                    println!("Scaling XOR gateway {} outgoing flows by factor {:.3} (sum was {:.3})", 
                        gateway.id, scale_factor, current_sum);
                    
                    for flow_id in &out_flows {
                        if let Some(weight) = flow_weights.get_mut(flow_id) {
                            *weight *= scale_factor;
                            scaled_flows.insert(flow_id.clone());
                        }
                    }
                }
            } else if out_flows.len() == 1 {
                // Single outgoing flow should be 1.0
                if let Some(flow_id) = out_flows.first() {
                    if let Some(weight) = flow_weights.get_mut(flow_id) {
                        *weight = 1.0;
                        scaled_flows.insert(flow_id.clone());
                        println!("Setting single XOR gateway {} outgoing flow to 1.0", gateway.id);
                    }
                }
            }
        }
    }
    
    // Scale diverging AND gateways: largest outgoing flow = 1.0, others scale proportionally
    for gateway in &bpmn.and_gateways {
        if gateway.direction == GatewayDirection::Diverging {
            let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
            if out_flows.len() > 1 {
                // Find maximum outgoing flow weight
                let max_weight = out_flows.iter()
                    .filter_map(|flow_id| flow_weights.get(flow_id))
                    .fold(0.0f64, |acc, &w| acc.max(w));
                
                if max_weight > 0.0 {
                    // Scale all outgoing flows so the maximum becomes 1.0
                    let scale_factor = 1.0 / max_weight;
                    println!("Scaling AND gateway {} outgoing flows by factor {:.3} (max was {:.3})", 
                        gateway.id, scale_factor, max_weight);
                    
                    for flow_id in &out_flows {
                        if let Some(weight) = flow_weights.get_mut(flow_id) {
                            *weight *= scale_factor;
                            scaled_flows.insert(flow_id.clone());
                        }
                    }
                }
            } else if out_flows.len() == 1 {
                // Single outgoing flow should be 1.0
                if let Some(flow_id) = out_flows.first() {
                    if let Some(weight) = flow_weights.get_mut(flow_id) {
                        *weight = 1.0;
                        scaled_flows.insert(flow_id.clone());
                        println!("Setting single AND gateway {} outgoing flow to 1.0", gateway.id);
                    }
                }
            }
        }
    }
    
    // Scale diverging OR gateways: largest outgoing flow = 1.0, others scale proportionally (same as AND)
    for gateway in &bpmn.or_gateways {
        if gateway.direction == GatewayDirection::Diverging {
            let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
            if out_flows.len() > 1 {
                // Find maximum outgoing flow weight
                let max_weight = out_flows.iter()
                    .filter_map(|flow_id| flow_weights.get(flow_id))
                    .fold(0.0f64, |acc, &w| acc.max(w));
                
                if max_weight > 0.0 {
                    // Scale all outgoing flows so the maximum becomes 1.0
                    let scale_factor = 1.0 / max_weight;
                    println!("Scaling OR gateway {} outgoing flows by factor {:.3} (max was {:.3})", 
                        gateway.id, scale_factor, max_weight);
                    
                    for flow_id in &out_flows {
                        if let Some(weight) = flow_weights.get_mut(flow_id) {
                            *weight *= scale_factor;
                            scaled_flows.insert(flow_id.clone());
                        }
                    }
                }
            } else if out_flows.len() == 1 {
                // Single outgoing flow should be 1.0
                if let Some(flow_id) = out_flows.first() {
                    if let Some(weight) = flow_weights.get_mut(flow_id) {
                        *weight = 1.0;
                        scaled_flows.insert(flow_id.clone());
                        println!("Setting single OR gateway {} outgoing flow to 1.0", gateway.id);
                    }
                }
            }
        }
    }
    
    // Scale outgoing flows from tasks and other nodes
    for node in &bpmn.nodes {
        let out_flows = outgoing.get(&node.id).cloned().unwrap_or_default();
        if !out_flows.is_empty() {
            // Check if any of these flows have already been scaled
            let unscaled_flows: Vec<_> = out_flows.iter()
                .filter(|flow_id| !scaled_flows.contains(*flow_id))
                .collect();
            
            if !unscaled_flows.is_empty() {
                if unscaled_flows.len() == 1 {
                    // Single outgoing flow should be 1.0
                    if let Some(flow_id) = unscaled_flows.first() {
                        if let Some(weight) = flow_weights.get_mut(*flow_id) {
                            *weight = 1.0;
                            scaled_flows.insert((*flow_id).clone());
                            println!("Setting single task {} outgoing flow to 1.0", node.id);
                        }
                    }
                } else {
                    // Multiple outgoing flows: treat like XOR (sum = 1.0)
                    let current_sum: f64 = unscaled_flows.iter()
                        .filter_map(|flow_id| flow_weights.get(*flow_id))
                        .sum();
                    
                    if current_sum > 0.0 {
                        let scale_factor = 1.0 / current_sum;
                        println!("Scaling task {} outgoing flows by factor {:.3} (sum was {:.3})", 
                            node.id, scale_factor, current_sum);
                        
                        for flow_id in &unscaled_flows {
                            if let Some(weight) = flow_weights.get_mut(*flow_id) {
                                *weight *= scale_factor;
                                scaled_flows.insert((*flow_id).clone());
                            }
                        }
                    }
                }
            }
        }
    }
    
    // Scale outgoing flows from converging gateways
    for gateway in &bpmn.xor_gateways {
        if gateway.direction == GatewayDirection::Converging {
            let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
            let unscaled_flows: Vec<_> = out_flows.iter()
                .filter(|flow_id| !scaled_flows.contains(*flow_id))
                .collect();
            
            if !unscaled_flows.is_empty() {
                for flow_id in &unscaled_flows {
                    if let Some(weight) = flow_weights.get_mut(*flow_id) {
                        *weight = 1.0;
                        scaled_flows.insert((*flow_id).clone());
                        println!("Setting converging XOR gateway {} outgoing flow to 1.0", gateway.id);
                    }
                }
            }
        }
    }
    
    for gateway in &bpmn.and_gateways {
        if gateway.direction == GatewayDirection::Converging {
            let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
            let unscaled_flows: Vec<_> = out_flows.iter()
                .filter(|flow_id| !scaled_flows.contains(*flow_id))
                .collect();
            
            if !unscaled_flows.is_empty() {
                for flow_id in &unscaled_flows {
                    if let Some(weight) = flow_weights.get_mut(*flow_id) {
                        *weight = 1.0;
                        scaled_flows.insert((*flow_id).clone());
                        println!("Setting converging AND gateway {} outgoing flow to 1.0", gateway.id);
                    }
                }
            }
        }
    }
    
    for gateway in &bpmn.or_gateways {
        if gateway.direction == GatewayDirection::Converging {
            let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
            let unscaled_flows: Vec<_> = out_flows.iter()
                .filter(|flow_id| !scaled_flows.contains(*flow_id))
                .collect();
            
            if !unscaled_flows.is_empty() {
                for flow_id in &unscaled_flows {
                    if let Some(weight) = flow_weights.get_mut(*flow_id) {
                        *weight = Fraction::from(1);
                        scaled_flows.insert((*flow_id).clone());
                    }
                }
            }
        }
    }
    
    // Create final sequence flows with scaled weights
    let mut sequence_flows = Vec::new();
    for flow in &bpmn.sequence_flows {
        let flow_id = format!("flow_{}_{}", flow.source_id, flow.target_id);
        let weight = flow_weights.get(&flow_id).copied().unwrap_or(Fraction::from(1));
        
        sequence_flows.push(StochasticSequenceFlow {
            source_id: flow.source_id.clone(),
            target_id: flow.target_id.clone(),
            weight,
        });
    }

    // Create tasks with original weights (no scaling)
    let mut tasks = Vec::new();
    for node in &bpmn.nodes {
        let weight = node_weights_by_id.get(&node.id).copied().unwrap_or(0.0);
        tasks.push(StochasticTask {
            id: node.id.clone(),
            weight,
        });
    }

    Ok(StochasticBusinessProcessModelAndNotation {
        tasks,
        start_event: bpmn.start_event.id.clone(),
        xor_gateways: bpmn.xor_gateways.iter().map(|g| g.id.clone()).collect(),
        and_gateways: bpmn.and_gateways.iter().map(|g| g.id.clone()).collect(),
        or_gateways: bpmn.or_gateways.iter().map(|g| g.id.clone()).collect(),
        end_events: bpmin.end_events.len(),
        sequence_flows,
    })
}
*/

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

    //Setup Problem: Minimize sum of absolute slacks, ignore flow weights in optimization
    let mut problem = Problem::new(OptimizationDirection::Minimize);

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
                    problem.add_constraint([(flow_var, Fraction::from(1)), (first_var, -Fraction::from(1)), (slack, -Fraction::from(1))], ComparisonOp::Le, Fraction::from(0));
                    problem.add_constraint([(flow_var, Fraction::from(1)), (first_var, -Fraction::from(1)), (slack, Fraction::from(1))], ComparisonOp::Ge, Fraction::from(0));
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
        let mut con_ge = sum_vars.clone();
        con_ge.push((slack, Fraction::from(1)));
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
                        problem.add_constraint([(in_var, Fraction::from(1)), (out_var, -Fraction::from(1)), (slack, Fraction::from(1))], ComparisonOp::Ge, Fraction::from(1));
                        sum_vars.push((out_var, -Fraction::from(1)))
                    }
                }
                let slack = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
                sum_vars.push((in_var, Fraction::from(1)));
                sum_vars.push((slack, -Fraction::from(1)));
                problem.add_constraint(sum_vars, ComparisonOp::Le, -Fraction::from(1));
            }
        }else{
            if let Some(&out_var) = flow_vars.get(&out_flows[0]){
                let mut sum_vars = vec![];
                for in_flow in in_flows{
                    if let Some(&in_var) = flow_vars.get(&in_flow){
                        let slack = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
                        problem.add_constraint([(out_var, Fraction::from(1)), (in_var, -Fraction::from(1)), (slack, Fraction::from(1))], ComparisonOp::Ge, Fraction::from(1));
                        sum_vars.push((in_var, -Fraction::from(1)))
                    }
                }
                let slack = problem.add_var(Fraction::from(1), (Fraction::from(0), Fraction::infinity()));
                sum_vars.push((out_var, Fraction::from(1)));
                sum_vars.push((slack, -Fraction::from(1)));
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
    let mut scaled_flows = HashSet::new();
    for gateway in &bpmn.xor_gateways{
        let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
        //if gateway.direction == GatewayDirection::Diverging {
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
        //}
    }

    for gateway in &bpmn.and_gateways{
        let out_flows = outgoing.get(&gateway.id).cloned().unwrap_or_default();
        //if gateway.direction == GatewayDirection::Diverging{
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
        //}
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

    /// Test function that loads BPMN model and event log from files
    
    #[test]
    #[ignore]
    fn solve_minimal_example(){
        use crate::optimisation_algorithms::simplex::{Simplex, SimplexConstraint};
        let program = Simplex::minimize(vec![Fraction::from(0), Fraction::from(0), Fraction::from(1), Fraction::from(1)])
        .with(vec![
            SimplexConstraint::LessThan(vec![Fraction::from(1), Fraction::from(0), -Fraction::from(1), Fraction::from(0)], Fraction::from(1)),
            SimplexConstraint::LessThan(vec![-Fraction::from(1), Fraction::from(0), -Fraction::from(1), Fraction::from(0)], -Fraction::from(1)),
            SimplexConstraint::LessThan(vec![Fraction::from(0), Fraction::from(1), Fraction::from(0), -Fraction::from(1)], Fraction::from(1)),
            SimplexConstraint::LessThan(vec![Fraction::from(0), -Fraction::from(1), Fraction::from(0), -Fraction::from(1)], -Fraction::from(1)),
        ]);
        let mut simplex = program.unwrap();
        println!("Result: {:?}", simplex.solve());
        for var in 1..5{
            println!("Var {}: {:?}", var, simplex.get_var(var));
        }

    }


    #[test]
    #[ignore]
    fn test_simplex(){
        use simplex::*;
        let program = Simplex::minimize(&vec![0.0, 0.0, 1.0, 1.0])
        .with(vec![
            SimplexConstraint::LessThan(vec![1.0, 0.0, -1.0, 0.0], 1.0),
            SimplexConstraint::LessThan(vec![-1.0, 0.0, -1.0, 0.0], -1.0),
            SimplexConstraint::LessThan(vec![0.0, 1.0, 0.0, -1.0], 1.0),
            SimplexConstraint::LessThan(vec![0.0, -1.0, 0.0, -1.0], -1.0),
        ]);
        let mut simplex = program.unwrap();
        println!("Result: {:?}", simplex.solve());
        for var in 0..5{
            println!("Var {}: {:?}", var, simplex.get_var(var));
        }
    }
    
    #[test]
    #[ignore]
    fn test_good_lp(){
        use good_lp::{variables, constraint, default_solver, SolverModel, Solution};
        variables! { 
            vars:
                x1 >= 0.0;
                x2 >= 0.0;
                s1 >= 0.0;
                s2 >= 0.0;
        }

        // Objective: Minimize s1 + s2
        let objective = s1 + s2;

        // Constraints:
        // x1 - s1 <= 1
        // x1 + s1 >= 1
        // x2 - s2 <= 1
        // x2 + s2 >= 1
        let mut model = vars.minimise(objective).using(default_solver);

        model = model
            .with(constraint!(x1 - s1 <= 1.0))
            .with(constraint!(-x1 - s1 <= -1.0))
            .with(constraint!(x2 - s2 <= 1.0))
            .with(constraint!(-x2 + -s2 <= -1.0));

        let solution = model.solve().unwrap();
        println!("x1: {:?}", solution.value(x1));
        println!("x2: {:?}", solution.value(x2));
        println!("s1: {:?}", solution.value(s1));
        println!("s2: {:?}", solution.value(s2));
    }


    #[test]
    #[ignore]
    fn test_weight_propagation_from_files() {
        use crate::ebi_traits::ebi_trait_event_log::IndexTrace;
        
        // File paths
        let bpmn_file = r"C:\Users\larso\OneDrive\Dokumente\RWTH\SS25\Thesis\test_models\runningExample.bpmn";
        let log_file = r"C:\Users\larso\OneDrive\Dokumente\RWTH\SS25\Thesis\Eventlogs\runningExample.xes";
        
        
        println!("=== Loading files for weight propagation test ===");
        
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
        
        // Estimate frequencies from the event log
        println!("\n=== Analyzing event log ===");
        let frequencies = frequency_estimator(&event_log as &dyn EbiTraitFiniteStochasticLanguage, &bpmn_model);
        
        println!("Activity frequencies:");
        for (node_id, fraction) in &frequencies {
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
        }
        
        // Use frequencies directly as weights (already mapped to node IDs)
        let weights: HashMap<String, Fraction> = frequencies;
        
        // Apply weight propagation
        println!("\n=== Applying weight propagation with good_lp ===");
        let result: Result<StochasticBusinessProcessModelAndNotation, anyhow::Error> = weight_propagtion_micro_lp(&bpmn_model, &weights);
        
        match result {
            Ok(stochastic_bpmn) => {
                println!("Weight propagation completed successfully!");
                
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