use crate::ebi_objects::business_process_model_and_notation::{
    BusinessProcessModelAndNotation, BPMNNode, BPMNGateway, GatewayDirection
};
use crate::ebi_objects::stochastic_business_process_model_and_notation::{
    StochasticBusinessProcessModelAndNotation, StochasticSequenceFlow, StochasticTask
};
use good_lp::{variables, variable, constraint, Expression, default_solver, SolverModel, Solution};
use crate::ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage;
use std::collections::{HashMap, HashSet, VecDeque};
use crate::math::fraction::MaybeExact;
use crate::math::fraction::Fraction;
use crate::math::traits::Zero;
use crate::math::matrix::Matrix;


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
        if probability.is_exact() {
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
    }
    
    // Map activity names to node IDs
    for (activity_name, frequency) in activity_counts {
        if let Some(node_id) = bpmn.label.iter()
            .find(|(_id, label)| **label == activity_name)
            .map(|(id, _label)| id) {
            count.insert(node_id.clone(), frequency);
        } else {
            // Fallback: assume activity name is the node ID
            count.insert(activity_name, frequency);
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
                    let current = &window[0];
                    let next = &window[1];
                    let current_str = current.to_string();
                    let next_str = next.to_string();
                    // Skip if the model indicates they can be concurrent or are the same
                    if concurrent.contains(&(current_str.clone(), next_str.clone())) || next_str == current_str || concurrent.contains(&(next_str.clone(), current_str.clone())) {
                        continue;
                    }
                    let current_count = counts.get(&next_str).cloned().unwrap_or_else(|| Fraction::from((0, 1)));
                    counts.insert(next_str, &current_count + &fraction_to_add);
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
                    let current = &window[0];
                    let next = &window[1];
                    let current_str = current.to_string();
                    let next_str = next.to_string();
                    // Skip if the model indicates they can be concurrent or are the same
                    if concurrent.contains(&(current_str.clone(), next_str.clone())) || next_str == current_str {
                        continue;
                    }
                    let current_count = counts.get(&current_str).cloned().unwrap_or_else(|| Fraction::from((0, 1)));
                    counts.insert(current_str, &current_count + &fraction_to_add);
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

pub fn weight_propagation_good_lp(bpmn: &BusinessProcessModelAndNotation, weights: &HashMap<String, Fraction>) -> anyhow::Result<StochasticBusinessProcessModelAndNotation> {
    
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
                        *weight = 1.0;
                        scaled_flows.insert((*flow_id).clone());
                        println!("Setting converging OR gateway {} outgoing flow to 1.0", gateway.id);
                    }
                }
            }
        }
    }
    
    // Create final sequence flows with scaled weights
    let mut sequence_flows = Vec::new();
    for flow in &bpmn.sequence_flows {
        let flow_id = format!("flow_{}_{}", flow.source_id, flow.target_id);
        let weight = flow_weights.get(&flow_id).copied().unwrap_or(1.0);
        
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
        end_events: bpmn.end_events.len(),
        sequence_flows,
    })
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
        let _h = weight_propagation_good_lp as fn(&BusinessProcessModelAndNotation, &HashMap<String, Fraction>) -> anyhow::Result<StochasticBusinessProcessModelAndNotation>;
        
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
    /// Usage: cargo test test_weight_propagation_from_files -- --ignored
    #[test]
    #[ignore] // Use --ignored flag to run this test
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
        let result: Result<StochasticBusinessProcessModelAndNotation, anyhow::Error> = weight_propagation_good_lp(&bpmn_model, &weights);
        
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