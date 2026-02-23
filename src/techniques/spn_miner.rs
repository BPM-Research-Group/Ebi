use crate::{ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, techniques::unit_earth_movers_stochastic_conformance::UnitEarthMoversStochasticConformance};
use ebi_objects::ebi_arithmetic::{Zero, fraction::{approximate::Approximate, fraction::Fraction, fraction_enum::FractionEnum}};
use std::collections::HashMap;
use crate::ebi_objects::{Activity,StochasticLabelledPetriNet};
use grb::prelude::*;
use indexmap::IndexMap;


pub struct Config {
    pub max_places: usize,
    pub max_init_tokens: i64,
    pub max_arc_weight: i64,
    pub weight_lb: f64,
    pub weight_ub: f64,
    pub big_m: f64,
    pub time_limit: f64,
    pub mip_gap: f64,
}

#[derive(Debug)]
struct TrieNode {
    id: usize,
    activity: Option<usize>,
    children: HashMap<usize, usize>,
    is_trace_end: bool,
    parent: Option<usize>,
    prefix: Vec<usize>,
}

pub trait SPNMiner {
     fn mine_stochastic_petri_net(&self) -> grb::Result<StochasticLabelledPetriNet>;
}

impl SPNMiner for dyn EbiTraitFiniteStochasticLanguage {

    fn mine_stochastic_petri_net(&self) -> grb::Result<StochasticLabelledPetriNet> {
        let mut best_spn = StochasticLabelledPetriNet::new();
        let mut best_uemsc = Fraction::zero();

        for max_places in 2..5{
            // Setup - adjusted bounds for correct multiset combinations
            let max_init_tokens = 127;
            let max_arc_weight = 127;
            let time_limit = 60.0 * max_places as f64; 
            let weight_lb = 0.0001; // be strictly > 0 to avoid breaking bilinear constraints
            let weight_ub = 1.0;
            let mip_gap = 0.0001;

            let activity_values: Vec<Activity> = self.activity_key().name2activity.values().cloned().collect();
            let trans_num = activity_values.len();
            println!("Number of transitions (distinct activities): {:?}", activity_values); 

            let act_to_tidx: HashMap<Activity, usize> = activity_values
                .iter()
                .enumerate()
                .map(|(i, &a)| (a, i))
                .collect();

            let mut mapped_traces: IndexMap<Vec<usize>, f64> = IndexMap::new();
            for (trace, probability) in self.iter_traces_probabilities() {
                let mapped: Vec<usize> = trace.iter().map(|a| act_to_tidx[a]).collect();
                *mapped_traces.entry(mapped).or_insert(0.0) += probability.clone().approximate().unwrap();
            }

            let n_distinct = mapped_traces.len();

            let mut nodes = vec![TrieNode {
                id: 0,
                activity: None,
                children: HashMap::new(),
                is_trace_end: false,
                parent: None,
                prefix: vec![],
            }];

            for (mapped_trace, _prob) in &mapped_traces {
                let mut cur = 0usize;
                let mut prefix_so_far: Vec<usize> = vec![];
                for &t_idx in mapped_trace {
                    prefix_so_far.push(t_idx);
                    if let Some(&child_id) = nodes[cur].children.get(&t_idx) {
                        cur = child_id;
                    } else {
                        let new_id = nodes.len();
                        nodes[cur].children.insert(t_idx, new_id);
                        nodes.push(TrieNode {
                            id: new_id,
                            activity: Some(t_idx),
                            children: HashMap::new(),
                            is_trace_end: false,
                            parent: Some(cur),
                            prefix: prefix_so_far.clone(),
                        });
                        cur = new_id;
                    }
                }
                nodes[cur].is_trace_end = true;
            }

            let n_nodes = nodes.len();
            let n_places = if max_places > 0 { max_places } else { 2 * trans_num };

            let max_trace_len: usize = self.iter_traces().map(|t| t.len()).max().unwrap_or(0);
            
            // Big-M remains computationally safe for larger arc weights
            let big_m: f64 = (max_init_tokens as f64)
                + (max_trace_len as f64) * (max_arc_weight as f64)
                + (max_arc_weight as f64)
                + 1.0; 

            println!(
                "Problem size: {} transitions, {} candidate places, {} trie nodes, {} distinct traces",
                trans_num, n_places, n_nodes, n_distinct
            );

            let env = Env::new("")?;
            let mut model = Model::with_env("one_stage_discovery", &env)?;
            model.set_param(param::NonConvex, 2)?;
            model.set_param(param::MIPGap, mip_gap)?;
            model.set_param(param::TimeLimit, time_limit)?;
            model.set_param(param::OutputFlag, 0)?; 
            model.set_param(param::NumericFocus, 1)?;

            // Use (max_arc_weight + 1) to ensure the max weight is actually included in the bounds
            let mut in_arc: Vec<Vec<Var>> = Vec::with_capacity(n_places);
            for p in 0..n_places {
                let mut row = Vec::with_capacity(trans_num);
                for t in 0..trans_num {
                    row.push(add_intvar!(
                        model,
                        name: &format!("in_{p}_{t}"),
                        bounds: 0..(max_arc_weight + 1)
                    )?);
                }
                in_arc.push(row);
            }

            let mut out_arc: Vec<Vec<Var>> = Vec::with_capacity(n_places);
            for p in 0..n_places {
                let mut row = Vec::with_capacity(trans_num);
                for t in 0..trans_num {
                    row.push(add_intvar!(
                        model,
                        name: &format!("out_{p}_{t}"),
                        bounds: 0..(max_arc_weight + 1)
                    )?);
                }
                out_arc.push(row);
            }

            let mut m0: Vec<Var> = Vec::with_capacity(n_places);
            for p in 0..n_places {
                m0.push(add_intvar!(
                    model,
                    name: &format!("m0_{p}"),
                    bounds: 0..(max_init_tokens + 1)
                )?);
            }

            let mut w: Vec<Var> = Vec::with_capacity(trans_num);
            for t in 0..trans_num {
                w.push(add_ctsvar!(
                    model,
                    name: &format!("w_{t}"),
                    bounds: weight_lb..weight_ub
                )?);
            }

            let mut e_var: Vec<Var> = Vec::with_capacity(n_nodes);
            for nid in 0..n_nodes {
                e_var.push(add_binvar!(model, name: &format!("E_{nid}"))?);
            }

            let mut enabled: Vec<Vec<Var>> = Vec::with_capacity(n_nodes);
            for nid in 0..n_nodes {
                let mut row = Vec::with_capacity(trans_num);
                for t in 0..trans_num {
                    row.push(add_binvar!(model, name: &format!("en_{nid}_{t}"))?);
                }
                enabled.push(row);
            }

            let mut ge: Vec<Vec<Vec<Var>>> = Vec::with_capacity(n_nodes);
            for nid in 0..n_nodes {
                let mut t_vec = Vec::with_capacity(trans_num);
                for t in 0..trans_num {
                    let mut p_vec = Vec::with_capacity(n_places);
                    for p in 0..n_places {
                        p_vec.push(add_binvar!(model, name: &format!("ge_{nid}_{t}_{p}"))?);
                    }
                    t_vec.push(p_vec);
                }
                ge.push(t_vec);
            }

            let trace_indices: Vec<usize> = nodes
                .iter()
                .filter(|n| n.is_trace_end)
                .map(|n| n.id)
                .collect();
            let n_traces = trace_indices.len();

            let mut d_var: Vec<Var> = Vec::with_capacity(n_traces);
            for i in 0..n_traces {
                d_var.push(add_binvar!(model, name: &format!("D_{i}"))?);
            }

            let mut step_prob: HashMap<usize, Var> = HashMap::new();
            let mut cum_prob: HashMap<usize, Var> = HashMap::new();

            let cum_prob_root = add_ctsvar!(model, name: "cum_prob_0", bounds: 1.0..1.0)?;
            cum_prob.insert(0, cum_prob_root);

            for nid in 1..n_nodes {
                step_prob.insert(
                    nid,
                    add_ctsvar!(model, name: &format!("sp_{nid}"), bounds: 0.0..1.0)?,
                );
                cum_prob.insert(
                    nid,
                    add_ctsvar!(model, name: &format!("cp_{nid}"), bounds: 0.0..1.0)?,
                );
            }

            let mut sum_en_w: HashMap<usize, Var> = HashMap::new();
            for nid in 1..n_nodes {
                sum_en_w.insert(
                    nid,
                    add_ctsvar!(
                        model,
                        name: &format!("sew_{nid}"),
                        bounds: 0.0..(trans_num as f64 * weight_ub + 1.0)
                    )?,
                );
            }

            let mut aux_ew: HashMap<usize, Vec<Var>> = HashMap::new();
            for nid in 1..n_nodes {
                let mut row = Vec::with_capacity(trans_num);
                for t in 0..trans_num {
                    row.push(add_ctsvar!(
                        model,
                        name: &format!("aew_{nid}_{t}"),
                        bounds: 0.0..weight_ub
                    )?);
                }
                aux_ew.insert(nid, row);
            }

            let mut contrib: Vec<Var> = Vec::with_capacity(n_traces);
            let mut slack: Vec<Var> = Vec::with_capacity(n_traces);
            for i in 0..n_traces {
                contrib.push(add_ctsvar!(model, name: &format!("contrib_{i}"), bounds: 0.0..1.0)?);
                slack.push(add_ctsvar!(model, name: &format!("slack_{i}"), bounds: 0.0..1.0)?);
            }

            model.update()?;

            let marking_expr = |p: usize, nid: usize| -> Expr {
                let prefix = &nodes[nid].prefix;
                let mut expr: Expr = m0[p].into();
                for &ti in prefix {
                    expr = expr + out_arc[p][ti] - in_arc[p][ti];
                }
                expr
            };

            model.add_constr("E_root", c!(e_var[0] == 1.0))?;

            for nid in 0..n_nodes {
                for t in 0..trans_num {
                    for p in 0..n_places {
                        let diff: Expr = marking_expr(p, nid) - in_arc[p][t];

                        model.add_constr(
                            &format!("ge_fwd_{nid}_{t}_{p}"),
                            c!(diff.clone() >= -big_m * (1.0 - ge[nid][t][p])),
                        )?;
                        model.add_constr(
                            &format!("ge_rev_{nid}_{t}_{p}"),
                            c!(diff.clone() <= -1.0 + (big_m + 1.0) * ge[nid][t][p]),
                        )?;
                    }

                    let ge_vars: Vec<Var> = (0..n_places).map(|p| ge[nid][t][p]).collect();
                    model.add_genconstr_and(
                        &format!("en_and_{nid}_{t}"),
                        enabled[nid][t],
                        ge_vars,
                    )?;
                }
            }

            for nid in 1..n_nodes {
                let par = nodes[nid].parent.unwrap();
                let t_act = nodes[nid].activity.unwrap();

                model.add_genconstr_and(
                    &format!("E_eq2_{nid}"),
                    e_var[nid],
                    [e_var[par], enabled[par][t_act]],
                )?;
            }

            for nid in 0..n_nodes {
                for p in 0..n_places {
                    let mk = marking_expr(p, nid);
                    model.add_genconstr_indicator(
                        &format!("nonneg_{nid}_{p}"),
                        e_var[nid],
                        true,
                        c!(mk >= 0.0),
                    )?;
                }
            }

            for (i, &nid) in trace_indices.iter().enumerate() {
                let mut not_en: Vec<Var> = Vec::with_capacity(trans_num);
                for t in 0..trans_num {
                    let v = add_binvar!(model, name: &format!("noten_{i}_{t}"))?;
                    not_en.push(v);
                }
                model.update()?;

                for t in 0..trans_num {
                    model.add_constr(
                        &format!("noten_def_{i}_{t}"),
                        c!(not_en[t] + enabled[nid][t] == 1.0),
                    )?;
                }

                let mut and_args = vec![e_var[nid]];
                and_args.extend(not_en.iter());
                model.add_genconstr_and(
                    &format!("D_eq3_{i}"),
                    d_var[i],
                    and_args,
                )?;
            }

            for nid in 1..n_nodes {
                let par = nodes[nid].parent.unwrap();
                let t_act = nodes[nid].activity.unwrap();
                let ew_vars = aux_ew.get(&nid).unwrap();

                for t in 0..trans_num {
                    model.add_constr(&format!("aew_ub1_{nid}_{t}"), c!(ew_vars[t] <= w[t]))?;
                    model.add_constr(&format!("aew_ub2_{nid}_{t}"), c!(ew_vars[t] <= weight_ub * enabled[par][t]))?;
                    model.add_constr(&format!("aew_lb_{nid}_{t}"), c!(ew_vars[t] >= w[t] - weight_ub * (1.0 - enabled[par][t])))?;
                }

                let sum_expr: Expr = ew_vars.iter().fold(Expr::from(0.0), |acc, &v| acc + v);
                model.add_constr(&format!("sew_def_{nid}"), c!(sum_en_w[&nid] == sum_expr))?;

                model.add_genconstr_indicator(&format!("sp_zero_{nid}"), e_var[nid], false, c!(step_prob[&nid] == 0.0))?;
                model.add_genconstr_indicator(&format!("cp_zero_{nid}"), e_var[nid], false, c!(cum_prob[&nid] == 0.0))?;

                model.add_qconstr(
                    &format!("sp_def_{nid}"),
                    c!(step_prob[&nid] * sum_en_w[&nid] - w[t_act] * e_var[nid] == 0.0),
                )?;

                model.add_qconstr(
                    &format!("cp_def_{nid}"),
                    c!(cum_prob[&nid] - cum_prob[&par] * step_prob[&nid] == 0.0),
                )?;
            }

            for (i, &nid) in trace_indices.iter().enumerate() {
                model.add_constr(&format!("con_ub1_{i}"), c!(contrib[i] <= cum_prob[&nid]))?;
                model.add_constr(&format!("con_ub2_{i}"), c!(contrib[i] <= d_var[i]))?;
                model.add_constr(&format!("con_lb_{i}"), c!(contrib[i] >= cum_prob[&nid] - 1.0 + d_var[i]))?;
            }

            let trace_node_to_prob: HashMap<usize, f64> = {
                let mut map = HashMap::new();
                for (mapped_trace, &prob) in &mapped_traces {
                    let mut cur = 0usize;
                    for &t_idx in mapped_trace {
                        cur = nodes[cur].children[&t_idx];
                    }
                    map.insert(cur, prob);
                }
                map
            };

            for (i, &nid) in trace_indices.iter().enumerate() {
                let freq = trace_node_to_prob.get(&nid).copied().unwrap_or(0.0);
                model.add_constr(&format!("slk_lb_{i}"), c!(slack[i] >= freq - contrib[i]))?;
            }

            for p in 0..n_places.saturating_sub(1) {
                let sum_p: Expr = (0..trans_num)
                    .flat_map(|t| [in_arc[p][t], out_arc[p][t]])
                    .fold(Expr::from(0.0), |acc, v| acc + v)
                    + m0[p];
                let sum_p1: Expr = (0..trans_num)
                    .flat_map(|t| [in_arc[p + 1][t], out_arc[p + 1][t]])
                    .fold(Expr::from(0.0), |acc, v| acc + v)
                    + m0[p + 1];
                model.add_constr(&format!("sym_{p}"), c!(sum_p >= sum_p1))?;
            }

            let obj: Expr = slack.iter().fold(Expr::from(0.0), |acc, &v| acc + v);
            model.set_objective(obj.clone(), Minimize)?;

            model.optimize()?;

            let status = model.status()?;
            println!("Gurobi status: {:?}", status);

            let obj_val = model.get_attr(attr::ObjVal)?;
            let uemsc = 1.0 - obj_val;
            println!("Objective uemsc (total slack): {:.6}", uemsc);

            if status != Status::Optimal && status != Status::SubOptimal && status != Status::TimeLimit {
                eprintln!("WARNING: Solver did not find an optimal solution (status = {:?})", status);
            }

            let mut sol_in: Vec<Vec<u64>> = vec![vec![0; trans_num]; n_places];
            let mut sol_out: Vec<Vec<u64>> = vec![vec![0; trans_num]; n_places];
            let mut sol_m0: Vec<u64> = vec![0; n_places];
            let mut sol_w: Vec<f64> = vec![0.0; trans_num];

            for p in 0..n_places {
                sol_m0[p] = model.get_obj_attr(attr::X, &m0[p])?.round() as u64;
                for t in 0..trans_num {
                    sol_in[p][t] = model.get_obj_attr(attr::X, &in_arc[p][t])?.round() as u64;
                    sol_out[p][t] = model.get_obj_attr(attr::X, &out_arc[p][t])?.round() as u64;
                }
            }
            for t in 0..trans_num {
                sol_w[t] = model.get_obj_attr(attr::X, &w[t])?;
            }

            let kept_places: Vec<usize> = (0..n_places)
                .filter(|&p| {
                    sol_m0[p] > 0 || (0..trans_num).any(|t| sol_in[p][t] > 0 || sol_out[p][t] > 0)
                })
                .collect();

            let mut slpn = StochasticLabelledPetriNet::new();
            slpn.activity_key = self.activity_key().clone();

            for &orig_p in &kept_places {
                let new_p = slpn.add_place();
                let _ = slpn.initial_marking.increase(new_p, sol_m0[orig_p]);
            }

            for t in 0..trans_num {
                let activity: Activity = activity_values[t];
                let weight: f64 = sol_w[t];
                let _tidx = slpn.add_transition(Some(activity), FractionEnum::Approx(weight));
            }

            for (new_p, &orig_p) in kept_places.iter().enumerate() {
                for t in 0..trans_num {
                    let in_w = sol_in[orig_p][t];
                    for _ in 0..in_w {
                        let _ = slpn.add_place_transition_arc(new_p, t, 1);
                    }
                    let out_w = sol_out[orig_p][t];
                    for _ in 0..out_w {
                        let _ = slpn.add_transition_place_arc(t, new_p, 1);
                    }   
                }
            }

            let uemsc_res = self.unit_earth_movers_stochastic_conformance(Box::new(slpn.clone())).unwrap();

            println!("current {:.6} with predefined place number: {}", uemsc_res, max_places);

            if uemsc_res > best_uemsc.clone() {
                best_spn = slpn.clone();
                best_uemsc = uemsc_res.clone();
            }
        }
        return Ok(best_spn);
    } 
}