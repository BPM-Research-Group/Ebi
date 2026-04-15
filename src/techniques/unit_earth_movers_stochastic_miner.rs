use crate::ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage;

use ebi_objects::{
    ActivityKeyTranslator, HasActivityKey, LabelledPetriNet,
    StochasticLabelledPetriNet,
    ebi_arithmetic::{Fraction,fraction::{approximate::Approximate,fraction_enum::FractionEnum}}
};
use rayon::iter::ParallelIterator;

use anyhow::{Context, Error};
use crate::techniques::slpn_miner_cross_product::EquationSystemConstructor;
use std::{
    sync::{Arc, Mutex},
};

use std::collections::HashMap;

use grb::prelude::*;
use grb::expr::QuadExpr;

use super::slpn_miner_cross_product::{LinearSystem, SymbolicProbability};

// ============================================================================
// Input / Output types
// ============================================================================

/// One trace with its observed log probability and its symbolic equation system.
pub struct TraceInput {
    /// L(σ): the probability of this trace in the event log
    pub log_probability: f64,
    /// The cross-product equation system whose initial variable gives p̃_M(σ)
    pub system: LinearSystem,
}

/// The solution returned by the optimizer.
pub struct UemscSolution {
    /// The optimal uEMSC value: 1 − Σ max(L(σ) − p̃_M(σ), 0)
    pub uemsc_value: f64,
    /// Optimised weight for each transition (indexed by transition index, length = num_transitions)
    pub weights: Vec<f64>,
    /// Optimised model probability for each trace (same order as input)
    pub trace_probabilities: Vec<f64>,
}

// ============================================================================
// Optimizer
// ============================================================================

/// Maximise the unit Earth Mover's Stochastic Conformance:
///
///   maximise  1 − Σ_{σ ∈ L̄}  max( L(σ) − p̃_M(σ), 0 )
///
/// Decision variables are the transition weights w_t ≥ ε.
/// The model probability of each trace is derived from the `LinearSystem`
/// via bilinear constraints (Gurobi `NonConvex = 2`).
///
/// # Arguments
/// * `traces` – one entry per unique trace: its log probability and its
///              cross-product equation system.
/// * `num_transitions` – total number of transitions in the Petri net.
///
/// # Returns
/// The optimal weights and the achieved uEMSC value.
pub fn maximize_uemsc(traces: &[TraceInput], num_transitions: usize) -> grb::Result<UemscSolution> {
    // ── Environment & model ────────────────────────────────────────────
    let env = Env::new("")?;
    let mut model = Model::with_env("two_stage_discovery", &env)?;
    model.set_param(param::NonConvex, 2)?;
    model.set_param(param::MIPGap, 0.02)?;
    model.set_param(param::TimeLimit, 1800.0)?;
    model.set_param(param::FeasibilityTol, 1e-9)?;

    // ── Step 1: Create weight variables for ALL transitions  w_t ≥ ε  ──
    let weight_lb = 0.000001;
    let weight_ub = 100.0;
    let mut w: Vec<Var> = Vec::with_capacity(num_transitions);
    for t in 0..num_transitions {
        let v = add_ctsvar!(model, name: &format!("w_{}", t), bounds: weight_lb..weight_ub)?;
        w.push(v);
    }
    model.update()?;

    // ── Step 2: Create *global* ρ variables ─────────────────────────────
    //
    // ρ = w_fired / Σ w_enabled   is uniquely determined by
    // (fired_transition, sorted_enabled_set).
    //
    // Multiple traces that share the same choice point reuse the same ρ.
    //
    // Key: (fired, sorted_enabled)  →  Gurobi Var
    let mut rho_global: HashMap<(usize, Vec<usize>), Var> = HashMap::new();
    let mut rho_counter = 0usize;

    for ti in traces {
        if ti.system.is_structurally_zero() { continue; }
        for sp in ti.system.coeff_to_prob.values() {
            let key = rho_key(sp);
            if rho_global.contains_key(&key) {
                continue;
            }
            let rho_var = add_ctsvar!(
                model,
                name: &format!("rho_{}", rho_counter),
                bounds: 0.0..1.0
            )?;
            rho_global.insert(key, rho_var);
            rho_counter += 1;
        }
    }
    model.update()?;

    // Linking constraints:  ρ · Σ w_enabled  =  w_fired
    //
    //   ρ · w_e1 + ρ · w_e2 + … − w_fired = 0      (quadratic)
    {
        let mut link_id = 0usize;
        for ((fired, enabled), &rho_var) in &rho_global {
            let mut qe = QuadExpr::new();
            for &e in enabled {
                qe.add_qterm(1.0, rho_var, w[e]);
            }
            qe.add_term(-1.0, w[*fired]);
            model.add_qconstr(
                &format!("link_{}", link_id),
                c!(qe == 0.0),
            )?;
            link_id += 1;
        }
    }
    model.update()?;

    // ── Step 3: Per-trace variables & constraints ───────────────────────
    //
    // For each trace σ we create:
    //   • x_i  variables  (probability of reaching final from state i)
    //   • equation constraints from the LinearSystem
    //   • d_σ  ≥ 0  deviation variable:  d_σ ≥ L(σ) − x_initial
    //
    // Each equation  x_lhs = Σ ρ_j · x_target_j   is a bilinear constraint.
    //
    // OPTIMISATION: traces that are structurally impossible (all paths lead
    // to dead ends, so p̃_M(σ) = 0 for any weights) are skipped entirely.
    // Their deviation max(L(σ) − 0, 0) = L(σ) is accumulated as a constant
    // penalty on the objective.
    let mut d_vars: Vec<Var> = Vec::new();
    let mut x_initial_per_trace: Vec<Option<Var>> = Vec::with_capacity(traces.len());
    let mut constant_penalty: f64 = 0.0;

    for (ti_idx, ti) in traces.iter().enumerate() {
        // Skip traces the model can never produce
        if ti.system.is_structurally_zero() {
            // log::debug!(
            //     "Trace {} is structurally impossible (p̃_M = 0), penalty += {:.6}",
            //     ti_idx, ti.log_probability
            // );
            constant_penalty += ti.log_probability;
            x_initial_per_trace.push(None);
            continue;
        }

        let sys = &ti.system;
        let pfx = format!("t{}", ti_idx);

        // ── 4a. Create x variables for this trace's cross-product states ──
        let n = sys.variables.len();
        let mut x: Vec<Var> = Vec::with_capacity(n);
        for i in 0..n {
            let v = add_ctsvar!(
                model,
                name: &format!("{}_{}", pfx, sys.variables[i]),
                bounds: 0.0..1.0
            )?;
            x.push(v);
        }
        model.update()?;

        // ── 4b. Map each coefficient name → global ρ variable ─────────────
        let coeff_rho: HashMap<&str, Var> = sys
            .coeff_to_prob
            .iter()
            .map(|(name, sp)| {
                let key = rho_key(sp);
                (name.as_str(), rho_global[&key])
            })
            .collect();

        // ── 4c. Add equation constraints ──────────────────────────────────
        for eq in &sys.equations {
            if eq.is_final {
                // x_lhs = 1
                model.add_constr(
                    &format!("{}_final_{}", pfx, eq.lhs),
                    c!(x[eq.lhs] == 1.0),
                )?;
            } else if eq.terms.is_empty() {
                // Dead-end:  x_lhs = 0
                model.add_constr(
                    &format!("{}_dead_{}", pfx, eq.lhs),
                    c!(x[eq.lhs] == 0.0),
                )?;
            } else {
                // x_lhs  =  Σ ρ_j · x_target_j
                // ⟹   x_lhs − Σ ρ_j · x_target_j  =  0   (quadratic)
                let mut qe = QuadExpr::new();
                qe.add_term(1.0, x[eq.lhs]);           // + x_lhs
                for term in &eq.terms {
                    let rho_var = coeff_rho[term.coefficient.as_str()];
                    qe.add_qterm(-1.0, rho_var, x[term.variable]); // − ρ·x_target
                }
                model.add_qconstr(
                    &format!("{}_eq_{}", pfx, eq.lhs),
                    c!(qe == 0.0),
                )?;
            }
        }

        // ── 4d. Deviation variable ────────────────────────────────────────
        //
        //   d_σ ≥ 0   and   d_σ ≥ L(σ) − x_initial
        //
        // Together with minimising Σ d_σ this gives  d_σ = max(L(σ)−p̃_M(σ), 0).
        let d = add_ctsvar!(model, name: &format!("{}_d", pfx), bounds: 0.0..)?;
        let x_init = x[sys.initial_variable];
        model.add_constr(
            &format!("{}_dev", pfx),
            c!(d >= ti.log_probability - x_init),
        )?;

        d_vars.push(d);
        x_initial_per_trace.push(Some(x_init));
    }

    // ── Step 5: Objective ───────────────────────────────────────────────
    //
    //   maximise  1 − Σ d_σ − constant_penalty
    //   ⟺  minimise  Σ d_σ        (constant_penalty doesn't affect optimisation)
    //
    // The constant_penalty from structurally-zero traces is added back when
    // computing the final uEMSC value.
    {
        let sum_d: grb::expr::Expr = d_vars.iter().copied().grb_sum();
        model.set_objective(sum_d, Minimize)?;
    }

    log::info!(
        "Gurobi model: {} d-variables, constant penalty from impossible traces = {:.6}",
        d_vars.len(),
        constant_penalty
    );

    // ── Step 6: Optimise ────────────────────────────────────────────────
    model.optimize()?;

    let status = model.status()?;

    if status != Status::Optimal && status != Status::SubOptimal && status != Status::TimeLimit {
        eprintln!("WARNING: Solver did not find an optimal solution (status = {:?})", status);
    }

    // ── Step 7: Extract solution ────────────────────────────────────────
    let obj_val: f64 = model.get_attr(attr::ObjVal)?;
    // uEMSC = 1 − (Σ d_σ from Gurobi) − (constant penalty from impossible traces)
    let uemsc_value = 1.0 - obj_val - constant_penalty;

    let mut weights = Vec::with_capacity(num_transitions);
    for t in 0..num_transitions {
        weights.push(model.get_obj_attr(attr::X, &w[t])?);
    }

    let mut trace_probs = Vec::with_capacity(traces.len());
    for opt_var in &x_initial_per_trace {
        match opt_var {
            Some(x_init) => trace_probs.push(model.get_obj_attr(attr::X, &x_init)?),
            None => trace_probs.push(0.0), // structurally impossible trace
        }
    }

    Ok(UemscSolution {
        uemsc_value,
        weights,
        trace_probabilities: trace_probs,
    })
}

// ============================================================================
// Helpers
// ============================================================================

/// Canonical key for a ρ variable: (fired transition, sorted enabled set).
fn rho_key(sp: &SymbolicProbability) -> (usize, Vec<usize>) {
    let mut den = sp.denominator.clone();
    den.sort();
    (sp.numerator, den)
}

pub trait UEMSCStochasticMinerLPN {
    fn mine_uemsc_stochastic_lpn(
        self,
        language: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> StochasticLabelledPetriNet;
}


impl UEMSCStochasticMinerLPN for LabelledPetriNet {
    fn mine_uemsc_stochastic_lpn(
        self,
        log: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> StochasticLabelledPetriNet {
         // ── 1. Translate activity keys ──────────────────────────────────────
        let mut activity_key = self.activity_key().clone();
        let translator = Arc::new(ActivityKeyTranslator::new(
            log.activity_key(),
            &mut activity_key,
        ));
        let error: Arc<Mutex<Option<Error>>> = Arc::new(Mutex::new(None));

        log::info!("Building equation systems for all traces");

        // ── 2. Build equation systems + collect log probabilities (parallel) ─
        let trace_data: Vec<(LinearSystem, f64)> = log
            .par_iter_traces_probabilities()
            .filter_map(|(trace, probability)| {

                // println!("Probability for trace {:?} is approximately {:.6}", trace, probability.clone().approximate().unwrap());
                let translator: Arc<ActivityKeyTranslator> = Arc::clone(&translator);
                let trace_translated = translator.translate_trace(&trace);

                let result = self
                    .create_equation_system(&trace_translated)
                    .with_context(|| format!("Building equation system for trace {:?}", trace));

                match result {
                    Ok(system) => {
                        let log_prob: f64 = probability.clone().approximate().unwrap();
                        Some((system, log_prob))
                    }
                    Err(err) => {
                        *Arc::clone(&error).lock().unwrap() = Some(err);
                        None
                    }
                }
            })
            .collect::<Vec<_>>();

        // ── 4. Build TraceInputs for the optimizer ──────────────────────────
        let trace_inputs: Vec<TraceInput> = trace_data
            .into_iter()
            .map(|(system, log_probability)| TraceInput {
                log_probability,
                system,
            })
            .collect();

        let num_transitions = self.transition2input_places.len();

        log::info!(
            "Optimising uEMSC: {} unique traces, {} transitions",
            trace_inputs.len(),
            num_transitions
        );

        // ── 5. Run Gurobi optimisation ──────────────────────────────────────
        let solution = maximize_uemsc(&trace_inputs, num_transitions).unwrap();

        log::info!("Optimal uEMSC = {:.6}", solution.uemsc_value);
        // for (t, w) in solution.weights.iter().enumerate() {
        //     log::info!("  transition {} → weight {:.6}", t, w);
        // }
        // for (i, (p, ti)) in solution.trace_probabilities.iter().zip(&trace_inputs).enumerate() {
        //     log::debug!(
        //         "  trace {} → p̃_M = {:.6}  (L = {:.6})",
        //         i, p, ti.log_probability
        //     );
        // }

        // ── 5. Convert f64 weights to Fraction and build SLPN ───────────
        let weights: Vec<Fraction> = solution
            .weights
            .iter()
            .map(|&w| FractionEnum::Approx(w))
            .collect();

        (self, weights).into()
    }
}
