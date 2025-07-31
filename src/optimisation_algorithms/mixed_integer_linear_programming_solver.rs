use log::debug;
use microlp::{ComparisonOp, Error, OptimizationDirection, VarDomain};
use sprs::CompressedStorage;

use crate::{
    math::{
        fraction::Fraction,
        traits::{Infinite, One, Signed, Zero},
    },
    optimisation_algorithms::{
        mixed_integer_linear_programming::{CsVec, Solution, Variable},
        mixed_integer_linear_programming_helpers::{resized_view, to_dense},
        mixed_integer_linear_programming_lu::{lu_factorize, ScratchSpace},
        mixed_integer_linear_programming_sparse::{ScatteredVec, SparseVec},
    },
};

type CsMat = sprs::CsMatI<Fraction, usize>;

#[derive(Clone)]
pub(crate) struct Solver {
    pub(crate) num_vars: usize,

    orig_obj_coeffs: Vec<Fraction>,
    orig_var_mins: Vec<Fraction>,
    orig_var_maxs: Vec<Fraction>,
    pub(crate) orig_var_domains: Vec<VarDomain>,
    //pub(crate) orig_int_vars: Vec<VarDomain>,
    orig_constraints: CsMat, // excluding rhs
    orig_constraints_csc: CsMat,
    orig_rhs: Vec<Fraction>,

    enable_primal_steepest_edge: bool,
    enable_dual_steepest_edge: bool,

    is_primal_feasible: bool,
    is_dual_feasible: bool,

    // Updated on each pivot
    /// For each var: whether it is basic/non-basic and the corresponding index.
    var_states: Vec<VarState>,
    basis_solver: BasisSolver,

    /// For each constraint the corresponding basic var.
    basic_vars: Vec<usize>,
    basic_var_vals: Vec<Fraction>,
    basic_var_mins: Vec<Fraction>,
    basic_var_maxs: Vec<Fraction>,
    dual_edge_sq_norms: Vec<Fraction>,

    /// Remaining variables. (idx -> var), 'nb' means 'non-basic'
    nb_vars: Vec<usize>,
    nb_var_obj_coeffs: Vec<Fraction>,
    nb_var_vals: Vec<Fraction>,
    nb_var_states: Vec<NonBasicVarState>,
    nb_var_is_fixed: Vec<bool>,
    primal_edge_sq_norms: Vec<Fraction>,

    pub(crate) cur_obj_val: Fraction,

    // Recomputed on each pivot
    col_coeffs: SparseVec,
    sq_norms_update_helper: Vec<Fraction>,
    inv_basis_row_coeffs: SparseVec,
    row_coeffs: ScatteredVec,
}

#[derive(Clone, Debug)]
enum VarState {
    Basic(usize),
    NonBasic(usize),
}

#[derive(Clone, Debug)]
struct NonBasicVarState {
    at_min: bool,
    at_max: bool,
}

impl std::fmt::Debug for Solver {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Solver")?;
        writeln!(
            f,
            "num_vars: {}, num_constraints: {}, is_primal_feasible: {}, is_dual_feasible: {}",
            self.num_vars,
            self.num_constraints(),
            self.is_primal_feasible,
            self.is_dual_feasible,
        )?;
        writeln!(f, "orig_obj_coeffs:\n{:?}", self.orig_obj_coeffs)?;
        writeln!(f, "orig_var_mins:\n{:?}", self.orig_var_mins)?;
        writeln!(f, "orig_var_maxs:\n{:?}", self.orig_var_maxs)?;
        writeln!(f, "orig_constraints:")?;
        for row in self.orig_constraints.outer_iterator() {
            writeln!(f, "{:?}", to_dense(&row))?;
        }
        writeln!(f, "orig_rhs:\n{:?}", self.orig_rhs)?;
        writeln!(f, "basic_vars:\n{:?}", self.basic_vars)?;
        writeln!(f, "basic_var_vals:\n{:?}", self.basic_var_vals)?;
        writeln!(f, "dual_edge_sq_norms:\n{:?}", self.dual_edge_sq_norms)?;
        writeln!(f, "nb_vars:\n{:?}", self.nb_vars)?;
        writeln!(f, "nb_var_vals:\n{:?}", self.nb_var_vals)?;
        writeln!(f, "nb_var_obj_coeffs:\n{:?}", self.nb_var_obj_coeffs)?;
        writeln!(f, "primal_edge_sq_norms:\n{:?}", self.primal_edge_sq_norms)?;
        writeln!(f, "cur_obj_val: {:?}", self.cur_obj_val)?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
enum BranchKind {
    Floor,
    Ceil,
    Exact, // fixed value
}

#[derive(Clone, Debug)]
struct Step {
    pub(crate) start_solution: Solution,
    pub(crate) var: Variable,
    pub(crate) kind: BranchKind,
    pub(crate) start_val: i64,
}

impl Solver {
    pub(crate) fn try_new(
        obj_coeffs: &[Fraction],
        var_mins: &[Fraction],
        var_maxs: &[Fraction],
        constraints: &[(CsVec, ComparisonOp, Fraction)],
        var_domains: &[VarDomain],
    ) -> Result<Self, Error> {
        let enable_steepest_edge = true; // TODO: make user-settable.

        let num_vars = obj_coeffs.len();

        assert_eq!(num_vars, var_mins.len());
        assert_eq!(num_vars, var_maxs.len());
        let mut orig_var_mins = var_mins.to_vec();
        let mut orig_var_maxs = var_maxs.to_vec();

        let mut var_states = vec![];

        let mut nb_vars = vec![];
        let mut nb_var_vals = vec![];
        let mut nb_var_states = vec![];

        let mut obj_val = Fraction::zero();

        let mut is_dual_feasible = true;

        for v in 0..num_vars {
            // choose initial variable values

            let min = orig_var_mins[v];
            let max = orig_var_maxs[v];
            if min > max {
                return Err(Error::Infeasible);
            }

            // initially all user-created variables are non-basic
            var_states.push(VarState::NonBasic(nb_vars.len()));
            nb_vars.push(v);

            // Try to choose values to achieve dual feasibility.
            let init_val = if min == max {
                // Fixed variable, the obj. coeff doesn't matter.
                min
            } else if min.is_infinite() && max.is_infinite() {
                // Free variable, if we are lucky and obj. coeff is zero, then dual-feasible.
                if !obj_coeffs[v].is_zero() {
                    //TODO should this use float_eq?
                    is_dual_feasible = false;
                }
                Fraction::zero()
            } else if obj_coeffs[v].is_positive() {
                // We need a finite value and prefer min for dual feasibility.
                if min.is_finite() {
                    min
                } else {
                    is_dual_feasible = false;
                    max
                }
            } else if obj_coeffs[v].is_negative() {
                // We need a finite value and prefer max for dual feasibility.
                if max.is_finite() {
                    max
                } else {
                    is_dual_feasible = false;
                    min
                }
            } else if min.is_finite() {
                // Obj. coeff is zero, just take any finite value,
                // dual feasibility will be satisfied.
                min
            } else {
                max
            };

            nb_var_vals.push(init_val);
            obj_val += &init_val * &obj_coeffs[v];

            nb_var_states.push(NonBasicVarState {
                at_min: init_val == min,
                at_max: init_val == max,
            });
        }

        let mut constraint_coeffs = vec![];
        let mut orig_rhs = vec![];

        // Initially, all slack vars are basic.
        let mut basic_vars = vec![];
        let mut basic_var_vals = vec![];
        let mut basic_var_mins = vec![];
        let mut basic_var_maxs = vec![];

        for (coeffs, cmp_op, rhs) in constraints {
            let rhs = *rhs;

            if coeffs.indices().is_empty() {
                let is_tautological = match cmp_op {
                    ComparisonOp::Eq => rhs.is_zero(),
                    ComparisonOp::Le => rhs.is_not_negative(),
                    ComparisonOp::Ge => rhs.is_not_positive(),
                };

                if is_tautological {
                    continue;
                } else {
                    return Err(Error::Infeasible);
                }
            }

            constraint_coeffs.push(coeffs.clone());
            orig_rhs.push(rhs);

            let (slack_var_min, slack_var_max) = match cmp_op {
                ComparisonOp::Le => (Fraction::zero(), Fraction::infinity()),
                ComparisonOp::Ge => (Fraction::neg_infinity(), Fraction::zero()),
                ComparisonOp::Eq => (Fraction::zero(), Fraction::zero()),
            };

            orig_var_mins.push(slack_var_min);
            orig_var_maxs.push(slack_var_max);

            basic_var_mins.push(slack_var_min);
            basic_var_maxs.push(slack_var_max);

            let cur_slack_var = var_states.len();
            var_states.push(VarState::Basic(basic_vars.len()));
            basic_vars.push(cur_slack_var);

            let mut lhs_val = Fraction::zero();
            for (var, &coeff) in coeffs.iter() {
                lhs_val += &coeff * &nb_var_vals[var];
            }
            basic_var_vals.push(&rhs - &lhs_val);
        }

        let num_constraints = constraint_coeffs.len();
        let num_total_vars = num_vars + num_constraints;

        let mut orig_obj_coeffs = obj_coeffs.to_vec();
        orig_obj_coeffs.resize(num_total_vars, Fraction::zero());

        let mut orig_constraints = CsMat::empty(CompressedStorage::CSR, num_total_vars);
        for (cur_slack_var, coeffs) in constraint_coeffs.into_iter().enumerate() {
            let mut coeffs = into_resized(coeffs, num_total_vars);
            coeffs.append(num_vars + cur_slack_var, Fraction::one());
            orig_constraints = orig_constraints.append_outer_csvec(coeffs.view());
        }
        let orig_constraints_csc = orig_constraints.to_csc();

        let is_primal_feasible = basic_var_vals
            .iter()
            .zip(&basic_var_mins)
            .zip(&basic_var_maxs)
            .all(|((&val, &min), &max)| val >= min && val <= max);

        let need_artificial_obj = !is_primal_feasible && !is_dual_feasible;

        let enable_dual_steepest_edge = enable_steepest_edge;
        let dual_edge_sq_norms = if enable_dual_steepest_edge {
            vec![1.0; basic_vars.len()]
        } else {
            vec![]
        };

        // If is dual feasible at start, we don't need lengthy primal phase2.
        // Thus we can skip expensive calculations for primal sq. norms.
        let enable_primal_steepest_edge = enable_steepest_edge && !is_dual_feasible;
        let sq_norms_update_helper = if enable_primal_steepest_edge {
            vec![Fraction::zero(); num_total_vars - num_constraints]
        } else {
            vec![]
        };

        let mut nb_var_obj_coeffs = vec![];
        let mut primal_edge_sq_norms = vec![];
        for (&var, state) in nb_vars.iter().zip(&nb_var_states) {
            //guaranteed to be a valid index
            let col = orig_constraints_csc.outer_view(var).unwrap();

            if need_artificial_obj {
                let coeff = if state.at_min && !state.at_max {
                    Fraction::one()
                } else if state.at_max && !state.at_min {
                    -Fraction::one()
                } else {
                    Fraction::zero()
                };
                nb_var_obj_coeffs.push(coeff);
            } else {
                nb_var_obj_coeffs.push(orig_obj_coeffs[var]);
            }

            if enable_primal_steepest_edge {
                primal_edge_sq_norms.push(col.squared_l2_norm() + Fraction::one());
            }
        }

        let cur_obj_val = if need_artificial_obj {
            Fraction::zero()
        } else {
            obj_val
        };

        let mut scratch = ScratchSpace::with_capacity(num_constraints);
        let lu_factors = lu_factorize(
            basic_vars.len(),
            |c| {
                orig_constraints_csc
                    .outer_view(basic_vars[c])
                    //guaranteed to be a valid index
                    .unwrap()
                    .into_raw_storage()
            },
            Fraction::from((1, 10)),
            &mut scratch,
        )?;
        let lu_factors_transp = lu_factors.transpose();

        let nb_var_is_fixed = vec![false; nb_vars.len()];

        let res = Self {
            num_vars,
            orig_obj_coeffs,
            orig_var_mins,
            orig_var_maxs,
            orig_constraints,
            orig_constraints_csc,
            orig_rhs,
            orig_var_domains: var_domains.to_vec(),
            /*orig_int_vars: var_domains
            .to_vec()
            .into_iter()
            .filter(|d| matches!(d, VarDomain::Integer | VarDomain::Boolean))
            .collect(),*/
            enable_primal_steepest_edge,
            enable_dual_steepest_edge,
            is_primal_feasible,
            is_dual_feasible,
            var_states,
            basis_solver: BasisSolver {
                lu_factors,
                lu_factors_transp,
                scratch,
                eta_matrices: EtaMatrices::new(num_constraints),
                rhs: ScatteredVec::empty(num_constraints),
            },
            basic_vars,
            basic_var_vals,
            basic_var_mins,
            basic_var_maxs,
            dual_edge_sq_norms,
            nb_vars,
            nb_var_obj_coeffs,
            nb_var_vals,
            nb_var_states,
            nb_var_is_fixed,
            primal_edge_sq_norms,
            cur_obj_val,
            col_coeffs: SparseVec::new(),
            sq_norms_update_helper,
            inv_basis_row_coeffs: SparseVec::new(),
            row_coeffs: ScatteredVec::empty(num_total_vars - num_constraints),
        };

        debug!(
            "initialized solver: vars: {}, constraints: {}, primal feasible: {}, dual feasible: {}, nnz: {}",
            res.num_vars,
            res.orig_constraints.rows(),
            res.is_primal_feasible,
            res.is_dual_feasible,
            res.orig_constraints.nnz(),
        );

        Ok(res)
    }

    pub(crate) fn get_value(&self, var: usize) -> &Fraction {
        match self.var_states[var] {
            VarState::Basic(idx) => &self.basic_var_vals[idx],
            VarState::NonBasic(idx) => &self.nb_var_vals[idx],
        }
    }

    pub(crate) fn fix_var(&mut self, var: usize, val: Fraction) -> Result<(), Error> {
        if val < self.orig_var_mins[var] || val > self.orig_var_maxs[var] {
            return Err(Error::Infeasible);
        }

        let col = match self.var_states[var] {
            VarState::Basic(row) => {
                // if var was basic, remove it.
                self.calc_row_coeffs(row);
                let pivot_info = self.choose_entering_col_dual(row, val)?;
                self.calc_col_coeffs(pivot_info.col);
                self.pivot(&pivot_info)?;
                pivot_info.col
            }

            VarState::NonBasic(col) => {
                self.calc_col_coeffs(col);

                let diff = val - self.nb_var_vals[col];
                for (r, coeff) in self.col_coeffs.iter() {
                    self.basic_var_vals[r] -= diff * coeff;
                }
                self.cur_obj_val += diff * self.nb_var_obj_coeffs[col];
                self.nb_var_vals[col] = val;

                col
            }
        };

        self.nb_var_states[col] = NonBasicVarState {
            at_min: true,
            at_max: true,
        };
        self.nb_var_is_fixed[col] = true;

        self.is_primal_feasible = false;
        self.restore_feasibility()
    }

    /// Return true if the var was really unset.
    pub(crate) fn unfix_var(&mut self, var: usize) -> bool {
        if let VarState::NonBasic(col) = self.var_states[var] {
            if !std::mem::replace(&mut self.nb_var_is_fixed[col], false) {
                return false;
            }

            let cur_val = self.nb_var_vals[col];
            self.nb_var_states[col] = NonBasicVarState {
                at_min: float_eq(cur_val, self.orig_var_mins[var]),
                at_max: float_eq(cur_val, self.orig_var_maxs[var]),
            };

            // Shouldn't result in error, presumably problem was solvable before this variable
            // was fixed.
            self.is_dual_feasible = false;
            //TODO check unwrap
            self.optimize().unwrap();
            true
        } else {
            false
        }
    }

    pub(crate) fn add_gomory_cut(&mut self, var: usize) -> Result<(), Error> {
        if let VarState::Basic(row) = self.var_states[var] {
            self.calc_row_coeffs(row);

            let mut cut_coeffs = SparseVec::new();
            for (col, &coeff) in self.row_coeffs.iter() {
                let var = self.nb_vars[col];
                cut_coeffs.push(var, coeff.floor() - coeff);
            }

            let cut_bound = self.basic_var_vals[row].floor() - self.basic_var_vals[row];
            let num_total_vars = self.num_total_vars();
            self.add_constraint(
                cut_coeffs.into_csvec(num_total_vars),
                ComparisonOp::Le,
                cut_bound,
            )
        } else {
            panic!("var {:?} is not basic!", var);
        }
    }

    pub(crate) fn num_constraints(&self) -> usize {
        self.orig_constraints.rows()
    }

    fn num_total_vars(&self) -> usize {
        self.num_vars + self.num_constraints()
    }

    pub(crate) fn initial_solve(&mut self) -> Result<(), Error> {
        if !self.is_primal_feasible {
            self.restore_feasibility()?;
        }

        if !self.is_dual_feasible {
            self.recalc_obj_coeffs()?;
            self.optimize()?;
        }

        // Disable updates of primal sq. norms, because lengthy primal simplex runs
        // are unlikely after the initial solve.
        self.enable_primal_steepest_edge = false;

        Ok(())
    }

    pub(crate) fn solve_integer(
        &mut self,
        cur_solution: Solution,
        direction: OptimizationDirection,
    ) -> Result<(), Error> {
        let mut best_cost = if direction == OptimizationDirection::Maximize {
            Fraction::neg_infinity()
        } else {
            Fraction::infinity()
        };
        let mut best_solution = None;
        debug!("{:?}", cur_solution.iter().collect::<Vec<_>>());
        let mut dfs_stack =
            if let Some(var) = choose_branch_var(&cur_solution, &self.orig_var_domains) {
                debug!(
                    "starting branch&bound, current obj. value: {:.2}",
                    self.cur_obj_val
                );
                new_steps(cur_solution, var, &self.orig_var_domains)
            } else {
                debug!(
                    "found optimal solution with initial relaxation! cost: {:.2}",
                    self.cur_obj_val
                );
                return Ok(());
            };

        for iter in 0.. {
            //guaranteed to have at an element
            let cur_step = match dfs_stack.pop() {
                Some(step) => step,
                None => break,
            };
            let mut cur_solution = cur_step.start_solution.clone();
            let branch_direction = match cur_step.kind {
                BranchKind::Floor => ComparisonOp::Le,
                BranchKind::Ceil => ComparisonOp::Ge,
                BranchKind::Exact => ComparisonOp::Eq,
            };
            let new_solution = match branch_direction {
                ComparisonOp::Le | ComparisonOp::Ge => cur_solution.add_constraint(
                    [(cur_step.var, Fraction::one())],
                    branch_direction,
                    cur_step.start_val as f64,
                ),
                ComparisonOp::Eq => cur_solution.fix_var(cur_step.var, cur_step.start_val as f64),
            };
            if let Ok(new_solution) = new_solution {
                cur_solution = new_solution;
            } else {
                // No feasible solution with current constraints
                debug!(
                    "[iter {} (search depth {})] pruned solution, infeasible",
                    iter,
                    dfs_stack.len()
                );
                continue;
            }

            let obj_val = cur_solution.objective();
            if !is_solution_better(direction, best_cost, obj_val) {
                debug!(
                    "[iter {} (search depth {})] pruned solution, cost: {:.2}",
                    iter,
                    dfs_stack.len(),
                    obj_val
                );
                // Branch is worse than best solution
                continue;
            }

            if let Some(var) = choose_branch_var(&cur_solution, &self.orig_var_domains) {
                // Search deeper
                let steps = new_steps(cur_solution, var, &self.orig_var_domains);
                dfs_stack.extend(steps);
            } else {
                // Found integral solution
                if is_solution_better(direction, best_cost, obj_val) {
                    debug!(
                        "[iter {} (search depth {})] found new best solution, cost: {:.2}",
                        iter,
                        dfs_stack.len(),
                        obj_val
                    );
                    best_cost = obj_val;
                    best_solution = Some(cur_solution);
                }
            }
        }

        if let Some(solution) = best_solution {
            *self = solution.solver;
            Ok(())
        } else {
            Err(Error::Infeasible)
        }
    }

    fn optimize(&mut self) -> Result<(), Error> {
        for iter in 0.. {
            if iter % 1000 == 0 {
                let (num_vars, infeasibility) = self.calc_dual_infeasibility();
                debug!(
                    "optimize iter {}: obj.: {}, non-optimal coeffs: {} ({})",
                    iter, self.cur_obj_val, num_vars, infeasibility,
                );
            }

            if let Some(pivot_info) = self.choose_pivot()? {
                self.pivot(&pivot_info)?;
            } else {
                debug!(
                    "found optimum in {} iterations, obj.: {}",
                    iter + 1,
                    self.cur_obj_val,
                );
                break;
            }
        }

        self.is_dual_feasible = true;
        Ok(())
    }

    fn restore_feasibility(&mut self) -> Result<(), Error> {
        let obj_str = if self.is_dual_feasible {
            "obj."
        } else {
            "artificial obj."
        };

        for iter in 0.. {
            if iter % 1000 == 0 {
                let (num_vars, infeasibility) = self.calc_primal_infeasibility();
                debug!(
                    "restore feasibility iter {}: {}: {}, infeas. vars: {} ({})",
                    iter, obj_str, self.cur_obj_val, num_vars, infeasibility,
                );
            }

            if let Some((row, leaving_new_val)) = self.choose_pivot_row_dual() {
                self.calc_row_coeffs(row);
                let pivot_info = self.choose_entering_col_dual(row, leaving_new_val)?;
                self.calc_col_coeffs(pivot_info.col);
                self.pivot(&pivot_info)?;
            } else {
                debug!(
                    "restored feasibility in {} iterations, {}: {}",
                    iter + 1,
                    obj_str,
                    self.cur_obj_val,
                );
                break;
            }
        }

        self.is_primal_feasible = true;
        Ok(())
    }

    pub(crate) fn add_constraint(
        &mut self,
        mut coeffs: CsVec,
        cmp_op: ComparisonOp,
        rhs: Fraction,
    ) -> Result<(), Error> {
        assert!(self.is_primal_feasible);
        assert!(self.is_dual_feasible);

        if coeffs.indices().is_empty() {
            let is_tautological = match cmp_op {
                ComparisonOp::Eq => rhs.is_zero(),
                ComparisonOp::Le => rhs.is_not_negative(),
                ComparisonOp::Ge => rhs.is_not_positive(),
            };

            return if is_tautological {
                Ok(())
            } else {
                Err(Error::Infeasible)
            };
        }

        let slack_var = self.num_total_vars();
        let (slack_var_min, slack_var_max) = match cmp_op {
            ComparisonOp::Le => (Fraction::zero(), Fraction::infinity()),
            ComparisonOp::Ge => (Fraction::neg_infinity(), Fraction::zero()),
            ComparisonOp::Eq => (Fraction::zero(), Fraction::zero()),
        };

        self.orig_obj_coeffs.push(Fraction::zero());
        self.orig_var_mins.push(slack_var_min);
        self.orig_var_maxs.push(slack_var_max);
        self.var_states.push(VarState::Basic(self.basic_vars.len()));
        self.basic_vars.push(slack_var);
        self.basic_var_mins.push(slack_var_min);
        self.basic_var_maxs.push(slack_var_max);

        let mut lhs_val = Fraction::zero();
        for (var, &coeff) in coeffs.iter() {
            let val = match self.var_states[var] {
                VarState::Basic(idx) => self.basic_var_vals[idx],
                VarState::NonBasic(idx) => self.nb_var_vals[idx],
            };
            lhs_val += &val * &coeff;
        }
        self.basic_var_vals.push(&rhs - &lhs_val);

        let new_num_total_vars = self.num_total_vars() + 1;
        let mut new_orig_constraints = CsMat::empty(CompressedStorage::CSR, new_num_total_vars);
        for row in self.orig_constraints.outer_iterator() {
            new_orig_constraints =
                new_orig_constraints.append_outer_csvec(resized_view(&row, new_num_total_vars));
        }
        coeffs = into_resized(coeffs, new_num_total_vars);
        coeffs.append(slack_var, Fraction::one());
        new_orig_constraints = new_orig_constraints.append_outer_csvec(coeffs.view());

        self.orig_rhs.push(rhs);

        self.orig_constraints = new_orig_constraints;
        self.orig_constraints_csc = self.orig_constraints.to_csc();

        self.basis_solver
            .reset(&self.orig_constraints_csc, &self.basic_vars)?;

        if self.enable_primal_steepest_edge || self.enable_dual_steepest_edge {
            // existing tableau rows didn't change, so we calc the last row
            // and add its contribution to the sq. norms.
            self.calc_row_coeffs(self.num_constraints() - 1);

            if self.enable_primal_steepest_edge {
                for (c, &coeff) in self.row_coeffs.iter() {
                    self.primal_edge_sq_norms[c] += coeff * coeff;
                }
            }

            if self.enable_dual_steepest_edge {
                self.dual_edge_sq_norms
                    .push(self.inv_basis_row_coeffs.sq_norm());
            }
        }

        self.is_primal_feasible = false;
        self.restore_feasibility()
    }

    /// Number of infeasible basic vars and sum of their infeasibilities.
    fn calc_primal_infeasibility(&self) -> (usize, Fraction) {
        let mut num_vars = 0;
        let mut infeasibility = Fraction::zero();
        for ((&val, &min), &max) in self
            .basic_var_vals
            .iter()
            .zip(&self.basic_var_mins)
            .zip(&self.basic_var_maxs)
        {
            if val < min {
                num_vars += 1;
                infeasibility += min - val;
            } else if val > max {
                num_vars += 1;
                infeasibility += val - max;
            }
        }
        (num_vars, infeasibility)
    }

    /// Number of infeasible obj. coeffs and sum of their infeasibilities.
    fn calc_dual_infeasibility(&self) -> (usize, Fraction) {
        let mut num_vars = 0;
        let mut infeasibility = Fraction::zero();
        for (&obj_coeff, var_state) in self.nb_var_obj_coeffs.iter().zip(&self.nb_var_states) {
            if !(var_state.at_min && obj_coeff.is_not_negative()
                || var_state.at_max && obj_coeff.is_not_positive())
            {
                num_vars += 1;
                infeasibility += obj_coeff.abs();
            }
        }
        (num_vars, infeasibility)
    }

    /// Calculate current coeffs column for a single non-basic variable.
    fn calc_col_coeffs(&mut self, c_var: usize) {
        let var = self.nb_vars[c_var];
        //guaranteed to be a valid index
        let orig_col = self.orig_constraints_csc.outer_view(var).unwrap();
        self.basis_solver
            .solve(orig_col.iter())
            .to_sparse_vec(&mut self.col_coeffs);
    }

    /// Calculate current coeffs row for a single constraint (permuted according to nb_vars).
    fn calc_row_coeffs(&mut self, r_constr: usize) {
        self.basis_solver
            .solve_transp(std::iter::once((r_constr, &Fraction::one())))
            .to_sparse_vec(&mut self.inv_basis_row_coeffs);

        self.row_coeffs.clear_and_resize(self.nb_vars.len());
        for (r, &coeff) in self.inv_basis_row_coeffs.iter() {
            //guaranteed to be a valid index
            for (v, &val) in self.orig_constraints.outer_view(r).unwrap().iter() {
                if let VarState::NonBasic(idx) = self.var_states[v] {
                    *self.row_coeffs.get_mut(idx) += val * coeff;
                }
            }
        }
    }

    fn choose_pivot(&mut self) -> Result<Option<PivotInfo>, Error> {
        let entering_c = {
            let filtered_obj_coeffs = self
                .nb_var_obj_coeffs
                .iter()
                .zip(&self.nb_var_states)
                .enumerate()
                .filter_map(|(col, (&obj_coeff, var_state))| {
                    // Choose only among non-basic vars that can be changed
                    // with objective decreasing.
                    if (var_state.at_min && obj_coeff.is_not_negative())
                        || (var_state.at_max && obj_coeff.is_not_positive())
                    {
                        None
                    } else {
                        Some((col, obj_coeff))
                    }
                });

            let mut best_col = None;
            let mut best_score = Fraction::neg_infinity();
            if self.enable_primal_steepest_edge {
                for (col, obj_coeff) in filtered_obj_coeffs {
                    let score = obj_coeff * obj_coeff / self.primal_edge_sq_norms[col];
                    if score > best_score {
                        best_col = Some(col);
                        best_score = score;
                    }
                }
            } else {
                for (col, obj_coeff) in filtered_obj_coeffs {
                    let score = obj_coeff.abs();
                    if score > best_score {
                        best_col = Some(col);
                        best_score = score;
                    }
                }
            }

            if let Some(col) = best_col {
                col
            } else {
                return Ok(None);
            }
        };

        let entering_cur_val = self.nb_var_vals[entering_c];
        // If true, entering variable will increase (because the objective function must decrease).
        let entering_diff_sign = self.nb_var_obj_coeffs[entering_c].is_negative();
        let entering_other_val = if entering_diff_sign {
            self.orig_var_maxs[self.nb_vars[entering_c]]
        } else {
            self.orig_var_mins[self.nb_vars[entering_c]]
        };

        self.calc_col_coeffs(entering_c);

        let get_leaving_var_step = |r: usize, coeff: Fraction| -> Fraction {
            let val = self.basic_var_vals[r];
            // leaving_diff = -entering_diff * coeff. From this we can determine
            // in which direction this basic var will change and select appropriate bound.
            if (entering_diff_sign && coeff.is_negative())
                || (!entering_diff_sign && coeff.is_positive())
            {
                let max = self.basic_var_maxs[r];
                if val < max {
                    max - val
                } else {
                    Fraction::zero()
                }
            } else {
                let min = self.basic_var_mins[r];
                if val > min {
                    val - min
                } else {
                    Fraction::zero()
                }
            }
        };

        // Harris rule. See e.g.
        // Gill, P. E., Murray, W., Saunders, M. A., & Wright, M. H. (1989).
        // A practical anti-cycling procedure for linearly constrained optimization.
        // Mathematical Programming, 45(1-3), 437-474.
        //
        // https://link.springer.com/content/pdf/10.1007/BF01589114.pdf

        // First, we determine the max change in entering variable so that basic variables
        // remain feasible using relaxed bounds.
        let mut max_step = (entering_other_val - entering_cur_val).abs();
        for (r, &coeff) in self.col_coeffs.iter() {
            let coeff_abs = coeff.abs();
            if coeff_abs.is_not_positive() {
                continue;
            }

            // By which amount can we change the entering variable so that the limit on this
            // basic var is not violated. The var with the minimum such amount becomes leaving.
            let cur_step = (get_leaving_var_step(r, coeff) + EPS) / coeff_abs;
            if cur_step < max_step {
                max_step = cur_step;
            }
        }

        // Second, we choose among variables with steps less than max_step a variable with the biggest
        // abs. coefficient as the leaving variable. This means that we get numerically more stable
        // basis at the price of slight infeasibility of some basic variables.
        let mut leaving_r = None;
        let mut leaving_new_val = Fraction::zero();
        let mut pivot_coeff_abs = Fraction::neg_infinity();
        let mut pivot_coeff = Fraction::zero();
        for (r, &coeff) in self.col_coeffs.iter() {
            let coeff_abs = coeff.abs();
            if coeff_abs.is_not_positive() {
                continue;
            }

            let cur_step = get_leaving_var_step(r, coeff) / coeff_abs;
            if cur_step <= max_step && coeff_abs > pivot_coeff_abs {
                leaving_r = Some(r);
                leaving_new_val = if (entering_diff_sign && coeff.is_negative())
                    || (!entering_diff_sign && coeff.is_positive())
                {
                    self.basic_var_maxs[r]
                } else {
                    self.basic_var_mins[r]
                };
                pivot_coeff = coeff;
                pivot_coeff_abs = coeff_abs;
            }
        }

        if let Some(row) = leaving_r {
            self.calc_row_coeffs(row);

            let entering_diff = (self.basic_var_vals[row] - leaving_new_val) / pivot_coeff;
            let entering_new_val = entering_cur_val + entering_diff;

            Ok(Some(PivotInfo {
                col: entering_c,
                entering_new_val,
                entering_diff,
                elem: Some(PivotElem {
                    row,
                    coeff: pivot_coeff,
                    leaving_new_val,
                }),
            }))
        } else {
            if entering_other_val.is_infinite() {
                return Err(Error::Unbounded);
            }

            Ok(Some(PivotInfo {
                col: entering_c,
                entering_new_val: entering_other_val,
                entering_diff: entering_other_val - entering_cur_val,
                elem: None,
            }))
        }
    }

    fn choose_pivot_row_dual(&self) -> Option<(usize, Fraction)> {
        let infeasibilities = self
            .basic_var_vals
            .iter()
            .zip(&self.basic_var_mins)
            .zip(&self.basic_var_maxs)
            .enumerate()
            .filter_map(|(r, ((&val, &min), &max))| {
                if val < min {
                    Some((r, min - val))
                } else if val > max {
                    Some((r, val - max))
                } else {
                    None
                }
            });

        let mut leaving_r = None;
        let mut max_score = Fraction::neg_infinity();
        if self.enable_dual_steepest_edge {
            for (r, infeasibility) in infeasibilities {
                let sq_norm = self.dual_edge_sq_norms[r];
                let score = infeasibility * infeasibility / sq_norm;
                if score > max_score {
                    leaving_r = Some(r);
                    max_score = score;
                }
            }
        } else {
            for (r, infeasibility) in infeasibilities {
                if infeasibility > max_score {
                    leaving_r = Some(r);
                    max_score = infeasibility;
                }
            }
        }

        leaving_r.map(|r| {
            let val = self.basic_var_vals[r];
            let min = self.basic_var_mins[r];
            let max = self.basic_var_maxs[r];

            // If we choose this var as leaving, its new val will be at the boundary
            // which is violated.
            // Why is that? We must maintain primal optimality (a.k.a. dual feasibility) for
            // the leaving variable, thus new_obj_coeff must be >= 0 if new_val is min, and <= 0
            // if new_val is max. Sign of the leaving var obj coeff:
            // sign(new_obj_coeff) = -sign(old_obj_coeff) * sign(pivot_coeff).
            // Another constraint is that we must not decrease primal objective.
            // As sign(obj_val_diff) = -sign(old_obj_coeff) * sign(leaving_diff) * sign(pivot_coeff)
            // must be >= 0, we conclude that sign(new_obj_coeff) = sign(leaving_diff).
            // From this we see that if old val was < min, dual feasibility is maintained if the
            // new var is min (analogously for max).
            let new_val = if val < min {
                min
            } else if val > max {
                max
            } else {
                unreachable!();
            };
            (r, new_val)
        })
    }

    fn choose_entering_col_dual(
        &self,
        row: usize,
        leaving_new_val: Fraction,
    ) -> Result<PivotInfo, Error> {
        // True if the new obj. coeff. must be nonnegative in a dual-feasible configuration.
        let leaving_diff_sign = leaving_new_val > self.basic_var_vals[row];

        fn clamp_obj_coeff(mut obj_coeff: Fraction, var_state: &NonBasicVarState) -> Fraction {
            if var_state.at_min && obj_coeff.is_negative() {
                obj_coeff = Fraction::zero();
            }
            if var_state.at_max && obj_coeff.is_positive() {
                obj_coeff = Fraction::zero();
            }
            obj_coeff
        }

        let is_eligible_var = |coeff: Fraction, var_state: &NonBasicVarState| -> bool {
            let entering_diff_sign = if coeff >= EPS {
                !leaving_diff_sign
            } else if coeff <= -EPS {
                leaving_diff_sign
            } else {
                return false;
            };

            if entering_diff_sign {
                !var_state.at_max
            } else {
                !var_state.at_min
            }
        };

        // Harris rule. See e.g.
        // Gill, P. E., Murray, W., Saunders, M. A., & Wright, M. H. (1989).
        // A practical anti-cycling procedure for linearly constrained optimization.
        // Mathematical Programming, 45(1-3), 437-474.
        //
        // https://link.springer.com/content/pdf/10.1007/BF01589114.pdf

        // First, we determine the max step (change in the leaving variable obj. coeff that still
        // leaves us with a dual-feasible state) using relaxed bounds.
        let mut max_step = Fraction::infinity();
        for (c, &coeff) in self.row_coeffs.iter() {
            let var_state = &self.nb_var_states[c];
            if !is_eligible_var(coeff, var_state) {
                continue;
            }

            let obj_coeff = clamp_obj_coeff(self.nb_var_obj_coeffs[c], var_state);
            let cur_step = (obj_coeff.abs() + EPS) / coeff.abs();
            if cur_step < max_step {
                max_step = cur_step;
            }
        }

        // Second, we choose among the variables satisfying the relaxed step bound
        // the one with the biggest pivot coefficient. This allows for a much more
        // numerically stable basis at the price of slight infeasibility in dual variables.
        let mut entering_c = None;
        let mut pivot_coeff_abs = Fraction::neg_infinity();
        let mut pivot_coeff = Fraction::zero();
        for (c, &coeff) in self.row_coeffs.iter() {
            let var_state = &self.nb_var_states[c];
            if !is_eligible_var(coeff, var_state) {
                continue;
            }

            let obj_coeff = clamp_obj_coeff(self.nb_var_obj_coeffs[c], var_state);

            // If we change obj. coeff of the leaving variable by this amount,
            // obj. coeff if the current variable will reach the bound of dual infeasibility.
            // Variable with the tightest such bound is the entering variable.
            let cur_step = obj_coeff.abs() / coeff.abs();
            if cur_step <= max_step {
                let coeff_abs = coeff.abs();
                if coeff_abs > pivot_coeff_abs {
                    entering_c = Some(c);
                    pivot_coeff_abs = coeff_abs;
                    pivot_coeff = coeff;
                }
            }
        }

        if let Some(col) = entering_c {
            let entering_diff = (self.basic_var_vals[row] - leaving_new_val) / pivot_coeff;
            let entering_new_val = self.nb_var_vals[col] + entering_diff;

            Ok(PivotInfo {
                col,
                entering_new_val,
                entering_diff,
                elem: Some(PivotElem {
                    row,
                    leaving_new_val,
                    coeff: pivot_coeff,
                }),
            })
        } else {
            Err(Error::Infeasible)
        }
    }

    fn pivot(&mut self, pivot_info: &PivotInfo) -> Result<(), Error> {
        // TODO: periodically (say, every 1000 pivots) recalc basic vars and object coeffs
        // from scratch for numerical stability.

        self.cur_obj_val += self.nb_var_obj_coeffs[pivot_info.col] * pivot_info.entering_diff;

        let entering_var = self.nb_vars[pivot_info.col];

        if pivot_info.elem.is_none() {
            // "entering" var is still non-basic, it just changes value from one limit
            // to the other.
            self.nb_var_vals[pivot_info.col] = pivot_info.entering_new_val;
            for (r, coeff) in self.col_coeffs.iter() {
                self.basic_var_vals[r] -= pivot_info.entering_diff * coeff;
            }
            let var_state = &mut self.nb_var_states[pivot_info.col];
            var_state.at_min = pivot_info.entering_new_val == self.orig_var_mins[entering_var];
            var_state.at_max = pivot_info.entering_new_val == self.orig_var_maxs[entering_var];
            return Ok(());
        }
        //guaranteed, none variant already handled
        let pivot_elem = pivot_info.elem.as_ref().unwrap();
        let pivot_coeff = pivot_elem.coeff;

        // Update basic vars stuff

        for (r, coeff) in self.col_coeffs.iter() {
            if r == pivot_elem.row {
                self.basic_var_vals[r] = pivot_info.entering_new_val;
            } else {
                self.basic_var_vals[r] -= pivot_info.entering_diff * coeff;
            }
        }

        self.basic_var_mins[pivot_elem.row] = self.orig_var_mins[entering_var];
        self.basic_var_maxs[pivot_elem.row] = self.orig_var_maxs[entering_var];

        if self.enable_dual_steepest_edge {
            self.update_dual_sq_norms(pivot_elem.row, pivot_coeff);
        }

        // Update non-basic vars stuff

        let leaving_var = self.basic_vars[pivot_elem.row];

        self.nb_var_vals[pivot_info.col] = pivot_elem.leaving_new_val;
        let leaving_var_state = &mut self.nb_var_states[pivot_info.col];
        leaving_var_state.at_min = pivot_elem.leaving_new_val == self.orig_var_mins[leaving_var];
        leaving_var_state.at_max = pivot_elem.leaving_new_val == self.orig_var_maxs[leaving_var];

        let pivot_obj = self.nb_var_obj_coeffs[pivot_info.col] / pivot_coeff;
        for (c, &coeff) in self.row_coeffs.iter() {
            if c == pivot_info.col {
                self.nb_var_obj_coeffs[c] = -pivot_obj;
            } else {
                self.nb_var_obj_coeffs[c] -= pivot_obj * coeff;
            }
        }

        if self.enable_primal_steepest_edge {
            self.update_primal_sq_norms(pivot_info.col, pivot_coeff);
        }

        // Update basis itself

        self.basic_vars[pivot_elem.row] = entering_var;
        self.var_states[entering_var] = VarState::Basic(pivot_elem.row);
        self.nb_vars[pivot_info.col] = leaving_var;
        self.var_states[leaving_var] = VarState::NonBasic(pivot_info.col);

        // A simple heuristic to choose when to recompute LU factorization.
        // Note: a possible failure mode is that the LU factorization accidentally
        // generates a lot of fill-in and doesn't get recomputed for a long time.
        let eta_matrices_nnz = self.basis_solver.eta_matrices.coeff_cols.nnz();
        if eta_matrices_nnz < self.basis_solver.lu_factors.nnz() {
            self.basis_solver
                .push_eta_matrix(&self.col_coeffs, pivot_elem.row, pivot_coeff);
        } else {
            self.basis_solver
                .reset(&self.orig_constraints_csc, &self.basic_vars)?;
        }
        Ok(())
    }

    fn update_primal_sq_norms(&mut self, entering_col: usize, pivot_coeff: Fraction) {
        // Computations for the steepest edge pivoting rule. See
        // Forrest, J. J., & Goldfarb, D. (1992).
        // Steepest-edge simplex algorithms for linear programming.
        // Mathematical programming, 57(1-3), 341-374.
        //
        // https://link.springer.com/content/pdf/10.1007/BF01581089.pdf

        let tmp = self.basis_solver.solve_transp(self.col_coeffs.iter());
        // now tmp contains the v vector from the article.

        for &r in tmp.indices() {
            //guaranteed to be a valid index
            for &v in self.orig_constraints.outer_view(r).unwrap().indices() {
                if let VarState::NonBasic(idx) = self.var_states[v] {
                    self.sq_norms_update_helper[idx] = Fraction::zero();
                }
            }
        }
        // now significant positions in sq_norms_update_helper are cleared.

        for (r, &coeff) in tmp.iter() {
            //guaranteed to be a valid index
            for (v, &val) in self.orig_constraints.outer_view(r).unwrap().iter() {
                if let VarState::NonBasic(idx) = self.var_states[v] {
                    self.sq_norms_update_helper[idx] += val * coeff;
                }
            }
        }
        // now sq_norms_update_helper contains transp(N) * v vector.

        // Calculate pivot_sq_norm directly to avoid loss of precision.
        let pivot_sq_norm = self.col_coeffs.sq_norm() + 1.0;
        // assert!((self.primal_edge_sq_norms[entering_col] - pivot_sq_norm).abs() < 0.1);

        let pivot_coeff_sq = pivot_coeff * pivot_coeff;
        for (c, &r_coeff) in self.row_coeffs.iter() {
            if c == entering_col {
                self.primal_edge_sq_norms[c] = pivot_sq_norm / pivot_coeff_sq;
            } else {
                self.primal_edge_sq_norms[c] += -2.0 * r_coeff * self.sq_norms_update_helper[c]
                    / pivot_coeff
                    + pivot_sq_norm * r_coeff * r_coeff / pivot_coeff_sq;
            }

            assert!(self.primal_edge_sq_norms[c].is_finite());
        }
    }

    fn update_dual_sq_norms(&mut self, leaving_row: usize, pivot_coeff: Fraction) {
        // Computations for the dual steepest edge pivoting rule.
        // See the same reference (Forrest, Goldfarb).

        let tau = self.basis_solver.solve(self.inv_basis_row_coeffs.iter());

        // Calculate pivot_sq_norm directly to avoid loss of precision.
        let pivot_sq_norm = self.inv_basis_row_coeffs.sq_norm();
        // assert!((self.dual_edge_sq_norms[leaving_row] - pivot_sq_norm).abs() < 0.1);

        let pivot_coeff_sq = pivot_coeff * pivot_coeff;
        for (r, &col_coeff) in self.col_coeffs.iter() {
            if r == leaving_row {
                self.dual_edge_sq_norms[r] = pivot_sq_norm / pivot_coeff_sq;
            } else {
                self.dual_edge_sq_norms[r] += -2.0 * col_coeff * tau.get(r) / pivot_coeff
                    + pivot_sq_norm * col_coeff * col_coeff / pivot_coeff_sq;
            }

            assert!(self.dual_edge_sq_norms[r].is_finite());
        }
    }

    #[allow(dead_code)]
    fn recalc_basic_var_vals(&mut self) -> Result<(), Error> {
        let mut cur_vals = self.orig_rhs.clone();
        for (i, var) in self.nb_vars.iter().enumerate() {
            let val = self.nb_var_vals[i];
            if val != 0.0 {
                //guaranteed to be a valid index
                for (r, &coeff) in self.orig_constraints_csc.outer_view(*var).unwrap().iter() {
                    cur_vals[r] -= val * coeff;
                }
            }
        }

        if self.basis_solver.eta_matrices.len() > 0 {
            self.basis_solver
                .reset(&self.orig_constraints_csc, &self.basic_vars)?;
        }

        self.basis_solver
            .lu_factors
            .solve_dense(&mut cur_vals, &mut self.basis_solver.scratch);
        self.basic_var_vals = cur_vals;
        Ok(())
    }

    fn recalc_obj_coeffs(&mut self) -> Result<(), Error> {
        if self.basis_solver.eta_matrices.len() > 0 {
            self.basis_solver
                .reset(&self.orig_constraints_csc, &self.basic_vars)?;
        }

        let multipliers = {
            let mut rhs = vec![0.0; self.num_constraints()];
            for (c, &var) in self.basic_vars.iter().enumerate() {
                rhs[c] = self.orig_obj_coeffs[var];
            }
            self.basis_solver
                .lu_factors_transp
                .solve_dense(&mut rhs, &mut self.basis_solver.scratch);
            rhs
        };

        self.nb_var_obj_coeffs.clear();
        for &var in &self.nb_vars {
            //guaranteed to be a valid index
            let col = self.orig_constraints_csc.outer_view(var).unwrap();
            let dot_prod: Fraction = col.iter().map(|(r, val)| val * multipliers[r]).sum();
            self.nb_var_obj_coeffs
                .push(self.orig_obj_coeffs[var] - dot_prod);
        }

        self.cur_obj_val = Fraction::zero();
        for (r, &var) in self.basic_vars.iter().enumerate() {
            self.cur_obj_val += self.orig_obj_coeffs[var] * self.basic_var_vals[r];
        }
        for (c, &var) in self.nb_vars.iter().enumerate() {
            self.cur_obj_val += self.orig_obj_coeffs[var] * self.nb_var_vals[c];
        }
        Ok(())
    }

    #[allow(dead_code)]
    fn recalc_primal_sq_norms(&mut self) {
        self.primal_edge_sq_norms.clear();
        for &var in &self.nb_vars {
            //guaranteed to be a valid index
            let col = self.orig_constraints_csc.outer_view(var).unwrap();
            let sq_norm = self.basis_solver.solve(col.iter()).sq_norm() + 1.0;
            self.primal_edge_sq_norms.push(sq_norm);
        }
    }
}

#[derive(Debug)]
struct PivotInfo {
    col: usize,
    entering_new_val: Fraction,
    entering_diff: Fraction,

    /// Contains info about the intersection between pivot row and column.
    /// If it is None, objective can be decreased without changing the basis
    /// (simply by changing the value of non-basic variable chosen as entering)
    elem: Option<PivotElem>,
}

#[derive(Debug)]
struct PivotElem {
    row: usize,
    coeff: Fraction,
    leaving_new_val: Fraction,
}

/// Stuff related to inversion of the basis matrix
#[derive(Clone)]
struct BasisSolver {
    lu_factors: LUFactors,
    lu_factors_transp: LUFactors,
    scratch: ScratchSpace,
    eta_matrices: EtaMatrices,
    rhs: ScatteredVec,
}

impl BasisSolver {
    fn push_eta_matrix(&mut self, col_coeffs: &SparseVec, r_leaving: usize, pivot_coeff: Fraction) {
        let coeffs = col_coeffs.iter().map(|(r, &coeff)| {
            let val = if r == r_leaving {
                1.0 - 1.0 / pivot_coeff
            } else {
                coeff / pivot_coeff
            };
            (r, val)
        });
        self.eta_matrices.push(r_leaving, coeffs);
    }

    fn reset(&mut self, orig_constraints_csc: &CsMat, basic_vars: &[usize]) -> Result<(), Error> {
        self.scratch.clear_sparse(basic_vars.len());
        self.eta_matrices.clear_and_resize(basic_vars.len());
        self.rhs.clear_and_resize(basic_vars.len());
        self.lu_factors = lu_factorize(
            basic_vars.len(),
            |c| {
                orig_constraints_csc
                    .outer_view(basic_vars[c])
                    //guaranteed to be a valid index
                    .unwrap()
                    .into_raw_storage()
            },
            0.1,
            &mut self.scratch,
        )?;
        self.lu_factors_transp = self.lu_factors.transpose();
        Ok(())
    }

    fn solve<'a>(&mut self, rhs: impl Iterator<Item = (usize, &'a Fraction)>) -> &ScatteredVec {
        self.rhs.set(rhs);
        self.lu_factors.solve(&mut self.rhs, &mut self.scratch);

        // apply eta matrices (Vanderbei p.139)
        for idx in 0..self.eta_matrices.len() {
            let r_leaving = self.eta_matrices.leaving_rows[idx];
            let coeff = *self.rhs.get(r_leaving);
            for (r, &val) in self.eta_matrices.coeff_cols.col_iter(idx) {
                *self.rhs.get_mut(r) -= coeff * val;
            }
        }

        &mut self.rhs
    }

    /// Pass right-hand side via self.rhs
    fn solve_transp<'a>(
        &mut self,
        rhs: impl Iterator<Item = (usize, &'a Fraction)>,
    ) -> &ScatteredVec {
        self.rhs.set(rhs);
        // apply eta matrices in reverse (Vanderbei p.139)
        for idx in (0..self.eta_matrices.len()).rev() {
            let mut coeff = 0.0;
            // eta col `dot` rhs_transp
            for (i, &val) in self.eta_matrices.coeff_cols.col_iter(idx) {
                coeff += val * self.rhs.get(i);
            }
            let r_leaving = self.eta_matrices.leaving_rows[idx];
            *self.rhs.get_mut(r_leaving) -= coeff;
        }

        self.lu_factors_transp
            .solve(&mut self.rhs, &mut self.scratch);
        &mut self.rhs
    }
}

#[derive(Clone, Debug)]
struct EtaMatrices {
    leaving_rows: Vec<usize>,
    coeff_cols: SparseMat,
}

impl EtaMatrices {
    fn new(n_rows: usize) -> EtaMatrices {
        EtaMatrices {
            leaving_rows: vec![],
            coeff_cols: SparseMat::new(n_rows),
        }
    }

    fn len(&self) -> usize {
        self.leaving_rows.len()
    }

    fn clear_and_resize(&mut self, n_rows: usize) {
        self.leaving_rows.clear();
        self.coeff_cols.clear_and_resize(n_rows);
    }

    fn push(&mut self, leaving_row: usize, coeffs: impl Iterator<Item = (usize, Fraction)>) {
        self.leaving_rows.push(leaving_row);
        self.coeff_cols.append_col(coeffs);
    }
}

fn into_resized(vec: CsVec, len: usize) -> CsVec {
    let (mut indices, mut data) = vec.into_raw_storage();

    while let Some(&i) = indices.last() {
        if i < len {
            // TODO: binary search
            break;
        }

        indices.pop();
        data.pop();
    }

    CsVec::new(len, indices, data)
}

fn choose_branch_var(cur_solution: &Solution, domain: &[VarDomain]) -> Option<Variable> {
    let mut max_divergence = 0.0;
    let mut max_var = None;
    for (var, &val) in cur_solution {
        if domain[var.0] == VarDomain::Real {
            continue;
        }
        let divergence = Fraction::abs(val - val.round());
        if divergence > EPS && divergence > max_divergence {
            max_divergence = divergence;
            max_var = Some(var);
        }
    }
    max_var
}

fn get_branch_min_max(var: Variable, current_solution: &Solution) -> (i64, i64) {
    let min = current_solution[var].floor() as i64;
    let max = current_solution[var].ceil() as i64;
    (min, max)
}

fn new_steps(start_solution: Solution, var: Variable, var_domains: &[VarDomain]) -> Vec<Step> {
    //TODO swap order of the branches by prioritizing the branch that improves the objective function
    let (min, max) = get_branch_min_max(var, &start_solution);
    if min == max {
        return vec![];
    }
    let is_bool = var_domains[var.0] == VarDomain::Boolean;
    let lower_branch = Step {
        start_solution: start_solution.clone(),
        var,
        kind: if is_bool {
            BranchKind::Exact
        } else {
            BranchKind::Floor
        },
        start_val: min,
    };
    let higher_branch = Step {
        start_solution,
        var,
        kind: if is_bool {
            BranchKind::Exact
        } else {
            BranchKind::Ceil
        },
        start_val: max,
    };
    vec![lower_branch, higher_branch]
}

fn is_solution_better(direction: OptimizationDirection, best: Fraction, current: Fraction) -> bool {
    match direction {
        OptimizationDirection::Maximize => current > best,
        OptimizationDirection::Minimize => current < best,
    }
}
