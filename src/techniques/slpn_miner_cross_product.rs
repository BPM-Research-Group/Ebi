use std::collections::{HashMap, VecDeque};
use std::fmt;

use ebi_objects::Activity;
use anyhow::{Result, anyhow};

use crate::{
    ebi_framework::displayable::Displayable,
    ebi_traits::ebi_trait_semantics::EbiTraitSemantics,
    semantics::semantics::Semantics,
};

// Symbolic Math Types (Poly, FactoredPoly, SymbolicFraction)
// Helper to exactly divide one sorted sequence of symbols by another.
fn divide_monomial(m1: &[String], m2: &[String]) -> Option<Vec<String>> {
    let mut res = Vec::new();
    let mut i = 0;
    let mut j = 0;
    while i < m1.len() && j < m2.len() {
        if m1[i] == m2[j] {
            i += 1; j += 1;
        } else if m1[i] < m2[j] {
            res.push(m1[i].clone()); i += 1;
        } else {
            return None;
        }
    }
    if j < m2.len() { return None; }
    while i < m1.len() {
        res.push(m1[i].clone()); i += 1;
    }
    Some(res)
}

/// A multivariate polynomial with integer coefficients and symbolic variables.
#[derive(Clone, Debug)]
pub struct Poly {
    terms: HashMap<Vec<String>, i64>,
}

impl Poly {
    fn new(val: i64) -> Self {
        let mut terms = HashMap::new();
        if val != 0 { terms.insert(vec![], val); }
        Poly { terms }
    }

    fn sym(name: &str) -> Self {
        let mut terms = HashMap::new();
        terms.insert(vec![name.to_string()], 1);
        Poly { terms }
    }

    fn add(&self, other: &Poly) -> Self {
        let mut terms = self.terms.clone();
        for (vars, coef) in &other.terms {
            *terms.entry(vars.clone()).or_insert(0) += coef;
        }
        terms.retain(|_, v| *v != 0);
        Poly { terms }
    }

    fn sub(&self, other: &Poly) -> Self {
        let mut terms = self.terms.clone();
        for (vars, coef) in &other.terms {
            *terms.entry(vars.clone()).or_insert(0) -= coef;
        }
        terms.retain(|_, v| *v != 0);
        Poly { terms }
    }

    fn mul(&self, other: &Poly) -> Self {
        let mut terms = HashMap::new();
        for (vars1, coef1) in &self.terms {
            for (vars2, coef2) in &other.terms {
                let mut new_vars = vars1.clone();
                new_vars.extend(vars2.clone());
                new_vars.sort();
                *terms.entry(new_vars).or_insert(0) += coef1 * coef2;
            }
        }
        terms.retain(|_, v| *v != 0);
        Poly { terms }
    }

    fn is_zero(&self) -> bool { self.terms.is_empty() }

    fn leading_term(&self) -> Option<(Vec<String>, i64)> {
        self.terms.iter()
            .max_by(|(k1, _), (k2, _)| {
                if k1.len() != k2.len() { k1.len().cmp(&k2.len()) }
                else { k1.cmp(k2) }
            })
            .map(|(k, v)| (k.clone(), *v))
    }

    fn exact_div(&self, other: &Poly) -> Option<Poly> {
        if other.is_zero() { return None; }
        if self.is_zero() { return Some(Poly::new(0)); }

        let mut remainder = self.clone();
        let mut quotient = Poly::new(0);
        let divisor_lt = other.leading_term()?;

        while !remainder.is_zero() {
            let rem_lt = remainder.leading_term()?;
            if rem_lt.1 % divisor_lt.1 != 0 { return None; }
            let coef_div = rem_lt.1 / divisor_lt.1;

            if let Some(vars_div) = divide_monomial(&rem_lt.0, &divisor_lt.0) {
                let mut term_map = HashMap::new();
                term_map.insert(vars_div, coef_div);
                let q_term = Poly { terms: term_map };

                quotient = quotient.add(&q_term);
                let sub_poly = q_term.mul(other);
                remainder = remainder.sub(&sub_poly);
            } else {
                return None;
            }
        }
        Some(quotient)
    }
}

impl fmt::Display for Poly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_zero() { return write!(f, "0"); }
        let mut keys: Vec<_> = self.terms.keys().collect();
        keys.sort_by(|a, b| if a.len() != b.len() { a.len().cmp(&b.len()) } else { a.cmp(b) });

        let mut parts = Vec::new();
        for vars in keys {
            let coef = self.terms[vars];
            let mut term_str = String::new();
            if coef == -1 && !vars.is_empty() { term_str.push('-'); }
            else if coef != 1 || vars.is_empty() {
                term_str.push_str(&coef.to_string());
                if !vars.is_empty() { term_str.push('*'); }
            }
            term_str.push_str(&vars.join("*"));
            parts.push(term_str);
        }
        let res = parts.join(" + ").replace("+ -", "- ");
        if res.is_empty() { write!(f, "0") } else { write!(f, "{}", res) }
    }
}

/// A list of polynomial factors kept unexpanded for efficiency.
#[derive(Clone, Debug)]
pub struct FactoredPoly { factors: Vec<Poly> }

impl FactoredPoly {
    fn new(p: Poly) -> Self { FactoredPoly { factors: vec![p] } }

    fn mul(&self, other: &FactoredPoly) -> Self {
        let mut new = self.factors.clone();
        new.extend(other.factors.clone());
        FactoredPoly { factors: new }
    }

    fn to_poly(&self) -> Poly {
        let mut res = Poly::new(1);
        for f in &self.factors { res = res.mul(f); }
        res
    }
}

impl fmt::Display for FactoredPoly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.factors.is_empty() { return write!(f, "1"); }
        let mut parts = Vec::new();
        let mut sign = "";
        for factor in &self.factors {
            let s = format!("{}", factor);
            if s == "1" { continue; }
            if s == "-1" { sign = "-"; continue; }
            if factor.terms.len() > 1 { parts.push(format!("({})", s)); }
            else { parts.push(s); }
        }
        if parts.is_empty() { return write!(f, "{}1", sign); }
        write!(f, "{}{}", sign, parts.join(" * "))
    }
}

/// Computes the LCM of two factored denominators without expanding identical brackets.
fn lcm_factored(f1: &FactoredPoly, f2: &FactoredPoly) -> (FactoredPoly, Poly, Poly) {
    let mut lcm_factors = f1.factors.clone();
    let mut self_mult = Poly::new(1);
    let mut other_mult = Poly::new(1);
    let mut used_in_self = vec![false; f1.factors.len()];

    for f_other in &f2.factors {
        let mut found = false;
        for (i, f_self) in f1.factors.iter().enumerate() {
            if !used_in_self[i] {
                if let Some(q) = f_self.exact_div(f_other) {
                    let qs = format!("{}", q);
                    if qs == "1" || qs == "-1" {
                        used_in_self[i] = true; found = true;
                        if qs == "-1" { self_mult = self_mult.mul(&Poly::new(-1)); }
                        break;
                    }
                } else if let Some(q) = f_other.exact_div(f_self) {
                    if format!("{}", q) == "-1" {
                        used_in_self[i] = true; found = true;
                        other_mult = other_mult.mul(&Poly::new(-1));
                        break;
                    }
                }
            }
        }
        if !found {
            lcm_factors.push(f_other.clone());
            self_mult = self_mult.mul(f_other);
        }
    }
    for (i, f_self) in f1.factors.iter().enumerate() {
        if !used_in_self[i] { other_mult = other_mult.mul(f_self); }
    }
    (FactoredPoly { factors: lcm_factors }, self_mult, other_mult)
}

/// An algebraic SymbolicFraction: numerator / denominator, both in factored form.
#[derive(Clone, Debug)]
pub struct SymbolicFraction {
    num: FactoredPoly,
    den: FactoredPoly,
}

impl SymbolicFraction {
    fn new(val: i64) -> Self {
        SymbolicFraction { num: FactoredPoly::new(Poly::new(val)), den: FactoredPoly::new(Poly::new(1)) }
    }

    fn sym(name: &str) -> Self {
        SymbolicFraction { num: FactoredPoly::new(Poly::sym(name)), den: FactoredPoly::new(Poly::new(1)) }
    }

    fn is_zero(&self) -> bool { self.num.factors.iter().any(|f| f.is_zero()) }

    fn mul(&self, other: &SymbolicFraction) -> Self {
        if self.is_zero() || other.is_zero() { return SymbolicFraction::new(0); }
        let mut res = SymbolicFraction { num: self.num.mul(&other.num), den: self.den.mul(&other.den) };
        res.simplify();
        res
    }

    fn div(&self, other: &SymbolicFraction) -> Self {
        if self.is_zero() { return SymbolicFraction::new(0); }
        let mut res = SymbolicFraction { num: self.num.mul(&other.den), den: self.den.mul(&other.num) };
        res.simplify();
        res
    }

    fn add(&self, other: &SymbolicFraction) -> Self {
        if self.is_zero() { return other.clone(); }
        if other.is_zero() { return self.clone(); }

        let (lcm_den, self_mult, other_mult) = lcm_factored(&self.den, &other.den);
        let new_num = self.num.to_poly().mul(&self_mult).add(&other.num.to_poly().mul(&other_mult));

        let mut res = SymbolicFraction { num: FactoredPoly::new(new_num), den: lcm_den };
        res.simplify();
        res
    }

    fn sub(&self, other: &SymbolicFraction) -> Self {
        let neg_other = SymbolicFraction {
            num: other.num.mul(&FactoredPoly::new(Poly::new(-1))),
            den: other.den.clone(),
        };
        self.add(&neg_other)
    }

    fn simplify(&mut self) {
        if self.is_zero() {
            self.num = FactoredPoly::new(Poly::new(0));
            self.den = FactoredPoly::new(Poly::new(1));
            return;
        }

        let mut d = 0;
        while d < self.den.factors.len() {
            let den_factor = &self.den.factors[d];
            let mut canceled = false;
            for n in 0..self.num.factors.len() {
                if let Some(quotient) = self.num.factors[n].exact_div(den_factor) {
                    self.num.factors[n] = quotient;
                    self.den.factors.remove(d);
                    canceled = true; break;
                }
            }
            if !canceled { d += 1; }
        }

        let mut sign = 1;
        self.num.factors.retain(|f| {
            let s = format!("{}", f);
            if s == "1" { false } else if s == "-1" { sign *= -1; false } else { true }
        });
        if self.num.factors.is_empty() { self.num.factors.push(Poly::new(1)); }
        if sign == -1 { self.num.factors[0] = self.num.factors[0].mul(&Poly::new(-1)); }

        let mut den_sign = 1;
        self.den.factors.retain(|f| {
            let s = format!("{}", f);
            if s == "1" { false } else if s == "-1" { den_sign *= -1; false } else { true }
        });
        if self.den.factors.is_empty() { self.den.factors.push(Poly::new(1)); }
        if den_sign == -1 {
            self.num.factors[0] = self.num.factors[0].mul(&Poly::new(-1));
            self.den.factors[0] = self.den.factors[0].mul(&Poly::new(-1));
        }

        for f in &mut self.den.factors {
            if let Some((_, coef)) = f.leading_term() {
                if coef < 0 {
                    *f = f.mul(&Poly::new(-1));
                    self.num.factors[0] = self.num.factors[0].mul(&Poly::new(-1));
                }
            }
        }
    }
}

impl fmt::Display for SymbolicFraction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_zero() { return write!(f, "0"); }
        let n = format!("{}", self.num);
        let d = format!("{}", self.den);
        if d == "1" { write!(f, "{}", n) } else { write!(f, "{} / {}", n, d) }
    }
}

/// Gaussian elimination on a symbolic matrix to solve Ax = b.
fn solve_system(mut matrix: Vec<Vec<SymbolicFraction>>, mut rhs: Vec<SymbolicFraction>) -> Vec<SymbolicFraction> {
    let n = matrix.len();
    for i in 0..n {
        let mut pivot = i;
        while pivot < n && matrix[pivot][i].is_zero() { pivot += 1; }
        if pivot == n { continue; }

        matrix.swap(i, pivot);
        rhs.swap(i, pivot);

        let pivot_val = matrix[i][i].clone();
        for j in 0..n { matrix[i][j] = matrix[i][j].div(&pivot_val); }
        rhs[i] = rhs[i].div(&pivot_val);

        for j in 0..n {
            if i != j {
                let factor = matrix[j][i].clone();
                if factor.is_zero() { continue; }
                for k in 0..n {
                    let sub_val = factor.mul(&matrix[i][k]);
                    matrix[j][k] = matrix[j][k].sub(&sub_val);
                }
                let sub_rhs = factor.mul(&rhs[i]);
                rhs[j] = rhs[j].sub(&sub_rhs);
            }
        }
    }
    rhs
}

// ============================================================================
// Equation System Types
// ============================================================================

#[derive(Debug, Clone)]
pub struct SymbolicProbability {
    pub numerator: usize,        // the fired transition
    pub denominator: Vec<usize>, // all enabled transitions at the source state
}

impl fmt::Display for SymbolicProbability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let den_str: Vec<String> = self.denominator.iter().map(|d| d.to_string()).collect();
        write!(f, "(t{} / <{}>)", self.numerator, den_str.join(","))
    }
}

/// A single monomial: integer_coefficient * prod(SymbolicProbability)
/// e.g. 2 * SP{1, <1,2>} * SP{3, <3,4>}
#[derive(Debug, Clone)]
pub struct SymbolicMonomial {
    pub coefficient: i64,
    pub factors: Vec<SymbolicProbability>,
}

impl fmt::Display for SymbolicMonomial {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.factors.is_empty() {
            return write!(f, "{}", self.coefficient);
        }
        if self.coefficient == 1 {
            // skip "1 * "
        } else if self.coefficient == -1 {
            write!(f, "-")?;
        } else {
            write!(f, "{} * ", self.coefficient)?;
        }
        for (i, sp) in self.factors.iter().enumerate() {
            if i > 0 { write!(f, " * ")?; }
            write!(f, "{}", sp)?;
        }
        Ok(())
    }
}

/// A polynomial in SymbolicProbability: sum of SymbolicMonomials
#[derive(Debug, Clone)]
pub struct SymbolicPolynomial {
    pub terms: Vec<SymbolicMonomial>,
}

impl fmt::Display for SymbolicPolynomial {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.terms.is_empty() { return write!(f, "0"); }
        for (i, m) in self.terms.iter().enumerate() {
            if i > 0 {
                if m.coefficient < 0 {
                    write!(f, " - ")?;
                    // Print absolute value version
                    let abs_m = SymbolicMonomial {
                        coefficient: -m.coefficient,
                        factors: m.factors.clone(),
                    };
                    write!(f, "{}", abs_m)?;
                } else {
                    write!(f, " + {}", m)?;
                }
            } else {
                write!(f, "{}", m)?;
            }
        }
        Ok(())
    }
}

/// The final solved result: numerator_polynomial / denominator_polynomial,
/// expressed entirely in terms of SymbolicProbability (no more a_i names).
#[derive(Debug, Clone)]
pub struct SymbolicResult {
    pub numerator: SymbolicPolynomial,
    pub denominator: SymbolicPolynomial,
}

impl fmt::Display for SymbolicResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let d = format!("{}", self.denominator);
        if d == "1" {
            write!(f, "{}", self.numerator)
        } else {
            write!(f, "({}) / ({})", self.numerator, self.denominator)
        }
    }
}

/// Convert a `Poly` (with `a_i` variable names) into a `SymbolicPolynomial`
/// by substituting each `a_i` with its `SymbolicProbability`.
fn poly_to_symbolic(
    poly: &Poly,
    coeff_to_prob: &HashMap<String, SymbolicProbability>,
) -> SymbolicPolynomial {
    if poly.is_zero() {
        return SymbolicPolynomial { terms: vec![] };
    }
    let mut result = Vec::new();
    let mut keys: Vec<_> = poly.terms.keys().collect();
    keys.sort_by(|a, b| if a.len() != b.len() { a.len().cmp(&b.len()) } else { a.cmp(b) });

    for vars in keys {
        let coef = poly.terms[vars];
        let factors: Vec<SymbolicProbability> = vars
            .iter()
            .map(|name| {
                coeff_to_prob
                    .get(name)
                    .cloned()
                    .expect(&format!("Unknown coefficient: {}", name))
            })
            .collect();
        result.push(SymbolicMonomial {
            coefficient: coef,
            factors,
        });
    }
    SymbolicPolynomial { terms: result }
}

/// Convert a solved `SymbolicFraction` into a `SymbolicResult`.
fn symbolic_fraction_to_symbolic(
    frac: &SymbolicFraction,
    coeff_to_prob: &HashMap<String, SymbolicProbability>,
) -> SymbolicResult {
    let num_poly = frac.num.to_poly();
    let den_poly = frac.den.to_poly();
    SymbolicResult {
        numerator: poly_to_symbolic(&num_poly, coeff_to_prob),
        denominator: poly_to_symbolic(&den_poly, coeff_to_prob),
    }
}

/// coefficient * variable, where coefficient is a symbolic name like "a_0"
#[derive(Debug, Clone)]
pub struct Term {
    pub coefficient: String,
    pub variable: usize,
}

/// x_lhs = sum(terms)          when is_final == false
/// x_lhs = 1                   when is_final == true
/// x_lhs = 0                   when is_final == false and terms is empty (dead end)
#[derive(Debug, Clone)]
pub struct LinearEquation {
    pub lhs: usize,
    pub terms: Vec<Term>,
    pub is_final: bool,
}

#[derive(Debug, Clone)]
pub struct LinearSystem {
    pub variables: Vec<String>,
    pub equations: Vec<LinearEquation>,
    pub initial_variable: usize,
    /// Maps each symbolic coefficient name (e.g. "a_0") to its transition semantics
    pub coeff_to_prob: HashMap<String, SymbolicProbability>,
}

impl fmt::Display for LinearSystem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, eq) in self.equations.iter().enumerate() {
            if i > 0 { write!(f, "\n")?; }
            let lhs_name = &self.variables[eq.lhs];

            if eq.is_final {
                write!(f, "{} = 1", lhs_name)?;
            } else if eq.terms.is_empty() {
                write!(f, "{} = 0", lhs_name)?;
            } else {
                write!(f, "{} = ", lhs_name)?;
                for (j, term) in eq.terms.iter().enumerate() {
                    if j > 0 { write!(f, " + ")?; }
                    write!(f, "{} * {}", term.coefficient, &self.variables[term.variable])?;
                }
            }
        }
        Ok(())
    }
}

impl LinearSystem {
    /// Returns true if the initial variable is guaranteed to be 0 regardless of
    /// weight values — i.e. no path from the initial state can reach a final state.
    ///
    /// This is detected via backward reachability: starting from all final states
    /// (x_i = 1), propagate backwards through the equation graph. If the initial
    /// variable is never reached, the trace is structurally impossible.
    ///
    /// Example that returns true:
    ///   x_0 = a_0*x_1 + a_1*x_2,  x_1 = a_3*x_4,  x_2 = 0,  x_4 = 0
    /// All paths from x_0 lead to dead ends only.
    pub fn is_structurally_zero(&self) -> bool {
        let n = self.variables.len();

        // Build reverse adjacency: if x_lhs depends on x_target, then
        // reverse[target] contains lhs.
        let mut reverse: Vec<Vec<usize>> = vec![vec![]; n];
        for eq in &self.equations {
            for term in &eq.terms {
                reverse[term.variable].push(eq.lhs);
            }
        }

        // BFS backward from all final states
        let mut reachable = vec![false; n];
        let mut queue = std::collections::VecDeque::new();
        for eq in &self.equations {
            if eq.is_final {
                reachable[eq.lhs] = true;
                queue.push_back(eq.lhs);
            }
        }
        while let Some(v) = queue.pop_front() {
            for &parent in &reverse[v] {
                if !reachable[parent] {
                    reachable[parent] = true;
                    queue.push_back(parent);
                }
            }
        }

        !reachable[self.initial_variable]
    }

    /// Solve the equation system symbolically via Gaussian elimination.
    /// Returns a `SymbolicResult` expressed in terms of `SymbolicProbability`
    /// (transition / enabled_transitions), not abstract `a_i` names.
    pub fn solve(&self) -> SymbolicResult {
        let n = self.variables.len();

        let mut matrix = vec![vec![SymbolicFraction::new(0); n]; n];
        let mut rhs = vec![SymbolicFraction::new(0); n];

        for eq in &self.equations {
            let row = eq.lhs;
            matrix[row][row] = SymbolicFraction::new(1);

            if eq.is_final {
                rhs[row] = SymbolicFraction::new(1);
            } else {
                for term in &eq.terms {
                    let neg_coeff = SymbolicFraction::sym(&term.coefficient)
                        .mul(&SymbolicFraction::new(-1));
                    matrix[row][term.variable] = matrix[row][term.variable].add(&neg_coeff);
                }
            }
        }

        let solution = solve_system(matrix, rhs);
        let frac = &solution[self.initial_variable];
        symbolic_fraction_to_symbolic(frac, &self.coeff_to_prob)
    }
}


pub trait EquationSystemConstructor {
    fn create_equation_system(&self, trace: &Vec<Activity>) -> Result<LinearSystem>;
}

impl EquationSystemConstructor for EbiTraitSemantics {
    fn create_equation_system(&self, trace: &Vec<Activity>) -> Result<LinearSystem> {
        match self {
            EbiTraitSemantics::Usize(_) => todo!(),
            EbiTraitSemantics::Marking(sem) => sem.create_equation_system(trace),
            EbiTraitSemantics::TreeMarking(_) => todo!(),
        }
    }
}

impl<T, State> EquationSystemConstructor for T
where
    T: Semantics<SemState = State> + Send + Sync + ?Sized,
    State: Displayable,
{
    fn create_equation_system(&self, trace: &Vec<Activity>) -> Result<LinearSystem> {
        if let Some(linear_system) = build_equation_system(self, trace) {
            Ok(linear_system)
        } else {
            Err(anyhow!(
                "The synchronous product has no way to terminate: \
                 it is impossible to reach a deadlock."
            ))
        }
    }
}

pub fn build_equation_system<T, State>(
    semantics: &T,
    trace: &Vec<Activity>,
) -> Option<LinearSystem>
where
    T: Semantics<SemState = State> + ?Sized,
    State: Displayable + Eq + std::hash::Hash + Clone,
{
    let initial = semantics.get_initial_state()?;
    let start = (0usize, initial);

    let mut state_to_var: HashMap<(usize, State), usize> = HashMap::new();
    let mut variables: Vec<String> = Vec::new();
    let mut equations: Vec<LinearEquation> = Vec::new();
    let mut queue: VecDeque<(usize, State)> = VecDeque::new();

    let get_or_create_var =
        |state: &(usize, State),
         state_to_var: &mut HashMap<(usize, State), usize>,
         variables: &mut Vec<String>,
         queue: &mut VecDeque<(usize, State)>|
         -> usize {
            if let Some(&idx) = state_to_var.get(state) {
                idx
            } else {
                let idx = variables.len();
                variables.push(format!("x_{}", idx));
                state_to_var.insert(state.clone(), idx);
                queue.push_back(state.clone());
                idx
            }
        };

    let initial_var = get_or_create_var(&start, &mut state_to_var, &mut variables, &mut queue);

    let mut coeff_to_prob: HashMap<String, SymbolicProbability> = HashMap::new();
    let mut coeff_counter: usize = 0;

    while let Some(current) = queue.pop_front() {
        let (trace_index, ref state) = current;
        let lhs = state_to_var[&current];

        if trace_index == trace.len() && semantics.is_final_state(state) {
            equations.push(LinearEquation { lhs, terms: vec![], is_final: true });
            continue;
        }

        let mut terms: Vec<Term> = Vec::new();
        let enabled = semantics.get_enabled_transitions(state);

        for transition in &enabled {
            let coeff_name = format!("a_{}", coeff_counter);
            coeff_to_prob.insert(
                coeff_name.clone(),
                SymbolicProbability {
                    numerator: *transition,
                    denominator: enabled.clone(),
                },
            );
            coeff_counter += 1;

            let mut new_state = state.clone();
            let _ = semantics.execute_transition(&mut new_state, *transition);

            if let Some(activity) = semantics.get_transition_activity(*transition) {
                if trace_index < trace.len() && activity == trace[trace_index] {
                    let successor = (trace_index + 1, new_state);
                    let target = get_or_create_var(
                        &successor, &mut state_to_var, &mut variables, &mut queue,
                    );
                    terms.push(Term { coefficient: coeff_name, variable: target });
                }
            } else {
                let successor = (trace_index, new_state);
                let target = get_or_create_var(
                    &successor, &mut state_to_var, &mut variables, &mut queue,
                );
                terms.push(Term { coefficient: coeff_name, variable: target });
            }
        }

        equations.push(LinearEquation { lhs, terms, is_final: false });
    }

    Some(LinearSystem {
        variables,
        equations,
        initial_variable: initial_var,
        coeff_to_prob,
    })
}