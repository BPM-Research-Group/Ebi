use crate::{ebi_traits::{ebi_trait_semantics::EbiTraitSemantics, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage}, 
semantics::semantics::Semantics, ebi_framework::displayable::Displayable};
use ebi_objects::{
    ebi_arithmetic::{
        fraction::{approximate::Approximate, fraction_enum::FractionEnum},
        Fraction,
    },
    Activity, ActivityKeyTranslator, HasActivityKey, LabelledPetriNet, StochasticLabelledPetriNet,
};
use ebi_objects::anyhow::{Context, Error, Result, anyhow, ensure};
use rayon::iter::ParallelIterator;
use std::{
    collections::{HashMap, VecDeque},
    fmt,
    sync::{Arc, Mutex},
};

use rayon::prelude::*;
use std::{
    time::{Duration, Instant},
};

const WEIGHT_LOWER_BOUND: f64 = 0.000001;
const WEIGHT_UPPER_BOUND: f64 = 100.0;
const LOG_WEIGHT_SPAN: f64 = 18.420680743952367; // ln(100 / 0.000001)
const OPTIMIZER_TIME_LIMIT: Duration = Duration::from_secs(300);
const MAX_GENERATIONS: usize = 10000;
const STALL_GENERATIONS: usize = 80;
const DIFFERENTIAL_WEIGHT: f64 = 0.8;
const CROSSOVER_PROBABILITY: f64 = 0.9;
const BASIN_HOP_ITERATIONS: usize = 160;
const BASIN_INITIAL_TEMPERATURE: f64 = 0.02;
const BASIN_COOLING_RATE: f64 = 0.985;
const BASIN_ADAPT_INTERVAL: usize = 16;
const BASIN_TARGET_ACCEPTANCE: f64 = 0.5;
const BASIN_STEPWISE_FACTOR: f64 = 0.85;
const BASIN_LOCAL_SWEEPS: usize = 40;
const OBJECTIVE_TOLERANCE: f64 = 1e-10;
const LINEAR_SOLVE_TOLERANCE: f64 = 1e-11;
const FIXED_POINT_TOLERANCE: f64 = 1e-12;
const FIXED_POINT_ITERATION_LIMIT: usize = 20_000;

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

// Equation System Types
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
            &EbiTraitSemantics::AutomatonState(_) | &EbiTraitSemantics::BPMNMarking(_) | &EbiTraitSemantics::FspolangMarking(_) => todo!(),
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

            if let Some(activity) = semantics.get_transition_activity(*transition, state) {
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

// ============================================================================
// Input / Output types
// ============================================================================

/// One trace with its observed log probability and its symbolic equation system.
pub struct TraceInput {
    /// L(σ): the probability of this trace in the event log.
    pub log_probability: f64,
    /// The cross-product equation system whose initial variable gives p̃_M(σ).
    pub system: LinearSystem,
}

/// The solution returned by the optimizer.
pub struct UemscSolution {
    /// The best uEMSC value found: 1 - Σ max(L(σ) - p̃_M(σ), 0).
    pub uemsc_value: f64,
    /// True only when the known upper bound of 1 was reached within tolerance.
    /// Basin hopping alone is not a global-optimality certificate.
    pub global_optimum_certified: bool,
    /// Optimised weight for each transition, indexed by transition index.
    pub weights: Vec<f64>,
    /// Optimised model probability for each trace, in input order.
    pub trace_probabilities: Vec<f64>,
}

// ============================================================================
// Optimizer
// ============================================================================

/// Maximise the unit Earth Mover's Stochastic Conformance:
///
/// `maximise 1 - Σ_{σ ∈ L̄} max(L(σ) - p̃_M(σ), 0)`.
///
/// Decision variables are the transition weights `w_t >= ε`. The model
/// probability of each trace is evaluated directly from its [`LinearSystem`].
/// Differential evolution explores the non-convex weight space, basin hopping
/// crosses between local minima, and coordinate search performs each local
/// minimisation. A fixed seed makes the complete search reproducible.
///
/// # Arguments
///
/// * `traces` - one entry per unique trace: its log probability and cross-product
///   equation system.
/// * `num_transitions` - total number of transitions in the Petri net.
///
/// # Returns
///
/// The best candidate weights and uEMSC value found within the search limit.
/// This derivative-free method does not certify global optimality.
pub fn maximize_uemsc(traces: &[TraceInput], num_transitions: usize) -> Result<UemscSolution> {
    let prepared_traces = prepare_traces(traces, num_transitions)?;

    // If every trace is structurally impossible there is nothing to optimise.
    if prepared_traces.iter().all(|trace| trace.structurally_zero) {
        let constant_penalty = prepared_traces
            .iter()
            .map(|trace| trace.log_probability)
            .sum::<f64>();
        return Ok(UemscSolution {
            uemsc_value: 1.0 - constant_penalty,
            global_optimum_certified: constant_penalty <= OBJECTIVE_TOLERANCE,
            weights: vec![1.0; num_transitions],
            trace_probabilities: vec![0.0; traces.len()],
        });
    }

    if num_transitions == 0 {
        let (deficit_sum, trace_probabilities) = evaluate_weights(&prepared_traces, &[])
            .ok_or_else(|| anyhow!("could not evaluate the transition-free model"))?;
        return Ok(UemscSolution {
            uemsc_value: 1.0 - deficit_sum,
            global_optimum_certified: deficit_sum <= OBJECTIVE_TOLERANCE,
            weights: Vec::new(),
            trace_probabilities,
        });
    }

    let started = Instant::now();
    let population_size = num_transitions.saturating_mul(8).clamp(24, 192);
    let mut rng = DeterministicRng::new(
        0x6a09_e667_f3bc_c909u64
            ^ (num_transitions as u64).rotate_left(17)
            ^ (traces.len() as u64).rotate_left(41),
    );

    // Differential evolution explores log-weights. Candidate zero is the
    // previous implementation's deterministic unit-weight warm start.
    let mut positions = Vec::with_capacity(population_size);
    positions.push(vec![0.0; num_transitions]);
    for _ in 1..population_size {
        let mut position: Vec<f64> = (0..num_transitions)
            .map(|_| rng.next_f64() * LOG_WEIGHT_SPAN)
            .collect();
        canonicalize_position(&mut position);
        positions.push(position);
    }

    let costs: Vec<f64> = positions
        .par_iter()
        .map(|position| candidate_cost(&prepared_traces, position))
        .collect();
    let mut evaluations = population_size;
    let mut population: Vec<Candidate> = positions
        .into_iter()
        .zip(costs)
        .map(|(position, cost)| Candidate { position, cost })
        .collect();
    let mut best = population
        .iter()
        .min_by(|left, right| left.cost.total_cmp(&right.cost))
        .cloned()
        .expect("the optimizer population is non-empty");
    ensure!(
        best.cost.is_finite(),
        "the Section 6 equation systems could not be evaluated"
    );

    let mut stalled_generations = 0usize;
    for _generation in 0..MAX_GENERATIONS {
        if started.elapsed() >= OPTIMIZER_TIME_LIMIT || best.cost <= OBJECTIVE_TOLERANCE {
            break;
        }

        let mut trials = Vec::with_capacity(population_size);
        for individual in 0..population_size {
            let [a, b, c] = distinct_population_indices(&mut rng, population_size, individual);
            let forced_dimension = rng.index(num_transitions);
            let mut trial = Vec::with_capacity(num_transitions);

            for dimension in 0..num_transitions {
                let use_mutant =
                    dimension == forced_dimension || rng.next_f64() < CROSSOVER_PROBABILITY;
                let value = if use_mutant {
                    population[a].position[dimension]
                        + DIFFERENTIAL_WEIGHT
                            * (population[b].position[dimension]
                                - population[c].position[dimension])
                } else {
                    population[individual].position[dimension]
                };
                trial.push(value.clamp(0.0, LOG_WEIGHT_SPAN));
            }
            canonicalize_position(&mut trial);
            trials.push(trial);
        }

        let trial_costs: Vec<f64> = trials
            .par_iter()
            .map(|position| candidate_cost(&prepared_traces, position))
            .collect();
        evaluations += population_size;

        let previous_best = best.cost;
        for ((individual, trial), trial_cost) in population.iter_mut().zip(trials).zip(trial_costs)
        {
            if trial_cost < individual.cost {
                *individual = Candidate {
                    position: trial,
                    cost: trial_cost,
                };
                if individual.cost < best.cost {
                    best = individual.clone();
                }
            }
        }

        if best.cost < previous_best - OBJECTIVE_TOLERANCE {
            stalled_generations = 0;
        } else {
            stalled_generations += 1;
        }
        if stalled_generations >= STALL_GENERATIONS {
            break;
        }
    }

    let basin_result = basin_hopping(&prepared_traces, best, &mut rng, started, num_transitions);
    evaluations += basin_result.evaluations;

    // A final high-accuracy local search polishes the lowest basin found.
    let (best, refinement_evaluations) = refine_coordinates(
        &prepared_traces,
        basin_result.best,
        started,
        num_transitions,
    );
    evaluations += refinement_evaluations;

    let weights = position_to_weights(&best.position);
    let (deficit_sum, trace_probabilities) = evaluate_weights(&prepared_traces, &weights)
        .ok_or_else(|| anyhow!("the best candidate could not be re-evaluated"))?;
    log::info!(
        "uEMSC search: {} evaluations, {} basin hops ({} accepted) in {:.3}s, deficit {:.9}",
        evaluations,
        basin_result.hops,
        basin_result.accepted,
        started.elapsed().as_secs_f64(),
        deficit_sum
    );
    if deficit_sum <= OBJECTIVE_TOLERANCE {
        log::info!("Global optimum certified within tolerance: uEMSC reached its upper bound of 1");
    } else {
        log::warn!(
            "Basin hopping found the best observed uEMSC candidate but cannot certify global optimality"
        );
    }

    Ok(UemscSolution {
        uemsc_value: 1.0 - deficit_sum,
        global_optimum_certified: deficit_sum <= OBJECTIVE_TOLERANCE,
        weights,
        trace_probabilities,
    })
}

// Helpers
#[derive(Clone)]
struct Candidate {
    position: Vec<f64>,
    cost: f64,
}

struct BasinHoppingResult {
    best: Candidate,
    evaluations: usize,
    hops: usize,
    accepted: usize,
}

struct PreparedCoefficient {
    numerator: usize,
    denominator: Vec<usize>,
}

#[derive(Clone)]
struct PreparedTerm {
    coefficient: usize,
    variable: usize,
}

#[derive(Clone)]
enum PreparedEquation {
    Final,
    Dead,
    Linear(Vec<PreparedTerm>),
}

struct PreparedTrace {
    log_probability: f64,
    structurally_zero: bool,
    initial_variable: usize,
    coefficients: Vec<PreparedCoefficient>,
    equations: Vec<PreparedEquation>,
}

fn prepare_traces(traces: &[TraceInput], num_transitions: usize) -> Result<Vec<PreparedTrace>> {
    traces
        .iter()
        .enumerate()
        .map(|(trace_index, trace)| prepare_trace(trace, trace_index, num_transitions))
        .collect()
}

fn prepare_trace(
    trace: &TraceInput,
    trace_index: usize,
    num_transitions: usize,
) -> Result<PreparedTrace> {
    ensure!(
        trace.log_probability.is_finite() && (0.0..=1.0).contains(&trace.log_probability),
        "trace {trace_index} has invalid log probability {}",
        trace.log_probability
    );

    if trace.system.is_structurally_zero() {
        return Ok(PreparedTrace {
            log_probability: trace.log_probability,
            structurally_zero: true,
            initial_variable: 0,
            coefficients: Vec::new(),
            equations: Vec::new(),
        });
    }

    let system = &trace.system;
    ensure!(
        system.initial_variable < system.variables.len(),
        "trace {trace_index} has initial variable {} but only {} variables",
        system.initial_variable,
        system.variables.len()
    );

    let mut coefficient_names: Vec<&str> =
        system.coeff_to_prob.keys().map(String::as_str).collect();
    coefficient_names.sort_unstable();
    let coefficient_indices: HashMap<&str, usize> = coefficient_names
        .iter()
        .enumerate()
        .map(|(index, &name)| (name, index))
        .collect();

    let mut coefficients = Vec::with_capacity(coefficient_names.len());
    for coefficient in coefficient_names {
        let symbolic_probability = &system.coeff_to_prob[coefficient];
        ensure!(
            symbolic_probability.numerator < num_transitions,
            "trace {trace_index} coefficient {coefficient} has invalid fired transition {}",
            symbolic_probability.numerator
        );
        ensure!(
            !symbolic_probability.denominator.is_empty(),
            "trace {trace_index} coefficient {coefficient} has an empty enabled set"
        );
        ensure!(
            symbolic_probability
                .denominator
                .contains(&symbolic_probability.numerator),
            "trace {trace_index} coefficient {coefficient} fires a transition that is not enabled"
        );
        ensure!(
            symbolic_probability
                .denominator
                .iter()
                .all(|&transition| transition < num_transitions),
            "trace {trace_index} coefficient {coefficient} contains an invalid enabled transition"
        );
        let mut enabled = symbolic_probability.denominator.clone();
        enabled.sort_unstable();
        enabled.dedup();
        ensure!(
            enabled.len() == symbolic_probability.denominator.len(),
            "trace {trace_index} coefficient {coefficient} contains a duplicate enabled transition"
        );
        coefficients.push(PreparedCoefficient {
            numerator: symbolic_probability.numerator,
            denominator: symbolic_probability.denominator.clone(),
        });
    }

    let mut equations: Vec<Option<PreparedEquation>> = vec![None; system.variables.len()];
    for equation in &system.equations {
        ensure!(
            equation.lhs < system.variables.len(),
            "trace {trace_index} equation has invalid lhs variable {}",
            equation.lhs
        );
        ensure!(
            equations[equation.lhs].is_none(),
            "trace {trace_index} has multiple equations for variable {}",
            equation.lhs
        );

        let prepared_equation = if equation.is_final {
            PreparedEquation::Final
        } else if equation.terms.is_empty() {
            PreparedEquation::Dead
        } else {
            let terms = equation
                .terms
                .iter()
                .map(|term| {
                    ensure!(
                        term.variable < system.variables.len(),
                        "trace {trace_index} equation {} has invalid target variable {}",
                        equation.lhs,
                        term.variable
                    );
                    let coefficient = coefficient_indices.get(term.coefficient.as_str()).copied();
                    ensure!(
                        coefficient.is_some(),
                        "trace {trace_index} equation {} uses unknown coefficient {}",
                        equation.lhs,
                        term.coefficient
                    );
                    Ok(PreparedTerm {
                        coefficient: coefficient.expect("checked above"),
                        variable: term.variable,
                    })
                })
                .collect::<Result<Vec<_>>>()?;
            PreparedEquation::Linear(terms)
        };
        equations[equation.lhs] = Some(prepared_equation);
    }

    ensure!(
        equations.iter().all(Option::is_some),
        "trace {trace_index} does not define exactly one equation for every variable"
    );

    Ok(PreparedTrace {
        log_probability: trace.log_probability,
        structurally_zero: false,
        initial_variable: system.initial_variable,
        coefficients,
        equations: equations
            .into_iter()
            .map(|equation| equation.expect("checked above"))
            .collect(),
    })
}

fn candidate_cost(traces: &[PreparedTrace], position: &[f64]) -> f64 {
    let weights = position_to_weights(position);
    evaluate_weights(traces, &weights)
        .map(|(deficit, _)| deficit)
        .unwrap_or(f64::INFINITY)
}

fn position_to_weights(position: &[f64]) -> Vec<f64> {
    if position.is_empty() {
        return Vec::new();
    }

    let minimum = position.iter().copied().fold(f64::INFINITY, f64::min);
    let relative: Vec<f64> = position
        .iter()
        .map(|&log_weight| (log_weight - minimum).exp())
        .collect();
    let maximum = relative.iter().copied().fold(1.0, f64::max);
    let scale = (WEIGHT_UPPER_BOUND / maximum).min(1.0);

    relative
        .into_iter()
        .map(|weight| (scale * weight).clamp(WEIGHT_LOWER_BOUND, WEIGHT_UPPER_BOUND))
        .collect()
}

fn evaluate_weights(traces: &[PreparedTrace], weights: &[f64]) -> Option<(f64, Vec<f64>)> {
    if weights
        .iter()
        .any(|weight| !weight.is_finite() || *weight <= 0.0)
    {
        return None;
    }

    let mut deficit_sum = 0.0;
    let mut probabilities = Vec::with_capacity(traces.len());
    for trace in traces {
        let probability = if trace.structurally_zero {
            0.0
        } else {
            solve_trace_probability(trace, weights)?
        };
        deficit_sum += (trace.log_probability - probability).max(0.0);
        probabilities.push(probability);
    }

    deficit_sum
        .is_finite()
        .then_some((deficit_sum, probabilities))
}

fn solve_trace_probability(trace: &PreparedTrace, weights: &[f64]) -> Option<f64> {
    let firing_probabilities: Vec<f64> = trace
        .coefficients
        .iter()
        .map(|coefficient| {
            let denominator = coefficient
                .denominator
                .iter()
                .map(|&transition| weights[transition])
                .sum::<f64>();
            (denominator > 0.0)
                .then_some(weights[coefficient.numerator] / denominator)
                .filter(|probability| probability.is_finite())
        })
        .collect::<Option<_>>()?;

    let variable_count = trace.equations.len();
    let mut matrix = vec![vec![0.0; variable_count]; variable_count];
    let mut rhs = vec![0.0; variable_count];
    for (lhs, equation) in trace.equations.iter().enumerate() {
        matrix[lhs][lhs] = 1.0;
        match equation {
            PreparedEquation::Final => rhs[lhs] = 1.0,
            PreparedEquation::Dead => {}
            PreparedEquation::Linear(terms) => {
                for term in terms {
                    matrix[lhs][term.variable] -= firing_probabilities[term.coefficient];
                }
            }
        }
    }

    let solution = solve_linear_system(matrix, rhs)
        .filter(|values| fixed_point_residual(trace, &firing_probabilities, values) <= 1e-8)
        .or_else(|| least_fixed_point(trace, &firing_probabilities))?;
    let probability = solution[trace.initial_variable];
    (probability.is_finite() && (-1e-8..=1.0 + 1e-8).contains(&probability))
        .then(|| probability.clamp(0.0, 1.0))
}

fn solve_linear_system(mut matrix: Vec<Vec<f64>>, mut rhs: Vec<f64>) -> Option<Vec<f64>> {
    let dimension = rhs.len();
    for column in 0..dimension {
        let pivot = (column..dimension).max_by(|&left, &right| {
            matrix[left][column]
                .abs()
                .total_cmp(&matrix[right][column].abs())
        })?;
        if matrix[pivot][column].abs() <= LINEAR_SOLVE_TOLERANCE {
            return None;
        }
        matrix.swap(column, pivot);
        rhs.swap(column, pivot);

        for row in column + 1..dimension {
            let (previous_rows, current_and_later_rows) = matrix.split_at_mut(row);
            let pivot_row = &previous_rows[column];
            let current_row = &mut current_and_later_rows[0];
            let factor = current_row[column] / pivot_row[column];
            if factor == 0.0 {
                continue;
            }
            current_row[column] = 0.0;
            for (entry, &pivot_entry) in current_row[column + 1..]
                .iter_mut()
                .zip(&pivot_row[column + 1..])
            {
                *entry -= factor * pivot_entry;
            }
            rhs[row] -= factor * rhs[column];
        }
    }

    let mut solution = vec![0.0; dimension];
    for row in (0..dimension).rev() {
        let remainder = (row + 1..dimension)
            .map(|column| matrix[row][column] * solution[column])
            .sum::<f64>();
        solution[row] = (rhs[row] - remainder) / matrix[row][row];
        if !solution[row].is_finite() {
            return None;
        }
    }
    Some(solution)
}

fn least_fixed_point(trace: &PreparedTrace, firing_probabilities: &[f64]) -> Option<Vec<f64>> {
    let mut values = vec![0.0; trace.equations.len()];
    for _ in 0..FIXED_POINT_ITERATION_LIMIT {
        let mut next = vec![0.0; values.len()];
        for (lhs, equation) in trace.equations.iter().enumerate() {
            next[lhs] = match equation {
                PreparedEquation::Final => 1.0,
                PreparedEquation::Dead => 0.0,
                PreparedEquation::Linear(terms) => terms
                    .iter()
                    .map(|term| firing_probabilities[term.coefficient] * values[term.variable])
                    .sum(),
            };
        }
        let change = next
            .iter()
            .zip(&values)
            .map(|(new, old)| (new - old).abs())
            .fold(0.0, f64::max);
        values = next;
        if change <= FIXED_POINT_TOLERANCE {
            return Some(values);
        }
    }

    (fixed_point_residual(trace, firing_probabilities, &values) <= 1e-8).then_some(values)
}

fn fixed_point_residual(
    trace: &PreparedTrace,
    firing_probabilities: &[f64],
    values: &[f64],
) -> f64 {
    trace
        .equations
        .iter()
        .enumerate()
        .map(|(lhs, equation)| {
            let expected = match equation {
                PreparedEquation::Final => 1.0,
                PreparedEquation::Dead => 0.0,
                PreparedEquation::Linear(terms) => terms
                    .iter()
                    .map(|term| firing_probabilities[term.coefficient] * values[term.variable])
                    .sum(),
            };
            (values[lhs] - expected).abs()
        })
        .fold(0.0, f64::max)
}

fn refine_coordinates(
    traces: &[PreparedTrace],
    best: Candidate,
    started: Instant,
    dimensions: usize,
) -> (Candidate, usize) {
    local_minimize(
        traces,
        best,
        started,
        dimensions,
        LOG_WEIGHT_SPAN / 8.0,
        1e-7,
        200,
    )
}

fn basin_hopping(
    traces: &[PreparedTrace],
    initial: Candidate,
    rng: &mut DeterministicRng,
    started: Instant,
    dimensions: usize,
) -> BasinHoppingResult {
    let (mut current, initial_evaluations) = local_minimize(
        traces,
        initial,
        started,
        dimensions,
        LOG_WEIGHT_SPAN / 16.0,
        1e-5,
        80,
    );
    let mut best = current.clone();
    let mut evaluations = initial_evaluations;
    let mut accepted = 0usize;
    let mut accepted_in_window = 0usize;
    let mut hops = 0usize;
    let mut step_size = LOG_WEIGHT_SPAN / 4.0;
    let mut temperature = BASIN_INITIAL_TEMPERATURE;

    for hop in 0..BASIN_HOP_ITERATIONS {
        if started.elapsed() >= OPTIMIZER_TIME_LIMIT || best.cost <= OBJECTIVE_TOLERANCE {
            break;
        }
        hops = hop + 1;

        let forced_dimension = rng.index(dimensions);
        let mut position = current.position.clone();
        for (dimension, coordinate) in position.iter_mut().enumerate() {
            if dimension == forced_dimension || rng.next_f64() < 0.65 {
                let displacement = (2.0 * rng.next_f64() - 1.0) * step_size;
                *coordinate = (*coordinate + displacement).clamp(0.0, LOG_WEIGHT_SPAN);
            }
        }
        canonicalize_position(&mut position);

        let trial = Candidate {
            cost: candidate_cost(traces, &position),
            position,
        };
        evaluations += 1;
        let (trial, local_evaluations) = local_minimize(
            traces,
            trial,
            started,
            dimensions,
            (step_size / 4.0).max(LOG_WEIGHT_SPAN / 512.0),
            1e-5,
            BASIN_LOCAL_SWEEPS,
        );
        evaluations += local_evaluations;

        if trial.cost < best.cost {
            best = trial.clone();
        }

        let increase = trial.cost - current.cost;
        let accept = increase <= 0.0
            || rng.next_f64() < (-increase / temperature.max(f64::MIN_POSITIVE)).exp();
        if accept {
            current = trial;
            accepted += 1;
            accepted_in_window += 1;
        }

        if (hop + 1) % BASIN_ADAPT_INTERVAL == 0 {
            let acceptance_rate = accepted_in_window as f64 / BASIN_ADAPT_INTERVAL as f64;
            if acceptance_rate > BASIN_TARGET_ACCEPTANCE {
                step_size /= BASIN_STEPWISE_FACTOR;
            } else {
                step_size *= BASIN_STEPWISE_FACTOR;
            }
            step_size = step_size.clamp(LOG_WEIGHT_SPAN / 256.0, LOG_WEIGHT_SPAN / 2.0);
            accepted_in_window = 0;
        }
        temperature = (temperature * BASIN_COOLING_RATE).max(1e-8);
    }

    BasinHoppingResult {
        best,
        evaluations,
        hops,
        accepted,
    }
}

fn local_minimize(
    traces: &[PreparedTrace],
    mut best: Candidate,
    started: Instant,
    dimensions: usize,
    initial_step: f64,
    minimum_step: f64,
    maximum_sweeps: usize,
) -> (Candidate, usize) {
    let mut evaluations = 0usize;
    let mut step = initial_step;
    let mut sweeps = 0usize;

    while step > minimum_step && sweeps < maximum_sweeps && started.elapsed() < OPTIMIZER_TIME_LIMIT
    {
        sweeps += 1;
        let mut improved = false;
        for dimension in 0..dimensions {
            for direction in [-1.0, 1.0] {
                let mut position = best.position.clone();
                position[dimension] =
                    (position[dimension] + direction * step).clamp(0.0, LOG_WEIGHT_SPAN);
                canonicalize_position(&mut position);
                if position == best.position {
                    continue;
                }
                let cost = candidate_cost(traces, &position);
                evaluations += 1;
                if cost < best.cost {
                    best = Candidate { position, cost };
                    improved = true;
                }
                if started.elapsed() >= OPTIMIZER_TIME_LIMIT || best.cost <= OBJECTIVE_TOLERANCE {
                    return (best, evaluations);
                }
            }
        }
        if !improved {
            step *= 0.5;
        }
    }
    (best, evaluations)
}

fn canonicalize_position(position: &mut [f64]) {
    let minimum = position.iter().copied().fold(f64::INFINITY, f64::min);
    for coordinate in position {
        *coordinate -= minimum;
    }
}

fn distinct_population_indices(
    rng: &mut DeterministicRng,
    population_size: usize,
    excluded: usize,
) -> [usize; 3] {
    let mut result = [0usize; 3];
    for index in 0..3 {
        loop {
            let candidate = rng.index(population_size);
            if candidate != excluded && !result[..index].contains(&candidate) {
                result[index] = candidate;
                break;
            }
        }
    }
    result
}

struct DeterministicRng {
    state: u64,
}

impl DeterministicRng {
    fn new(seed: u64) -> Self {
        Self { state: seed }
    }

    fn next_u64(&mut self) -> u64 {
        self.state = self.state.wrapping_add(0x9e37_79b9_7f4a_7c15);
        let mut value = self.state;
        value = (value ^ (value >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        value = (value ^ (value >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        value ^ (value >> 31)
    }

    fn next_f64(&mut self) -> f64 {
        ((self.next_u64() >> 11) as f64) * (1.0 / ((1u64 << 53) as f64))
    }

    fn index(&mut self, upper: usize) -> usize {
        (self.next_u64() % upper as u64) as usize
    }
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
        // Step 1: translate activity keys.
        let mut activity_key = self.activity_key().clone();
        let translator = Arc::new(ActivityKeyTranslator::new(
            log.activity_key(),
            &mut activity_key,
        ));
        let error: Arc<Mutex<Option<Error>>> = Arc::new(Mutex::new(None));

        log::info!("Building equation systems for all traces");

        // Step 2: build equation systems and collect probabilities in parallel.
        let trace_data: Vec<(LinearSystem, f64)> = log
            .par_iter_traces_probabilities()
            .filter_map(|(trace, probability)| {
                let translator = Arc::clone(&translator);
                let trace_translated = translator.translate_trace(&trace);
                let result = self
                    .create_equation_system(&trace_translated)
                    .with_context(|| format!("Building equation system for trace {trace:?}"));

                match result {
                    Ok(system) => {
                        let log_probability = probability.clone().approximate().unwrap();
                        Some((system, log_probability))
                    }
                    Err(build_error) => {
                        *Arc::clone(&error).lock().unwrap() = Some(build_error);
                        None
                    }
                }
            })
            .collect();

        // Omitting a failed trace would change Equation (5), so fail explicitly.
        if let Some(build_error) = error.lock().unwrap().take() {
            panic!("Could not build every trace equation system: {build_error:#}");
        }

        // Step 3: build optimizer inputs.
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

        // Step 4: run the self-contained pure-Rust optimisation.
        let solution = maximize_uemsc(&trace_inputs, num_transitions)
            .expect("The pure-Rust search could not evaluate the uEMSC discovery model");

        log::info!("Best uEMSC = {:.6}", solution.uemsc_value);

        // Step 5: convert f64 weights to Fraction and build the SLPN.
        let weights: Vec<Fraction> = solution
            .weights
            .iter()
            .map(|&weight| FractionEnum::Approx(weight))
            .collect();

        (self, weights).into()
    }
}