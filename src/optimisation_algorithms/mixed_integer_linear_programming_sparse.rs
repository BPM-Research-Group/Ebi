use sprs::{CsMat, CsVec};
use std::fmt::{Display, Formatter};

use crate::{
    math::{fraction::Fraction, traits::Zero},
    optimisation_algorithms::mixed_integer_linear_programming_helpers::to_dense,
};

#[derive(Clone, Debug, Default)]
pub(crate) struct SparseVec {
    indices: Vec<usize>,
    values: Vec<Fraction>,
}

impl SparseVec {
    pub(crate) fn new() -> SparseVec {
        SparseVec {
            indices: vec![],
            values: vec![],
        }
    }

    pub(crate) fn clear(&mut self) {
        self.indices.clear();
        self.values.clear();
    }

    pub(crate) fn push(&mut self, i: usize, val: Fraction) {
        self.indices.push(i);
        self.values.push(val);
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (usize, &Fraction)> {
        self.indices.iter().copied().zip(&self.values)
    }

    pub(crate) fn sq_norm(&self) -> Fraction {
        self.values.iter().map(|&v| v * v).sum()
    }

    pub(crate) fn into_csvec(self, len: usize) -> CsVec<Fraction> {
        //guaranteed to not panic
        CsVec::new_from_unsorted(len, self.indices, self.values).unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct ScatteredVec {
    pub(crate) values: Vec<Fraction>,
    pub(crate) is_nonzero: Vec<bool>,
    pub(crate) nonzero: Vec<usize>,
}

impl ScatteredVec {
    pub fn empty(n: usize) -> ScatteredVec {
        ScatteredVec {
            values: vec![0.0; n],
            is_nonzero: vec![false; n],
            nonzero: vec![],
        }
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (usize, &Fraction)> {
        self.nonzero.iter().map(move |&i| (i, &self.values[i]))
    }

    pub fn indices(&self) -> &[usize] {
        &self.nonzero
    }

    #[inline]
    pub fn get(&self, i: usize) -> &Fraction {
        &self.values[i]
    }

    #[inline]
    pub fn get_mut(&mut self, i: usize) -> &mut Fraction {
        if !std::mem::replace(&mut self.is_nonzero[i], true) {
            self.nonzero.push(i);
        }
        &mut self.values[i]
    }

    pub fn sq_norm(&self) -> Fraction {
        self.nonzero
            .iter()
            .map(|&i| self.values[i] * self.values[i])
            .sum()
    }

    pub fn clear(&mut self) {
        for &i in &self.nonzero {
            self.values[i] = Fraction::zero();
            self.is_nonzero[i] = false;
        }
        self.nonzero.clear();
    }

    pub fn clear_and_resize(&mut self, n: usize) {
        self.clear();
        self.values.resize(n, Fraction::zero());
        self.is_nonzero.resize(n, false);
    }

    pub fn set<'a, T>(&mut self, rhs: T)
    where
        T: IntoIterator<Item = (usize, &'a Fraction)>,
    {
        self.clear();
        for (i, &val) in rhs {
            self.is_nonzero[i] = true;
            self.nonzero.push(i);
            self.values[i] = val;
        }
    }

    pub(crate) fn to_sparse_vec(&self, lhs: &mut SparseVec) {
        lhs.clear();
        for &idx in &self.nonzero {
            lhs.indices.push(idx);
            lhs.values.push(self.values[idx].clone())
        }
    }

    #[cfg(test)]
    pub(crate) fn to_csvec(&self) -> CsVec<Fraction> {
        let mut indices = vec![];
        let mut data = vec![];
        for &i in &self.nonzero {
            let val = self.values[i];
            if val != 0.0 {
                indices.push(i);
                data.push(val);
            }
        }
        //guaranteed to be sorted, in range and with same size
        CsVec::new_from_unsorted(self.values.len(), indices, data).unwrap()
    }
}

/// Unordered sparse matrix with elements stored by columns
#[derive(Clone, Debug)]
pub struct SparseMat {
    n_rows: usize,
    indptr: Vec<usize>,
    indices: Vec<usize>,
    data: Vec<Fraction>,
}

impl SparseMat {
    pub(crate) fn new(n_rows: usize) -> SparseMat {
        SparseMat {
            n_rows,
            indptr: vec![0],
            indices: vec![],
            data: vec![],
        }
    }

    pub(crate) fn rows(&self) -> usize {
        self.n_rows
    }

    pub(crate) fn cols(&self) -> usize {
        self.indptr.len() - 1
    }

    pub(crate) fn nnz(&self) -> usize {
        self.data.len()
    }

    pub(crate) fn clear_and_resize(&mut self, n_rows: usize) {
        self.data.clear();
        self.indices.clear();
        self.indptr.clear();
        self.indptr.push(0);
        self.n_rows = n_rows;
    }

    pub(crate) fn push(&mut self, row: usize, val: Fraction) {
        self.indices.push(row);
        self.data.push(val);
    }

    pub(crate) fn seal_column(&mut self) {
        self.indptr.push(self.indices.len())
    }

    pub(crate) fn col_rows(&self, i_col: usize) -> &[usize] {
        &self.indices[self.indptr[i_col]..self.indptr[i_col + 1]]
    }

    pub(crate) fn col_rows_mut(&mut self, i_col: usize) -> &mut [usize] {
        &mut self.indices[self.indptr[i_col]..self.indptr[i_col + 1]]
    }

    pub(crate) fn col_data(&self, i_col: usize) -> &[Fraction] {
        &self.data[self.indptr[i_col]..self.indptr[i_col + 1]]
    }

    pub(crate) fn col_iter(&self, i_col: usize) -> impl Iterator<Item = (usize, &Fraction)> {
        self.col_rows(i_col)
            .iter()
            .copied()
            .zip(self.col_data(i_col))
    }

    pub(crate) fn append_col<T>(&mut self, col: T)
    where
        T: IntoIterator<Item = (usize, Fraction)>,
    {
        assert_eq!(*self.indptr.last().unwrap(), self.indices.len()); // prev column is sealed
        for (idx, val) in col {
            self.indices.push(idx);
            self.data.push(val);
        }
        self.seal_column();
    }

    pub(crate) fn into_csmat(self) -> CsMat<Fraction> {
        CsMat::new_csc(
            (self.cols(), self.n_rows),
            self.indptr,
            self.indices,
            self.data,
        )
    }

    pub(crate) fn to_csmat(&self) -> CsMat<Fraction> {
        self.clone().into_csmat()
    }

    pub(crate) fn transpose(&self) -> SparseMat {
        let mut out = SparseMat {
            n_rows: self.cols(),
            indptr: vec![],
            indices: vec![],
            data: vec![],
        };

        // calculate row counts and store them in the indptr array.
        out.indptr.clear();
        out.indptr.resize(self.rows() + 1, 0);
        for c in 0..self.cols() {
            for &r in self.col_rows(c) {
                out.indptr[r] += 1;
            }
        }

        // calculate cumulative counts so that indptr elements point to
        // the *ends* of each resulting row.
        for r in 1..out.indptr.len() {
            out.indptr[r] += out.indptr[r - 1];
        }

        // place the elements
        out.indices.clear();
        out.indices.resize(self.nnz(), 0);
        out.data.clear();
        out.data.resize(self.nnz(), Fraction::zero());
        for c in 0..self.cols() {
            for (r, &val) in self.col_iter(c) {
                out.indptr[r] -= 1;
                out.indices[out.indptr[r]] = c;
                out.data[out.indptr[r]] = val;
            }
        }

        //guaranteed to have at least one element
        *out.indptr.last_mut().unwrap() = self.nnz();

        out
    }
}

#[derive(Clone)]
pub(crate) struct TriangleMat {
    pub(crate) nondiag: SparseMat,
    /// Diag elements, None means all 1's
    pub(crate) diag: Option<Vec<Fraction>>,
}

impl TriangleMat {
    pub(crate) fn rows(&self) -> usize {
        self.nondiag.rows()
    }

    pub(crate) fn cols(&self) -> usize {
        self.nondiag.cols()
    }

    pub(crate) fn transpose(&self) -> TriangleMat {
        TriangleMat {
            nondiag: self.nondiag.transpose(),
            diag: self.diag.clone(),
        }
    }

    #[cfg(test)]
    pub(crate) fn to_csmat(&self) -> CsMat<Fraction> {
        let mut tri_mat = sprs::TriMat::new((self.rows(), self.cols()));
        if let Some(diag) = self.diag.as_ref() {
            for (i, &val) in diag.iter().enumerate() {
                tri_mat.add_triplet(i, i, val);
            }
        } else {
            for i in 0..self.rows() {
                use crate::math::traits::One;

                tri_mat.add_triplet(i, i, Fraction::one());
            }
        }

        for c in 0..self.nondiag.cols() {
            for (r, &val) in self.nondiag.col_iter(c) {
                tri_mat.add_triplet(r, c, val);
            }
        }

        tri_mat.to_csc()
    }
}

impl std::fmt::Debug for TriangleMat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "nondiag:")?;
        for row in self.nondiag.to_csmat().to_csr().outer_iterator() {
            writeln!(f, "{:?}", to_dense(&row))?
        }
        writeln!(f, "diag: {:?}", self.diag)?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Perm {
    pub(crate) orig2new: Vec<usize>,
    pub(crate) new2orig: Vec<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    SingularMatrix,
}
impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let r = match self {
            Error::SingularMatrix => "Singular matrix",
        };
        write!(f, "{}", r)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn mat_transpose() {
        init();
        let mut mat = SparseMat::new(2);
        mat.push(0, Fraction::from((11, 10)));
        mat.push(1, Fraction::from((22, 10)));
        mat.seal_column();
        mat.push(1, Fraction::from((33, 10)));
        mat.seal_column();
        mat.push(0, Fraction::from((44, 10)));
        mat.seal_column();

        let transp = mat.transpose();
        assert_eq!(&transp.indptr, &[0, 2, 4]);
        assert_eq!(&transp.indices, &[2, 0, 1, 0]);
        assert_eq!(
            &transp.data,
            &[
                Fraction::from((44, 10)),
                Fraction::from((11, 10)),
                Fraction::from((33, 10)),
                Fraction::from((22, 10))
            ]
        );
    }
}
