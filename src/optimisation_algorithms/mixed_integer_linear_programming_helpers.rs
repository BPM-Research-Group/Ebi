use sprs::{CsVecBase, CsVecView};
use std::ops::Deref;

use crate::math::{fraction::Fraction, traits::Zero};

pub(crate) fn resized_view<IStorage, DStorage>(
    vec: &CsVecBase<IStorage, DStorage, Fraction>,
    len: usize,
) -> CsVecView<Fraction>
where
    IStorage: Deref<Target = [usize]>,
    DStorage: Deref<Target = [Fraction]>,
{
    let mut indices = vec.indices();
    let mut data = vec.data();
    while let Some(&i) = indices.last() {
        if i < len {
            // TODO: binary search
            break;
        }

        indices = &indices[..(indices.len() - 1)];
        data = &data[..(data.len() - 1)];
    }

    CsVecView::new(len, indices, data)
}

pub(crate) fn to_dense<IStorage, DStorage>(
    vec: &CsVecBase<IStorage, DStorage, Fraction>,
) -> Vec<Fraction>
where
    IStorage: Deref<Target = [usize]>,
    DStorage: Deref<Target = [Fraction]>,
{
    let mut dense = vec![Fraction::zero(); vec.dim()];
    vec.scatter(&mut dense);
    dense
}

#[cfg(test)]
use sprs::{CsMat, CsVec};

#[cfg(test)]
pub(crate) fn to_sparse(slice: &[Fraction]) -> CsVec<Fraction> {
    let mut res = CsVec::empty(slice.len());
    for (i, val) in slice.iter().enumerate() {
        if !val.is_zero() {
            res.append(i, val.clone());
        }
    }
    res
}

#[cfg(test)]
pub(crate) fn assert_matrix_eq(mat: &CsMat<Fraction>, reference: &[Vec<Fraction>]) {
    let mat = mat.to_csr();
    assert_eq!(mat.rows(), reference.len());
    for (r, row) in mat.outer_iterator().enumerate() {
        assert_eq!(to_dense(&row), reference[r], "matrices differ in row {}", r);
    }
}
