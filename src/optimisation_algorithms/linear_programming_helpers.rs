use ebi_arithmetic::{f0, ebi_number::Zero, fraction::Fraction};
use sprs::{CsVecBase, CsVecView};
use std::ops::Deref;

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

    // Safety: new indices and data are the same size,indices are still sorted and all indices
    // are less than the new length. Thus, all CsVecView invariants are satisfied.
    unsafe { CsVecView::new_uncheked(len, indices, data) }
    // unsafe { CsVecView::new_view_raw(len, data.len(), indices.as_ptr(), data.as_ptr()) }
}

pub(crate) fn to_dense<IStorage, DStorage>(vec: &CsVecBase<IStorage, DStorage, Fraction>) -> Vec<Fraction>
where
    IStorage: Deref<Target = [usize]>,
    DStorage: Deref<Target = [Fraction]>,
{
    let mut dense = vec![f0!(); vec.dim()];
    vec.scatter(&mut dense);
    dense
}

#[cfg(test)]
use sprs::{CsMat, CsVec};

#[cfg(test)]
pub(crate) fn to_sparse(slice: &[Fraction]) -> CsVec<Fraction> {
    let mut res = CsVec::empty(slice.len());
    for (i, val) in slice.iter().enumerate() {
        use ebi_arithmetic::ebi_number::Zero;

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
