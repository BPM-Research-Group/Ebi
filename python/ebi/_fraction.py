import warnings
from fractions import Fraction


class EbiFraction(Fraction):
    """An exact result of Ebi: a fractions.Fraction.

    Indexing is the form that earlier versions returned, [float approximation, numerator, denominator].
    It is deprecated; use float(x), x.numerator and x.denominator.
    """

    def __getitem__(self, index):
        warnings.warn(
            "indexing an Ebi fraction is deprecated; use float(x), x.numerator and x.denominator",
            DeprecationWarning,
            stacklevel=2,
        )
        return (float(self), self.numerator, self.denominator)[index]
