"""Tests of the Python API. Build the extension into the ebi-pm environment first: maturin develop"""
import unittest
from collections import deque
from fractions import Fraction
from pathlib import Path

import ebi
from support import automaton_of

TESTFILES = Path(__file__).resolve().parents[2] / "testfiles"


# The functions of the ebi-pm 0.3.14 package on PyPI, as the installed wheel
# lists them. A release must not lose any of them without that being decided.
PUBLISHED = (Path(__file__).parent / "published_api_0_3_14.txt").read_text()


class ExposedFunctions(unittest.TestCase):
    def test_every_function_of_0_3_14_is_still_exposed(self):
        missing = [name for name in PUBLISHED.split() if not hasattr(ebi, name)]
        self.assertEqual(missing, [])


class ConvertStochasticDirectlyFollowsModel(unittest.TestCase):
    def test_converts_a_model_given_as_text(self):
        model = (TESTFILES / "aa-ab-ba.sdfm").read_text()
        result = ebi.convert_stochastic_directly_follows_model(model)
        self.assertTrue(result.startswith("stochastic directly follows model"))


SPACED_LABELS_MODEL = """finite stochastic language
# number of traces
1
# trace 0
# probability
1
# number of events
2
Send Fine
Create Fine
"""


class ProbabilityTrace(unittest.TestCase):
    def setUp(self):
        self.model = (TESTFILES / "aa-ab-ba.slang").read_text()

    def test_probability_of_a_trace_given_as_a_list(self):
        self.assertEqual(ebi.probability_trace(self.model, ["b", "a"]), Fraction(3, 5))

    def test_a_trace_that_is_not_in_the_model_has_probability_zero(self):
        self.assertEqual(ebi.probability_trace(self.model, ["b", "b"]), Fraction(0))

    def test_an_unknown_activity_has_probability_zero(self):
        self.assertEqual(ebi.probability_trace(self.model, ["a", "unknown"]), Fraction(0))

    def test_an_activity_label_may_contain_spaces(self):
        result = ebi.probability_trace(SPACED_LABELS_MODEL, ["Send Fine", "Create Fine"])
        self.assertEqual(result, Fraction(1))

    def test_a_trace_given_as_a_tuple_is_accepted(self):
        self.assertEqual(ebi.probability_trace(self.model, ("b", "a")), Fraction(3, 5))

    def test_a_single_string_is_rejected_instead_of_being_taken_as_one_activity(self):
        with self.assertRaises(ValueError):
            ebi.probability_trace(self.model, "b a")

    def test_any_sequence_of_strings_is_accepted(self):
        trace = deque(["b", "a"])
        self.assertEqual(
            ebi.probability_trace(self.model, trace), Fraction(3, 5)
        )

    def test_a_set_is_rejected_because_it_has_no_order(self):
        with self.assertRaises(ValueError):
            ebi.probability_trace(self.model, {"a", "b"})

    def test_a_generator_is_rejected(self):
        with self.assertRaises(ValueError):
            ebi.probability_trace(self.model, (x for x in ["b", "a"]))

    def test_an_element_that_is_not_a_string_is_rejected(self):
        with self.assertRaises(ValueError):
            ebi.probability_trace(self.model, ["b", 1])


class CohortAnalysis(unittest.TestCase):
    def setUp(self):
        self.log = (TESTFILES / "cohort_test.xes").read_text()

    def rows(self, result):
        """The rows under the header. Rows with equal EMSC have no fixed
        order, so they are sorted."""
        lines = [line.split() for line in result.splitlines() if line.strip()]
        return sorted(lines[1:])

    def test_attribute_values_are_listed_with_their_emsc(self):
        result = ebi.analyse_cohort_analysis(self.log, 0, 0.05)
        self.assertEqual(
            self.rows(result),
            [
                ["flow", "express", "0", "0"],
                ["flow", "standard", "0", "0"],
                ["region", "north", "1", "1"],
                ["region", "south", "1", "1"],
            ],
        )

    def test_the_minimum_size_may_be_a_float_a_fraction_or_a_text(self):
        expected = self.rows(ebi.analyse_cohort_analysis(self.log, 0, 0.05))
        for size in (Fraction(1, 20), "1/20"):
            result = ebi.analyse_cohort_analysis(self.log, 0, size)
            self.assertEqual(self.rows(result), expected)

    def test_text_that_is_not_a_log_is_rejected(self):
        with self.assertRaises(ValueError):
            ebi.analyse_cohort_analysis("not a log", 0, 0.05)


class DiscoverAlergia(unittest.TestCase):
    def setUp(self):
        self.log = (TESTFILES / "alergia.xes").read_text()

    def test_an_automaton_is_learned_from_a_log(self):
        result = ebi.discover_alergia(self.log, 0.05)
        self.assertEqual(
            automaton_of(result),
            (
                0,
                [
                    (0, 0, "0", Fraction(10, 31)),
                    (0, 0, "1", Fraction(6, 31)),
                ],
            ),
        )

    def test_alpha_may_be_a_float_a_fraction_or_a_text(self):
        expected = automaton_of(ebi.discover_alergia(self.log, 0.05))
        for alpha in (Fraction(1, 20), "1/20"):
            result = ebi.discover_alergia(self.log, alpha)
            self.assertEqual(automaton_of(result), expected)

    def test_text_that_is_not_a_log_is_rejected(self):
        with self.assertRaises(ValueError):
            ebi.discover_alergia("not a log", 0.05)


class ModelHandles(unittest.TestCase):
    def setUp(self):
        self.text = (TESTFILES / "aa-ab-ba_occ.slpn").read_text()
        self.model = ebi.Model.from_text(self.text)

    def test_a_model_gives_the_same_answers_as_its_text(self):
        for trace in (["a", "a"], ["b", "a"], ["b", "b"]):
            self.assertEqual(
                ebi.probability_trace(self.model, trace), ebi.probability_trace(self.text, trace)
            )

    def test_a_model_can_be_used_where_the_command_needs_another_kind_of_input(self):
        self.assertEqual(
            ebi.probability_explain_trace(self.model, 0.5, ["a", "a"]),
            ebi.probability_explain_trace(self.text, 0.5, ["a", "a"]),
        )

    def test_a_model_can_be_written_as_text_and_loaded_again(self):
        again = ebi.Model.from_text(self.model.to_text())
        self.assertEqual(
            ebi.probability_trace(again, ["b", "a"]), ebi.probability_trace(self.text, ["b", "a"])
        )

    def test_a_model_knows_what_it_is(self):
        self.assertIn("Petri net", self.model.object_type)

    def test_text_that_is_not_a_model_is_rejected(self):
        with self.assertRaises(ValueError):
            ebi.Model.from_text("this is not a model")

    def test_a_model_is_not_taken_for_a_trace(self):
        with self.assertRaises(ValueError):
            ebi.probability_trace(self.text, self.model)


class FractionResults(unittest.TestCase):
    def setUp(self):
        model = (TESTFILES / "aa-ab-ba.slang").read_text()
        self.result = ebi.probability_trace(model, ["b", "a"])

    def test_a_fraction_is_a_python_fraction(self):
        self.assertIsInstance(self.result, Fraction)
        self.assertEqual((self.result.numerator, self.result.denominator), (3, 5))
        self.assertEqual(float(self.result), 0.6)

    def test_a_negative_fraction_keeps_its_sign_in_every_form(self):
        negative = ebi.EbiFraction(-3, 5)
        self.assertEqual(negative, Fraction(-3, 5))
        self.assertEqual(float(negative), -0.6)
        with self.assertWarns(DeprecationWarning):
            self.assertEqual(list(negative), [-0.6, -3, 5])

    def test_other_fraction_results_are_python_fractions_too(self):
        model = (TESTFILES / "aa-ab-ba_ali.slpn").read_text()
        language = (TESTFILES / "aa.lang").read_text()
        self.assertEqual(ebi.probability_log(model, language), Fraction(0))

    def test_indexing_still_gives_the_old_float_numerator_denominator_but_is_deprecated(self):
        for index, expected in enumerate([0.6, 3, 5]):
            with self.assertWarns(DeprecationWarning):
                self.assertEqual(self.result[index], expected)
        with self.assertWarns(DeprecationWarning):
            self.assertEqual(list(self.result), [0.6, 3, 5])


if __name__ == "__main__":
    unittest.main()
