"""Tests that need pm4py; skipped when it is not installed."""
import unittest

try:
    from pm4py.objects.log.importer.xes import importer as xes_importer
    from pm4py.objects.log.obj import Event, EventLog, Trace
except ImportError as error:
    raise unittest.SkipTest("pm4py is not installed") from error

from pathlib import Path

import ebi
from support import automaton_of

TESTFILES = Path(__file__).resolve().parents[2] / "testfiles"


def log_of(*traces):
    log = EventLog()
    for activities in traces:
        trace = Trace()
        for activity in activities:
            trace.append(Event({"concept:name": activity}))
        log.append(trace)
    return log


class LogHandles(unittest.TestCase):
    def setUp(self):
        self.pm4py_log = log_of("ab", "ab", "ac", "abc")
        self.log = ebi.Log.from_pm4py(self.pm4py_log)

    def test_a_log_gives_the_same_answers_as_the_pm4py_log(self):
        self.assertEqual(ebi.analyse_variety(self.log), ebi.analyse_variety(self.pm4py_log))

    def test_a_log_is_a_language_of_traces(self):
        self.assertEqual(
            ebi.probability_trace(self.log, ["a", "b"]), ebi.probability_trace(self.pm4py_log, ["a", "b"])
        )

    def test_a_pm4py_log_is_still_accepted_directly(self):
        self.assertEqual(ebi.analyse_variety(self.pm4py_log), ebi.analyse_variety(self.log))

    def test_a_log_is_not_taken_for_a_number(self):
        model = ebi.Model.from_text((TESTFILES / "aa-ab-ba_occ.slpn").read_text())
        with self.assertRaises(ValueError):
            ebi.probability_explain_trace(model, self.log, ["a"])

    def test_something_that_is_not_a_log_is_rejected(self):
        with self.assertRaises(ValueError):
            ebi.Log.from_pm4py("not a log")


def cohort_rows(result):
    """The rows under the header. Rows with equal EMSC have no fixed order,
    so they are sorted."""
    lines = [line.split() for line in result.splitlines() if line.strip()]
    return sorted(lines[1:])


class CohortAnalysis(unittest.TestCase):
    def setUp(self):
        path = TESTFILES / "cohort_test.xes"
        self.text = path.read_text()
        self.log = xes_importer.apply(str(path))
        self.expected = cohort_rows(
            ebi.analyse_cohort_analysis(self.text, 0, 0.05)
        )

    def test_a_pm4py_log_gives_the_same_answer_as_its_xes_text(self):
        result = ebi.analyse_cohort_analysis(self.log, 0, 0.05)
        self.assertEqual(cohort_rows(result), self.expected)

    def test_a_log_handle_gives_the_same_answer_as_its_xes_text(self):
        log = ebi.Log.from_pm4py(self.log)
        result = ebi.analyse_cohort_analysis(log, 0, 0.05)
        self.assertEqual(cohort_rows(result), self.expected)


class DiscoverAlergia(unittest.TestCase):
    def setUp(self):
        path = TESTFILES / "alergia.xes"
        self.text = path.read_text()
        self.log = xes_importer.apply(str(path))

    def test_a_pm4py_log_gives_the_same_automaton_as_its_xes_text(self):
        result = ebi.discover_alergia(self.log, 0.05)
        expected = ebi.discover_alergia(self.text, 0.05)
        self.assertEqual(automaton_of(result), automaton_of(expected))

    def test_a_log_handle_gives_the_same_automaton_as_its_xes_text(self):
        log = ebi.Log.from_pm4py(self.log)
        result = ebi.discover_alergia(log, 0.05)
        expected = ebi.discover_alergia(self.text, 0.05)
        self.assertEqual(automaton_of(result), automaton_of(expected))


if __name__ == "__main__":
    unittest.main()
