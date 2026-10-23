import tempfile
import unittest
from fractions import Fraction
from pathlib import Path

import bench_prob_trac as bench

LOG = """<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/">
  <trace>
    <string key="concept:name" value="case 1"/>
    <event><string key="concept:name" value="a"/></event>
    <event><string key="concept:name" value="Send Fine"/></event>
  </trace>
  <trace>
    <event><string key="concept:name" value="b"/></event>
  </trace>
</log>
"""


class ReadTraces(unittest.TestCase):
    def test_the_activity_labels_of_every_trace_are_read_in_order(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "log.xes"
            path.write_text(LOG, encoding="utf-8")
            self.assertEqual(
                bench.read_traces(path), [["a", "Send Fine"], ["b"]]
            )

    def test_the_name_of_the_trace_is_not_an_activity(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "log.xes"
            path.write_text(LOG, encoding="utf-8")
            self.assertNotIn("case 1", sum(bench.read_traces(path), []))


class Variants(unittest.TestCase):
    def test_each_distinct_trace_is_kept_once_in_order_of_appearance(self):
        traces = [["a", "b"], ["a"], ["a", "b"], ["b"], ["a"]]
        self.assertEqual(bench.variants(traces), [["a", "b"], ["a"], ["b"]])

    def test_traces_that_differ_in_order_are_different_variants(self):
        self.assertEqual(
            bench.variants([["a", "b"], ["b", "a"]]), [["a", "b"], ["b", "a"]]
        )


class Spread(unittest.TestCase):
    def test_indices_are_spread_evenly_over_the_log(self):
        self.assertEqual(bench.spread(10, 5), [0, 2, 4, 6, 8])

    def test_a_small_log_is_taken_whole(self):
        self.assertEqual(bench.spread(3, 10), [0, 1, 2])


class CliProbability(unittest.TestCase):
    def test_an_exact_value_is_read_when_the_command_prints_one(self):
        self.assertEqual(
            bench.cli_probability("Approximately 0.6\n3/5\n\n"),
            (0.6, Fraction(3, 5)),
        )

    def test_only_the_approximation_is_read_when_there_is_no_exact_value(self):
        self.assertEqual(
            bench.cli_probability("Approximately 0.6\n\n"), (0.6, None)
        )


class Agree(unittest.TestCase):
    def test_exact_values_must_be_equal(self):
        self.assertTrue(bench.agree((0.6, Fraction(3, 5)), Fraction(3, 5)))
        self.assertFalse(bench.agree((0.6, Fraction(3, 5)), Fraction(1, 2)))

    def test_approximations_are_compared_with_a_tolerance(self):
        self.assertTrue(bench.agree((0.6, None), 0.6000000000001))
        self.assertFalse(bench.agree((0.6, None), 0.61))


class PassesToRun(unittest.TestCase):
    def test_all_the_repeats_run_when_they_fit_in_the_budget(self):
        self.assertEqual(bench.passes_to_run(1.0, 5, 60.0), 5)

    def test_fewer_run_when_the_budget_runs_out(self):
        self.assertEqual(bench.passes_to_run(20.0, 5, 60.0), 3)

    def test_none_run_when_one_pass_alone_uses_the_budget(self):
        self.assertEqual(bench.passes_to_run(100.0, 5, 60.0), 0)

    def test_a_pass_that_takes_no_time_does_not_divide_by_zero(self):
        self.assertEqual(bench.passes_to_run(0.0, 5, 60.0), 5)


class EstimatedTotal(unittest.TestCase):
    def test_the_total_is_the_mean_of_the_sample_times_the_count(self):
        self.assertEqual(bench.estimated_total([1.0, 1.0, 4.0], 10), 20.0)

    def test_a_long_tail_is_not_ignored_the_way_a_median_would_ignore_it(self):
        sample = [0.1] * 9 + [10.0]
        self.assertAlmostEqual(bench.estimated_total(sample, 100), 109.0)


class HowEstimated(unittest.TestCase):
    def test_a_total_over_every_variant_is_measured(self):
        self.assertEqual(
            bench.how_estimated(89, 89), "measured on all 89 variants"
        )

    def test_a_total_over_a_sample_is_an_estimate(self):
        self.assertEqual(
            bench.how_estimated(100, 1511), "estimated from 100 variants"
        )


class ModelNameFromCpuinfo(unittest.TestCase):
    def test_the_first_model_name_is_taken(self):
        text = (
            "processor\t: 0\n"
            "model name\t: AMD EPYC 9474F 48-Core Processor\n"
            "processor\t: 1\n"
            "model name\t: something else\n"
        )
        self.assertEqual(
            bench.model_name_from_cpuinfo(text),
            "AMD EPYC 9474F 48-Core Processor",
        )

    def test_text_without_a_model_name_is_an_error(self):
        with self.assertRaises(ValueError):
            bench.model_name_from_cpuinfo("processor\t: 0\n")


class ParseArguments(unittest.TestCase):
    def test_a_model_and_a_log_are_given(self):
        args = bench.parse_arguments(["m.slpn", "l.xes"])
        self.assertEqual(args.model, Path("m.slpn"))
        self.assertEqual(args.log, Path("l.xes"))
        self.assertFalse(args.pm4py_log)

    def test_a_log_that_is_its_own_model_is_given_once(self):
        args = bench.parse_arguments(["l.xes", "--pm4py-log"])
        self.assertEqual(args.model, Path("l.xes"))
        self.assertEqual(args.log, Path("l.xes"))
        self.assertTrue(args.pm4py_log)


class SpeedupText(unittest.TestCase):
    def test_a_small_ratio_keeps_a_decimal(self):
        self.assertEqual(bench.speedup_text(1.28), "1.3x")
        self.assertEqual(bench.speedup_text(4.4), "4.4x")

    def test_a_large_ratio_is_a_whole_number(self):
        self.assertEqual(bench.speedup_text(88.4), "88x")


class SecondsText(unittest.TestCase):
    def test_a_short_time_is_shown_in_milliseconds(self):
        self.assertEqual(bench.seconds_text(0.0004), "0.4 ms")
        self.assertEqual(bench.seconds_text(0.95), "950.0 ms")

    def test_a_long_time_is_shown_in_seconds(self):
        self.assertEqual(bench.seconds_text(2.34), "2.3 s")


if __name__ == "__main__":
    unittest.main()
