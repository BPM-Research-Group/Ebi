import unittest

import ebi_python


class Summarise(unittest.TestCase):
    def test_a_passing_run(self):
        log = "....\n" + "-" * 70 + "\nRan 37 tests in 0.125s\n\nOK\n"
        self.assertEqual(
            ebi_python.summarise(log), ("37 Python tests, all passed", [])
        )

    def test_failures_and_errors_are_named(self):
        log = (
            "FE...\n"
            + "=" * 70
            + "\nERROR: test_b (test_x.C.test_b)\n"
            + "-" * 70
            + "\nTraceback ...\n"
            + "=" * 70
            + "\nFAIL: test_a (test_x.C.test_a)\n"
            + "-" * 70
            + "\nRan 5 tests in 0.01s\n\nFAILED (failures=1, errors=1)\n"
        )
        self.assertEqual(
            ebi_python.summarise(log),
            ("5 Python tests, failures=1, errors=1", ["test_b", "test_a"]),
        )

    def test_a_log_without_a_result_points_to_the_log(self):
        self.assertEqual(
            ebi_python.summarise("error: could not compile\n"), ("see log", [])
        )


if __name__ == "__main__":
    unittest.main()
