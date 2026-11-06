"""Helpers shared by the Python tests."""
import json
from fractions import Fraction


def automaton_of(result):
    """The initial state and the transitions of an SDFA, as comparable
    values, so that the order in which they are written does not matter."""
    automaton = json.loads(result)
    transitions = sorted(
        (t["from"], t["to"], t["label"], Fraction(t["prob"]))
        for t in automaton["transitions"]
    )
    return automaton["initialState"], transitions
