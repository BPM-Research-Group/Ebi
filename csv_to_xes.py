"""
Converts a Celonis CSV export to XES format for use with Ebi.

CSV columns expected:
  _CASE_KEY   - case identifier
  ACTIVITY_EN - activity name
  EVENTTIME   - event timestamp (ISO 8601, e.g. 2026-06-09T04:47:41)
  ZTERM       - payment term        (promoted to trace attribute)
  ORT01       - billing city        (promoted to trace attribute)
  NAME1       - customer name       (promoted to trace attribute)
  REGIO       - US state            (promoted to trace attribute)
  BUKRS       - company code        (promoted to trace attribute)

Usage:
  python csv_to_xes.py input.csv output.xes
"""

import csv
import sys
from collections import defaultdict
from xml.etree.ElementTree import Element, SubElement, ElementTree, indent

CASE_ID_COL   = "_CASE_KEY"
ACTIVITY_COL  = "ACTIVITY_EN"
TIMESTAMP_COL = "EVENTTIME"

# These columns are treated as trace-level (case) attributes.
# Value is taken from the first event row for each case.
TRACE_ATTR_COLS = ["ZTERM", "ORT01", "NAME1", "REGIO", "BUKRS"]


def convert(input_path: str, output_path: str) -> None:
    # --- read CSV ----------------------------------------------------------
    cases: dict[str, list[dict]] = defaultdict(list)
    with open(input_path, newline="", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        for row in reader:
            case_key = row[CASE_ID_COL].strip()
            if case_key:
                cases[case_key].append(row)

    print(f"Read {sum(len(v) for v in cases.values())} events across {len(cases)} cases.")

    # --- build XES ---------------------------------------------------------
    log_elem = Element("log")
    log_elem.set("xes.version", "1.0")
    log_elem.set("xes.features", "")
    log_elem.set("xmlns", "http://www.xes-standard.org/")

    for ext_name, prefix, uri in [
        ("Concept", "concept", "http://www.xes-standard.org/concept.xesext"),
        ("Time",    "time",    "http://www.xes-standard.org/time.xesext"),
    ]:
        ext = SubElement(log_elem, "extension")
        ext.set("name", ext_name)
        ext.set("prefix", prefix)
        ext.set("uri", uri)

    for case_key, events in cases.items():
        # sort events by timestamp
        events_sorted = sorted(events, key=lambda r: r[TIMESTAMP_COL])

        trace = SubElement(log_elem, "trace")

        # case concept:name
        _str(trace, "concept:name", case_key)

        # trace-level attributes from first event row
        first = events_sorted[0]
        for col in TRACE_ATTR_COLS:
            val = first.get(col, "").strip()
            if val:
                _str(trace, col, val)

        # events
        for row in events_sorted:
            activity = row[ACTIVITY_COL].strip()
            timestamp = row[TIMESTAMP_COL].strip()
            if not activity or not timestamp:
                continue

            event = SubElement(trace, "event")
            _str(event, "concept:name", activity)
            _date(event, "time:timestamp", timestamp)

    # --- write XES ---------------------------------------------------------
    tree = ElementTree(log_elem)
    indent(tree, space="  ")
    tree.write(output_path, encoding="utf-8", xml_declaration=True)
    print(f"Written to {output_path}")


def _str(parent: Element, key: str, value: str) -> None:
    el = SubElement(parent, "string")
    el.set("key", key)
    el.set("value", value)


def _date(parent: Element, key: str, value: str) -> None:
    el = SubElement(parent, "date")
    el.set("key", key)
    el.set("value", value)


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python csv_to_xes.py input.csv output.xes")
        sys.exit(1)
    convert(sys.argv[1], sys.argv[2])
