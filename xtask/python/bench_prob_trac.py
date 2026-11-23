#!/usr/bin/env python3
"""Time the probability of every trace variant of a log under a model.

    python xtask/python/bench_prob_trac.py MODEL LOG [--exe PATH]
    python xtask/python/bench_prob_trac.py LOG --pm4py-log [--exe PATH]

A trace variant is a distinct trace. A log repeats traces, and a repeated trace
has the same probability, so each variant is asked once.

The old way, from Python, was to run the command line once per variant. The
new way is a call into the Python package, with the model given as text or as
an ebi.Model that was parsed once. This script times all three on the same
data.

With --pm4py-log the log is its own model, and the two Python ways are a PM4Py
log passed on every call and an ebi.Log converted once (this needs pm4py).

MODEL is a file `ebi prob trac` accepts (for example an .slpn). LOG is an .xes
or .xes.gz log whose variants are queried. The package under test is whichever
`ebi` this interpreter imports, so install the release wheel first; build the
command line with `cargo build --release`. Before anything is timed, the
three ways must agree on the probability of every variant that the command
line is run on.

Standard library only, apart from the ebi package itself.
"""
import argparse
import gzip
import os
import platform
import statistics
import subprocess
import sys
import time
import xml.etree.ElementTree as ET
from fractions import Fraction
from importlib import metadata
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def local_name(tag: str) -> str:
    return tag.rsplit("}", 1)[-1]


def read_traces(path: Path) -> list[list[str]]:
    """The concept:name of every event of every trace, in order."""
    opener = gzip.open if path.suffix == ".gz" else open
    traces = []
    with opener(path, "rb") as f:
        for _, element in ET.iterparse(f, events=("end",)):
            if local_name(element.tag) != "trace":
                continue
            labels = [
                attribute.get("value")
                for event in element
                if local_name(event.tag) == "event"
                for attribute in event
                if attribute.get("key") == "concept:name"
            ]
            traces.append(labels)
            element.clear()
    return traces


def variants(traces: list[list[str]]) -> list[list[str]]:
    """The distinct traces, in order of first appearance."""
    seen = set()
    distinct = []
    for trace in traces:
        key = tuple(trace)
        if key not in seen:
            seen.add(key)
            distinct.append(trace)
    return distinct


def spread(count: int, wanted: int) -> list[int]:
    """`wanted` indices spread evenly over range(count)."""
    if wanted >= count:
        return list(range(count))
    return [i * count // wanted for i in range(wanted)]


def cli_probability(output: str) -> tuple[float, Fraction | None]:
    """The approximation, and the exact value if the command printed one."""
    lines = [line.strip() for line in output.splitlines() if line.strip()]
    approximate = float(lines[0].removeprefix("Approximately "))
    exact = Fraction(lines[1]) if len(lines) > 1 else None
    return approximate, exact


def agree(cli: tuple[float, Fraction | None], value) -> bool:
    """Whether a Python result is the same probability as the CLI's."""
    approximate, exact = cli
    if exact is not None and isinstance(value, Fraction):
        return value == exact
    return abs(float(value) - approximate) <= 1e-9 * max(1.0, abs(approximate))


def timed(call) -> float:
    started = time.perf_counter()
    call()
    return time.perf_counter() - started


def passes_to_run(
    first_pass_seconds: float, repeats: int, budget: float
) -> int:
    """How many more passes fit in the budget, at most `repeats`."""
    if first_pass_seconds <= 0:
        return repeats
    return max(0, min(repeats, int(budget // first_pass_seconds)))


def estimated_total(seconds: list[float], count: int) -> float:
    """The time for `count` variants, from the times of an evenly spread
    sample. It is the mean times the count; a median would ignore the long
    tail that a few slow variants make."""
    return statistics.mean(seconds) * count


def how_estimated(sampled: int, total: int) -> str:
    if sampled >= total:
        return f"measured on all {total} variants"
    return f"estimated from {sampled} variants"


def speedup_text(ratio: float) -> str:
    if ratio < 10:
        return f"{ratio:.1f}x"
    return f"{ratio:.0f}x"


def seconds_text(seconds: float) -> str:
    if seconds < 1:
        return f"{seconds * 1000:.1f} ms"
    return f"{seconds:.1f} s"


def percentile(values: list[float], fraction: float) -> float:
    ordered = sorted(values)
    return ordered[min(len(ordered) - 1, int(fraction * len(ordered)))]


def model_name_from_cpuinfo(text: str) -> str:
    """The first `model name` of the text of /proc/cpuinfo."""
    for line in text.splitlines():
        if line.startswith("model name"):
            return line.partition(":")[2].strip()
    raise ValueError("no model name")


def cpu_name() -> str:
    """The processor's name as the operating system reports it. A virtual
    machine can present a generic family and model to platform.processor()."""
    try:
        if sys.platform == "win32":
            import winreg

            key = winreg.OpenKey(
                winreg.HKEY_LOCAL_MACHINE,
                r"HARDWARE\DESCRIPTION\System\CentralProcessor\0",
            )
            return winreg.QueryValueEx(key, "ProcessorNameString")[0].strip()
        return model_name_from_cpuinfo(Path("/proc/cpuinfo").read_text())
    except (OSError, ValueError):
        return platform.processor() or platform.machine()


def git_commit() -> str:
    try:
        result = subprocess.run(
            ["git", "rev-parse", "--short", "HEAD"],
            cwd=ROOT, capture_output=True, text=True, check=True,
        )
        return result.stdout.strip()
    except (OSError, subprocess.CalledProcessError):
        return "unknown"


def describe_environment(
    args, ebi, traces: list[list[str]], distinct: list[list[str]]
) -> None:
    try:
        version = metadata.version("ebi-pm")
    except metadata.PackageNotFoundError:
        version = "unknown"
    lengths = [len(trace) for trace in distinct]
    print("Environment")
    print(
        f"- machine: {platform.platform()}, {cpu_name()}, "
        f"{os.cpu_count()} logical processors"
    )
    print(f"- python: {platform.python_version()}, ebi-pm {version}")
    print(f"- package: {ebi.__file__}")
    print(f"- command line: {args.exe} ({args.exe.stat().st_size} bytes)")
    print(f"- source: commit {git_commit()}")
    print(f"- model: {args.model.name} ({args.model.stat().st_size} bytes)")
    print(
        f"- log: {args.log.name}, {len(traces)} traces in "
        f"{len(distinct)} variants; {statistics.mean(lengths):.1f} events "
        f"per variant on average, longest {max(lengths)}"
    )
    print()


def default_executable() -> Path:
    name = "ebi.exe" if sys.platform == "win32" else "ebi"
    return ROOT / "target" / "release" / name


def parse_arguments(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("model", type=Path)
    parser.add_argument(
        "log", type=Path, nargs="?",
        help="the log whose variants are queried (default: MODEL itself)",
    )
    parser.add_argument(
        "--pm4py-log", action="store_true",
        help="the model is the log itself, read with PM4Py: time passing the "
        "PM4Py log on every call against an ebi.Log converted once",
    )
    parser.add_argument("--exe", type=Path, default=default_executable())
    parser.add_argument(
        "--cli-sample", type=int, default=100,
        help="variants to run the command line on (default 100)",
    )
    parser.add_argument(
        "--repeats", type=int, default=5,
        help="timed passes over all the variants in Python (default 5)",
    )
    parser.add_argument(
        "--budget", type=float, default=60.0,
        help="seconds to spend on the timed passes of each way; fewer passes "
        "run when one pass is slow (default 60)",
    )
    args = parser.parse_args(argv)
    if args.log is None:
        args.log = args.model
    return args


def main() -> None:
    args = parse_arguments(sys.argv[1:])
    if not args.exe.exists():
        sys.exit(f"{args.exe} is missing; build it with cargo build --release")

    import ebi

    traces = read_traces(args.log)
    if not traces:
        sys.exit(f"{args.log} has no traces")
    distinct = variants(traces)
    if args.pm4py_log:
        from pm4py.objects.log.importer.xes import importer as xes_importer

        pm4py_log = xes_importer.apply(str(args.log))
        ways = [
            ("PM4Py log", pm4py_log),
            ("ebi.Log", ebi.Log.from_pm4py(pm4py_log)),
        ]
    else:
        model_text = args.model.read_text(encoding="utf-8")
        ways = [
            ("text", model_text),
            ("handle", ebi.Model.from_text(model_text)),
        ]
    describe_environment(args, ebi, traces, distinct)

    sample = spread(len(distinct), args.cli_sample)
    command = [str(args.exe), "prob", "trac", str(args.model)]
    for index in sample[:3]:  # warm the file cache; not timed
        subprocess.run(command + distinct[index], capture_output=True)
    cli_seconds = []
    for index in sample:
        started = time.perf_counter()
        run = subprocess.run(
            command + distinct[index], capture_output=True, text=True
        )
        cli_seconds.append(time.perf_counter() - started)
        if run.returncode != 0:
            sys.exit(
                f"the command line failed on variant {index}: {run.stderr}"
            )
        expected = cli_probability(run.stdout)
        for way, model in ways:
            value = ebi.probability_trace(model, distinct[index])
            if not agree(expected, value):
                sys.exit(
                    f"variant {index}: the command line gives {expected} but "
                    f"the Python call with the model as {way} gives {value}"
                )
    print(f"Checked: all three agree on {len(sample)} variants.\n")

    results = {}
    for way, model in ways:
        def one_pass():
            for trace in distinct:
                ebi.probability_trace(model, trace)

        warm_up = timed(one_pass)  # not counted, unless it uses the budget
        more = passes_to_run(warm_up, args.repeats, args.budget)
        results[way] = [timed(one_pass) for _ in range(more)] or [warm_up]

    count = len(distinct)
    estimate = estimated_total(cli_seconds, count)
    print(f"| way | mean per variant | all {count} variants | speed-up |")
    print("|---|---|---|---|")
    print(
        f"| command line, one process per variant "
        f"| {seconds_text(estimate / count)} "
        f"(median {seconds_text(statistics.median(cli_seconds))}, "
        f"90th percentile {seconds_text(percentile(cli_seconds, 0.9))}) "
        f"| {seconds_text(estimate)} ({how_estimated(len(sample), count)}) "
        f"| 1x |"
    )
    for way, _ in ways:
        total = statistics.median(results[way])
        low, high = min(results[way]), max(results[way])
        print(
            f"| Python, model as {way} | {seconds_text(total / count)} "
            f"| {seconds_text(total)} (range {seconds_text(low)} to "
            f"{seconds_text(high)} over {len(results[way])} "
            f"pass{'es' if len(results[way]) != 1 else ''}) "
            f"| {speedup_text(estimate / total)} |"
        )


if __name__ == "__main__":
    main()
