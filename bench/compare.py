#!/usr/bin/env python3
"""Compare JSON reports produced by bench_perf.exe."""

import argparse
import json
import math
import sys
from pathlib import Path

GROUPS = [
    ("URL encoding (+)", "url.plus_true."),
    ("URL encoding (%20)", "url.plus_false."),
    ("HTML encoding", "html."),
    ("Netconversion.convert", "netconversion.convert_utf8."),
    ("Netconversion.ustring_of_uarray", "netconversion.ustring_of_uarray."),
]

MATCH_FIELDS = ("input_size", "batch_size", "iterations", "operations")


def load(path: Path):
    with path.open(encoding="utf-8") as handle:
        data = json.load(handle)
    if not isinstance(data, list):
        raise ValueError(f"{path}: expected a top-level JSON array")
    indexed = {}
    for item in data:
        if not isinstance(item, dict) or not isinstance(item.get("name"), str):
            raise ValueError(f"{path}: every entry must be an object with a name")
        name = item["name"]
        if name in indexed:
            raise ValueError(f"{path}: duplicate benchmark {name!r}")
        indexed[name] = item
    return indexed


def format_time(seconds):
    if seconds < 1e-6:
        return f"{seconds * 1e9:.1f} ns"
    if seconds < 1e-3:
        return f"{seconds * 1e6:.1f} us"
    return f"{seconds * 1e3:.1f} ms"


def format_bytes(value):
    if abs(value) < 1024:
        return f"{value:.1f} B"
    if abs(value) < 1024**2:
        return f"{value / 1024:.1f} KiB"
    return f"{value / 1024**2:.1f} MiB"


def per_operation(item, field):
    return float(item[field]) / int(item["operations"])


def speedup(old, new):
    old_time = per_operation(old, "wall_seconds")
    new_time = per_operation(new, "wall_seconds")
    return old_time / new_time


def validate(reference, current):
    old_names = set(reference)
    new_names = set(current)
    if old_names != new_names:
        missing = sorted(old_names - new_names)
        extra = sorted(new_names - old_names)
        details = []
        if missing:
            details.append("missing from current: " + ", ".join(missing))
        if extra:
            details.append("missing from reference: " + ", ".join(extra))
        raise ValueError("benchmark sets differ; " + "; ".join(details))
    for name in sorted(old_names):
        old = reference[name]
        new = current[name]
        for field in MATCH_FIELDS:
            if old.get(field) != new.get(field):
                raise ValueError(
                    f"{name}: {field} differs: {old.get(field)!r} != {new.get(field)!r}"
                )


def print_performance(rows):
    name_width = max(9, max(len(name) for name, _, _ in rows))
    print(
        f"{'benchmark':<{name_width}}  {'reference':>11}  {'current':>11}"
        f"  {'time delta':>10}  {'speedup':>8}  {'old MiB/s':>10}  {'new MiB/s':>10}"
    )
    for name, old, new in rows:
        operations = int(old["operations"])
        old_time = float(old["wall_seconds"]) / operations
        new_time = float(new["wall_seconds"]) / operations
        delta = (new_time / old_time - 1.0) * 100.0
        ratio = old_time / new_time
        input_bytes = int(old["input_size"]) * operations
        old_mibs = input_bytes / float(old["wall_seconds"]) / 1024**2
        new_mibs = input_bytes / float(new["wall_seconds"]) / 1024**2
        print(
            f"{name:<{name_width}}  {format_time(old_time):>11}"
            f"  {format_time(new_time):>11}  {delta:>+9.1f}%"
            f"  {ratio:>7.2f}x  {old_mibs:>10.1f}  {new_mibs:>10.1f}"
        )


def print_allocations(rows):
    name_width = max(9, max(len(name) for name, _, _ in rows))
    print(
        f"{'benchmark':<{name_width}}  {'minor B/op reference -> current':>32}"
        f"  {'minor delta':>11}  {'major B/op reference -> current':>32}"
        f"  {'GC m/M old -> new':>17}"
    )
    for name, old, new in rows:
        minor_old = per_operation(old, "minor_allocated_bytes")
        minor_new = per_operation(new, "minor_allocated_bytes")
        major_old = per_operation(old, "major_allocated_bytes_direct")
        major_new = per_operation(new, "major_allocated_bytes_direct")
        minor_delta = (
            f"{(minor_new / minor_old - 1.0) * 100.0:+.1f}%"
            if minor_old != 0.0
            else "n/a"
        )
        old_gc = f"{old['minor_collections']}/{old['major_collections']}"
        new_gc = f"{new['minor_collections']}/{new['major_collections']}"
        print(
            f"{name:<{name_width}}  "
            f"{format_bytes(minor_old) + ' -> ' + format_bytes(minor_new):>32}  "
            f"{minor_delta:>11}  "
            f"{format_bytes(major_old) + ' -> ' + format_bytes(major_new):>32}  "
            f"{old_gc + ' -> ' + new_gc:>17}"
        )


def geometric_mean(values):
    return math.exp(sum(math.log(value) for value in values) / len(values))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("reference", type=Path)
    parser.add_argument("current", type=Path)
    args = parser.parse_args()

    try:
        reference = load(args.reference)
        current = load(args.current)
        validate(reference, current)
    except (OSError, ValueError, json.JSONDecodeError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 2

    all_ratios = []
    summaries = []
    for title, prefix in GROUPS:
        names = sorted(name for name in reference if name.startswith(prefix))
        if not names:
            continue
        rows = [(name, reference[name], current[name]) for name in names]
        ratios = [speedup(old, new) for _, old, new in rows]
        all_ratios.extend(ratios)
        summaries.append((title, geometric_mean(ratios), len(rows)))
        print(f"\n== {title} ==\n")
        print_performance(rows)
        print("\nAllocations and collections:\n")
        print_allocations(rows)

    print("\n== Summary ==\n")
    print(f"{'group':<38} {'benchmarks':>10} {'geomean speedup':>17}")
    for title, ratio, count in summaries:
        print(f"{title:<38} {count:>10} {ratio:>16.2f}x")
    if all_ratios:
        print(f"{'Overall':<38} {len(all_ratios):>10} {geometric_mean(all_ratios):>16.2f}x")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
