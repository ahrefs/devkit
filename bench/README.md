# Performance benchmarks

The suite covers the performance-sensitive changes on
`sc/perf-work-2026-08`:

- URL encoding with and without `+` substitution;
- UTF-8 HTML encoding through `Web.htmlencode`;
- UTF-8 `Netconversion.convert`;
- UTF-8 `Netconversion.ustring_of_uarray`.

## Compare the branch with its fixed reference

```sh
make bench-compare
```

The default runs 100 measured batches and one warmup batch per benchmark. To
change that or select a benchmark group:

```sh
make bench-compare BENCH_ARGS='--iterations 500 --warmup-iterations 2'
make bench-compare BENCH_ARGS='--iterations 500 --path url.plus_true'
make bench-compare BENCH_ARGS='--path netconversion.convert_utf8.ascii_5000'
```

The comparison uses the exact same benchmark source in the current checkout
and in a temporary worktree at the fixed merge-base commit
`d1e80c727ec9ebbf83df8757af7e358af9a1b7a5`. Dune's cache is disabled for both
builds.

The command prints old/new timing, throughput, allocation and collection
tables. It leaves the raw reports at:

```text
_build/bench-compare/reference.json
_build/bench-compare/current.json
```

To regenerate only the comparison table from existing reports:

```sh
python3 bench/compare.py \
  _build/bench-compare/reference.json \
  _build/bench-compare/current.json
```

## Run only the current checkout

List benchmark names:

```sh
dune exec bench/bench_perf.exe -- --list
```

Create a report:

```sh
dune exec bench/bench_perf.exe -- \
  --revision current \
  --iterations 500 \
  --path html \
  --output /tmp/html-bench.json
```

Each benchmark processes inputs in batches of approximately 64 KiB so short
function calls are not dominated by the measurement loop. Every JSON object
records the batch size, measured batch count and resulting total operation
count.

Timing uses `Unix.gettimeofday` and `Unix.times`. Allocation counters use
`Gc.counters`, converted from words to bytes. `major_allocated_bytes_direct`
is total major allocation minus promoted allocation. Collection counts come
from `Gc.quick_stat` snapshots.
