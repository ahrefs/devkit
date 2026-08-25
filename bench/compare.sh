#!/usr/bin/env bash
set -euo pipefail

# Keep the two builds independent; cached artifacts would invalidate the
# old-versus-new comparison.
export DUNE_CACHE=disabled

# This is the merge base of sc/perf-work-2026-08 and master when the
# benchmark suite was added. Keep it fixed so comparisons stay reproducible.
readonly REFERENCE_COMMIT=d1e80c727ec9ebbf83df8757af7e358af9a1b7a5

root=$(git rev-parse --show-toplevel)
current_commit=$(git -C "$root" rev-parse HEAD)
results_dir="$root/_build/bench-compare"
reference_json="$results_dir/reference.json"
current_json="$results_dir/current.json"
worktree=$(mktemp -d "${TMPDIR:-/tmp}/devkit-bench-reference.XXXXXX")
rmdir "$worktree"

cleanup() {
  git -C "$root" worktree remove --force "$worktree" >/dev/null 2>&1 || true
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

git -C "$root" cat-file -e "${REFERENCE_COMMIT}^{commit}"
git -C "$root" worktree add --detach "$worktree" "$REFERENCE_COMMIT" >/dev/null

# The benchmark only uses APIs that exist at the reference commit. Copying the
# exact same source into the worktree avoids benchmarking two different suites.
cp -R "$root/bench" "$worktree/bench"
mkdir -p "$results_dir"

printf 'Building reference %s\n' "$REFERENCE_COMMIT"
dune build --root "$worktree" bench/bench_perf.exe
printf 'Building current   %s\n' "$current_commit"
dune build --root "$root" bench/bench_perf.exe

printf '\nRunning reference benchmarks\n'
"$worktree/_build/default/bench/bench_perf.exe" \
  --revision "$REFERENCE_COMMIT" --output "$reference_json" "$@"

printf '\nRunning current benchmarks\n'
"$root/_build/default/bench/bench_perf.exe" \
  --revision "$current_commit" --output "$current_json" "$@"

printf '\nComparing results\n'
python3 "$root/bench/compare.py" "$reference_json" "$current_json"

printf '\nRaw reports:\n  %s\n  %s\n' "$reference_json" "$current_json"
