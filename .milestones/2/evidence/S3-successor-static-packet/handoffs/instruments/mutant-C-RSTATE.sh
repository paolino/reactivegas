#!/usr/bin/env bash
# SS-4 frozen C-RSTATE: single-atom mutant build (U-CHAIN) + loading proof.
# Layer: U-CHAIN (incremental production rebuild wall time per module, retained per-module lines).
# Target closure: Reactivegas.State (primary); conditional higher target Reactivegas.Invariants requires separately authorized isolation. Exact target: Reactivegas.State (no ... , no presumed full-tree).
# Diff: diffs/C-RSTATE.diff (single-atom, well-typed; mutated definition compiles, theorem meant to fail need not).
# Toolchain: Lean 4.25.0 pinned. Counts as U-CHAIN; timer/restore counted separately.
set -uo pipefail
SCRATCH=/tmp/reactivegas-ms2-measure-scratch
OUT=/tmp/reactivegas-ms2-measure-output
DIFF=/tmp/reactivegas/ms2/e-lean-compliance/s3-successor-spec-1/handoffs/instruments/diffs/C-RSTATE.diff
TARGET=Reactivegas.State
OUTPREFIX=m-C-RSTATE
echo "ACTUAL-CWD-AT-COMMAND-BOUNDARY: $(pwd)"
echo "SCRATCH-HEAD-BEFORE: $(git -C "$SCRATCH" rev-parse HEAD)"
echo "SCRATCH-PORCELAIN-BEFORE: $(git -C "$SCRATCH" status --porcelain=v1 | wc -l)"
echo "DIFF-SHA: $(sha256sum "$DIFF")"
echo "TARGET: $TARGET"
echo "CLOSURE: Reactivegas.State (primary); conditional higher target Reactivegas.Invariants requires separately authorized isolation"
echo "LEAN-TOOLCHAIN: $(cat "$SCRATCH/lean/lean-toolchain")"
echo "LAKE-VERSION: $(lake --version)"
echo "LEAN_PATH: ${LEAN_PATH:-<unset>}"
S0=$(date +%s%N)
git -C "$SCRATCH" apply "$DIFF"; C0=$?
E0=$(date +%s%N)
echo "$C0" > "$OUT/${OUTPREFIX}-apply.exit"
echo $(( (E0-S0)/1000000 )) > "$OUT/${OUTPREFIX}-apply.ms"
echo "APPLY-EXIT: $C0 MS: $(cat "$OUT/${OUTPREFIX}-apply.ms")"
[ "$C0" -ne 0 ] && { echo "MUTANT-APPLY-FAILED (charged setup, retained, not retried)"; exit 91; }
git -C "$SCRATCH" diff > "$OUT/${OUTPREFIX}-applied.diff"
git -C "$SCRATCH" status --porcelain=v1 > "$OUT/${OUTPREFIX}-dirty-status"
echo "DIRTY-PATHS: $(wc -l < "$OUT/${OUTPREFIX}-dirty-status") (must be 1)"
cd "$SCRATCH/lean" || { echo "CD-FAILED"; exit 90; }
echo "ACTUAL-CWD-BEFORE-LAKE: $(pwd)"
S=$(date +%s%N)
timeout --signal=TERM --kill-after=15s 300s lake build "$TARGET" > "$OUT/${OUTPREFIX}.stdout" 2> "$OUT/${OUTPREFIX}.stderr"; C=$?
E=$(date +%s%N)
echo "$C" > "$OUT/${OUTPREFIX}.exit"
echo $(( (E-S)/1000000 )) > "$OUT/${OUTPREFIX}.ms"
echo "BUILD-EXIT: $C MS: $(cat "$OUT/${OUTPREFIX}.ms")"
echo "BUILT-LINES: $(grep -c '^ℹ.*Built' "$OUT/${OUTPREFIX}.stdout")"
echo "REPLAYED-LINES: $(grep -c '^ℹ.*Replayed' "$OUT/${OUTPREFIX}.stdout")"
echo "ERROR-LINES: $(grep -c '^error:' "$OUT/${OUTPREFIX}.stdout" "$OUT/${OUTPREFIX}.stderr")"
echo "EXPECTED-OBSERVABLE: PRIMARY: State.refundAll_sum theorem (Reactivegas/State.lean:159) fails in imported producer module FIRST (fold dropped to m). Higher-row deny/fail fund-equation (Invariants.lean:211/322) is CONDITIONAL observation path separately bound, not conflated; single-mutant full build halts at first failing obligation, downstream flips need isolation design."
echo "WRONG-REASON-REJECTION: Wrong reason: deny/fail RED without prior refundAll_sum failure must be rejected as misordered attribution."
echo "LOADING-EVIDENCE: Built vs Replayed lines above distinguish fresh vs cached diagnostics; source byte identity alone proves no cached/replayed absence."
echo "RESTORATION: via restore.sh $TARGET $OUTPREFIX (U-RESTORE, counted separately)"
