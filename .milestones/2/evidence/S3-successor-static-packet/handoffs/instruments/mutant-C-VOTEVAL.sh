#!/usr/bin/env bash
# SS-4 frozen C-VOTEVAL: single-atom mutant build (U-CHAIN) + loading proof.
# Layer: U-CHAIN (incremental production rebuild wall time per module, retained per-module lines).
# Target closure: KelGroups.Vote.Validate -> KelGroups.Vote.Fold -> KelGroups.Vote.Invariants. Exact target: KelGroups.Vote.Invariants (no ... , no presumed full-tree).
# Diff: diffs/C-VOTEVAL.diff (single-atom, well-typed; mutated definition compiles, theorem meant to fail need not).
# Toolchain: Lean 4.25.0 pinned. Counts as U-CHAIN; timer/restore counted separately.
set -uo pipefail
SCRATCH=/tmp/reactivegas-ms2-measure-scratch
OUT=/tmp/reactivegas-ms2-measure-output
DIFF=/tmp/reactivegas/ms2/e-lean-compliance/s3-successor-spec-1/handoffs/instruments/diffs/C-VOTEVAL.diff
TARGET=KelGroups.Vote.Invariants
OUTPREFIX=m-C-VOTEVAL
echo "ACTUAL-CWD-AT-COMMAND-BOUNDARY: $(pwd)"
echo "SCRATCH-HEAD-BEFORE: $(git -C "$SCRATCH" rev-parse HEAD)"
echo "SCRATCH-PORCELAIN-BEFORE: $(git -C "$SCRATCH" status --porcelain=v1 | wc -l)"
echo "DIFF-SHA: $(sha256sum "$DIFF")"
echo "TARGET: $TARGET"
echo "CLOSURE: KelGroups.Vote.Validate -> KelGroups.Vote.Fold -> KelGroups.Vote.Invariants"
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
echo "EXPECTED-OBSERVABLE: nonresponsabile/openQuestion hval REDs (Vote/Invariants.lean:967 nonresponsabile_event_noop: nonadmin opens fresh question in positive franchise view, bypass admits/effects it, identity falsified); cast/renounce arms unaffected (Row63 survive)."
echo "WRONG-REASON-REJECTION: Wrong reason: cast-path failure when mutant touches openQuestion arm only (:58) must be rejected as misattribution."
echo "LOADING-EVIDENCE: Built vs Replayed lines above distinguish fresh vs cached diagnostics; source byte identity alone proves no cached/replayed absence."
echo "RESTORATION: via restore.sh $TARGET $OUTPREFIX (U-RESTORE, counted separately)"
