#!/usr/bin/env bash
# SS-4 frozen U-CHECK: isolated proof/check elaboration, fully-qualified, by decide.
# Layer: U-CHECK (elaboration of witness/check layer, isolated with timer receipt).
# NOT #eval, NOT unqualified, NOT runtime replay. Counts as U-CHECK, never as U-REPLAY.
# Toolchain: Lean 4.25.0 pinned.
set -uo pipefail
SCRATCH=/tmp/reactivegas/ms2-measure-scratch
OUT=/tmp/reactivegas-ms2-measure-output
CHECK_SRC=/tmp/reactivegas/ms2/e-lean-compliance/s3-successor-spec-1/handoffs/instruments/Check.lean
echo "ACTUAL-CWD-AT-COMMAND-BOUNDARY: $(pwd)"
echo "CHECK-SRC-SHA: $(sha256sum "$CHECK_SRC")"
echo "CHECK-SRC-CONTENT-HEAD: $(head -n 5 "$CHECK_SRC")"
grep -q "Reactivegas.checkSweepIdempotent = true := by decide" "$CHECK_SRC" || { echo "CHECK-SRC-NOT-FULLY-QUALIFIED-BY-DECIDE"; exit 90; }
grep -q "#eval" "$CHECK_SRC" && { echo "CHECK-SRC-CONTAINS-EVAL-REJECTED"; exit 90; }
cd "$SCRATCH/lean" || { echo "CD-FAILED"; exit 90; }
echo "ACTUAL-CWD-BEFORE-LEAN: $(pwd)"
echo "LEAN_PATH: ${LEAN_PATH:-<unset>}"
S=$(date +%s%N)
timeout --signal=TERM --kill-after=15s 120s lake env lean "$CHECK_SRC" > "$OUT/m09-check.stdout" 2> "$OUT/m09-check.stderr"; C=$?
E=$(date +%s%N)
echo "$C" > "$OUT/m09-check.exit"
echo $(( (E-S)/1000000 )) > "$OUT/m09-check.ms"
echo "CHECK-EXIT: $C"
echo "CHECK-MS: $(cat "$OUT/m09-check.ms")"
echo "CHECK-STDOUT-BYTES: $(wc -c < "$OUT/m09-check.stdout")"
echo "CHECK-STDERR-BYTES: $(wc -c < "$OUT/m09-check.stderr")"
[ "$C" -ne 0 ] && { echo "U-CHECK-MUST-BE-GREEN exit $C (charged, retained)"; exit 91; }
[ -s "$OUT/m09-check.stdout" ] && { echo "U-CHECK-UNEXPECTED-STDOUT (clean elaboration must be silent beyond ACTUAL-CWD lines)"; exit 92; }
