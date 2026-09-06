#!/usr/bin/env bash
# SS-4 frozen restore: independent restoration + matching rebuild to GREEN.
# Layer: U-RESTORE (replay-after-restore verification, counted separately).
# Used after every mutant cycle; never hidden as preparation.
set -uo pipefail
SCRATCH=/tmp/reactivegas-ms2-measure-scratch
OUT=/tmp/reactivegas-ms2-measure-output
TARGET="$1"
OUTPREFIX="$2"
echo "ACTUAL-CWD-AT-COMMAND-BOUNDARY: $(pwd)"
echo "RESTORE-TARGET: $TARGET"
echo "RESTORE-OUTPREFIX: $OUTPREFIX"
S0=$(date +%s%N)
git -C "$SCRATCH" checkout -- .; C0=$?
E0=$(date +%s%N)
echo "$C0" > "$OUT/${OUTPREFIX}-restore-checkout.exit"
echo $(( (E0-S0)/1000000 )) > "$OUT/${OUTPREFIX}-restore-checkout.ms"
echo "CHECKOUT-EXIT: $C0 MS: $(cat "$OUT/${OUTPREFIX}-restore-checkout.ms")"
echo "PORCELAIN-AFTER-CHECKOUT: $(git -C "$SCRATCH" status --porcelain=v1 | wc -l)"
[ "$C0" -ne 0 ] && { echo "RESTORE-CHECKOUT-FAILED (charged, retained)"; exit 91; }
[ -s "$OUT/${OUTPREFIX}-restore-checkout-porcelain.txt" ] && true
git -C "$SCRATCH" status --porcelain=v1 > "$OUT/${OUTPREFIX}-restore-porcelain.txt"
[ -s "$OUT/${OUTPREFIX}-restore-porcelain.txt" ] && { echo "RESTORE-LEFT-DIRTY (charged)"; cat "$OUT/${OUTPREFIX}-restore-porcelain.txt"; exit 92; }
cd "$SCRATCH/lean" || { echo "CD-FAILED"; exit 90; }
echo "ACTUAL-CWD-BEFORE-LAKE: $(pwd)"
S=$(date +%s%N)
timeout --signal=TERM --kill-after=15s 300s lake build "$TARGET" > "$OUT/${OUTPREFIX}-restore.stdout" 2> "$OUT/${OUTPREFIX}-restore.stderr"; C=$?
E=$(date +%s%N)
echo "$C" > "$OUT/${OUTPREFIX}-restore.exit"
echo $(( (E-S)/1000000 )) > "$OUT/${OUTPREFIX}-restore.ms"
echo "RESTORE-BUILD-EXIT: $C MS: $(cat "$OUT/${OUTPREFIX}-restore.ms")"
[ "$C" -ne 0 ] && { echo "RESTORE-BUILD-MUST-BE-GREEN exit $C (charged, retained)"; exit 93; }
echo "RESTORE-GREEN-OK"
