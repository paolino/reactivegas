#!/usr/bin/env bash
# SS-4 frozen U-COLD: initial clean baseline, FIRST before any mutant cycle.
# Layer: U-COLD (full cold `lake build` wall time with retained log).
# Toolchain: Lean 4.25.0 pinned (lean-toolchain `leanprover/lean4:v4.25.0`).
# Counts under actual type U-COLD; timer setup counted separately below, never hidden.
set -uo pipefail
SCRATCH=/tmp/reactivegas-ms2-measure-scratch
OUT=/tmp/reactivegas-ms2-measure-output
echo "ACTUAL-CWD-AT-COMMAND-BOUNDARY: $(pwd)"
echo "SCRATCH-HEAD-BEFORE: $(git -C "$SCRATCH" rev-parse HEAD)"
echo "SCRATCH-PORCELAIN-BEFORE: $(git -C "$SCRATCH" status --porcelain=v1 | wc -l)"
echo "LAKE-DIR-BEFORE: $([ -d "$SCRATCH/lean/.lake" ] && echo present || echo absent)"
echo "OLEANS-BEFORE: $(find "$SCRATCH" -name '*.olean' 2>/dev/null | wc -l)"
echo "LEAN-TOOLCHAIN: $(cat "$SCRATCH/lean/lean-toolchain")"
echo "LAKE-VERSION: $(lake --version)"
echo "LEAN-VERSION: $(lean --version)"
echo "WHICH-LAKE: $(command -v lake)"
echo "WHICH-LEAN: $(command -v lean)"
echo "SHA-LAKE-BIN: $(sha256sum "$(readlink -f "$(command -v lake)")")"
echo "SHA-LEAN-BIN: $(sha256sum "$(readlink -f "$(command -v lean)")")"
echo "LEAN_PATH: ${LEAN_PATH:-<unset>}"
cd "$SCRATCH/lean" || { echo "CD-FAILED"; exit 90; }
echo "ACTUAL-CWD-BEFORE-LAKE: $(pwd)"
S=$(date +%s%N)
timeout --signal=TERM --kill-after=15s 600s lake build > "$OUT/m00-cold.stdout" 2> "$OUT/m00-cold.stderr"; C=$?
E=$(date +%s%N)
echo "$C" > "$OUT/m00-cold.exit"
echo $(( (E-S)/1000000 )) > "$OUT/m00-cold.ms"
echo "COLD-EXIT: $C"
echo "COLD-MS: $(cat "$OUT/m00-cold.ms")"
echo "COLD-LOG-TAIL:"; tail -n 20 "$OUT/m00-cold.stdout"
find "$SCRATCH/lean/.lake" -name '*.olean' 2>/dev/null | sort > "$OUT/m00-cold-oleans.txt"
echo "OLEANS-AFTER: $(wc -l < "$OUT/m00-cold-oleans.txt")"
echo "BUILD-COMPLETED-LINE: $(grep -c 'Build completed successfully' "$OUT/m00-cold.stdout")"
[ "$C" -ne 0 ] && { echo "COLD-BASELINE-MUST-BE-GREEN exit $C (charged, retained, not retried)"; exit 91; }
