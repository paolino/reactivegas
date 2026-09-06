#!/usr/bin/env bash
# SS-4 frozen U-REPLAY-PROD: executable production, counted separately from execution.
# Layer: U-REPLAY-PROD (executable production with timer receipt).
# Target: corpusExport lean_exe (root Reactivegas.CorpusExport), closure CorpusExport->Trace->Invariants.
# Toolchain: Lean 4.25.0 pinned. Artifact production and execution counted separately; #eval never relabelled as runtime.
set -uo pipefail
SCRATCH=/tmp/reactivegas-ms2-measure-scratch
OUT=/tmp/reactivegas-ms2-measure-output
echo "ACTUAL-CWD-AT-COMMAND-BOUNDARY: $(pwd)"
echo "SCRATCH-HEAD: $(git -C "$SCRATCH" rev-parse HEAD)"
echo "SCRATCH-PORCELAIN: $(git -C "$SCRATCH" status --porcelain=v1 | wc -l)"
echo "LEAN-TOOLCHAIN: $(cat "$SCRATCH/lean/lean-toolchain")"
echo "LAKE-VERSION: $(lake --version)"
echo "LEAN_PATH: ${LEAN_PATH:-<unset>}"
cd "$SCRATCH/lean" || { echo "CD-FAILED"; exit 90; }
echo "ACTUAL-CWD-BEFORE-LAKE: $(pwd)"
S=$(date +%s%N)
timeout --signal=TERM --kill-after=15s 300s lake build corpusExport > "$OUT/m10-replay-prod.stdout" 2> "$OUT/m10-replay-prod.stderr"; C=$?
E=$(date +%s%N)
echo "$C" > "$OUT/m10-replay-prod.exit"
echo $(( (E-S)/1000000 )) > "$OUT/m10-replay-prod.ms"
echo "PROD-EXIT: $C MS: $(cat "$OUT/m10-replay-prod.ms")"
echo "EXE-PATH: $SCRATCH/lean/.lake/build/bin/corpusExport"
echo "EXE-SHA: $(sha256sum "$SCRATCH/lean/.lake/build/bin/corpusExport")"
[ "$C" -ne 0 ] && { echo "REPLAY-PROD-MUST-BE-GREEN (charged, retained)"; exit 91; }
