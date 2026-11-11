#!/usr/bin/env bash
# SS-4 frozen U-REPLAY-EXEC-GREEN: runtime replay of prebuilt artifact, separate from build.
# Layer: U-REPLAY-EXEC (isolated runtime replay with timer receipt, fixed corpus, false-value control separate).
# Invokes prebuilt .lake/build/bin/corpusExport (from replay-build.sh) separately from its build.
# Fixed corpus: seedCorpus/emitIntegratedCorpus via exe write+check (live-bound, nonzero extents).
# NOT #eval during elaboration. Artifact production vs execution counted separately.
set -uo pipefail
SCRATCH=/tmp/reactivegas-ms2-measure-scratch
OUT=/tmp/reactivegas-ms2-measure-output
EXE="$SCRATCH/lean/.lake/build/bin/corpusExport"
ECON="$OUT/replay-econ.json"
INT="$OUT/replay-int.json"
echo "ACTUAL-CWD-AT-COMMAND-BOUNDARY: $(pwd)"
echo "EXE-SHA-BEFORE: $(sha256sum "$EXE")"
echo "EXE-PATH: $EXE"
S=$(date +%s%N)
timeout --signal=TERM --kill-after=15s 120s "$EXE" "$ECON" "$INT" > "$OUT/m11-replay-exec-green-write.stdout" 2> "$OUT/m11-replay-exec-green-write.stderr"; C1=$?
E=$(date +%s%N)
echo "$C1" > "$OUT/m11-replay-exec-green-write.exit"
echo $(( (E-S)/1000000 )) > "$OUT/m11-replay-exec-green-write.ms"
echo "WRITE-EXIT: $C1 MS: $(cat "$OUT/m11-replay-exec-green-write.ms")"
[ "$C1" -ne 0 ] && { echo "REPLAY-WRITE-MUST-BE-GREEN (charged)"; exit 91; }
echo "ECON-SHA: $(sha256sum "$ECON")"
echo "INT-SHA: $(sha256sum "$INT")"
echo "ECON-BYTES: $(wc -c < "$ECON")"
echo "INT-BYTES: $(wc -c < "$INT")"
S2=$(date +%s%N)
timeout --signal=TERM --kill-after=15s 120s "$EXE" check "$ECON" "$INT" > "$OUT/m11-replay-exec-green-check.stdout" 2> "$OUT/m11-replay-exec-green-check.stderr"; C2=$?
E2=$(date +%s%N)
echo "$C2" > "$OUT/m11-replay-exec-green-check.exit"
echo $(( (E2-S2)/1000000 )) > "$OUT/m11-replay-exec-green-check.ms"
echo "CHECK-EXIT: $C2 MS: $(cat "$OUT/m11-replay-exec-green-check.ms")"
cat "$OUT/m11-replay-exec-green-check.stdout"
grep -q "corpus-check: ntraces=" "$OUT/m11-replay-exec-green-check.stdout" || { echo "REPLAY-GREEN-MISSING-LIVE-BOUND-LINE"; exit 92; }
[ "$C2" -ne 0 ] && { echo "REPLAY-CHECK-MUST-BE-GREEN (charged)"; exit 93; }
