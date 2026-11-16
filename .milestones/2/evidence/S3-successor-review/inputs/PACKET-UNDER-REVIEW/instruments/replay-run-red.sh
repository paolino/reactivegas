#!/usr/bin/env bash
# SS-4 frozen U-REPLAY-EXEC-RED: false-value negative control for runtime replay.
# Layer: U-REPLAY-EXEC (negative control, counted separately, proves replay can fail).
# Takes GREEN econ wrapper from replay-run-green.sh, copies to RED path, replaces
# first member key with ZZZ via sed (deterministic, exact argv below), then
# `corpusExport check` must exit 1 with FAIL economic line (view differs).
# This is the required false-value control; a replay without it proves nothing.
set -uo pipefail
SCRATCH=/tmp/reactivegas-ms2-measure-scratch
OUT=/tmp/reactivegas-ms2-measure-output
EXE="$SCRATCH/lean/.lake/build/bin/corpusExport"
ECON_GREEN="$OUT/replay-econ.json"
INT_GREEN="$OUT/replay-int.json"
ECON_RED="$OUT/replay-econ-red.json"
echo "ACTUAL-CWD-AT-COMMAND-BOUNDARY: $(pwd)"
echo "EXE-SHA: $(sha256sum "$EXE")"
cp "$ECON_GREEN" "$ECON_RED"
sed -i 's/"key":"[^"]*"/"key":"ZZZ"/' "$ECON_RED"
echo "RED-ECON-SHA: $(sha256sum "$ECON_RED")"
echo "RED-DIFF-LINES: $(diff "$ECON_GREEN" "$ECON_RED" | wc -l) (must be nonzero)"
[ "$(diff "$ECON_GREEN" "$ECON_RED" | wc -l)" -eq 0 ] && { echo "RED-MUTATION-SILENTLY-FAILED-TO-APPLY (charged)"; exit 90; }
S=$(date +%s%N)
timeout --signal=TERM --kill-after=15s 120s "$EXE" check "$ECON_RED" "$INT_GREEN" > "$OUT/m12-replay-exec-red.stdout" 2> "$OUT/m12-replay-exec-red.stderr"; C=$?
E=$(date +%s%N)
echo "$C" > "$OUT/m12-replay-exec-red.exit"
echo $(( (E-S)/1000000 )) > "$OUT/m12-replay-exec-red.ms"
echo "RED-CHECK-EXIT: $C MS: $(cat "$OUT/m12-replay-exec-red.ms") (must be nonzero)"
cat "$OUT/m12-replay-exec-red.stdout"
cat "$OUT/m12-replay-exec-red.stderr"
[ "$C" -eq 0 ] && { echo "RED-CONTROL-RETURNED-GREEN (charged, replay cannot fail -> vacuous)"; exit 91; }
grep -q "FAIL economic" "$OUT/m12-replay-exec-red.stdout" "$OUT/m12-replay-exec-red.stderr" || { echo "RED-CONTROL-WRONG-REASON (must be FAIL economic: view differs)"; exit 92; }
echo "RED-CONTROL-OK (fails for intended reason)"
