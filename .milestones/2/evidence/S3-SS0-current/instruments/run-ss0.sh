#!/usr/bin/env bash
# SS-0 frozen runner. ONE execution. Four operations, fixed order, fixed timeouts.
# Wall ceiling 15 min; 420+150+150+60 = 780s of command time.
set -uo pipefail
R=/tmp/reactivegas/ms2/e-lean-compliance/measure-owner-s3-ss0
W=/code/reactivegas-66-s3-ss0-scratch
E=$R/evidence
mkdir "$E/run" || { echo "REFUSE: $E/run exists; this runner never overwrites a prior run" >&2; exit 90; }
O=$E/run
exec > >(tee "$O/runner.stdout") 2> >(tee "$O/runner.stderr" >&2)

say(){ printf '%s  %s\n' "$(date -u +%FT%T.%NZ)" "$*"; }
blocker(){ say "BLOCKER: $*"; say "restoring owned scratch source WITHOUT another verification build"; 
  ( cd "$W" && git checkout -- . && git status --porcelain=v1 > "$O/final-status" ); 
  say "restored; porcelain lines: $(wc -l < "$O/final-status")"; exit 91; }

say "SS-0 START"
# --- identity, environment, no candidate-worktree inheritance ---
{ echo "cwd=$W"; ( cd "$W" && git rev-parse HEAD; git status --porcelain=v1 | wc -l ); 
  echo "LEAN_PATH=${LEAN_PATH:-<unset>}"; command -v lake lean nix; 
  for b in lake lean; do p=$(command -v $b 2>/dev/null) && [ -n "$p" ] && sha256sum "$(readlink -f "$p")"; done
  echo "--- any reference to a candidate/repair worktree in env? ---"
  env | grep -iE 'reactivegas-66-(s4b|s3-repair|s3-phase1)' || echo "none"
} > "$O/identity.txt" 2>&1
grep -q "^3590c0015b84fd58004bf6fb44dd18b107304c48$" "$O/identity.txt" || blocker "scratch HEAD is not 3590c001"

# --- OP1 cold baseline (substantive) ---
say "OP1 cold baseline: lake build Reactivegas.Invariants (timeout 420s)"
s=$(date +%s%N)
timeout --signal=TERM --kill-after=20s 420s nix develop --quiet --no-write-lock-file -c bash "$R/instruments/cold.sh" > "$O/op1.stdout" 2> "$O/op1.stderr"; c1=$?
e=$(date +%s%N); echo "$c1" > "$O/op1.exit"; echo $(( (e-s)/1000000 )) > "$O/op1.ms"
say "OP1 exit=$c1 ms=$(cat "$O/op1.ms")"
[ "$c1" -ne 0 ] && blocker "OP1 cold baseline must be GREEN; exit $c1 is an unexpected setup/build failure"
find "$W/lean/.lake" -name '*.olean' | sort > "$O/op1-oleans.txt"; sha256sum $(cat "$O/op1-oleans.txt") > "$O/op1-oleans.sha256" 2>/dev/null

# --- OP2 single-atom mutant (substantive). EXPECTED RED is a RESULT, not a stop. ---
say "OP2 applying frozen atom"
( cd "$W" && git apply "$R/instruments/SS0-atom.diff" ) || blocker "frozen atom failed to apply"
( cd "$W" && git diff > "$O/op2-applied.diff"; git status --porcelain=v1 > "$O/op2-dirty-status" )
say "OP2 mutated source recorded (deliberately dirty, NOT clean): $(wc -l < "$O/op2-dirty-status") path(s)"
s=$(date +%s%N)
timeout --signal=TERM --kill-after=20s 150s nix develop --quiet --no-write-lock-file -c bash "$R/instruments/mutant.sh" > "$O/op2.stdout" 2> "$O/op2.stderr"; c2=$?
e=$(date +%s%N); echo "$c2" > "$O/op2.exit"; echo $(( (e-s)/1000000 )) > "$O/op2.ms"
say "OP2 exit=$c2 ms=$(cat "$O/op2.ms")  (non-zero is the EXPECTED observed result, not a stop)"
if [ "$c2" -eq 0 ]; then blocker "OP2 returned GREEN under the atom; a surprising GREEN is NOT success"; fi
if [ "$c2" -eq 124 ] || [ "$c2" -eq 137 ]; then blocker "OP2 timed out; that is a setup failure, not a semantic result"; fi

# --- OP3 restore + matching build (substantive) ---
say "OP3 restoring and rebuilding"
( cd "$W" && git checkout -- . && git status --porcelain=v1 > "$O/op3-restored-status" )
[ -s "$O/op3-restored-status" ] && blocker "restore left the scratch dirty"
s=$(date +%s%N)
timeout --signal=TERM --kill-after=20s 150s nix develop --quiet --no-write-lock-file -c bash "$R/instruments/restore.sh" > "$O/op3.stdout" 2> "$O/op3.stderr"; c3=$?
e=$(date +%s%N); echo "$c3" > "$O/op3.exit"; echo $(( (e-s)/1000000 )) > "$O/op3.ms"
say "OP3 exit=$c3 ms=$(cat "$O/op3.ms")"
[ "$c3" -ne 0 ] && blocker "OP3 restore build must return to GREEN; exit $c3"

# --- OP4 U-CHECK isolated elaboration (targeted), AFTER the clean restore ---
say "OP4 U-CHECK isolated elaboration of Reactivegas.checkSweepIdempotent (timeout 60s)"
s=$(date +%s%N)
timeout --signal=TERM --kill-after=20s 60s nix develop --quiet --no-write-lock-file -c bash "$R/instruments/check.sh" > "$O/op4.stdout" 2> "$O/op4.stderr"; c4=$?
e=$(date +%s%N); echo "$c4" > "$O/op4.exit"; echo $(( (e-s)/1000000 )) > "$O/op4.ms"
say "OP4 exit=$c4 ms=$(cat "$O/op4.ms")"

( cd "$W" && git rev-parse HEAD; git status --porcelain=v1 ) > "$O/final-status"
sha256sum "$O"/* 2>/dev/null > "$E/run.sha256"
say "SS-0 COMPLETE exits: op1=$c1 op2=$c2 op3=$c3 op4=$c4"
