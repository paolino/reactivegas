#!/usr/bin/env bash
# SS-0 v2 frozen runner. ONE execution. Repairs ONLY the cwd/flake-resolution
# transport defect of v1; subject, atom, targets and expectations are unchanged.
# Wall bound 15 min INCLUDING termination overhead: 360+140+140+55 = 695s nominal,
# 4x15s kill-after = 60s worst case, leaving ~145s for nix/process overhead.
set -uo pipefail
R=/tmp/reactivegas/ms2/e-lean-compliance/measure-owner-s3-ss0
I=$R/instruments-v2
W=/code/reactivegas-66-s3-ss0-scratch
E=$R/evidence
mkdir "$E/run-v2" || { echo "REFUSE: $E/run-v2 exists; never overwrites a prior run" >&2; exit 90; }
O=$E/run-v2
exec > >(tee "$O/runner.stdout") 2> >(tee "$O/runner.stderr" >&2)
T0=$(date +%s)
say(){ printf '%s  [+%ss]  %s\n' "$(date -u +%FT%T.%NZ)" "$(( $(date +%s) - T0 ))" "$*"; }
blocker(){ say "BLOCKER: $*"; say "restoring owned scratch source WITHOUT another verification build";
  ( cd "$W" && git checkout -- . && git status --porcelain=v1 > "$O/final-status" );
  say "restored; porcelain lines: $(wc -l < "$O/final-status")"; say "SS-0 v2 STOPPED"; exit 91; }
# every nix invocation runs FROM the scratch dir AND names the flake explicitly
run(){ local secs=$1 script=$2 out=$3
  ( cd "$W" && timeout --signal=TERM --kill-after=15s "${secs}s" \
      nix develop "$W" --quiet --no-write-lock-file -c bash "$script" ) > "$out.stdout" 2> "$out.stderr"; }

say "SS-0 v2 START"
{ echo "runner-invoking-cwd=$(pwd)"; echo "intended-scratch=$W";
  ( cd "$W" && echo "scratch-HEAD=$(git rev-parse HEAD)"; echo "scratch-porcelain=$(git status --porcelain=v1 | wc -l)" )
  echo "lake-present-before=$( [ -d "$W/lean/.lake" ] && echo yes || echo no )"
  echo "oleans-before=$(find "$W" -name '*.olean' 2>/dev/null | wc -l)"
  echo "LEAN_PATH=${LEAN_PATH:-<unset>}"
  for b in nix lake lean; do p=$(command -v $b 2>/dev/null); [ -n "$p" ] && { echo "which-$b=$p"; sha256sum "$(readlink -f "$p")" 2>/dev/null; }; done
  echo "--- env referencing a candidate/repair worktree? ---"; env | grep -iE 'reactivegas-66-(s4b|s3-repair|s3-phase1)' || echo none
} > "$O/identity.txt" 2>&1
grep -q "scratch-HEAD=3590c0015b84fd58004bf6fb44dd18b107304c48" "$O/identity.txt" || blocker "scratch HEAD is not 3590c001"
grep -q "lake-present-before=no" "$O/identity.txt" || blocker ".lake present before OP1; the run would not be cold"

say "OP1 cold baseline (substantive) lake build Reactivegas.Invariants t=360s"
s=$(date +%s%N); run 360 "$I/cold.sh" "$O/op1"; c1=$?; e=$(date +%s%N)
echo "$c1" > "$O/op1.exit"; echo $(( (e-s)/1000000 )) > "$O/op1.ms"
say "OP1 exit=$c1 ms=$(cat "$O/op1.ms") cwd-line=$(grep -m1 ACTUAL-CWD-BEFORE-LAKE "$O/op1.stdout" || echo MISSING)"
[ "$c1" -ne 0 ] && blocker "OP1 cold baseline must be GREEN; exit $c1 is a setup/build failure, not a result"
find "$W/lean/.lake" -name '*.olean' 2>/dev/null | sort > "$O/op1-oleans.txt"; say "OP1 produced $(wc -l < "$O/op1-oleans.txt") oleans"

say "OP2 applying frozen atom"
( cd "$W" && git apply "$I/SS0-atom.diff" ) || blocker "frozen atom failed to apply"
( cd "$W" && git diff > "$O/op2-applied.diff"; git status --porcelain=v1 > "$O/op2-dirty-status" )
say "OP2 source DELIBERATELY DIRTY (recorded, not called clean): $(wc -l < "$O/op2-dirty-status") path(s)"
s=$(date +%s%N); run 140 "$I/mutant.sh" "$O/op2"; c2=$?; e=$(date +%s%N)
echo "$c2" > "$O/op2.exit"; echo $(( (e-s)/1000000 )) > "$O/op2.ms"
say "OP2 exit=$c2 ms=$(cat "$O/op2.ms")"
[ "$c2" -eq 0 ] && blocker "OP2 returned GREEN under the atom; a surprising GREEN is NOT success"
{ [ "$c2" -eq 124 ] || [ "$c2" -eq 137 ]; } && blocker "OP2 timed out; setup failure, not a semantic result"
# ---- outcome classification: a nonzero exit ALONE is not a named semantic RED ----
{ echo "=== did the mutated Reactivegas.Step COMPILE? ==="
  grep -nE "Built Reactivegas\.Step|Replayed Reactivegas\.Step|error:.*Step\.lean" "$O/op2.stdout" "$O/op2.stderr" || echo "NO-STEP-LINE-FOUND"
  echo "=== error at the named obligation Invariants.lean:197 (step_grant_inv)? ==="
  grep -nE "Invariants\.lean:19[0-9]" "$O/op2.stdout" "$O/op2.stderr" || echo "NO-197-LINE-FOUND"
  echo "=== any diagnostic at or beyond :211 (step_deny_inv) — REACH evidence? ==="
  grep -nE "Invariants\.lean:(2[1-9][0-9]|[3-9][0-9]{2}|[0-9]{4,})" "$O/op2.stdout" "$O/op2.stderr" || echo "NO-BEYOND-211-LINE-FOUND"
  echo "=== all Invariants.lean error lines, in order ==="
  grep -oE "Invariants\.lean:[0-9]+" "$O/op2.stdout" "$O/op2.stderr" | sort -u -t: -k2 -n || true
} > "$O/op2-classification.txt" 2>&1
say "OP2 classification written; a nonzero exit alone is NOT a named semantic RED — see op2-classification.txt"

say "OP3 restore + matching build (substantive) t=140s"
( cd "$W" && git checkout -- . && git status --porcelain=v1 > "$O/op3-restored-status" )
[ -s "$O/op3-restored-status" ] && blocker "restore left the scratch dirty"
s=$(date +%s%N); run 140 "$I/restore.sh" "$O/op3"; c3=$?; e=$(date +%s%N)
echo "$c3" > "$O/op3.exit"; echo $(( (e-s)/1000000 )) > "$O/op3.ms"
say "OP3 exit=$c3 ms=$(cat "$O/op3.ms")"
[ "$c3" -ne 0 ] && blocker "OP3 restore build must return to GREEN; exit $c3"

say "OP4 U-CHECK isolated elaboration (targeted) t=55s"
s=$(date +%s%N); run 55 "$I/check.sh" "$O/op4"; c4=$?; e=$(date +%s%N)
echo "$c4" > "$O/op4.exit"; echo $(( (e-s)/1000000 )) > "$O/op4.ms"
say "OP4 ACTUAL exit=$c4 ms=$(cat "$O/op4.ms")"

( cd "$W" && echo "HEAD=$(git rev-parse HEAD)"; git status --porcelain=v1 ) > "$O/final-status"
sha256sum "$O"/* 2>/dev/null > "$E/run-v2.sha256"
say "SS-0 v2 TERMINAL — op1=$c1 op2=$c2 op3=$c3 OP4-ACTUAL=$c4 ; wall=$(( $(date +%s) - T0 ))s"
