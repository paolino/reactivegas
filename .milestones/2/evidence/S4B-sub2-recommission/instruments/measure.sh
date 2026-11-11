#!/usr/bin/env bash
set -euo pipefail
runtime=/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2
work=/code/reactivegas-66-s4b-audit5
cd "$runtime"
sha256sum -c instruments/MEASUREMENT.sha256
mkdir evidence/M1
exec >evidence/M1/runner.stdout 2>evidence/M1/runner.stderr
finish() {
    local code=$?
    trap - EXIT
    cd "$runtime"
    printf '%s\n' "$code" >evidence/M1/exit
    date -u +%FT%TZ >evidence/M1/finished
    git -C "$work" status --porcelain=v1 >evidence/M1/candidate-final-status
    sha256sum evidence/M1/* >evidence/M1.sha256
    exit "$code"
}
trap finish EXIT
date -u +%FT%TZ >evidence/M1/started
test "$(git -C "$work" rev-parse HEAD)" = 94bb7bb64324a48f7361252556b4d15e45b3923f
test "$(git -C "$work" rev-parse HEAD^{tree})" = 3ee3dc26deff4fde7b7ed9d3f253dd0fbd5efced
test -z "$(git -C "$work" status --porcelain=v1)"
test ! -e "$work/lean/.lake"
test -z "$(find "$work/lean" -name '*.olean' -print -quit)"
(cd "$work" && sha256sum -c "$runtime/instruments/candidate-inputs.sha256") >evidence/M1/input-verification.log
cd "$work"
printf '%s  MEASUREMENT-CHARGE  M1-S substantive=1/12 targeted=0/80; full audit START withheld\n' "$(date -u +%FT%TZ)" >>"$runtime/STATUS.md"
set +e
timeout --signal=TERM --kill-after=30s 1800s nix develop --quiet --no-write-lock-file -c bash "$runtime/instruments/build-command.sh" >"$runtime/evidence/M1/build.stdout" 2>"$runtime/evidence/M1/build.stderr"
code=$?
set -e
printf '%s\n' "$code" >"$runtime/evidence/M1/build.exit"
if test "$code" -ne 0; then exit "$code"; fi
find lean/.lake -type f -name '*.olean' -exec sha256sum {} + >"$runtime/evidence/M1/oleans.sha256"
printf '%s  MEASUREMENT-CHARGE  M1-T substantive=1/12 targeted=1/80; single inventory elaboration\n' "$(date -u +%FT%TZ)" >>"$runtime/STATUS.md"
set +e
timeout --signal=TERM --kill-after=30s 300s nix develop --quiet --no-write-lock-file -c bash "$runtime/instruments/census-command.sh" >"$runtime/evidence/M1/census.stdout" 2>"$runtime/evidence/M1/census.stderr"
code=$?
set -e
printf '%s\n' "$code" >"$runtime/evidence/M1/census.exit"
exit "$code"
