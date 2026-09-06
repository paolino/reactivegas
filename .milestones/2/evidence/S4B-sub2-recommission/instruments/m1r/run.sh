#!/usr/bin/env bash
set -euo pipefail
runtime=/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2
work=/code/reactivegas-66-s4b-audit5
cd "$runtime"
sha256sum -c instruments/m1r/MANIFEST.sha256
sha256sum -c instruments/MEASUREMENT.sha256
sha256sum -c evidence/M1.sha256
mkdir evidence/M1R
exec >evidence/M1R/runner.stdout 2>evidence/M1R/runner.stderr
finish() {
    local code=$?
    trap - EXIT
    cd "$runtime"
    printf '%s\n' "$code" >evidence/M1R/exit
    date -u +%FT%TZ >evidence/M1R/finished
    git -C "$work" status --porcelain=v1 >evidence/M1R/candidate-final-status
    sha256sum evidence/M1R/* >evidence/M1R.sha256
    exit "$code"
}
trap finish EXIT
date -u +%FT%TZ >evidence/M1R/started
test "$(git -C "$work" rev-parse HEAD)" = 94bb7bb64324a48f7361252556b4d15e45b3923f
test "$(git -C "$work" rev-parse HEAD^{tree})" = 3ee3dc26deff4fde7b7ed9d3f253dd0fbd5efced
test -z "$(git -C "$work" status --porcelain=v1)"
(cd "$work" && sha256sum -c "$runtime/instruments/candidate-inputs.sha256" && sha256sum -c "$runtime/evidence/M1/oleans.sha256") >evidence/M1R/prerequisites.log
cd "$work"
printf '%s  MEASUREMENT-CHARGE  M1R-T substantive=1/12 targeted=2/80; A-002 single additional elaboration; no audit START\n' "$(date -u +%FT%TZ)" >>"$runtime/STATUS.md"
set +e
timeout --signal=TERM --kill-after=30s 300s nix develop --quiet --no-write-lock-file -c bash "$runtime/instruments/m1r/census-command.sh" >"$runtime/evidence/M1R/census.stdout" 2>"$runtime/evidence/M1R/census.stderr"
code=$?
set -e
printf '%s\n' "$code" >"$runtime/evidence/M1R/census.exit"
exit "$code"
