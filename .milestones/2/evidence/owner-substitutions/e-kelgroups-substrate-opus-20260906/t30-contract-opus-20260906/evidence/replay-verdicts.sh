#!/usr/bin/env bash
# Re-derives pf8 invocation-1 per-case verdicts from PRESERVED streams only.
# Reads predecessor evidence read-only; reproduces run_case/assert_absent
# predicates byte-for-byte as written in scratch/pf8/run.sh.
set -u
C=/tmp/reactivegas/ms2/e-kelgroups-substrate/t30-contract/scratch/pf8/cases
MIS=0
rc() { # name want pats...
  n="$1" want="$2"; shift 2
  got="$(cat "$C/$n/exit")"
  ok=1; why=""
  [ "$got" = "$want" ] || { ok=0; why="exit($got!=$want)"; }
  for p in "$@"; do
    grep -qF -e "$p" "$C/$n/stdout" "$C/$n/stderr" || { ok=0; why="$why missing[$p]"; }
  done
  if [ "$ok" -eq 1 ]; then printf 'CASE %-5s AS-PREDICTED (exit=%s)\n' "$n" "$got"
  else printf 'CASE %-5s MISPREDICT  (exit=%s)%s\n' "$n" "$got" "$why"; MIS=$((MIS+1)); fi
}
aa() { # name forbidden...
  n="$1"; shift; bad=0
  for p in "$@"; do
    if grep -qF -e "$p" "$C/$n/stdout" "$C/$n/stderr"; then
      printf 'CASE %-5s MISPREDICT  (forbidden present: %s)\n' "$n" "$p"; bad=1
    fi
  done
  [ "$bad" -eq 0 ] || MIS=$((MIS+1))
}
rc A1 0 "FINAL: PASS" "traversed=4 frozen=4" "3-pinned KelGroups.Vote.Types" "4-type Foo exact"
aa A1 "FINAL: RED" "DRIFT-FAIL" "DRIFT-REFUSE"
rc A2 1 "4-count" "traversed=3 frozen=4"
rc A3 3 "ZERO data rows" "vacuous pass REFUSED"
rc A4 3 "ZERO data rows"
rc A5 1 "duplicate mapping rows" "traversed=4 frozen=4"
aa A5 "4-count"
rc A6 1 "traversed=5 frozen=4"
rc A7 1 "empty dump" "FINAL: RED"
rc A8 3 "ZERO .hi candidates"
rc A9 3 "ambiguous selection REFUSED"
rc A10 1 "stale inheritance refused"
rc A11 0 "FINAL: PASS"
aa A11 "FINAL: RED" "DRIFT-FAIL" "DRIFT-REFUSE"
rc A12 3 "no producer evidence"
rc A13 3 "BUILD_RECEIPT absent"
rc A14 3 "unknown MODE"
rc A15 1 "REQ-B has NO successful" "REQ-C has NO successful"
rc A16 1 "ZERO successful execution records"
rc A17 1 "1-clean" "uncommitted bytes"
rc A18 1 "1-position-lean" "rebind procedure"
aa A18 "differs from frozen bytes" "ZERO data rows" "unbound config"
rc A19 1 "1-position-lean"
rc A20 1 "1-hash lean/KelGroups/Vote/Types.lean" "re-review required" "0-overlay-base"
aa A20 "1-pin-lean" "FINAL: PASS"
rc A21 0 "FINAL: PASS" "0-overlay-base"
aa A21 "FINAL: RED" "DRIFT-FAIL" "DRIFT-REFUSE"
rc A22 1 "1-hash-hs: lib/KelGroups/Vote/Types.hs differs" "0-overlay-base" "FINAL: RED"
aa A22 "1-pin-lean" "rebind procedure" "FINAL: PASS"
rc A23a 3 "BUILD_MARKER absent"
rc A23b 3 "not a regular file"
rc A23c 3 "not a regular file"
rc A23d 3 "unbound config:" "FROZEN_ROWS" "LEG4_LOG"
if grep -l "FINAL: PASS" "$C"/A23*/stdout 2>/dev/null | grep -q .; then
  printf 'CASE %-5s MISPREDICT  (setup failure printed PASS)\n' A23e; MIS=$((MIS+1))
else printf 'CASE %-5s AS-PREDICTED (no setup failure prints PASS)\n' A23e; fi
rc A24 1 "expected exact line [Fo] ABSENT"
rc A25 3 "unbound config:" "JOIN_ROWS" "BUILD_RECEIPT"
printf '===== REPLAY: mispredicts=%s =====\n' "$MIS"
