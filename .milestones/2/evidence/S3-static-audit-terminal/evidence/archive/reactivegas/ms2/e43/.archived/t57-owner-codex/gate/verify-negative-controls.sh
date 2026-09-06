#!/usr/bin/env bash
set -euo pipefail

repo=/code/reactivegas-issue-57
gate_root=/tmp/reactivegas/ms2/e43/t57-owner-codex/gate

expect_lean_red() {
  local name=$1 file=$2 expect=$3
  local log="$gate_root/falsify-${name}.log" rc=0
  nix develop --quiet --no-write-lock-file -c bash -c \
    "cd lean && lake env lean '$file'" >"$log" 2>&1 || rc=$?
  [[ $rc -ne 0 ]] || { printf 'FALSIFY-FAIL %s stayed green\n' "$name"; return 1; }
  grep -qF "$expect" "$log" || {
    tail -20 "$log" >&2
    printf 'FALSIFY-FAIL %s red for wrong reason\n' "$name"
    return 1
  }
  printf 'FALSIFY-OK %s exit=%s reason=%s sha256=%s\n' \
    "$name" "$rc" "$expect" "$(sha256sum "$log" | cut -d' ' -f1)"
}

"$gate_root/falsify-surface.sh"

expect_lean_red r45-baseline \
  "$gate_root/instruments/r45-production-noop.lean" \
  'validateVoteEvent legacyThreshold before "stranger" removal'
expect_lean_red arbitrary-state-baseline \
  "$gate_root/instruments/arbitrary-and-surface-noop.lean" \
  'applyVoteEvent legacyThreshold arbitraryPreState'
expect_lean_red no-expiry-member-baseline \
  "$gate_root/instruments/no-expiry-member.lean" \
  'PreservesQuestionSemantics'

for row in PARTITION DISJOINT NOSTALE FRANCHISE POLICYFREE BYPASS; do
  lower=$(tr '[:upper:]' '[:lower:]' <<<"$row")
  expect_lean_red "mutant-$lower" "$gate_root/instruments/mutant-$lower.lean" \
    "MUTATION-APPLIED:$row"
done

printf 'FALSIFY-SUMMARY pass=10 fail=0 toolchain=Lean-4.25.0 base=bb3ac41a\n'
