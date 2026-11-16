#!/usr/bin/env bash
set -euo pipefail

worktree=/code/reactivegas-t54-audit-a1
runtime=/tmp/reactivegas/ms2/e43/t54-vote-coverage/auditor-slice-a-s1
candidate=757dac98aecce705e44eda6c9283a5da01b02827
candidate_lib="$worktree/lean/.lake/build/lib/lean"
run_root=$(mktemp -d "$runtime/instruments/run.XXXXXX")
src="$run_root/src"
lib="$run_root/lib/lean"
logs="$run_root/logs"
mkdir -p "$src/KelGroups/Vote" "$lib/KelGroups/Vote" "$logs"
cp -a "$candidate_lib/." "$lib/"
for module in State Fold Tests Invariants; do
  cp "$worktree/lean/KelGroups/Vote/$module.lean" "$src/KelGroups/Vote/$module.lean"
done

[[ $(git -C "$worktree" rev-parse HEAD) == "$candidate" ]]
[[ -z $(git -C "$worktree" status --porcelain=v1 --untracked-files=no) ]]
[[ -f "$candidate_lib/KelGroups/Vote/Fold.olean" ]]

compile_one() {
  local source=$1 output=$2 log=$3
  set +e
  (cd "$src" && LEAN_PATH="$lib:$candidate_lib" lean "$source" -o "$output") >"$log" 2>&1
  local rc=$?
  set -e
  printf '%s' "$rc"
}

# RED 1 — freeze the policy instead of reading the supplied threshold.
state_source="$worktree/lean/KelGroups/Vote/State.lean"
[[ $(grep -Fc 'let required := threshold (franchiseSize gs)' "$state_source") -eq 1 ]]
sed 's/let required := threshold (franchiseSize gs)/let required := legacyThreshold (franchiseSize gs)/' \
  "$state_source" >"$src/KelGroups/Vote/State.lean"
[[ $(grep -Fc 'let required := legacyThreshold (franchiseSize gs)' \
  "$src/KelGroups/Vote/State.lean") -eq 1 ]]
[[ $(grep -Fc 'let required := threshold (franchiseSize gs)' \
  "$src/KelGroups/Vote/State.lean") -eq 0 ]]

rc=$(compile_one "KelGroups/Vote/State.lean" \
  "$lib/KelGroups/Vote/State.olean" "$logs/policy-state.log")
[[ $rc -eq 0 ]]
rc=$(compile_one "KelGroups/Vote/Fold.lean" \
  "$lib/KelGroups/Vote/Fold.olean" "$logs/policy-fold.log")
[[ $rc -eq 0 ]]
rc=$(compile_one "KelGroups/Vote/Tests.lean" \
  "$lib/KelGroups/Vote/Tests.olean" "$logs/policy-tests.log")
[[ $rc -ne 0 ]]
grep -q 'Tests.lean:94:0: Expression' "$logs/policy-tests.log"
grep -q 'did not evaluate to `true`' "$logs/policy-tests.log"
printf 'MUTANT-RED row=INV-54-POLICYFREE mutation=hardcode-legacyThreshold check=zeroThresholdPassesWithNoBallot exit=%s applied=1\n' "$rc"

# RED 2 — admit a cast without the current-franchise guard.
rm -f "$lib/KelGroups/Vote/State.olean" "$lib/KelGroups/Vote/Fold.olean" \
  "$lib/KelGroups/Vote/Tests.olean" "$lib/KelGroups/Vote/Invariants.olean"
cp "$candidate_lib/KelGroups/Vote/State.olean" "$lib/KelGroups/Vote/State.olean"
fold_source="$worktree/lean/KelGroups/Vote/Fold.lean"
[[ $(grep -Fc 'if isResponsabile signer gs then' "$fold_source") -eq 1 ]]
sed 's/if isResponsabile signer gs then/if true then/' \
  "$fold_source" >"$src/KelGroups/Vote/Fold.lean"
[[ $(grep -Fc 'if true then' "$src/KelGroups/Vote/Fold.lean") -eq 1 ]]
[[ $(grep -Fc 'if isResponsabile signer gs then' "$src/KelGroups/Vote/Fold.lean") -eq 0 ]]

rc=$(compile_one "KelGroups/Vote/Fold.lean" \
  "$lib/KelGroups/Vote/Fold.olean" "$logs/franchise-fold.log")
[[ $rc -eq 0 ]]
rc=$(compile_one "KelGroups/Vote/Invariants.lean" \
  "$lib/KelGroups/Vote/Invariants.olean" "$logs/franchise-invariants.log")
[[ $rc -ne 0 ]]
grep -q 'error:.*Invariants.lean' "$logs/franchise-invariants.log"
grep -q 'isResponsabile' "$logs/franchise-invariants.log"
printf 'MUTANT-RED row=INV-54-FRANCHISE mutation=bypass-current-responsabile-guard check=franchise-proof exit=%s applied=1\n' "$rc"

# GREEN — the same shadow compiler path must accept the untouched candidate.
rm -f "$lib/KelGroups/Vote/State.olean" "$lib/KelGroups/Vote/Fold.olean" \
  "$lib/KelGroups/Vote/Tests.olean" "$lib/KelGroups/Vote/Invariants.olean"
cp "$state_source" "$src/KelGroups/Vote/State.lean"
cp "$fold_source" "$src/KelGroups/Vote/Fold.lean"
cmp -s "$state_source" "$src/KelGroups/Vote/State.lean"
cmp -s "$fold_source" "$src/KelGroups/Vote/Fold.lean"

for module in State Fold Tests Invariants; do
  rc=$(compile_one "KelGroups/Vote/$module.lean" "$lib/KelGroups/Vote/$module.olean" \
    "$logs/control-${module,,}.log")
  [[ $rc -eq 0 ]]
done
printf 'CONTROL-GREEN candidate=%s modules=State,Fold,Tests,Invariants\n' "$candidate"

[[ -z $(git -C "$worktree" status --porcelain=v1 --untracked-files=no) ]]
printf 'TRACKED-TREE-CLEAN candidate=%s\n' "$candidate"
