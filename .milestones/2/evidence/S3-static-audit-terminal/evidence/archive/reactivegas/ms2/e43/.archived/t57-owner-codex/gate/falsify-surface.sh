#!/usr/bin/env bash
set -euo pipefail

source_repo=/code/reactivegas-issue-57
checker=/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/check-event-boundary.sh
run_dir=$(mktemp -d /tmp/reactivegas-t57-surface.XXXXXX)
cleanup() {
  git -C "$run_dir" restore --staged --worktree . >/dev/null 2>&1 || true
  git -C "$source_repo" worktree remove "$run_dir" >/dev/null 2>&1 || true
}
trap cleanup EXIT
git -C "$source_repo" worktree add --detach "$run_dir" bb3ac41a1456c50b1bba7dafd522c174461b42ea >/dev/null

"$checker" "$run_dir" >/dev/null

sed -i '/^deriving DecidableEq/i\  | auditBypass' "$run_dir/lean/KelGroups/Vote/Event.lean"
sed -i '/^  | \.renounce _ => gs/i\  | .auditBypass => gs' "$run_dir/lean/KelGroups/Vote/Fold.lean"

out=$({ "$checker" "$run_dir"; } 2>&1) && {
  printf 'FALSIFY-FAIL surface bypass stayed green\n' >&2
  exit 1
}
grep -qF 'authorization boundary missing constructor: auditBypass' <<<"$out" || {
  printf '%s\n' "$out" >&2
  printf 'FALSIFY-FAIL surface bypass red for wrong reason\n' >&2
  exit 1
}
printf 'FALSIFY-OK surface-bypass reason=authorization-boundary-missing-constructor\n'
