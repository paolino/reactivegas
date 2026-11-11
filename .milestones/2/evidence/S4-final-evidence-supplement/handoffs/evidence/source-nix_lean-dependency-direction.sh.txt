#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

scan_imports() {
  local namespace=$1
  shift
  local path existing=()
  for path in "$@"; do
    [[ -e "$path" ]] && existing+=("$path")
  done
  [[ ${#existing[@]} -eq 0 ]] && return 0
  grep -rnE "^[[:space:]]*import[[:space:]]+${namespace}([.[:space:]]|$)" \
    -- "${existing[@]}" 2>/dev/null || true
}

violations=$(scan_imports Reactivegas lean/KelGroups.lean lean/KelGroups)
if [[ -n "$violations" ]]; then
  printf '%s\n' "forbidden Reactivegas.* import below the KelGroups substrate:" >&2
  printf '%s\n' "$violations" >&2
  exit 1
fi

# A zero-result scanner is evidence only after the same matcher finds a known
# import. This also catches accidental regex breakage in the durable checker.
control=$(scan_imports Reactivegas lean/Reactivegas.lean lean/Reactivegas | wc -l)
if [[ "$control" -lt 1 ]]; then
  printf '%s\n' "dependency scanner could not find a known Reactivegas import" >&2
  exit 1
fi

printf 'lean dependency direction: OK (control imports=%s)\n' "$control"
