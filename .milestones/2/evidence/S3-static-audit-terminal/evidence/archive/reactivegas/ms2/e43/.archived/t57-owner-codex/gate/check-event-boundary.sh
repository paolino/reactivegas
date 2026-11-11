#!/usr/bin/env bash
set -euo pipefail

repo=${1:?usage: check-event-boundary.sh REPO}
event_file=$repo/lean/KelGroups/Vote/Event.lean
validate_file=$repo/lean/KelGroups/Vote/Validate.lean

mapfile -t constructors < <(
  sed -n '/^inductive VoteEvent where/,/^deriving /p' "$event_file" |
    sed -nE 's/^[[:space:]]*\|[[:space:]]+([A-Za-z][A-Za-z0-9_]*).*/\1/p'
)
[[ ${#constructors[@]} -gt 0 ]] || {
  printf 'gate: FAIL event-surface scanner found no VoteEvent constructors\n' >&2
  exit 1
}

validation=$(sed -n '/^def validateVoteEvent /,/^end KelGroups\.Vote/p' "$validate_file")
[[ -n $validation ]] || {
  printf 'gate: FAIL validateVoteEvent boundary not found\n' >&2
  exit 1
}

if grep -qE '^[[:space:]]*\|[[:space:]]+_[[:space:]]*=>' <<<"$validation"; then
  printf 'gate: FAIL VoteEvent authorization boundary has wildcard fallback\n' >&2
  exit 1
fi

for constructor in "${constructors[@]}"; do
  if ! grep -qE "^[[:space:]]*\\|[[:space:]]+\\.${constructor}([[:space:]]|$)" <<<"$validation"; then
    printf 'gate: FAIL authorization boundary missing constructor: %s\n' "$constructor" >&2
    exit 1
  fi
done

printf 'gate: ok   exhaustive VoteEvent authorization constructors=%s wildcard=absent\n' \
  "${#constructors[@]}"
