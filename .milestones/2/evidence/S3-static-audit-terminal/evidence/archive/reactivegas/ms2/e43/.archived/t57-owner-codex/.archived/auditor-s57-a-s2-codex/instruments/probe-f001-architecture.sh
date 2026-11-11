#!/usr/bin/env bash
set -euo pipefail

repo=${1:?usage: probe-f001-architecture.sh REPO}
rejected=400f5b2829eeae27faeb0994ba8cfcc03c37dd3d
candidate=9d68abb0930bb31d9bcd1116979765e974547ffd
fold_path=lean/KelGroups/Vote/Fold.lean
inv_path=lean/KelGroups/Vote/Invariants.lean
validate_path=lean/KelGroups/Vote/Validate.lean

effect_region() {
  git -C "$repo" show "$1:$fold_path" |
    sed -n '/^def effectedState /,/^\/-- One fold step/p'
}

rejected_effect=$(effect_region "$rejected")
candidate_effect=$(effect_region "$candidate")
auth_identifiers='isResponsabile|validateVoteEvent|franchise(Size)?|hasAdmin|notResponsabile'

rejected_auth=$(grep -cE "$auth_identifiers" <<<"$rejected_effect" || true)
candidate_auth=$(grep -cE "$auth_identifiers" <<<"$candidate_effect" || true)
validator_auth=$(git -C "$repo" show "$candidate:$validate_path" |
  grep -cE 'isResponsabile|notResponsabile' || true)

(( rejected_auth > 0 )) || {
  printf 'PROBE-FAIL rejected seed has no detectable effect-local authorization\n' >&2
  exit 1
}
(( candidate_auth == 0 )) || {
  printf 'PROBE-FAIL candidate effect region retains authorization identifiers count=%s\n' "$candidate_auth" >&2
  exit 1
}
(( validator_auth > 0 )) || {
  printf 'PROBE-FAIL positive control cannot find authorization in validator\n' >&2
  exit 1
}

for constructor in openQuestion cast renounce admitMember removeMember setRoles; do
  grep -qE "^[[:space:]]*\|[[:space:]]+\.${constructor}([[:space:]]|$)" \
    <<<"$candidate_effect" || {
      printf 'PROBE-FAIL candidate effect missing constructor=%s\n' "$constructor" >&2
      exit 1
    }
done
! grep -qE '^[[:space:]]*\|[[:space:]]+_[[:space:]]*=>' <<<"$candidate_effect" || {
  printf 'PROBE-FAIL candidate effect contains wildcard arm\n' >&2
  exit 1
}

candidate_inv=$(git -C "$repo" show "$candidate:$inv_path")
tally_region=$(sed -n '/^private theorem effectedState_tally_growth /,/^private theorem tally_keys_franchised_from /p' <<<"$candidate_inv")
call_region=$(sed -n '/^private theorem tally_keys_franchised_from /,/^\/-- INV-54-FRANCHISE/p' <<<"$candidate_inv")

grep -qF '(admitted : validateVoteEvent θ gs signer event = Except.ok ())' \
  <<<"$tally_region" || {
    printf 'PROBE-FAIL tally-growth theorem lacks validator admission premise\n' >&2
    exit 1
  }
grep -qF 'simp [validateVoteEvent, hresp] at admitted' <<<"$tally_region" || {
  printf 'PROBE-FAIL tally-growth proof does not consume admission on unauthorized cast\n' >&2
  exit 1
}
grep -qF 'effectedState_tally_growth θ initial signed.1 signed.2 k hval' \
  <<<"$call_region" || {
    printf 'PROBE-FAIL tally-growth call site does not pass validator .ok equation\n' >&2
    exit 1
  }

printf 'PROBE-OK rejected_effect_auth=%s candidate_effect_auth=%s validator_auth=%s constructors=6 wildcard=absent admitted_premise=consumed\n' \
  "$rejected_auth" "$candidate_auth" "$validator_auth"
