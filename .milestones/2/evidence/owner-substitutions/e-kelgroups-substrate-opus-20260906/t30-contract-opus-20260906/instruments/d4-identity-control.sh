#!/usr/bin/env bash
# F-A can-fail control, ON REAL DUMPS — not on a fixture.
# Proves the redesigned rule matches exact identities and REFUSES substrings,
# prefixes, suffixes and the raw-line form. Both directions, real bytes.
set -u
IDENT="${IDENT:?normalizer path required}"
EVD="${EVD:?evidence dir required}"
fail=0
present() { # dump name
  if bash "$IDENT" "$1" | grep -qxF -e "$2"; then printf 'PASS present  [%s] in %s\n' "$2" "$(basename "$1")"
  else printf 'FAIL present  [%s] MISSING from %s\n' "$2" "$(basename "$1")"; fail=1; fi }
absent() { # dump name
  if bash "$IDENT" "$1" | grep -qxF -e "$2"; then printf 'FAIL absent   [%s] WRONGLY matched in %s\n' "$2" "$(basename "$1")"; fail=1
  else printf 'PASS absent   [%s] correctly refused in %s\n' "$2" "$(basename "$1")"; fi }

E="$EVD/s30-0a-Event.dump"; J="$EVD/s30-0a-JSON.dump"
# positive: exact type names, exact constructors, exact record selectors
present "$E" GroupEvent; present "$E" App; present "$E" Base
present "$E" BaseMutation; present "$E" ChangeRolesVoted
present "$J" ServerError; present "$J" StaleTip; present "$J" subSigner
# negative: strict prefixes of real identities must never match
absent "$E" GroupEven; absent "$E" Group; absent "$E" Ap
absent "$J" StaleTi; absent "$J" subSigne
# negative: strict suffixes must never match
absent "$E" roupEvent; absent "$J" ubSigner
# negative: the raw dump line form is not an identity
absent "$E" 'GroupEvent{App Base}'; absent "$E" '  GroupEvent{App Base}'
# negative: a name that exists in the OTHER module must not leak in
absent "$E" ServerError; absent "$J" GroupEvent
# the exact defect F-A named: the OLD rule against the RAW dump finds nothing
if grep -qxF -e GroupEvent "$E"; then printf 'FAIL oldrule  raw-dump grep -qxF matched (unexpected)\n'; fail=1
else printf 'PASS oldrule  raw-dump grep -qxF -e GroupEvent is ABSENT — the F-A defect, on real bytes\n'; fi
printf '===== D4-IDENTITY-CONTROL: %s =====\n' "$([ $fail -eq 0 ] && echo PASS || echo FAIL)"
exit $fail
