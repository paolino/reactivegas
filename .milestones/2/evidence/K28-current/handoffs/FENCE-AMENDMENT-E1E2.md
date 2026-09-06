# FENCE-AMENDMENT-E1E2 — bounded surface expansion (desk NOTE-006, option (a))

Ticket owner `t28-app-api`. Authority: desk NOTE-006 via epic NOTE-006
(same-requirement mechanical, option (a) adopted); child Q-003 verified at
source (E1/E2 confirmed + E1-additional found by ticket-owner sweep).
Status: AUTHORIZED bounds below (RED bytes hashed); COMPLETION (post-change
diff substantiation + behavior evidence) pending GREEN submission review.

## Authorized bounds (no more, no less)

E1 — three desk-verified sites, each exactly `+ , pendingBase = Map.empty`
(semantics-preserving; assert scope members/pendingProposals/authMode):
- `test/InvariantsSpec.hs:112-117` (Invariant-4 construction)
- `test/InvariantsSpec.hs:135-141` (`emptyState'`)
- `test/StoreInvariantsSpec.hs:204-210` (`emptyGS`)
E1-additional (ticket-owner delegation per NOTE-006, same mechanical class,
disclosed here): `test/ValidateSpec.hs:318` (`emptyGS`), identical one-line
fix (child's single-line grep missed it — the NOTE-006 search trap;
whitespace-robust EOL-anchored discovery applied).
E2 — `lib/KelGroups/Server/JSON.hs` JSON-ONLY: GroupState toJSON
(+`pendingBase` emit) / fromJSON (bounded `.:?` + `.!=` + 4-field
construct); ValidationError `ReservedKey` arms both directions
(unknown-tag fail preserved); five new orphan blocks (`DirectCommand`,
`BaseMutation`-Voted, `BaseChange`, `IntegratedEvent`, `PendingBase`) for
owned Store persist/replay. No endpoints, no client, no behavior beyond
codecs; `Trivial.hs` + historical untouched.
Pre-approved equivalent class (no repeat desk loop): mechanical
constructor 4th-field completions + JSON codec completions for the new
types/fields, same shapes — ticket owner VERSIONS + DISCLOSES at submission
review. Anything else → BLOCK as separate scope.

## RED-byte hashes (pre-change identity)

- `test/InvariantsSpec.hs` blob `24d7d3db…`
- `test/StoreInvariantsSpec.hs` blob `f8978465…`
- `test/ValidateSpec.hs` blob `400e8ba2…`
- `lib/KelGroups/Server/JSON.hs` blob `87a611a1…`
(HEAD `570fe4a…`, tree clean at authorization.)

## Discovery method (search-trap closure)

Single-line `GroupState{` finds NOTHING (all constructions are
`GroupState`-newline-`{` layout). Method used: EOL-anchored `GroupState$` +
next-line brace match + positional-application sweep + record-pattern sweep +
single-line-layout check (zero hits) + owned/unowned classification.
Negative results: no positional applications outside owned `State.hs:57`;
no record patterns; `emptyState` callers need no change; `.:` readers ignore
extra fields. Compiler missing-field errors remain the fail-closed backstop
at GREEN leg-3/4/6.

## Gate impact: NONE (no leg names these sites; legs compile+execute them as-is)

## COMPLETION — pending GREEN submission review

To be filled: post-change diff (must show ONLY authorized hunks +
disclosed equivalents), behavior evidence pointers ((i) roundtrip at actual
values incl. non-empty pendingBase, (ii) old-row compat, (iii)
malformed-new refusal, (iv) reopen/replay with pending content),
historical-suite greenness, REGDIR row deltas (none expected — no example
renames required by E1/E2).
