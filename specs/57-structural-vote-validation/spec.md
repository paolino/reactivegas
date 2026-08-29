# Spec — #57 structural vote validation

Issue: https://github.com/paolino/reactivegas/issues/57 (parent #43)

Frozen issue body SHA-256:
`18dd3cfe9ae6f42a5ca1324419436893f87106e17f449034a1fc2791b21cedf9`.
This amendment is bounded to the re-cut from rejected local candidate
`c433ff769fc35329050411054324c19b5b100fdb`.

Artifact ceiling: 180 lines / 12 KiB.

## Observable outcome

The production vote fold rejects every inadmissible signer/event pair before
the event can affect any part of `VoteState`. Rejection is exact complete-state
identity for an arbitrary pre-state, not identity obtained later from a
well-formedness premise.

## Requirements

- **R57-01 — one production boundary.** `applyVoteEvent` has one validation
  boundary before both the event effect and the closure sweep. A rejected event
  returns the input `VoteState` exactly; no membership, franchise, question,
  tally, closure, or verdict computation is reached.
- **R57-02 — exhaustive authorization.** The boundary's authorization decision
  is total and explicit over the complete `VoteEvent` surface. It contains no
  wildcard fallback and no constructor list or boolean side registry. Adding a
  constructor with an effect while leaving authorization unchanged must fail
  mechanically at the authorization boundary.
- **R57-03 — universal rejection theorem.** For arbitrary threshold, signer,
  event, error, and pre-state, a validation error implies
  `applyVoteEvent ... = preState`. The theorem has no `VoteWellFormed`,
  reachability, event-kind, or constructor-specific premise.
- **R57-04 — universal signer authorization.** Once a franchise exists, a
  signer who is not a current responsabile is rejected for every `VoteEvent`.
  This includes `admitMember`, `removeMember`, and `setRoles`; they are not
  exceptions. The existing empty-franchise admission capability remains only
  as the bootstrap needed to make `foldVote` reach a franchise; this re-cut
  adds no Slice-B R-66/R-67 admission-shape semantics.
- **R57-05 — production R-45 oracle.** In a reachable state with three
  responsabili and an open one-assent question, `stranger/removeMember` is
  rejected and leaves the entire state unchanged. The retained bypass seed
  must fail for this R-45 reason.
- **R57-06 — inherited rows are re-demonstrated.** `INV-54-PARTITION`,
  `INV-54-DISJOINT`, `INV-54-NOSTALE`, `INV-54-FRANCHISE`, and
  `INV-54-POLICYFREE` are freshly exercised against the repaired fold with
  their frozen instruments or hash-bound equivalent controls.
- **R57-07 — no-expiry is semantic.** `no_expiry` uses a premise stating that
  the target question's ballots, the franchise, and the proposer's standing
  are preserved. The premise is not an event-constructor whitelist and is
  demonstrably true for a preserving non-admin member admission.
- **R57-08 — proof trust.** Accepted Lean has zero `sorry`, `admit`, `sorryAx`,
  custom axiom, `native_decide`, and `Lean.ofReduceBool`. Contractual theorem
  axiom sets are printed and limited to `propext`, `Classical.choice`, and
  `Quot.sound`.
- **R57-09 — boundary preservation.** The seven accepted Slice-1 modules under
  `lean/KelGroups/*.lean` remain blob-identical to `ccdda830`; the dependency
  direction remains KelGroups substrate to Reactivegas application only.
- **R57-10 — toolchain identity.** Every execution receipt names Lean 4.25.0.
  The inert `lean/lean-toolchain` 4.27 pin is not changed or reconciled here.

## Invariants and acceptance meaning

| Invariant | Severity | Failure | Success |
|---|---|---|---|
| `INV-57-BOUNDARY` | BLOCKING | an event effect or sweep is reachable without the single validation decision | every production step crosses one pre-effect boundary |
| `INV-57-NOOP` | BLOCKING | some rejected pair changes any `VoteState` field or needs a well-formedness premise | arbitrary rejected pairs are complete-state identity |
| `INV-57-AUTH` | BLOCKING | a non-responsabile changes membership, franchise, questions, tallies, closures, or verdict after bootstrap | every current constructor, including all three member/role events, is rejected and inert |
| `INV-57-EXHAUSTIVE` | BLOCKING | a new constructor plus effect compiles without an authorization decision | the seeded surface extension fails at the named boundary |
| `INV-57-NOEXPIRY` | ADVISORY-BUT-REQUIRED | preserving member events cannot satisfy the public theorem premise | the semantic premise covers the retained non-admin admission witness |
| `INV-54-{PARTITION,DISJOINT,NOSTALE,FRANCHISE,POLICYFREE}` | BLOCKING | an inherited frozen instrument survives or was merely copied forward | all five rows are freshly re-killed on the candidate |

## Source fence

Implementation writes are limited to:

- `lean/KelGroups/Vote/Validate.lean`
- `lean/KelGroups/Vote/Fold.lean`
- `lean/KelGroups/Vote/Invariants.lean`
- `lean/KelGroups/Vote/Tests.lean`
- the ticket-owner task stamp in `specs/57-structural-vote-validation/tasks.md`

All other production and proof paths are read-only. If this fence is
insufficient, implementation stops with a durable question naming the exact
path and reason.

## Non-goals

- No third event-local R-45 guard.
- No Slice-B R-66/R-67 admission-shape implementation.
- No composition proof or `lean/Reactivegas/**` change.
- No edit to `lean/lean-toolchain`, Nix, CI, documentation, Haskell, or the
  upstream kelgroups repository.
- No end-to-end enforcement claim, push, PR mutation, merge, or issue edit.

