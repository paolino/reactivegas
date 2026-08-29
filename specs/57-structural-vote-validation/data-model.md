# Data model — #57 structural vote validation

Artifact ceiling: 90 lines / 7 KiB. This re-cut changes validation and proof
relations; it adds no production state field or event constructor.

## Existing production data

`VoteEvent` remains the closed six-constructor surface:
`openQuestion`, `cast`, `renounce`, `admitMember`, `removeMember`, and
`setRoles`. `VoteState` remains the complete observable state: members, open
questions, and closure records (which carry the question, tallies, verdict,
and cause).

## Admissibility relation

Admissibility relates a threshold, arbitrary pre-state, signer, and event to
either success or a distinct `VoteError`.

Constraints:

- every constructor participates in one exhaustive decision;
- with a nonempty franchise, every admissible signer is a current
  responsabile, regardless of event constructor;
- empty-franchise member admission retains only the bootstrapping capability
  already needed by production traces;
- rejection is interpreted by the production fold as complete-state identity;
- adding an event constructor cannot acquire an admissibility default.

The relation is not represented as a list of constructors, a boolean registry,
or a wildcard fallback.

## Semantic question preservation

The no-expiry premise relates a pre-state, signed event, and target question.
It holds exactly when the event preserves the target's ballots, the current
franchise, and the proposer's standing. Its truth depends on those semantic
observations, not the event constructor.

A member admission with no admin role is a required positive coverage case:
when it preserves those observations, it satisfies the premise. A franchise
change, target ballot, or proposer-standing change does not satisfy it.

## State invariants

- **D57-1 complete rejection identity:** every field of the rejected result is
  definitionally/equationally the corresponding field of the arbitrary input.
- **D57-2 authorization closure:** after bootstrap, no non-responsabile signed
  event can change membership, franchise, questions, tallies, closures, or
  verdicts.
- **D57-3 semantic no-expiry:** an open target remains the same open question
  when the preservation relation holds.
- **D57-4 inherited carrier:** partition, tally disjointness, no-stale-open,
  cast-time franchise, and threshold-policy independence remain true of the
  repaired production fold.

