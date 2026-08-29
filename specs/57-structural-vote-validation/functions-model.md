# Functions model — #57 structural vote validation

Artifact ceiling: 100 lines / 8 KiB. Only changed/new signatures and
signature-level constraints are listed; there are no bodies or algorithms.

## `KelGroups.Vote.Validate`

- `validateVoteEvent (threshold : Threshold) (gs : VoteState)
  (signer : Key) (event : VoteEvent) : Except VoteError Unit`
  — changed constraint: total exhaustive authorization across the complete
  event surface, with no wildcard or side registry; for a nonempty franchise,
  success implies `isResponsabile signer gs = true`.

## `KelGroups.Vote.Fold`

- `applyVoteEvent (threshold : Threshold) (gs : VoteState)
  (signer : Key) (event : VoteEvent) : VoteState`
  — changed effect constraint: the sole validation result dominates both
  `effectedState` and `sweepClosures`; an error returns `gs` exactly.

`effectedState`, `sweepClosures`, `foldVote`, and `foldFrom` retain their public
signatures. No event effect owns authorization.

## `KelGroups.Vote.Invariants`

- `inadmissible_is_noop (threshold : Threshold) (gs : VoteState)
  (signer : Key) (event : VoteEvent) (error : VoteError)
  (rejected : validateVoteEvent threshold gs signer event = .error error) :
  applyVoteEvent threshold gs signer event = gs`
  — changed theorem: arbitrary pre-state, with no well-formedness premise.
- `nonresponsabile_event_noop (threshold : Threshold) (gs : VoteState)
  (signer : Key) (event : VoteEvent)
  (bootstrapped : franchiseSize gs > 0)
  (unauthorized : isResponsabile signer gs = false) :
  applyVoteEvent threshold gs signer event = gs`
  — new universal R-45 corollary; no event-kind premise.
- `PreservesQuestionSemantics (threshold : Threshold) (gs : VoteState)
  (signer : Key) (event : VoteEvent) (questionId : QuestionId) : Prop`
  — changed proof-side relation: semantic preservation of target ballots,
  franchise, and proposer standing; not a constructor whitelist.
- `no_expiry` — retains its production-prefix/event-list scope and uses
  `PreservesQuestionSemantics` as its preservation premise.

The contractual axiom set includes the two universal R-45 theorems,
`no_expiry`, `foldVote_wellFormed`, `ballots_nodup_disjoint`,
`open_questions_are_open`, `questions_partition`, `franchise_of_tallies`, and
`verdictOf_threshold_congr`.

## `KelGroups.Vote.Tests`

Executed production witnesses cover:

- rejection and complete-state identity for all six current event
  constructors from a bootstrapped state;
- explicit `admitMember`, `removeMember`, and `setRoles` franchise no-ops;
- the retained `stranger/removeMember` threshold/verdict trace;
- a preserving non-admin admission satisfying the semantic no-expiry premise;
- the existing partition, disjointness, no-stale, franchise, and policy-free
  point witnesses.

