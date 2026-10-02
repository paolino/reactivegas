# Data model — #68 (A-001 reading)

Fields, validation, and state invariants. Changed decisions only.

- `PendingProposal` / `PendingBase`: fields `proposal`/`mutation`,
  `proposer : Key`, `approvals : List Key` — UNCHANGED SHAPES. Changed
  MEANING of a well-formed entry (see below). No admission representability
  change (`BaseMutation` still cannot admit).
- `PendingWellFormed` (restated): `approvals.Nodup` (kept) AND
  (`adminCount ≤ 1` at the approving transition → no constraint beyond Nodup;
  `adminCount > 1` → `proposer ∉ approvals`). The sole-admin exception is part
  of the invariant, not a comment. Raw-domain boundary (F-01): with a
  validation-free fold, preservation of this count-indexed predicate is
  conditional on boundary admission (see R68-07 and the retained 7-event
  witness); unconditional raw-fold guarantees are the structural ones.
- `WellFormed`/`PendingCoherent`: same structure, restated predicate.
- `ValidationError`: NEW variant for proposer self-approval above n=1
  (exact name derived in-ticket by the owner; packet mandates: distinct from
  `alreadyApproved`, carries `(key : Key) (proposalId : ProposalId)`).
- `majority`, `adminCount`, membership relation, `DirectCommand`,
  sealed-hook types: UNTOUCHED.
- Vote-machine `Question.assents/dissents`: UNTOUCHED (alignment target, not
  a work item; no new theta default).
