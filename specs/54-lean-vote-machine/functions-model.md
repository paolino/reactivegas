# Functions model — #54 Slice 1

New declarations only: exact names, explicit argument names, types, and
signature-level constraints. **No bodies, no algorithms, no tactics.** The gate
checks that every name below exists in the delivered Lean (G5).

`α` is the application fold type parameter. `Key`, `Email`, `RoleName`,
`ProposalId` are `String` (see `data-model.md`).

## Two parameters that are threaded, never axiomatized

The Haskell hardcodes a Blake2b SAID digest and a CESR key decoder. Neither is
computable in this model, and neither may become an `axiom` (R-27/R-28). Both
are therefore **explicit function arguments**, instantiated concretely by the
point tests:

```text
digest   : Proposal → ProposalId      -- EP-DIGEST
validKey : Key → Bool                 -- EP-CESR
```

Threading `digest` through the fold is also the faithful reading: the Haskell's
digest is `show`-derived, so a concrete injective stub in the tests is the same
construction with a different hash.

## KelGroups.Types

```text
Admin                       : Type            -- publicAdmin | privateAdmin
Role                        : Type            -- adminRole (a : Admin) | appRole (name : RoleName)
Member                      : Type            -- fields: key, email, roles
isAdminRole (r : Role)      : Bool
hasAdmin    (rs : List Role): Bool
RoleDef α                   : Type            -- fields: canAdd, canRemove : α → Bool
GroupConfig α               : Type            -- field: roleDefs : List (RoleName × RoleDef α)
```

## KelGroups.Event

```text
Proposal                    : Type
  introduceMember (key : Key) (email : Email) (roles : List Role)
  removeMember    (key : Key)
  changeRoles     (key : Key) (roles : List Role)
BaseEvent                   : Type            -- propose (p : Proposal) | approve (pid : ProposalId)
GroupEvent α                : Type            -- base (e : BaseEvent) | app (a : α)
```

## KelGroups.State

```text
PendingProposal             : Type            -- fields: proposal, proposer, approvals
GroupState α                : Type            -- fields: members, pendingProposals, appFold
emptyState  (initial : α)                              : GroupState α
adminCount  (gs : GroupState α)                        : Nat
majority    (gs : GroupState α)                        : Nat
isAdmin     (pubKey : Key) (gs : GroupState α)         : Bool
isMember    (pubKey : Key) (gs : GroupState α)         : Bool
AuthMode                    : Type            -- bootstrap | normal
authMode    (gs : GroupState α)                        : AuthMode
```

`majority` returns `Nat` and uses natural division, giving `(n+1)/2`. Signature
constraint: it must be total and must return `0` at `adminCount = 0`.

## KelGroups.Fold

```text
AppFold α                                              : Type   -- α → α → α
enact        (gs : GroupState α) (p : Proposal)        : GroupState α
tryEnact     (gs : GroupState α) (pid : ProposalId)    : GroupState α
applyPropose (digest : Proposal → ProposalId) (gs : GroupState α)
             (signer : Key) (p : Proposal)             : GroupState α
applyApprove (gs : GroupState α) (signer : Key)
             (pid : ProposalId)                        : GroupState α
applyEvent   (digest : Proposal → ProposalId) (appFoldFn : AppFold α)
             (gs : GroupState α) (signer : Key)
             (evt : GroupEvent α)                      : GroupState α
foldGroup    (digest : Proposal → ProposalId) (appFoldFn : AppFold α)
             (initial : α) (events : List (Key × GroupEvent α))
                                                       : GroupState α
```

Signature-level constraints:

- every one of these is **total** — the Haskell fold cannot fail, and a model
  returning `Option` would not be faithful (contrast `lean/Reactivegas/Step.lean`,
  which is deliberately rejecting);
- `tryEnact` is the **only** declaration permitted to remove a pending proposal;
- `applyEvent`'s application case ignores `signer` (R-17).

## KelGroups.Validate

```text
ValidationError                                        : Type   -- ten constructors, data-model.md
validateEvent    (validKey : Key → Bool) (config : GroupConfig α)
                 (gs : GroupState α) (signer : Key)
                 (evt : GroupEvent α)                  : Except ValidationError Unit
validateBase     (validKey : Key → Bool) (config : GroupConfig α)
                 (gs : GroupState α) (signer : Key)
                 (evt : BaseEvent)                     : Except ValidationError Unit
validateProposal (validKey : Key → Bool) (config : GroupConfig α)
                 (gs : GroupState α) (signer : Key)
                 (p : Proposal)                        : Except ValidationError Unit
validateApproval (gs : GroupState α) (signer : Key)
                 (pid : ProposalId)                    : Except ValidationError Unit
```

Signature-level constraint: the result carries the **first** error in the
Haskell's evaluation order (R-22); an implementation that collects or reorders
errors does not satisfy this signature's contract.

## KelGroups.Invariants

Theorem names are part of the contract; the gate checks their existence and
their axiom sets (R-28).

```text
approvals_nodup                  -- VI-1
proposer_mem_approvals           -- VI-2
enact_implies_threshold_met      -- VI-3   (the property Slice 2 consumes)
members_change_implies_enacted   -- VI-4
member_key_coherent              -- VI-5
majority_table                   -- R-9, the pinned 0..5 threshold row
majority_not_strict_on_even      -- R-9, `2 * majority gs ≤ adminCount gs`
                                 --      for even positive admin counts
```

`enact_implies_threshold_met` and `members_change_implies_enacted` may take
`Function.Injective digest` as an **explicit hypothesis**. Discharging it with
an `axiom` is a rejection reason.

## KelGroups.Invariants — refuted candidates

Delivered as executed counterexamples, **not** theorems (R-VI-6/VI-7):

```text
stalePendingWitness       -- a trace reaching a pending proposal whose approvals
                          -- ≥ the CURRENT threshold, because an enactment
                          -- lowered the admin count
bootstrapNonMemberWitness -- a fully validated trace whose proposer is not a
                          -- member, because bootstrap never checks the signer
```

Each witness ships with the executed `#guard` that demonstrates the naive
invariant is false at that state.

## KelGroups.Tests

Point tests, one named declaration per case, executed at elaboration by `#guard`
(R-29). `native_decide` is forbidden (R-28).

Required cases: proposer auto-approval; propose-replaces-pending; duplicate
approval leaves approvals unchanged; approve on an unknown id is a no-op;
current-admin validation accepted and rejected; bootstrap accepted and rejected;
threshold at 0/1/2/3/4/5 admins; zero-admin immediate enactment; odd and even
enactment boundaries; enactment deletes exactly the enacted proposal; the two
enactment no-ops (remove-absent, change-roles-absent); introduce-overwrites; the
application-event signer discard; and one case per distinct `ValidationError`
constructor reachable in the model.

---

# Functions — Vote-coverage run (2026-08-29)

New or changed signatures only: names, explicit argument names, types, and
signature-level constraints. No bodies, no algorithms, no pseudocode.

## `KelGroups.Vote.Types`

- `Verdict` — closed inductive, three constructors (positive, negative, open).
- `Threshold` — abbreviation for `Nat → Nat`.
- `legacyThreshold (responsabili : Nat) : Nat`
- `zeroThreshold (responsabili : Nat) : Nat`
- `Ballot` — closed inductive, two constructors (assent, dissent).
- `QuestionKind` — closed inductive: collective; permission carrying
  `designee : Key`.
- `ClosureCause` — closed inductive: tally; franchiseChange; proposerDeparted;
  renounced.

## `KelGroups.Vote.State`

- `franchise (gs : VoteState) : List Key`
- `franchiseSize (gs : VoteState) : Nat`
- `isResponsabile (key : Key) (gs : VoteState) : Bool`
- `verdictOf (threshold : Threshold) (gs : VoteState) (question : Question) : Verdict`
  — the only place a verdict is decided; takes the threshold explicitly (R-46)
  and dispatches on `QuestionKind` so a permission verdict cannot reach the
  tally comparison (R-64).
- `lookupQuestion (questionId : QuestionId) (gs : VoteState) : Option Question`

## `KelGroups.Vote.Event`

- `VoteEvent` — closed inductive with exactly these constructors:
  `openQuestion (questionId) (kind) `,
  `cast (questionId) (ballot)`,
  `renounce (questionId)`,
  `admitMember (key) (email) (roles)`,
  `removeMember (key)`,
  `setRoles (key) (roles)`.
  The signer is supplied separately by the fold, matching Slice 1's
  `(Key × GroupEvent α)` shape. `admitMember` carries no question id and no
  threshold — that absence is R-66.

## `KelGroups.Vote.Validate`

- `VoteError` — closed inductive; distinct constructors at minimum for: caster
  is not a responsabile; question not found; caster is not the designee of a
  permission question; renouncer is not the proposer.
- `validateVoteEvent (threshold : Threshold) (gs : VoteState) (signer : Key) (event : VoteEvent) : Except VoteError Unit`

## `KelGroups.Vote.Fold`

- `placeBallot (voter : Key) (ballot : Ballot) (question : Question) : Question`
  — the one-position-per-responsabile placement; establishes VC-1 (R-56).
- `sweepClosures (threshold : Threshold) (gs : VoteState) : VoteState`
  — evaluates **every** open question and closes those with a verdict. Called
  by `applyVoteEvent` on every branch without exception; that unconditional
  call is R-51, and a branch that skips it is the mutation R-70 must redden.
- `closeProposerQuestions (cause : ClosureCause) (proposer : Key) (gs : VoteState) : VoteState`
  — R-59/R-60; every closure it writes carries the negative verdict.
- `applyVoteEvent (threshold : Threshold) (gs : VoteState) (signer : Key) (event : VoteEvent) : VoteState`
- `foldVote (threshold : Threshold) (events : List (Key × VoteEvent)) : VoteState`
  — the production fold every theorem and witness in R-68/R-69 is stated over.

## `KelGroups.Vote.Invariants`

Theorem names are contractual; each discharges the named requirement.

- `ballots_nodup_disjoint` — VC-1 / R-57.
- `open_questions_are_open` — VC-4 / R-52.
- `questions_partition` — VC-3 / R-61.
- `closure_of_departure_is_negative` — R-60.
- `permission_ignores_threshold` — R-64, stated so that `verdictOf` on a
  permission question is independent of the threshold argument and of the
  franchise size.
- `no_expiry` — R-54, over an event whose hypotheses say it touches neither the
  ballots of the question, nor the franchise, nor the proposer's standing.
- `admission_opens_no_question` — R-66/R-67.
- `foldVote_wellFormed` — the well-formedness carrier all of the above stand on.

## `KelGroups.Vote.Tests`

Executed witnesses, elaboration-time:

- `tiePassesUnderLegacyThreshold` — R-48a.
- `zeroThresholdPassesWithNoBallot` — R-48b.
- `departureCarriesStaleAssents` — R-53.
- `admissionIsImmediate` — R-66.
- plus one point test per distinct `VoteError` constructor.
