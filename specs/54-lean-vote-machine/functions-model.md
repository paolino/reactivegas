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
