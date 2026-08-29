# Data model — #54 Slice 1

Fields, relationships, validation vocabulary, and state invariants. No bodies.
Every row names its Haskell anchor in `/code/kelgroups` at `368b596`.

## Identifiers

| Concept | Shape | Haskell anchor | Note |
|---|---|---|---|
| member key | `String` | `Types.hs` — CESR `Text` key | compared, never parsed, inside the fold |
| email | `String` | `Types.hs` — `memberEmail :: Text` | carried, never interpreted |
| role name | `String` | `Types.hs` — `RoleName = Text` | opaque label |
| proposal id | `String` | `Types.hs` — `ProposalId = Text` | produced by the digest, EP-DIGEST |

These are `String` because the Haskell is `Text`. Abstracting them into type
parameters would be an idealization — the very move this ticket forbids — and it
would cost the decidable equality the point tests rely on. The **only** genuine
type parameter is the application fold type, which is a type parameter in the
Haskell too.

## Roles

- **Admin visibility** is a two-way distinction (public / private). Both are
  admin roles. Visibility affects nothing in the fold or in validation; it is
  carried because the port needs it. Anchor: `Types.hs` `Admin`.
- **Role** is either an admin role carrying a visibility, or an application role
  carrying a role name. Anchor: `Types.hs` `Role`.
- **A role set contains an admin** iff any member of it is an admin role.
  Anchor: `Types.hs` `isAdminRole` / `hasAdmin`.

## Member

| Field | Relationship | Anchor |
|---|---|---|
| key | equals the key it is indexed under (**VI-5**) | `Types.hs` `memberKey` |
| email | free | `Types.hs` `memberEmail` |
| roles | a role set; admin-ness is derived, never stored | `Types.hs` `memberRoles` |

## Group configuration

A mapping from application role names to a pair of predicates over the
application fold value — one licensing addition, one licensing removal. A role
name **absent** from the mapping is permitted in both directions. Anchor:
`Types.hs` `GroupConfig` / `RoleDef`. Carried abstractly: EP-ROLE-PRED.

## Group state (parameterized by the application fold type α)

| Field | Contents | Invariant | Anchor |
|---|---|---|---|
| members | key → member | keys duplicate-free; VI-5 | `State.hs` `members` |
| pending proposals | proposal id → pending proposal | ids duplicate-free | `State.hs` `pendingProposals` |
| app fold | a value of α | opaque to the base system | `State.hs` `appFold` |

The empty state has no members and no pending proposals, and carries a supplied
initial application value. Anchor: `State.hs` `emptyState`.

**α is never instantiated inside `lean/KelGroups/`.** That is the structural
form of R-2.

## Pending proposal

| Field | Contents | Invariant | Anchor |
|---|---|---|---|
| proposal | the proposed change | — | `State.hs` `proposal` |
| proposer | the key that proposed it | is one of the approvers (**VI-2**) | `State.hs` `proposer` |
| approvals | a set of keys | duplicate-free (**VI-1**) | `State.hs` `approvals` |

VI-1 is load-bearing arithmetic, not tidiness: the enactment threshold is
compared against the *cardinality* of this set.

## Derived quantities

| Quantity | Definition | Anchor |
|---|---|---|
| admin count | number of members whose role set contains an admin role | `State.hs` `adminCount` |
| threshold | `(admin count + 1) / 2` on natural division — **ceil(n/2)** | `State.hs` `majority` |
| is-admin | the key is a member and its role set contains an admin role | `State.hs` `isAdmin` |
| is-member | the key is present in members | `State.hs` `isMember` |
| auth mode | bootstrap iff admin count is zero, otherwise normal | `Bootstrap.hs` `authMode` |

Threshold table, which the point tests must pin exactly (**R-9**):

| admins | 0 | 1 | 2 | 3 | 4 | 5 |
|---|---|---|---|---|---|---|
| threshold | 0 | 1 | 1 | 2 | 2 | 3 |

For every **even positive** admin count the threshold is not a strict majority
(`2 × threshold ≤ admins`). Four admins are governed by two. This is a property
of the shipped Haskell and is delivered as a **proved theorem**, so the weakness
is on the record rather than in prose.

## Proposal payloads

Exactly three. Anchor: `Event.hs` `Proposal`.

| Payload | Carries | Enactment effect | Anchor |
|---|---|---|---|
| introduce member | key, email, initial roles | inserts, **overwriting** any existing entry for that key | `Fold.hs` `enact` |
| remove member | key | deletes; **no-op when absent** | `Fold.hs` `enact` |
| change roles | key, new role set | adjusts in place; **no-op when the member is absent** | `Fold.hs` `enact` |

The two no-ops and the overwrite are the awkward cases. They are faithful and
must survive review.

## Events

| Event | Carries | Anchor |
|---|---|---|
| base: propose | a proposal | `Event.hs` `Propose` |
| base: approve | a proposal id | `Event.hs` `Approve` |
| application | a value of α, opaque | `Event.hs` `App` |

Every event is folded together with the **signer's key**. The application case
**discards** the signer (R-17). Anchor: `Fold.hs` `applyEvent`.

## Validation error vocabulary

Ten constructors, all observable — the *identity* of the first error is part of
the contract (**R-22**). Anchor: `Validate.hs` `ValidationError`.

`NotAMember`, `NotAnAdmin`, `BootstrapRequiresAdmin`, `MemberAlreadyExists`,
`MemberNotFound`, `ProposalNotFound`, `AlreadyApproved`, `RoleAddPrecondition`,
`RoleRemovePrecondition`, `InvalidKey`.

`MemberNotFound` is reachable from two places in the Haskell, one of which is
**unreachable** because the caller already proved membership: EP-REDUNDANT-LOOKUP.
Model the redundancy; do not tidy it away.

## Key validity

An abstract predicate on keys (EP-CESR). The Haskell decodes CESR and demands an
Ed25519 public key; the model commits only to *some* decidable predicate, and
the fidelity matrix records that the real one is cryptographic.

## Fidelity matrix rows (R-25)

The delivered matrix must carry one row per entry in every table above, plus one
row per requirement R-6..R-24, plus one row per extension point EP-*, each
labelled `FAITHFUL`, `DIVERGENT`, or `EXTENSION`, and each naming the exact Lean
declaration and Haskell anchor. Rows labelled `DIVERGENT` or `EXTENSION` state
the consequence for the port.

---

# Data — Vote-coverage run (2026-08-29)

Fields, relationships, validation, and state invariants only. No bodies.

## Verdict

Three inhabitants, closed: **positive**, **negative**, **open**. `open` is the
legacy `Indecidibile` and is a distinct constructor, never `negative` plus a
flag and never an `Option` of a two-valued type (R-49).

## Threshold policy

A function from the current responsabile count to the required count. Carried
as an explicit parameter by every verdict evaluation (R-46). Two named
instances ship: the legacy `(n + 1) / 2`, and the `i == 0` policy that is
constantly zero (R-47). Neither is a default.

## Question kind

Either **collective** — decided by tally against the threshold — or
**permission**, carrying exactly one designee key (R-62). The designee is part
of the kind, so a permission question without a designee is not representable.

## Question

| Field | Type | Constraint |
|---|---|---|
| kind | question kind | fixed at opening; never changes |
| proposer | key | a responsabile at opening time |
| assents | list of key | duplicate-free |
| dissents | list of key | duplicate-free |

**No deadline, age, opened-at, or expiry field exists** (R-54). Absence is
load-bearing: the theorem stating a question cannot expire is provable only
because no such field is available to any transition.

State invariants on a question:

- **VC-1** assents and dissents are each duplicate-free and mutually disjoint
  (R-57). Disjointness is the half legacy lost, and it is what restores V-7.
- **VC-2** for a permission question, both lists hold at most the designee and
  nothing else (R-63).

## Closure record

| Field | Type | Meaning |
|---|---|---|
| question id | question id | which question closed |
| question | question | the question as it stood at closure |
| verdict | verdict | `positive` or `negative`; never `open` |
| cause | closure cause | tally, franchise change, proposer departure, or renunciation |

The cause is recorded because R-55 claims exactly three exit routes and R-53
requires the franchise-change route to be observable. A closure with verdict
`open` is not representable.

## Group state

| Field | Type | Constraint |
|---|---|---|
| members | assoc list key→member | key-coherent, keys duplicate-free |
| open | assoc list question-id→question | ids duplicate-free |
| closed | list of closure record | append-only |

State invariants over the whole state:

- **VC-3** *(no silent deletion, R-61)* every question id ever opened appears
  in exactly one of `open` and `closed`. The two partition.
- **VC-4** *(no stale open, R-52)* every question in `open` evaluates to the
  `open` verdict under the **current** franchise and the supplied threshold.
- **VC-5** *(franchise, R-44)* every key in any tally was a responsabile when
  it cast. Note it may no longer be one — V-3 counts recorded tallies against a
  current threshold, which is exactly what makes R-53 reachable.
- **VC-6** *(admission, R-66/R-67)* no closure record's question can add a
  member, and membership grows only by the admission event.

## Relationship to the faithful machine's data

`Member`, `Role`, `Admin`, `hasAdmin`, and the association-list operations are
reused unchanged from `KelGroups.Types`. `PendingProposal`, `Proposal`,
`BaseEvent`, and `GroupState` of the faithful machine are **not** reused and
**not** modified.
