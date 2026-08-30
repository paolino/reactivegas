# Data model — #62 one membership and role model

Artifact ceiling: 150 lines / 11 KiB. Fields, relationships, validation, and
state invariants only.

## Canonical group aggregate

`KelGroups.GroupState AppState` remains the aggregate. Its member relation is
the sole writable association of `Key` to `Member`; `Member.roles` is the sole
writable role assignment. Pending base proposals and `AppState` remain
separate aggregate components.

`KelGroups.GroupView` is an immutable projection of canonical membership and
roles. It contains no app payload and confers no capability to return or
replace a group aggregate. Pre/post views identify the exact base transition
whose consequences an app fold or hook observes.

State invariants:

- member association keys are unique and coherent with `Member.key`;
- `responsabile` is exactly `hasAdmin member.roles` in the canonical view;
- no app/vote state contains a member, role, user, responsabile, or franchise
  field;
- app transitions preserve the aggregate member relation;
- a committed base member/role transition and its app-hook result are one
  integrated result, or the whole transition rejects.

## Identity and reserved comune

All identity-bearing fields use `KelGroups.Key`. There is no Reactivegas
identity alias backed by `Nat`, bridge, conversion table, or paired key.
Collection identifiers remain economic identifiers and are not identities.

`comuneId` is one reserved `Key` in `conti`. It is not in canonical members,
has no roles, cannot sign, vote, propose, or be admitted, and is excluded from
boot and direct-admission reachability. Generic member-scoped account actions
do not apply to it; only the already ruled comune economic effects may touch
its account.

## Reactivegas app payload

The app payload contains exactly:

| Component | Key/value relation | Constraint |
| --- | --- | --- |
| `conti` | `Key` to integer balance | includes reserved `comuneId`; membership-scoped operations quantify canonical members |
| `casse` | `Key` to integer balance | keys acting as responsabili are resolved from canonical roles |
| `collections` | collection identifier to economic collection | participant/referente identities are `Key` |
| vote open questions | question identifier to question | ballots/proposer identities are `Key`; no franchise snapshot |
| vote closures | append-only closure records | each carries verdict/cause and no member snapshot |

No `users`, `responsabili`, `members`, `roles`, cached franchise, or group
state is embedded in this payload.

## Closed transition vocabularies

The direct base-command sum has one constructor: member admission with target
`Key`, email, and initial roles. Signer is supplied separately.

The Reactivegas proposal sum has member removal and role change only. It has
no admission constructor and no generic-proposal escape constructor.

The Reactivegas app-event sum contains only economic actions and app-scoped
vote question/closure actions. It has no member admission/removal or role
election/removal constructor. Event payloads contain targets and economic
parameters only; signer is not duplicated as an author field.

The vote-event sum contains question opening, ballot casting, and
renunciation only. The base-change sum distinguishes admission, member
removal, and role change so exhaustive hooks and route controls cannot ignore
a future substrate membership effect.

## Validation relations

Direct admission relates pre-state, signer, target key, email, and roles to
success or a distinct error. Success requires signer to be a current canonical
admin, target validity, target absence, and target inequality with `comuneId`.
There is no empty-franchise admission exception; boot supplies the founding
admin through the guarded initial-state relation.

Reactivegas proposals are authorized by current canonical admins and validated
against the member/role relation. App events are authorized from signer and
the canonical view. Vote events are authorized from the same view. Each
decision is total and exhaustive over its closed sum.

## Atomic base-to-app relation

A successful direct admission, member removal, or role change yields one
pre/post group-view pair and one base-change value. Before the new aggregate is
observable, the sealed Reactivegas hook must return the corresponding new app
payload.

- member removal absorbs the departing member's own conto;
- loss of admin status applies cassa and open-collection cleanup/refunds;
- every membership/role change recomputes every open question under the post
  canonical franchise, preserving tallies and recording any resulting closure;
- admission has no app-signable mirror and still triggers the post-franchise
  recomputation obligation when its roles affect the franchise.

Omitting cleanup or recomputation is a state-relation violation, not an
event-ordering discrepancy.

## Inherited vote invariants

Open/closed partition, tally disjointness, no stale open question, cast-time
franchise, threshold-policy independence, semantic no-expiry, validation
dominance, complete rejection identity, and non-franchise inertness quantify
the membership-free vote payload together with the canonical group view.
Their reachability carrier is the integrated group fold, not `foldVote` over
retired member events.
