# Spec — #62 one membership and role model

Issue: https://github.com/paolino/reactivegas/issues/62 (parent #43)

Authoritative inputs:

- issue body SHA-256 `540407ea7e784afce3a0e56ac49e9068a713f84627abcd3a47e1f627ade9d673`;
- A-009 SHA-256 `f99ee165d3cda955adc0515c8a2c3fbca0456adb897b8411b261c033aaf02303`;
- approved draft SHA-256 `1c30e0976a74edf2f1427fcd898f3803663936a600ef5741ea49dae9e9f06160`;
- discovery report SHA-256 `9c843ffc875adf6fede32a5bf8582a57c424a4347404f8b6c877762ff7c53d95`.

Artifact ceiling: 240 lines / 18 KiB.

## Observable outcome

A Reactivegas group has one writable membership and role representation,
`KelGroups.GroupState.members`. One current admin can admit a non-reserved
substrate `Key` through one direct base command. Every economic guard, vote
franchise, base cleanup, theorem, and emitted trace observes that same state;
the Reactivegas proposal and app-event types cannot express membership
admission or mutation.

## Requirements

- **R62-01 — canonical ownership.** `GroupState.members` is the sole writable
  membership/role store. The Reactivegas payload and vote payload contain no
  member or role field.
- **R62-02 — payload boundary.** Reactivegas app state owns only `conti`,
  `casse`, `collections`, open vote questions, and closure records. App state
  and app event are distinct types.
- **R62-03 — one identity.** Every member, account, signer, proposer, voter,
  and trace identity is `KelGroups.Key`. `Reactivegas.UserId` and every
  `Nat`/`String` conversion or bridge disappear. `comuneId` is one reserved
  `Key`, present only as an economic account key and never as a member.
- **R62-04 — read-only application context.** The integrated app fold receives
  signer plus canonical pre/post group views and returns only app payload or
  rejection. It cannot return or replace a `GroupState` or members list.
- **R62-05 — historical boundary.** The existing declaration
  `Reactivegas.Composition.baseEnacted_threshold_met` remains byte-identical
  to base `c50f5275`. Its generic fold surface is historical evidence and is
  not the Reactivegas production integration path.
- **R62-06 — direct admission.** Exactly one integrated base command inserts a
  member. It requires a current admin, rejects an existing key, and rejects
  `comuneId`; a non-admin refusal and admin success are production-reachable.
- **R62-07 — structural proposal restriction.** The Reactivegas-specific base
  proposal type contains role change and member removal only. It has no
  admission constructor and no conversion from an unrestricted generic
  proposal. Reintroducing `introduceMember` must break an exhaustive boundary.
- **R62-08 — closed app vocabulary.** `Reactivegas.Event` has no `addUser`,
  `electResponsabile`, `removeResponsabile`, or `removeMember`. Signer is the
  sole author identity; event payloads do not duplicate it.
- **R62-09 — atomic base consequences.** Every admitted base member/role
  transition invokes one sealed post-base hook inside the same integrated
  transition. The hook receives pre/post group views and returns app payload
  or rejection; there is no separately signable cleanup or sweep event.
- **R62-10 — economic cleanup.** Member departure absorbs that member's conto.
  Loss of admin status applies the existing cassa/collection/refund cleanup.
  These effects are derived from the real base pre/post transition.
- **R62-11 — vote recomputation.** Vote state has no members field and vote
  events have no member/role events. Vote authorization, franchise, threshold,
  and verdict read the canonical view. Every real base member/role transition
  recomputes open questions against the post-state franchise.
- **R62-12 — exhaustive routing.** Integrated app, direct-base,
  Reactivegas-proposal, base-change, and verdict sums have wildcard-free
  production classifications. A new constructor cannot compile or pass the
  gate until classified. The fourteen surviving economic constructors retain
  their accepted #54 producer classes: eleven direct and three app-decided;
  only the four retired membership/role constructors leave that table.
- **R62-13 — economic quantification.** Membership-scoped guards,
  `backdonate` cardinality/distribution, `solvent`, `insolvent`,
  `canCloseGroup`, boot reachability, and comune exclusion quantify the
  canonical view only.
- **R62-14 — integrated evidence.** A new theorem binds concrete successful
  role-change/member-removal base transitions to the corresponding app hook
  consequences. A route label, unjoined hypothesis, or record literal is not
  sufficient.
- **R62-15 — inherited vote contract.** All #57 rows R57-01…R57-10 and the
  inherited `PARTITION`, `DISJOINT`, `NOSTALE`, `FRANCHISE`, and `POLICYFREE`
  properties are restated over the integrated production path and freshly
  falsified. V-3 is reached by a base transition with unchanged tallies and no
  vote-local member event.
- **R62-16 — observable and trusted closure.** One integrated JSON trace shows
  admin admission, rejected non-admin admission, role change, admin/member
  departure, all economic cleanup effects, and franchise-only verdict
  closure. All load-bearing statements are mutation-bound, have production
  witnesses, print allowed axioms only, and pass clean full CI with zero proof
  escape hatches.

## Invariants

| ID | Failure meaning | Success meaning |
| --- | --- | --- |
| `INV-62-ONE-STORE` | any writable member/role copy exists outside `GroupState.members` | all consumers read one canonical representation |
| `INV-62-PAYLOAD-ONLY` | an app result can carry or replace base membership/roles | app folds and hooks return payload/rejection only |
| `INV-62-ONE-KEY` | identity crosses a bridge or `comuneId` is admitted | all identity is `Key`; the reserved key is unreachable as a member |
| `INV-62-DIRECT-ONLY` | admission is app-signable, proposal-driven, non-admin, or has a second route | one current-admin direct command is the only insertion path |
| `INV-62-ATOMIC-HOOK` | a successful base change can commit without cleanup or vote recomputation | the group and resulting payload change atomically |
| `INV-62-V3-BASE` | franchise-only closure needs a vote-local member event or new ballot | a real base transition alone changes the verdict |
| `INV-62-CLOSED-SUMS` | a new constructor escapes classification | every relevant sum is exhaustively classified |
| `INV-62-HISTORICAL` | the accepted theorem is weakened, renamed, or rewritten | its declaration is unchanged and a new integrated theorem carries new evidence |
| `INV-62-PROOF-TRUST` | a theorem is vacuous, unreachable, mutation-insensitive, or axiom-tainted | witnesses and mutants bind each claim to the production definition |

## Acceptance-to-gate contract

Each acceptance row has exactly one frozen gate row. The gate row owns both
the positive signal and the named negative control; a RED result for another
reason is not evidence.

| Acceptance | Gate | Failure | Success |
| --- | --- | --- | --- |
| `AC62-01` canonical payload/store | `G62-A-ONE-STORE` | duplicate store or forbidden payload field is found | only `GroupState.members` is writable |
| `AC62-02` duplicate seed | `G62-A-DUP-SEED` | any seeded `users`, `responsabili`, or vote `members` survives the scanner | all three seeds produce their named RED |
| `AC62-03` typed app fold | `G62-A-TYPED-FOLD` | state/event are conflated or signer/views/result type are missing | production signature has distinct types and payload-only result |
| `AC62-04` app preservation | `G62-A-APP-PRESERVES` | an app transition can alter members | definition mutant is RED and production theorem is GREEN |
| `AC62-05` atomic cleanup/sweep | `G62-B-ATOMIC-HOOK` | either post-base consequence can be omitted | both omission mutants are RED on real base transitions |
| `AC62-06` vote-local removal/V-3 | `G62-B-V3-BASE` | vote state/event owns membership or V-3 needs a ballot/member event | field/constructor seeds are RED and the base-only trace closes |
| `AC62-07` economic member-event removal | `G62-B-NO-APP-MEMBERSHIP` | any independently signable role/member event remains | all four constructors are absent and cleanup remains base-reachable |
| `AC62-08` direct admission authority | `G62-B-DIRECT-ADMIT` | a non-admin succeeds or an admin cannot admit | refusal is exact identity and one admin admission is reachable |
| `AC62-09` proposal restriction | `G62-B-NO-INTRODUCE` | Reactivegas can express or translate voted admission | seeded constructor fails at the exhaustive production boundary |
| `AC62-10` no `addUser` compatibility route | `G62-B-NO-ADDUSER` | `addUser` remains or maps to a no-op | constructor and route are absent; admission is base-only |
| `AC62-11` historical plus integrated theorem | `G62-C-THEOREMS` | old declaration drifts or new theorem lacks a concrete transition/hook join | old bytes match base and new theorem/axioms/witness pass |
| `AC62-12` canonical economic guards | `G62-C-ECONOMY` | any named guard/predicate counts a payload-local list | all named properties quantify the canonical view; source mutant is RED |
| `AC62-13` integrated trace | `G62-C-TRACE` | JSON has split state or misses a required transition/effect | replayed corpus exposes one state and every named effect |
| `AC62-14` exhaustive sums | `G62-C-EXHAUSTIVE` | a seeded app/base/proposal/change/verdict constructor escapes | every seed fails at its named classifier |
| `AC62-15` inherited #57 rows | `G62-C-INHERITED57` | any inherited theorem uses the retired vote member path or lacks fresh RED | all rows in `plan.md`'s inheritance matrix pass on the integrated path |
| `AC62-16` proof and CI closure | `G62-C-TRUST-CI` | escape hatch, unapproved axiom, statement drift, unwitnessed claim, or CI failure | zero debt, allowed axioms, mutation receipts, `lake build`, and full CI pass |

## Non-goals

- Voted admission or a direct/voted runtime switch.
- Compatibility no-ops for retired membership events.
- Equality proofs between independently writable membership lists.
- Haskell, coordinator, wasm, browser, simulator, live migration, #47, #48,
  or #59 changes.
- Editing the accepted `baseEnacted_threshold_met` declaration.
- Push, PR, issue/project mutation, readiness, merge, or downstream release.

## Dependency position

Base is exactly merge `c50f5275a42453ebc87a0c7011b3d8470fba4006`.
This ticket precedes #48 final integration, #59 finalization, #47
reconciliation, simulator re-binding, and child 3. #59 stays parked and its
last repair bounce is not consumed.
