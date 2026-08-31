# Plan — #62 one membership and role model

Artifact ceiling: 210 lines / 16 KiB.

## Fixed decisions

The integrated Reactivegas surface is new and typed; the accepted generic
`AppFold α`, `GroupEvent α`, `applyEventDetailed`, and
`baseEnacted_threshold_met` remain historical. No Reactivegas production root
may call that historical fold. This preserves the accepted theorem without
letting its conflated type shape constrain the replacement.

The replacement uses `GroupState AppState` as the aggregate,
`GroupState.members` as the canonical member/role store, a read-only
`GroupView`, a distinct app event parameter, a Reactivegas proposal type with
no admission constructor, and one direct base admission command. A sealed
base hook updates app payload from canonical pre/post views in the same
transition that changes the group.

The route change is exactly the four accepted membership/role mutations:
`addUser`, `electResponsabile`, `removeResponsabile`, and `removeMember` leave
the app event table. The other fourteen economic constructors keep their #54
classes (eleven direct, three app-decided). Any further reclassification is an
escalation, not an implementation choice.

The broad identity change makes all three slices `OWNER`: semantic review and
fresh independent audit are required. `LIGHT` is ineligible. `draft=NONE`.

## Source horizon and fence

Implementation may change only:

- `lean/KelGroups.lean` and `lean/KelGroups/**/*.lean`;
- `lean/Reactivegas.lean` and `lean/Reactivegas/**/*.lean`;
- task stamps in `specs/62-one-membership-model/tasks.md`.

Haskell, docs, Nix, workflow/dependency files, prior spec directories, and all
other paths are read-only. A new Lean module under either allowed namespace is
permitted only for a responsibility named in `modules-model.md`; it must be
rooted by the corresponding umbrella module. The accepted theorem declaration
is a byte-frozen sub-fence inside the otherwise writable Composition module.

## Slice S62-A — canonical integration boundary

Delivers R62-01…R62-05 and tasks T6210…T6215.

- Introduce the typed integrated group/app contract and canonical `GroupView`.
- Make Reactivegas state a payload with economy plus vote questions/closures,
  with no membership/role field.
- Replace every Reactivegas identity with `KelGroups.Key`; reserve `comuneId`
  in that same type and remove the old identity alias/bridge possibility.
- Make vote state membership-free and make vote observations accept the
  canonical view.
- Keep the historical fold/theorem isolated and unchanged; production import
  and call-graph controls distinguish it from the integrated path.

Slice acceptance is a compiling tree with the new path rooted and the old path
not called by Reactivegas production. Temporary absence of admission on the
new path is allowed until S62-B; a compatibility app event is not.

Frozen gate rows: `G62-A-ONE-STORE`, `G62-A-DUP-SEED`,
`G62-A-TYPED-FOLD`, `G62-A-APP-PRESERVES`.

## Slice S62-B — one transition system

Delivers R62-06…R62-12 and tasks T6220…T6226.

- Add the sole current-admin direct admission command with duplicate and
  reserved-key rejection.
- Close the Reactivegas proposal type over role change/member removal and
  close the app event type over economic/vote actions only.
- Route every successful base member/role transition through the sealed
  cleanup-and-recompute hook before the integrated transition can succeed.
- Remove vote-local member/role events and independently signable Reactivegas
  membership/role events.
- Rebind vote authorization and verdict recomputation to the canonical view;
  demonstrate V-3 from a real base transition without a new ballot.
- Freeze exhaustive app/direct/proposal/base-change/verdict classifications.

Frozen gate rows: `G62-B-ATOMIC-HOOK`, `G62-B-V3-BASE`,
`G62-B-NO-APP-MEMBERSHIP`, `G62-B-DIRECT-ADMIT`,
`G62-B-NO-INTRODUCE`, `G62-B-NO-ADDUSER`.

## Slice S62-C — proof, mutation, trace, and observable closure

Delivers R62-13…R62-16 and tasks T6230…T6236.

- Re-establish economic invariants and member-scoped guards on canonical
  membership.
- Add the new concrete integrated base-to-hook theorem without altering the
  accepted historical theorem.
- Re-prove and re-falsify every inherited #57/#54 row on the new production
  path.
- Emit and replay one integrated state/trace corpus covering every required
  transition, refusal, verdict change, and cleanup effect.
- Close constructor inventories, theorem statements, axioms, mutation
  receipts, proof debt, focused builds, and clean full CI.

Frozen gate rows: `G62-C-THEOREMS`, `G62-C-ECONOMY`, `G62-C-TRACE`,
`G62-C-EXHAUSTIVE`, `G62-C-INHERITED57`, `G62-C-TRUST-CI`.

## Inherited #57 evidence matrix

Prior receipts are historical only. `G62-C-INHERITED57` has one named leg per
row below; every leg runs on `applyIntegratedEvent` or a fold rooted in it and
has a fresh definition mutant.

| Evidence row | Re-bound obligation | Required RED |
| --- | --- | --- |
| `I57-01-BOUNDARY` | one validation decision dominates app effect and base hook | bypass integrated validation |
| `I57-02-EXHAUSTIVE` | authorization is total over the reduced vote event sum | add an unclassified vote event |
| `I57-03-NOOP` | arbitrary rejected signed events preserve the full integrated state | run an effect or hook after rejection |
| `I57-04-AUTH` | after boot, non-admin/non-franchise signers are inert for every remaining vote event | authorize one constructor by default |
| `I57-05-R45` | stranger influence cannot change a verdict | allow a stranger cast on a reachable open question |
| `I57-06-PARTITION` | open and closed question IDs partition all opened IDs | silently delete a question |
| `I57-06-DISJOINT` | assent/dissent are duplicate-free and disjoint | retain a voter on both sides |
| `I57-06-NOSTALE` | every open question is open under the post-transition canonical franchise | omit the base-triggered sweep |
| `I57-06-FRANCHISE` | every tally key was admin at cast time in the canonical view | admit an unfranchised ballot |
| `I57-06-POLICYFREE` | verdict depends only on the supplied threshold at canonical franchise size | hard-code a threshold policy |
| `I57-07-NOEXPIRY` | semantic ballot/franchise/proposer preservation keeps a question open | introduce expiry or weaken the premise |
| `I57-08-TRUST` | contractual statements have allowed axioms and no escape hatch | seed `sorry`/`admit` and theorem-statement drift |
| `I57-09-DIRECTION` | KelGroups remains independent of Reactivegas | seed a `Reactivegas` import below KelGroups |
| `I57-10-TOOLCHAIN` | receipts name the executing Lean and pinned source revision | mismatch the expected runtime identity |

## Ordered evidence per slice

1. Confirm exact base/tree, clean index, gate/manifest hashes, inbox, and the
   baseline CI receipt.
2. Commit the complete slice RED proof bundle and show each frozen gate row
   fails for its named reason.
3. Implement the slice within the source fence; no gate, oracle, or historical
   theorem edits.
4. Run focused Lean build, slice gate, accumulated ticket gate, zero-debt and
   axiom checks, then full repository CI where the slice contract requires it.
5. Submit a clean local candidate to a fresh alternate-family auditor; accept
   only a complete invariant matrix and fresh mutation receipts.
6. Stamp only the accepted slice tasks and mechanically prove the final tree
   equals the audited candidate plus that stamp.

## Proof-trust and constructor controls

- Zero `sorry`, `admit`, `sorryAx`, custom axioms, `native_decide`, and
  `Lean.ofReduceBool` in the ticket Lean scope.
- Contractual theorem statements are hash-bound before audit; names alone do
  not satisfy the gate.
- `#print axioms` permits only `propext`, `Classical.choice`, and `Quot.sound`.
- Each production theorem has an executed reachability witness and a
  definition mutant that makes the theorem or value oracle fail.
- Constructor scanners have positive controls, reject wildcard classifiers,
  and are falsified with seeded app, vote, direct-base, Reactivegas-proposal,
  base-change, and verdict constructors.
- The exact historical theorem block is compared byte-for-byte with
  `c50f5275:lean/Reactivegas/Composition.lean`.

## Commands and resource envelope

Focused readiness and evidence commands are the frozen slice gate, `cd lean &&
lake build`, `nix/lean-dependency-direction.sh`, and `nix develop --quiet -c
just ci`. Expensive Lean mutation builds are capped in the future owner packet;
each named mutant gets one compile/elaboration result and no result is accepted
when it fails for a different reason. Verbose output is stored through the
quiet receipt recorder.

## Stop conditions

Stop and file a question before changing the historical theorem, widening
outside the Lean/spec fence, retaining a second identity or member store,
adding voted admission, weakening structural absence to a runtime guard,
changing threshold policy, editing inherited theorem meaning, or requiring a
downstream ticket/worktree. Two audited submissions are the maximum for each
future OWNER campaign.
