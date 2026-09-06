# Plan — #68 proposer-is-not-an-assent

## Strategy

Single OWNER slice (S1): one semantic change across two parallel paths plus
proofs. No bisect-safe split exists — the historical fold and the integrated
fold must move together ("never repair only the historical fold"), and the
WellFormed restatement crosses both. Topology OWNER (not LIGHT): theorem
restatement and proof need semantic judgment no executable gate alone entails.

## Ordered steps

1. Planning (this seat, no builds): spec/plan/tasks, gate v1 (ruling-certain
   rows), Q-001 to desk. Planning commit (no production). — in progress.
2. A-001 received (option A bounded to V-2; B kept as negative witness).
   Awaiting implementation release (simulator determinism campaign in
   flight; held, not poked).
3. Freeze gate v3 (A-001 witness rows via fenced owner oracle
   specs/68-proposer-assent/witness-t68.lean + worker `#guard`s); falsify v1/v2 per class (costs
   builds; budget 6/submission, base-RED run counts openly).
4. Dispatch muse commit owner (mode=OWNER, draft=NONE) with hash-bound packet.
5. Fresh codex auditor (gpt-6-astra/high) in isolated read-only snapshot per
   submission; at most one repair bounce, then fresh re-audit.
6. Accept → task stamp → quiet final verification → push exact SHA → draft PR
   → CI green → design-content handoff → merge-permission request.

## Affected extent (discovered, read-only; reconciled finally from fresh build)

| Path | Change |
|---|---|
| lean/KelGroups/Fold.lean | `applyProposeDetailed` empty-open; approve bar/guard per A-001 |
| lean/KelGroups/Integration.lean | `.propose` empty-open into `pendingBase`; `.approve` bar/guard; `tryEnactBase` per A-001 |
| lean/KelGroups/State.lean | doc/type touch only if WellFormed moves; `majority` UNTOUCHED |
| lean/KelGroups/Validate.lean | approval refusal for self-approval above n=1 (option A): NEW error variant (NOT alreadyApproved) in `validateApproval` + `validateBaseApproval` |
| lean/KelGroups/Invariants.lean | `PendingWellFormed` restatement; `proposer_mem_approvals` → ruled replacement; threshold evidence; fold preservation proofs |
| lean/Reactivegas/Invariants.lean | wrappers; corpus emitters if approvals shape anion observations |
| lean/KelGroups/Tests.lean | proposer-approval expectations rewritten; NEW worker `#guard` theorems mirroring the oracle scenarios (R68-09) |
| lean/KelGroups/Trace*, CorpusGate | reconcile from fresh build; adopt, never weaken |
| specs/54-lean-vote-machine/* | VI-2 docs updated ONLY if they describe the base channel (vote machine itself untouched) |

## Constraints

No heavy builds and no child dispatch before desk release. One membership
store; direct-only admission; sealed hook; economic semantics; vote threshold
untouched; no docs writes. Auditor family/codex-pin fixed. 6-build budget per
submission. Never merge on stale base; re-verify base at acceptance.

## Risks

- Q-001 option C-adjacent ruling → replan (bounded: only witness rows move).
- #66 S1 inversion repair shifting shared files → rebase via git skill, never
  force; reconcile, never revert others' edits.
- Corpus freeze (#74) waiting on this slice → keep handoff cited and exact.
