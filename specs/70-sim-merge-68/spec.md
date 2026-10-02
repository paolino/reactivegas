# Simulator on the combined #76 + #68 Lean — #70 S4

Authority: [issue 70](https://github.com/paolino/reactivegas/issues/70); desk
order (2026-10-02) for the #76 combined landing: master `f0ef076` (#68,
proposer assent) merged into the simulator rebind; #76 merges this branch.
Base: `e9e9fd3` (#76's fixture commit on `098ed7a`, the accepted S3 head).
Ceiling: this artifact 90 lines / 6000 bytes. Every row is BLOCKING.

## Problem

Merging master `f0ef076` into `098ed7a` conflicts in the simulator's own
artifacts: `lean/TraceDriverV1.lean` traces B and C (#76 rebind and bind
refusal against #68's added non-proposer approvals and
`proposerSelfApproval` seeds), and every value derived from the Lean (claim
text, claim receipts, pinned blobs, the `LEAN_TRACES_V1` fixture, the
claim gate's `ACCEPTED_CORE`). Either side's value is stale on the merged
tree; each must be regenerated from the merged Lean.

## Acceptance

| ID | Must hold |
|---|---|
| S4-BASE | `e9e9fd3` and master `f0ef076` are ancestors of the head; integration by merge, no rewrite |
| S4-TRACE | traces B and C keep #76's rebind, spends and bind refusal AND carry #68's approvals: every enactment is reached by non-proposer approvals only; #68's proposer self-approval is a refused step; both drivers emit every trace through the production root with no seed refused other than the designated refusal steps |
| S4-REFUSE | the S3 refusals (unbound grant, closure spent twice, bind by a non-proposer or after a ballot, second bind) remain refused steps in the emitted traces |
| S4-FIXTURE | the `LEAN_TRACES_V1` fixture and its sha in the page are re-emitted from the drivers at the merged Lean; the trace gates replay them exactly |
| S4-CORE | the JS core reaches the Lean outcome on every trace step, including #68's zero approvals at creation and refused proposer self-approval, and #76's closure-derived authorizations |
| S4-KEEP | #68's own simulator changes on master (page, UI and teaching gates, scenario JSON) survive the merge unless a merged-Lean outcome requires otherwise, stated in the decision record |
| S4-PIN | claim text, claim receipts and pinned blobs are re-made from the merged Lean and pass the claim gate under the `HEAD` rule; `ACCEPTED_CORE.{commit,tree}` points at this branch's own last Lean commit (the merge) |
| S4-COVER | handler coverage over the derived `AppEvent` extent is green, with no hardcoded count |
| S4-SIMCI | `just simulator` exits 0 on the head |
| S4-LEAN | `just lean` exits 0 on the head |
| S4-CI | CI "Build and check" dispatched on the pushed branch (`workflow_dispatch`) is green |

## Invariants

| ID | Observable truth |
|---|---|
| INV-S4-FOLLOW | the simulator follows the merged Lean: `lean/Reactivegas/**`, `lean/KelGroups/**`, `lean/corpus/**` on the head equal the clean merge of `e9e9fd3` and `f0ef076`; nothing in this slice edits them |
| INV-S4-CANFAIL | each refusal row is judged by a control that fails if the core accepts it |
| INV-S4-NOPR | no PR is opened from this branch and nothing is merged by this lane; #76 merges it |

## Non-goals

- NG-1 #76's and #68's Lean semantics, proofs, Haskell, corpus.
- NG-2 the PR preview (runs on #76's PR after its merge).
- NG-3 deriving `ACCEPTED_CORE` in the claim gate (recorded follow-up).
