# Simulator re-bound onto #76's vote-derived economics — #70 S3

Authority: [issue 70](https://github.com/paolino/reactivegas/issues/70); desk
order (2026-10-02) answering #76's simulator questions: combined landing — this
branch is merged by #76 into its PR, not opened as a PR of its own.
Base: `76be807` (#76 branch: master `48a2f95` + driver compile edit), with
master `e96af8e` (claim-gate pins judged against `HEAD`) merged in.
Ceiling: this artifact 90 lines / 6000 bytes. Every row is BLOCKING.

## Problem

#76 adds `AppEvent.openBound`, `State.bindings`/`live` and refuses a grant,
deny or backdonation that no closure of a bound question authorizes. Trace B
in `lean/TraceDriverV1.lean` seeds exactly the refused shape (an unbound
`grantPermission`, a `denyPermission` with no closure), so the drivers cannot
emit on #76 and the simulator cannot follow it.

## Acceptance

| ID | Must hold |
|---|---|
| S3-BASE | `76be807` and master `e96af8e` are ancestors of the head; integration by merge, no rewrite |
| S3-TRACEB | every economic effect seeded in the drivers' traces is backed by a closure of a bound question in the same history (a positive bound question before each grant; a negative bound question before each deny); both drivers emit every trace through the production root with no seed refused |
| S3-REFUSE | the traces include #76's refusals as refused steps the simulator must reproduce: an unbound grant, a closure spent twice, a bind by a non-proposer or after a ballot |
| S3-FIXTURE | the `LEAN_TRACES_V1` fixture and its sha in the page are re-emitted from the drivers at this branch's Lean; the trace gates replay them exactly |
| S3-CORE | the JS core reaches the Lean outcome on every trace step: `openBound` binding its target, closure spending once, refusals unchanged state |
| S3-COVER | handler coverage over the derived `AppEvent` extent (now including `openBound`) is green, with no hardcoded count |
| S3-PIN | claim-gate receipts are re-made against this branch's own Lean and pass under the `HEAD` rule |
| S3-SIMCI | `just simulator` exits 0 on the head |
| S3-LEAN | `just lean` exits 0 on the head (the drivers compile and pass the mirror import reach) |
| S3-CI | CI "Build and check" dispatched on the pushed branch (`workflow_dispatch`) is green |

## Invariants

| ID | Observable truth |
|---|---|
| INV-S3-FOLLOW | the simulator follows #76's Lean; nothing in this slice changes a `lean/Reactivegas/**` or `lean/KelGroups/**` source |
| INV-S3-CANFAIL | each refusal row is judged by a control that fails if the core accepts it |
| INV-S3-NOPR | no PR is opened from this branch and nothing is merged by this lane; #76 merges it |

## Non-goals

- NG-1 #76's Lean semantics, proofs, Haskell, corpus.
- NG-2 the PR preview (no PR here; it runs on #76's PR after the merge).
- NG-3 L-7 refund presentation beyond what #76's Lean emits.
