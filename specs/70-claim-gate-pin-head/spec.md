# Claim-gate pins judged against the checkout under test — #70 S2

Authority: [issue 70](https://github.com/paolino/reactivegas/issues/70); desk
ruling on #68's option A (2026-10-02). Base: master `48a2f95`.
Ceiling: this artifact 80 lines / 5000 bytes. Every row is BLOCKING.

## Problem

`economics-simulator-claim-gate.mjs` judges each pin against `origin/master`:
the pin must be an ancestor of `origin/master` and the cited file's blob at the
pin must equal its blob on `origin/master`. A branch that edits a cited Lean
file and re-makes its receipts can therefore never pass, so no Lean-changing PR
(#68, #76, the #81 refund) can reach a green "Run the simulator gates".

## Stories

- A branch that changes a cited Lean file and re-makes the simulator receipts
  against its own Lean passes the claim gate.
- A branch that changes a cited Lean file without re-making the receipts fails
  it, naming the stale file.
- A pin that the checkout under test cannot reach still fails.

## Acceptance

| ID | Must hold |
|---|---|
| S2-REACH | every pin reachability check in the claim gate (source pins and the composition pin) asks "ancestor of `HEAD`", the commit under test, not `origin/master` |
| S2-BLOB | every "blob at the pin equals the current blob" comparison (cited sources, event source, composition module) compares against the blob at `HEAD` |
| S2-WT | the working-tree sha == receipt sha check is unchanged |
| S2-LEANBRANCH | an executed control: on a commit that edits a cited Lean file with receipts re-made against it, the claim gate passes; on the same edit without re-made receipts, it fails naming that file |
| S2-ORPHAN | the orphaned-pin control (a pin not an ancestor of `HEAD`) still fails, for source pins and the composition pin |
| S2-STALE | the stale-receipt control (blob at `HEAD` differs from the receipt) still fails |
| S2-CI | CI "Build and check" green on the pushed head, including "Run the simulator gates" |
| S2-PREVIEW | the PR preview job is green and the live preview bytes equal the tracked `economics-simulator.html` |

## Invariants

| ID | Observable truth |
|---|---|
| INV-S2-MASTER | when `HEAD` is the `origin/master` commit, the new rule accepts and rejects exactly what the old rule did |
| INV-S2-CANFAIL | each control behind S2-LEANBRANCH, S2-ORPHAN, S2-STALE runs inside `just simulator` and judges the production checker functions, not a copy |
| INV-S2-FIDELITY | a Lean-changing branch must carry receipts re-made against its own Lean; nothing in this slice lets a stale receipt pass |
| INV-S2-SHALLOW | the CI history recipe still makes every pin a resolvable ancestor of `HEAD` in the shallow `pull_request` checkout, or fails red |

## Non-goals

- NG-1 any simulator behaviour, page or core change.
- NG-2 any Lean source change.
- NG-3 re-pinning receipts for #68 or #76 (their lanes).
