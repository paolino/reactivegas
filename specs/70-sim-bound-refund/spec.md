# Simulator shows bound questions, #68 assent and the #81 L-7 refund — #70 S5-sim-b

Authority: [issue 70](https://github.com/paolino/reactivegas/issues/70); desk
ruling NOTE-D06 (2026-11-09) ruling 3; follow-up recorded from desk answer
A-003 (openBound display). Base: master `bbe9a0a`. Lean semantics: specs/76-runtime-composition,
specs/68-proposer-assent (A-001: proposer is not an assent),
specs/81-v5-lifecycle slice S81-B (L-7). Model position: Below — the
simulator renders the model; it changes no Lean and no semantics.
Ceiling: this artifact 100 lines / 7000 bytes. Every row is BLOCKING.

## Problem

The simulator core already follows #76's `openBound`, bindings and live
authorizations, #68's zero-approval creation and V-5 negative closures, but
the page shows none of the bindings or live authorizations: a reader cannot
see which purchase or quota a question decides, nor the authorization a
closure leaves for a responsabile to spend. The #81 L-7 refund path
(negative closure, then `denyPermission`) is not shown as a story.

## Stories

- A reader sees, on each open question, what it decides (permission for a
  purchase, or a quota), and after it closes, the authorization it left.
- A reader sees a new base proposal open at zero approvals and the proposer's
  own approval refused when the group has more than one admin.
- A reader sees a proposer renounce (or leave) a question bound to a purchase
  holding pledges, and every pledger refunded through `denyPermission`.

## Acceptance

| ID | Must hold |
|---|---|
| S5B-BOUND-SHOWN | every open question opened with `openBound` shows its target in words (permission for purchase `c`, or quota `w`) and its proposer; a question opened with `openQuestion` shows no target |
| S5B-LIVE-SHOWN | every live authorization in the core state is shown with its verdict and target; spending it (`grantPermission`, `denyPermission`, `backdonate`) removes it from the display in the same step; a closure of an unbound question shows none |
| S5B-68 | the page shows a new base proposal with zero approvals; with more than one admin the proposer's own approval is shown refused with the #68 refusal (not "already approved"); with one admin a separate approval by that admin enacts |
| S5B-L7-RENOUNCE | the proposer renounces a question bound to permission on `c` holding accepted and pending pledges: the question shows closed negative with cause renounced, a live negative authorization for `c` is shown, and a responsabile's `denyPermission c` shows each pledger's conto rising by exactly their pledged sum and `c` gone, with no authorization for `c` left |
| S5B-L7-DEPART | the same after the proposer's departure, both cases: proposer is not `c`'s referente (refund through `denyPermission c`), and proposer is `c`'s referente (refunded inside the departure step; no authorization outlives `c`) |
| S5B-REACH | each story above is reachable from the page's own controls, or from a committed scenario the page loads, without editing state by hand |
| S5B-FIDELITY | every shown balance, closure, binding and authorization is read from the core state produced by the core transition the trace gates replay; the page computes none of them itself |
| S5B-KEEP | every existing gate and `--selftest` control still passes and still fails where it failed; the operator layout ruling of 2026-09-06 ("expand": pannable canvas, stable angles, readable controls) holds |
| S5B-CI | hosted CI green on every job of the pushed head, including "Run the simulator gates" and the mutant ledger |
| S5B-PREVIEW | the PR preview job is green and the live preview bytes equal the tracked `economics-simulator.html` |

## Invariants

| ID | Observable truth |
|---|---|
| INV-S5B-CANFAIL | each of S5B-BOUND-SHOWN, S5B-LIVE-SHOWN, S5B-68, S5B-L7-RENOUNCE, S5B-L7-DEPART has a control run by `just simulator` that judges the built page or the core it embeds and fails when the display or the effect is removed, shown failing at RED |
| INV-S5B-NOFORK | no simulator-only rule: where the page and the Lean would disagree, the Lean wins and the question goes to the ticket owner |
| INV-S5B-PINS | Lean citations added to the page go through the claim gate (receipt re-made by its own `--emit-receipt`), never hand-written |

## Non-goals

- NG-1 any file under `lean/` (owned by #66 S5), including the trace drivers; a need for new Lean trace evidence is a question to the ticket owner, not an edit.
- NG-2 the random scenario generator (#70's later slice).
- NG-3 Voci (M3).
