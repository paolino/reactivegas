# Claim gate derives the accepted core pin — #70 S5-sim-a

Authority: [issue 70](https://github.com/paolino/reactivegas/issues/70); desk
ruling NOTE-D06 (2026-11-09), follow-up recorded from desk note D04.
Base: master `d9a479c`. Model position: Below — gate tooling that reads the
Lean; it changes no model semantics and no simulator behaviour.
Ceiling: this artifact 90 lines / 6000 bytes. Every row is BLOCKING.

## Problem

`economics-simulator-claim-gate.mjs` carries `ACCEPTED_CORE` as a literal
commit and tree (`89c78a7`), re-pinned by hand on every branch that changes
`lean/Reactivegas` or `lean/KelGroups`. The event inventory and every
discovered vocabulary are read at that literal. Master has since moved its
Lean (#81 L-7, #66 S3/S4b) and the pin no longer names the last Lean commit,
so a vocabulary added after it would be invisible to the coverage check. #66
S5 lands several Lean-changing PRs; a re-pin on each is churn and drift.

## Stories

- A branch that changes Lean under `lean/Reactivegas` or `lean/KelGroups`
  is judged against its own Lean with no edit to the claim gate.
- A vocabulary constructor added on such a branch without a simulator
  witness fails the claim gate, as today.
- A core pin the checkout cannot resolve, or that is not an ancestor of
  `HEAD`, fails loudly with a named message.

## Acceptance

| ID | Must hold |
|---|---|
| S5A-DERIVED | the core pin commit and tree are derived at run time from the history of `HEAD`: the last commit reachable from `HEAD` that changes a path under the vocabulary roots (`lean/Reactivegas`, `lean/KelGroups`); the claim gate source holds no literal commit or tree for the core pin |
| S5A-READ-AT-PIN | the event inventory, the vocabulary discovery and the vote-file control read Lean at the derived pin, through the same functions as today |
| S5A-UNRESOLVABLE | a derivation that yields no commit (no commit touches the roots, truncated history, unresolvable revision) makes the gate exit non-zero with a message naming the core pin and the cause; an executed `--selftest` control shows it |
| S5A-NONANCESTOR | a core pin that is not an ancestor of `HEAD` makes the gate exit non-zero with the existing orphan message; the existing orphan control keeps running and judges the production check |
| S5A-LEANBRANCH | an executed `--selftest` control: on a scratch commit that changes a file under the roots without changing any vocabulary, the gate derives that commit as the pin and passes with the claim gate file unchanged |
| S5A-VOCAB-RED | the existing unhandled-vocabulary control still fails when a constructor without a witness is added at the pin |
| S5A-KEEP | every other existing `--selftest` control still runs and still fails where it failed before; a control whose subject this slice removes is named in the commit message with the reason |
| S5A-COMPOSITION | `ACCEPTED_COMPOSITION` and its rules are unchanged |
| S5A-CI | hosted CI green on every job of the pushed head, including "Run the simulator gates" and the mutant ledger |
| S5A-PREVIEW | the PR preview job is green and the live preview bytes equal the tracked `economics-simulator.html` |

## Invariants

| ID | Observable truth |
|---|---|
| INV-S5A-CANFAIL | each control behind S5A-UNRESOLVABLE, S5A-NONANCESTOR, S5A-LEANBRANCH, S5A-VOCAB-RED runs inside `just simulator` and calls the production derivation and check functions, not a copy |
| INV-S5A-ONE-SOURCE | one derivation function produces the core pin; every reader of the core pin gets it from there |
| INV-S5A-FIDELITY | the simulator is judged against the Lean of the commit under test; nothing in this slice lets a vocabulary at `HEAD` escape the coverage check |
| INV-S5A-SHALLOW | in the CI `pull_request` checkout the history recipe still makes the derived pin resolvable, or the gate fails red |

## Non-goals

- NG-1 any simulator page, core or behaviour change (`economics-simulator.html`, `economics-simulator-core.mjs`).
- NG-2 any file under `lean/` (owned by #66 S5).
- NG-3 the composition pin and the receipt source pins.
