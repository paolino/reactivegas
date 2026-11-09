# Random scenario generator, slice G1 — #70

Authority: [issue 70](https://github.com/paolino/reactivegas/issues/70); desk
ruling NOTE-D06 ruling 4; plan approved A-007 (2026-11-09, plan sha256
aebde3f9…). Base: master `5418ca2`. Model position: Below — the generator
drives the JS core and checks laws the Lean proves; it changes no Lean. A
counterexample is a transcription divergence between the JS core and the
Lean; anything that looks like a model defect goes to the ticket owner as a
question before anything changes.
Ceiling: this artifact 100 lines / 7500 bytes. Every row is BLOCKING.

## Stories

- A reviewer runs `just simulator` and a seeded generator plays many
  reachable histories through the production core, checking the proved laws
  after every step.
- A constructor added to the Lean vocabularies is generated without touching
  the generator; one the generator cannot produce fails the gate.
- A transcription slip in the core (a refund short by one, a duplicated
  approval) is found by the generator within the CI budget.

## Acceptance

| ID | Must hold |
|---|---|
| G1-SURFACE | the generator's event surface is the set of constructors discovered from the pinned Lean vocabularies by the same discovery the claim gate uses (one module both import; no copy of the discovery, no hand-written list of event names); the core pin is the derived one (PR 104) |
| G1-COVER | every discovered constructor routed by the simulator is produced and passed through the production core within the CI run; a discovered constructor never produced is RED naming it; the denominator is the discovered extent, never the set the generator observed; constructors the claim gate marks NOT PRESENTED are reported with their reason, not silently skipped |
| G1-REACH | each generated step is drawn from what the current core state enables (members, roles, collections and pledges, open and bound questions, live authorizations); a fixed, stated minority of steps is deliberately invalid (at least: renounce by a non-proposer, ballot by a non-designee, unbound grant, spend of an already spent authorization) and each must be refused by the core with the state unchanged |
| G1-ORACLE | after every step the core's own law checkers (`lawViolations`, `kgLawViolations`, `bgLawViolations`, and any law the core already ties to a proved theorem) report no violation; solvency is asserted only on states the generator reached; each checked law names its Lean theorem through the claim receipt, and a law with no proved theorem is not asserted |
| G1-REPORT | a violation reports seed, step index, the event, the law and its theorem, and the full event prefix as a `reactivegas.trace` v1 envelope, labelled as a transcription divergence |
| G1-DET | the CI run is deterministic: seeded PRNG, a fixed seed set and bounded traces and steps stated in the gate; two runs give identical output |
| G1-BUDGET | the generator's CI run fits 60 s wall inside "Run the simulator gates", the budget is stated in the gate and the gate fails when it is exceeded |
| G1-CANFAIL | `--selftest` controls run by `just simulator`: (a) omission — the generator with one constructor removed is RED naming it; (b) at least two core mutants (refund short by one; approval list duplicated, or proposer counted above one admin) are found within the CI budget; (c) an invalid step the core wrongly accepts is RED |
| G1-WIRING | a new `economics-simulator-generator-gate.mjs` is discovered by `just simulator`'s gate glob; no workflow or justfile change |
| G1-KEEP | every existing gate and `--selftest` control still passes and still fails where it failed |
| G1-CI | hosted CI green on every job of the pushed head, including "Run the simulator gates" and the mutant ledger |
| G1-PREVIEW | the PR preview job is green and the live preview bytes equal the tracked `economics-simulator.html` |

## Invariants

| ID | Observable truth |
|---|---|
| INV-G1-ONE-CORE | the generator calls the production core module the page inlines (the scenario gate's drift check holds); it carries no second transition |
| INV-G1-ONE-SURFACE | constructor discovery exists once and both the claim gate and the generator use it |
| INV-G1-CANFAIL | each control behind G1-COVER, G1-REACH, G1-ORACLE judges the production generator and core, not a copy, and is shown failing at RED |

## Non-goals

- NG-1 shrinking and committing found counterexamples as scenarios (slice G2).
- NG-2 page checks on generated states and the purchase/quota target-swap row (slice G3).
- NG-3 any file under `lean/` (oracle option B deferred, A-007); Haskell; Voci.
