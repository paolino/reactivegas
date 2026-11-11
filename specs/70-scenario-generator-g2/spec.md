# Random scenario generator, slice G2: shrinking — #70

Authority: [issue 70](https://github.com/paolino/reactivegas/issues/70); desk
ruling NOTE-D06 ruling 4; plan approved A-007 (2026-11-09, plan sha256
aebde3f9…), slice G2. Base: master `ccadf8b`. Model position: Below — a shrunk
counterexample is a transcription divergence between the JS core and the
Lean; one that looks like a model defect goes to the ticket owner as a
question before anything changes.
Ceiling: this artifact 90 lines / 6500 bytes. Every row is BLOCKING.

## Stories

- When the generator finds a law violation, the reviewer gets the shortest
  history it can find that still violates the same law, as a scenario file
  the suite already knows how to replay.
- Committing that file makes the counterexample a permanent suite scenario.

## Acceptance

| ID | Must hold |
|---|---|
| G2-SHRINK | on a violation the generator shrinks the event sequence: it removes steps (chunks, then single steps) and then simplifies event arguments, keeping a candidate only when it is still reachable through the production core and still violates the same law at its last step; the result is a local minimum under those moves |
| G2-SAME-LAW | a shrink candidate that violates a different law, or none, or is refused at a step the original applied, is rejected |
| G2-EMIT | the shrunk history is emitted as a `reactivegas.trace` v1 envelope that `economics-simulator-scenario-gate.mjs` loads and replays unchanged; the report names seed, original length, shrunk length, law and theorem |
| G2-SCENARIO | the emitted envelope is accepted by the scenario gate's own loader and replay (exercised in `--selftest` on a mutant counterexample written to a scratch directory); the scenario gate needs no new format; nothing is committed to `economics-simulator-scenarios/` in this slice — a counterexample from the unmutated core is a question to the ticket owner first |
| G2-DET | shrinking is deterministic for a given seed and violation |
| G2-BUDGET | the generator gate, including any shrinking its own controls run, still fits the 60 s budget inside "Run the simulator gates", enforced by the gate |
| G2-CANFAIL | `--selftest` controls run by `just simulator`: (a) a planted core mutant's violation shrinks to at most a stated small number of steps, and the shrunk envelope replays to the same law violation; (b) a shrinker that keeps a candidate violating a different law is RED; (c) a shrinker that returns its input unshrunk is RED; (d) an emitted envelope the scenario gate rejects is RED |
| G2-KEEP | every existing gate and `--selftest` control still passes and still fails where it failed; G1's coverage, invalid share, laws and budget unchanged |
| G2-CI | hosted CI green on every job of the pushed head, including "Run the simulator gates" and the mutant ledger |
| G2-PREVIEW | the PR preview job is green and the live preview bytes equal the tracked `economics-simulator.html` |

## Invariants

| ID | Observable truth |
|---|---|
| INV-G2-ONE-CORE | shrinking replays candidates through the production core only; no second transition |
| INV-G2-NO-FALSE-SCENARIO | no counterexample is committed as a scenario unless the unmutated core produces it; a mutant-only counterexample lives in the selftest, not the suite |

## Non-goals

- NG-1 page checks on generated states and the purchase/quota target-swap row (slice G3).
- NG-3 `economics-simulator-claim-gate.mjs`, `economics-simulator-core.mjs`, `economics-simulator.html` (#66 PR 103 edits them in parallel).
- NG-2 any file under `lean/` (oracle option B deferred, A-007); Haskell; Voci.
