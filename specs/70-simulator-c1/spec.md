# Simulator C1 — re-bound to the merged Lean model — #70 / PR94

Authority: [issue 70](https://github.com/paolino/reactivegas/issues/70); operator
layout ruling 2026-09-06 "expand"; rulings V-5 and S-12 as landed in #81.
Base: `origin/master` `2bd9a20` (contains #81 `b131869`, #92 `2bd9a20`).
Candidate inherited: PR94 head `c037bf4` (simulator bytes = `48f76d9`).
Ceiling: this artifact 100 lines / 7000 bytes. Every row is BLOCKING.

## Stories

- The operator opens the PR preview and plays the simulator; every action it
  offers behaves as the merged Lean model behaves on the same input, including
  the #81 renounce/departure closures and the S-12 refusals.
- The simulator never shows an event performed by anyone but its caller.
- With many members and purchases both rings grow on a pannable, scrollable
  canvas; member angles stay put; every control is readable and every purchase
  reachable at the same time.
- A Lean constructor the simulator does not handle is a red check, whatever
  its spelling.

## Acceptance

| ID | Must hold |
|---|---|
| C1-BASE | the branch contains `2bd9a20`; published PR94 history (`c037bf4`) is an ancestor — integration by merge, no rewrite |
| C1-CI | the CI job "Build and check" is green on the pushed head, including `just lean` mirror import reach over `KelTraceDriverV1` and `TraceDriverV1` |
| C1-SIMCI | the simulator gates (build `--check`, claim, trace, vote-trace, scenario, teaching, ui, each with its selftest) run in CI on every push and are green on the head *(CI change in this ticket)* |
| C1-PIN | every tracked Lean source in the claim gate's discovered extent is pinned to its blob at the merged base; every citation resolves |
| C1-81 | for L-1, L-2, L-3, L-4, L-5, L-6, L-6a, L-6b, R-1, R-2 of `specs/81-v5-lifecycle/spec.md`, the simulator core reaches the same outcome as the Lean integrated step on the same input, judged against a Lean-emitted trace; where the page offers renounce or a departure it shows that outcome, and it never shows a refused event as performed |
| C1-F01 | caller identity is bound through decode, validate and replay: an event whose actor differs from its caller is refused, never substituted |
| C1-F02 | "expand": for every reachable member and purchase count in the swept set, including 8, 9, 10 and 103, no two member chips, purchases or controls overlap; member angles are stable as counts grow; the canvas pans and scrolls |
| C1-F03 | handler coverage is derived from the pinned Lean `Event` constructors (the #81 set included), spelling-independent; a constructor without a core handler or a named non-presented reason fails |
| C1-PREVIEW | the PR preview job is green on the pushed head and the live URL serves bytes whose sha256 equals the tracked `economics-simulator.html` at that head |

## Invariants

| ID | Observable truth |
|---|---|
| INV70-FOLLOW | the simulator follows the Lean; it forks no semantics. Where Lean and simulator disagree the simulator changes (Lean→simulator fidelity contract) |
| INV70-CANFAIL | every check behind C1-SIMCI, C1-81, C1-F01..F03 has a negative control executed by the same CI step that rejects a production mutant |
| INV70-EXTENT | coverage and pin checks quantify over the discovered extent, not a list; an empty or truncated extent is red |
| INV70-LEANLOCK | every local Lean/lake build runs under `/tmp/reactivegas/ms2/lean.lock` with the MemoryMax=24G scope |

## Non-goals

- NG-1 L-7 escrow refund presentation (open in the Lean, gated on #76).
- NG-2 the random scenario generator (later #70 slice).
- NG-3 #68 proposer-assent semantics beyond what master's Lean already holds.
- NG-4 any change under `lean/KelGroups`, `lean/Reactivegas` model sources, or
  `docs/en/design` (#71).
