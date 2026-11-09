# Random scenario generator, slice G3: page checks on generated states — #70

Authority: [issue 70](https://github.com/paolino/reactivegas/issues/70); desk
ruling NOTE-D06 ruling 4; plan approved A-007 (2026-11-09, plan sha256
aebde3f9…), slice G3; open row DISPLAY-TARGET-SWAP (A-006). Base: master
`282712b`. Model position: Below — the page shows the core state; it changes no
Lean.
Ceiling: this artifact 90 lines / 6500 bytes. Every row is BLOCKING.

## Stories

- On histories the generator plays, the built page shows each bound
  question's own target and each live authorization as the core state holds
  them, not only on the hand-written scenarios.
- A page that showed a purchase target where the core holds a quota target
  (or the reverse) fails the gate.

## Acceptance

| ID | Must hold |
|---|---|
| G3-PAGE-STATES | a fixed, stated set of generated histories (from G1's seeds) is loaded into the built page through its own loading path; after each loaded step the page's display of open bound questions (target kind and id, proposer), live authorizations (verdict, target) and closed questions (verdict, cause) equals what the core state at that step holds |
| G3-TARGET-SWAP | a display mutant that shows a purchase target as a quota target, or the reverse, is RED; a mutant that shows a stale live authorization after it is spent is RED (closes DISPLAY-TARGET-SWAP) |
| G3-READ-ONLY | the check reads the page; it never computes a target, authorization or closure itself — the expected values are read from the core state the generator produced |
| G3-BUDGET | the page checks fit the existing 60 s generator budget, or a separately stated budget the gate enforces, inside "Run the simulator gates" |
| G3-DEFECT | a mismatch on the unmutated page is a page defect: it goes to the ticket owner as a question, never an edit in this slice |
| G3-KEEP | every existing gate and `--selftest` control still passes and still fails where it failed; G1 and G2 unchanged in coverage, laws, shrinking and budget |
| G3-CI | hosted CI green on every job of the pushed head, including "Run the simulator gates" and the mutant ledger |
| G3-PREVIEW | the PR preview job is green and the live preview bytes equal the tracked `economics-simulator.html` |

## Invariants

| ID | Observable truth |
|---|---|
| INV-G3-CANFAIL | each mutant behind G3-TARGET-SWAP runs inside `just simulator`, mutates the built page the check reads, and is shown failing at RED |
| INV-G3-ONE-CORE | expected values come from the production core through the generator; no second transition |

## Non-goals

- NG-1 any edit to `economics-simulator.html`, `economics-simulator-core.mjs`, `economics-simulator-claim-gate.mjs` (#66 PR 103 edits them in parallel); a page defect is a question.
- NG-2 any file under `lean/` (oracle option B deferred, A-007); Haskell; Voci.
