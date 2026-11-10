# Lean-replay oracle over the vote corpus, slice O — #70

Authority: [issue 70](https://github.com/paolino/reactivegas/issues/70); desk
NOTE-D08 (2026-11-10) objective O, oracle option B (A-007, deferred until #75;
#75 landed in PR 113). Contract: R3.1 replay-context (#75 spec rows
replay-context, three-outcomes, context-controls). Base: master `72bde8a`.
Model position: Below — the simulator replays what Lean emitted; it changes no
Lean and no corpus byte.
Ceiling: this artifact 90 lines / 6500 bytes. Every row is BLOCKING.

## Stories

- The simulator replays `lean/corpus/vote.json` step by step through its own
  integrated root and agrees with every step Lean recorded.
- The only policy the replay uses is the threshold table in
  `lean/corpus/vote.context.json`; the simulator's own named thresholds play no
  part in it.
- When the replay cannot be trusted, the report says which of three things
  happened: the context was refused before replay, the table's domain ran short
  at a query, or a step disagreed.

## Acceptance

| ID | Must hold |
|---|---|
| O-REPLAY | on the committed `vote.json` + `vote.context.json` the replay passes: for every step, `accepted`, the post-state, `change` and `voteError` equal the corpus's, through the simulator's integrated root (the code the page runs) |
| O-SIDECAR-ONLY | every threshold query the replay makes reads the sidecar's `policies.threshold.table`; no named threshold function of the core is reached during the replay; `label`, `source` and `generatedFrom` have no operative force |
| O-PRE-REPLAY | an unrecognised `schema`/`version`, a `corpus.contentSha256` that is not the SHA-256 of the corpus bytes, a table failing R3.1 §3 rules 1–5, or an unknown `backdonateAuth.semantics` token is refused before any step runs, naming the rule |
| O-ABORT | a threshold query at an `n` outside the table's domain aborts at the point of query, reporting `n` and the domain; no pass is claimed; any steps already checked are reported only as a labelled prefix |
| O-MISMATCH | a disagreeing step is reported as a mismatch naming the step index and the field that differs |
| O-DISTINCT | the three outcomes are distinguishable in the gate output (distinct tags) and none is reported as another |
| O-WITNESS | before the controls, the gate replays the corpus under the `legacyThreshold` table and the `zeroThreshold` table over the same domain and exhibits the first step whose outcome differs at a queried `n >= 1` on a collective question; with none, the gate fails naming the corpus inadequate |
| O-CONTROLS | R3.1 §7 rows 1–9 run inside `just simulator`: 1 none → pass; 2 zero table → mismatch at the witnessed step; 3 digest corrupted → pre-replay; 4 table truncated to the valid prefix `{0…n-1}` below the witnessed `n` → no pre-replay refusal, then abort at exactly `n`; 5 duplicate key, 6 float value, 7 unknown semantics → pre-replay naming the rule; 8 label changed → pass; 9 restored → pass. Rows 2–7 shown failing |
| O-DIVERGENCE | a mismatch on the unmutated committed corpus is a transcription divergence: it goes to the ticket owner as a question, never an edit to make the replay agree |
| O-KEEP | every existing gate and `--selftest` control still passes and still fails where it failed; the page's behaviour under its named threshold is unchanged |
| O-BUDGET | the replay and its controls add at most 20 s to "Run the simulator gates", enforced by the gate |
| O-CI | hosted CI green on every job of the pushed head, including "Run the simulator gates" and the mutant ledger |
| O-PREVIEW | the PR preview job is green and the live preview bytes equal the tracked `economics-simulator.html` |

## Invariants

| ID | Observable truth |
|---|---|
| INV-O-ONE-ROOT | the replay runs the integrated root the page embeds; no second transition is written for the oracle |
| INV-O-CANFAIL | each failing control is a mutation of the bytes the replay reads (corpus or sidecar copy), run in-process, and is shown failing at RED |
| INV-O-POINT-OF-QUERY | the domain check happens where the threshold is queried; nothing pre-scans states to predict queried sizes |

## Non-goals

- NG-1 any file under `lean/` (the corpus and sidecar are read only); Haskell; Voci.
- NG-2 `economic.json`/`integrated.json` replay (no sidecar, #75 residual).
- NG-3 page display of the oracle (a gate, not a page feature); the pending/accepted display is slice D.
