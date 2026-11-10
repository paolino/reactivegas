# Plan — #66 S5

One pull request per slice, in this order. Each slice is re-planned on the
master it starts from, because the earlier slices move the ledger.

| order | slice | what changes | proven by |
|---|---|---|---|
| 1 | exact-inversions | `scripts/check-reactivegas-inversion-coverage` (converse over the discovered extent, extended negative control); conclusions in `lean/Reactivegas/Invariants.lean`; ledger renderings | `just lean`, `just lean-mutants` |
| 2 | observed-refusals | new theorems; the converse negative control's rejection reason; mutants on the stall guard; a stale comment in `scripts/check-lean-axioms`; ledger renderings | `just lean`, `just lean-mutants` |
| 3 | question-retention | one new theorem; ledger renderings | `just lean`, `just lean-mutants` |
| 4 | statement-triage | one disposition per row of the census taken at slice start (83 unconstrained rows on `d51d473`); the two refusal sites the observed-refusals audit found uncovered; a recorded-reason field in the ledger; ledger renderings; a measured projection of the hosted ledger job (64, 72, 92 minutes on the last three slices, timeout 120) | `just lean-mutants` |
| 5 | mirror-auxiliaries | `scripts/check-lean-mirrors` and its control | `just lean` |
| 6 | finite-history | a new Lean module outside the corpus exporter's import closure (validators, soundness and witness theorems, negative controls); a hosted step validating `lean/corpus/vote.json`; ledger renderings | `just lean`, the hosted step, `just lean-mutants` |

Slice 6 starts on `72bde8a`, after #75 landed the vote corpus.

## Constraints

- Lean runs one job at a time per lock slot with a 24 GB cap and no language
  server. The mutant ledger is a long run (about 35–60 minutes locally; hosted
  job timeout 120 minutes, last run 64 minutes).
- Hosted CI is the gate on every pushed head: "Build and check" and
  "Lean mutant ledger".
- Facts on `d9a479c`: 14 event constructors, 14 inversions, 6 held to the
  converse. Ledger: required 442, killed 245, open 197 (unconstrained 85,
  masked 83, inexpressible 29), refusal constructors survived 6.

## Simulator claim pins

The simulator claim gate pins source hashes and line citations of Lean files.
A slice that edits a pinned Lean file re-pins those data in the same pull
request (hashes, pin commits, citation lines only; no claim text), and its
merge request says so.
