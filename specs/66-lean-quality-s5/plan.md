# Plan — #66 S5

One pull request per slice, in this order. Each slice is re-planned on the
master it starts from, because the earlier slices move the ledger.

| order | slice | what changes | proven by |
|---|---|---|---|
| 1 | exact-inversions | `scripts/check-reactivegas-inversion-coverage` (converse over the discovered extent, extended negative control); conclusions in `lean/Reactivegas/Invariants.lean`; ledger renderings | `just lean`, `just lean-mutants` |
| 2 | observed-refusals | new theorems; ledger renderings | `just lean`, `just lean-mutants` |
| 3 | question-retention | one new theorem; ledger renderings | `just lean`, `just lean-mutants` |
| 4 | statement-triage | strengthened conclusions or recorded reasons, one row per theorem from the census taken at slice start; ledger renderings | `just lean-mutants` |
| 5 | mirror-auxiliaries | `scripts/check-lean-mirrors` and its control | `just lean` |

Not in S5: finite-history correspondence (waits for #75 and #71).

## Constraints

- Lean runs one job at a time per lock slot with a 24 GB cap and no language
  server. The mutant ledger is a long run (about 35–60 minutes locally; hosted
  job timeout 120 minutes, last run 64 minutes).
- Hosted CI is the gate on every pushed head: "Build and check" and
  "Lean mutant ledger".
- Facts on `d9a479c`: 14 event constructors, 14 inversions, 6 held to the
  converse. Ledger: required 442, killed 245, open 197 (unconstrained 85,
  masked 83, inexpressible 29), refusal constructors survived 6.
