# Modules model — #66 S5

Changed responsibilities only.

| owner | responsibility after S5 | slice |
|---|---|---|
| inversion coverage audit (`scripts/check-reactivegas-inversion-coverage`) | binds one inversion per event constructor and proves the converse of every bound inversion; its negative control covers both missing coverage and a non-exact conclusion | exact-inversions |
| inversion theorems (`lean/Reactivegas/Invariants.lean`) | each conclusion is the complete guard and exact successor of its branch | exact-inversions |
| refusal and retention theorems (new, under `lean/`, outside the model definitions) | observe each surviving refusal; retain open questions | observed-refusals, question-retention |
| mutant ledger renderings (`lean/*-MUTANTS.md`) | re-derived by the existing runner, never hand-edited | every Lean slice |
| mirror checker (`scripts/check-lean-mirrors`) | excludes auxiliaries by declaration kind | mirror-auxiliaries |

Dependency direction is unchanged: the audit and the runner read the
elaborated environment; theorems never read the audit.
