# Modules model — #66 S3

New or changed responsibilities only. Placement of individual files inside
these owners is the commit owner's.

| id | owner | responsibility | depends on |
|---|---|---|---|
| M-1 | Lean census (Lean code run by `lake env lean`, under `lean/`) | Elaborated-environment discovery: theorem extent, exclusion rule, statement closures, refusal vocabularies, declaration spans. The only producer of these facts. | the built `Reactivegas` and `KelGroups` libraries and CI-elaborated modules |
| M-2 | Mutant catalogue (data under `lean/`) | One entry per mutant: id, target definition, single-atom patch, claimed kills. | M-1 identities |
| M-3 | Mutant ledger (data under `lean/`) | One row per discovered authored theorem with its class and evidence fields (data-model E-3). | M-1, M-2 |
| M-4 | Runner (tracked script under `scripts/`) | Applies each mutant in isolation, captures positioned diagnostics, validates R-6…R-11 against M-1 facts, renders M-5, runs the negative controls. Fail-closed. | M-1, M-2, M-3 |
| M-5 | Renderings `lean/REACTIVEGAS-MUTANTS.md`, `lean/KELGROUPS-MUTANTS.md` | Human ledger, generated, never hand-edited. | M-3 via M-4 |
| M-6 | `lean/LEAN-CLARITY.md` | Clarity record (R-14). Hand-written. | none |
| M-7 | `justfile` recipe + `.github/workflows/ci.yaml` step | Mandatory CI path for M-4 (R-15). New recipe; the existing `lean` recipe body is not edited. | M-4 |

Dependency direction: M-4 consumes M-1 output; M-1 never reads M-2/M-3.
The ledger cannot be its own oracle: kill claims in M-2/M-3 are checked
against diagnostics M-4 observes, and closures M-1 computes.
