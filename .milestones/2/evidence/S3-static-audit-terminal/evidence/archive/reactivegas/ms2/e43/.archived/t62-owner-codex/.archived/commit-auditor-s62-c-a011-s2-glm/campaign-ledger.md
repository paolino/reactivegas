# S62-C A011 submission-2 FINAL audit campaign ledger

- Candidate: `6a56f90115e7624830da769da55a1ce1a3c5f5e3` (tree `911467a2829dfe311a45b3c35d116c8577a85142`)
- Rejected parent: `b90161cffb478db0855e81e0bc3ab23818bba161`
- Carried from: `.archived/commit-auditor-s62-c-a011-s1-codex-r1/campaign-ledger.md`
  SHA-256 `53e5bee359d31919a4c9f3ee343548bdfb82cf2b57935d12c63db3f3fa292896`
- Severity: all seven rows `BLOCKING`; `RESIDUAL` forbidden
- Builds after this audit: `23/40`; this audit `3/3` (`cold,warm,warm`); ceiling raises `0/2`

| Row | Verdict | Terminal state | Bound evidence |
| --- | --- | --- | --- |
| `G62-C-THEOREMS` | PASS | `KILLED` (carried) | Historical declaration blob `ab9b4aadb52fbbcdb62bb8de39f62acbc76f0ffbfa4c8eeb5d1d79f6fff334f4` byte-identical base→candidate; `integrated_theorem_witness_holds` prints `[propext]`; fresh slice + ticket gates pass. Not reopened. |
| `G62-C-ECONOMY` | PASS | `KILLED` (carried) | `canonical_economy_holds` prints `[propext]`; `Predicates.lean` and all economy surfaces byte-identical rejected→candidate (repair delta is one file). Not reopened. |
| `G62-C-EXHAUSTIVE` | PASS | `KILLED` (carried) | `exhaustive_inventories_hold` prints `[propext]`; inventory defs untouched by the repair delta; fresh ticket gate executes constructor seeds. Not reopened. |
| `G62-C-TRUST-CI` | PASS | `KILLED` (carried) | Fresh escape-hatch scan zero hits; all printed axiom sets within allowed `{propext}` (fresh probe); frozen gate hashes verified before and after; full CI green twice fresh. Not reopened. |
| `I57-01-BOUNDARY` | PASS | `KILLED` | Fresh instrument `evidence/probe-main2.log`: production `voteApply` admits (`true`), shipped `voteApplyDuplicate` reaches a second `validateVoteEvent` on the same signer/event and fails (`true`), bypass really admits and `checkVoteApplyBypassCaught=true`; the shipped check pattern goes red for a duplicate-production and for the submission-1 pure-duplicate shape; `checkI57Boundary` is bound by `i57_boundary_holds := by decide`, prints `[propext]`. |
| `G62-C-INHERITED57` | PASS | `KILLED` (carried) | `i57_disjoint_holds`, `i57_disjoint_mutant_caught`, `i57_franchise_mutant_caught`, `i57_policyfree_mutant_caught` all print `[propext]`; the I57-06 franchise/policyfree/disjoint sections are outside every repair hunk; `KelGroups/Vote/Invariants.lean` untouched. Not reopened. |
| `G62-C-TRACE` | FAIL | `BLOCKED` | Fresh instrument `evidence/probe-main2.log`: the repaired machinery is evaluator-correct — `checkIntegratedCorpus=true`, real `Lean.fromJson?` decode of the full emitter (`decodedLength=7`), omitted-state emitter dies, corrupted stored coordinate dies, all 7 typed mutants die through real `Lean.toJson`, decoded coordinates non-degenerate. But `git grep checkIntegratedCorpus` at the candidate has exactly one hit: the definition. No theorem, `#eval`, caller, gate row, or CI step evaluates it (`just ci` = compile only; both gates are textual + `just ci`). `evidence/probe-decide.log`: `by decide` cannot reduce the check ("reduction got stuck"), and `native_decide` is forbidden by the repo's own escape-hatch scan. The repair removed the only two prior bindings (`theorem integrated_corpus_holds`; the `checkIntegratedTheoremWitness` conjunct) and shipped no replacement, so no permanent red exists for this row's class. Exact blocking fact: the control is not kernel-decidable, and no lawful permanent-binding surface (decide theorem, gate execution hook, `#eval` red mechanism) exists within the frozen gate and forbidden-hatch fences of this campaign. |

Campaign is `CLOSED` at submission `2/2` FINAL: `rows=7 killed=6 residual=0 blocked=1 open=0`.
A blocking row did not terminate `KILLED`; per the auditor brief any finding ends this
campaign: `campaign=ENDED-RECUT-REQUIRED`. No repair bounce, residual, open row,
ceiling raise, or third submission exists.
