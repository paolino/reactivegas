# #66 S5 — statement completeness of the Lean specification

Issue: #66. Base: `d9a479c` (master after #81 refund on negative closure).
S3 (merged in #98) published, per theorem, which production mutant breaks it.
S5 acts on what that ledger and the inversion audit found: theorems whose
statements are weaker than the behaviour they describe.

Each slice below is its own pull request, in this order: exact inversions,
observed refusals, question retention, statement triage, mirror auxiliaries.
Finite-history correspondence is not part of S5: it waits for #75 and #71.

## User stories

- **exact-inversions-story** As a maintainer, every successful-step inversion
  theorem states the complete guard and the exact successor of its event, and
  the audit proves this for every event constructor the model has, not for a
  hand-picked list, so adding an event or dropping a guard from a conclusion
  fails CI.
- **observed-refusals-story** As a maintainer, every refusal the model can
  return is observed by some public statement, so weakening the guard that
  emits it breaks a theorem.
- **question-retention-story** As a maintainer, a vote question open before a
  successful event is still open, or closed and kept in the closed record,
  after it — on every route other than the proposer-departure closures #81
  owns.
- **statement-triage-story** As a maintainer, each theorem that no mutant of
  its own statement closure can break is either strengthened until one can, or
  carries a recorded reason why its weak statement is the intended one.
- **mirror-auxiliaries-story** As a maintainer, the mirror checker ignores
  compiler-generated auxiliary declarations by their kind, not by name
  patterns, and a negative control shows it still fails on a real gap.

## Requirements

| name | requirement |
|---|---|
| every-inversion-exact | For every `Reactivegas.Event` constructor, the inversion theorem the coverage audit binds to it has a conclusion that is sufficient for the successful step: the audit builds and proves the converse for each one. |
| tightness-over-discovered-extent | The set of inversions held to the converse is the set the audit discovers from the elaborated environment (one per constructor). No hand list of names selects it. |
| live-guard-conjuncts-stated | Every guard conjunct of a step branch appears in its inversion's conclusion; in particular `stalled s` being false for pledge, accept-pledge and close-purchase. Grant and deny, rewritten by #76, are held to the same converse. |
| tightness-negative-control | `--negative-control` additionally shows the converse check fails when a conclusion omits a live guard conjunct, on a constructor the former hand list did not name. |
| refusal-observed | Each of the six refusal constructors published as SURVIVED (`ProductionError.comuneReserved`; `ValidationError.bootstrapRequiresAdmin`, `memberNotFound`, `roleAddPrecondition`, `roleRemovePrecondition`, `invalidKey`) is the result of its entry point exactly when its guard fails, by a new theorem, so its guard mutant is KILLED; or the ledger records why no public statement can observe it. |
| question-retained | A new theorem: for every successful event other than the proposer-departure closures, every question open in the state before is open, or closed and present in `VoteState.closed`, in the state after. A route that drops a question is reported as a counterexample, not fixed. |
| unconstrained-triaged | Every OPEN ledger row whose reason is `unconstrained` has one disposition: strengthened (its row becomes KILLED) or a recorded reason the weak statement is intended. `masked` and `inexpressible` rows are published as they are. |
| mirror-auxiliaries-by-kind | `scripts/check-lean-mirrors` excludes compiler auxiliaries (`_sunfold`, `_unsafe_rec` and their kind) by declaration kind, and its negative control still fails on a real mirror gap. |
| ledger-current | After each slice, `lean/REACTIVEGAS-MUTANTS.md` and `lean/KELGROUPS-MUTANTS.md` are re-derived and `just lean-mutants` passes on the pushed head. |

## Fences

- No change to the Lean model (`Types`, `State`, `Step`, `Predicates`,
  `Lifecycle`, `Composition*` definitions, KelGroups definitions) or to
  production Haskell.
- No new antecedent on an existing theorem. A conclusion may be strengthened;
  a hypothesis may not change. Every consumer of a strengthened theorem still
  elaborates.
- `docs/en/design` is written only by #71.
- No existing statement is weakened.

## Rejection behaviour

- A conclusion missing a guard conjunct, or naming the wrong successor, fails
  `just lean` with the theorem's name.
- A ledger rendering that differs from the re-derived ledger fails
  `just lean-mutants`.
