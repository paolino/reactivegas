# #66 S3 — theorem-keyed mutant ledger and LEAN-CLARITY record

Issue: #66. Base: `890a74f` (origin/master). Closure-map rows: §2a, §2b, §7.
Previous S3 campaign: terminal, unaccepted (findings F-01, F-02, F-03, F-06,
F-07 and the tautological `setupAndRestoreIncluded`). Nothing from it is
reused as evidence; every kill in this slice is observed fresh in CI on the
pushed head.

## User stories

- **US-1** As a maintainer reading `lean/`, I can see for every authored
  theorem which shipped production definition, when mutated, makes that
  theorem fail — or why no such mutant exists — so "a theorem no mutant
  reddens constrains nothing" is answered per theorem, not as a percentage.
- **US-2** As a maintainer, a mutant only counts as a kill when it fails the
  named theorem for that theorem's own reason, and CI re-proves every claimed
  kill on every change.
- **US-3** As a future Lean reader, `LEAN-CLARITY.md` tells me honestly that
  the historical clarity experiment is void, and lists the known ambiguities
  with evidence.

## Requirements

| id | requirement |
|---|---|
| R-1 | The theorem extent is discovered from the **elaborated environment** of every Lean module the repository's CI builds or elaborates, `private` included. A source-text inventory is not the subject. |
| R-2 | Compiler-generated declarations leave the extent only by one stated rule; every excluded identity is printed. |
| R-3 | Each discovered authored theorem has exactly one ledger row, classified `KILLED`, `HELPER` or `OPEN`. |
| R-4 | `HELPER` is computed: the theorem's **statement** (its type, not its proof) has no production definition in its transitive constant closure. It is never hand-asserted. |
| R-5 | `OPEN` carries a per-identity reason and the identities of the admitted mutants attempted against it that did not kill it. An `OPEN` row with no attempted mutant is invalid unless its reason states why no single-atom production mutant is expressible. |
| R-6 | A mutant is one tracked patch changing exactly one production definition (not a theorem, test, fixture, oracle or checker). Under the mutant, the mutated definition itself elaborates without error. |
| R-7 | "Mutant *m* kills theorem *t*" holds only when, under *m*, Lean reports an error positioned inside *t*'s declaration span **and** the definition *m* mutates lies in *t*'s statement closure. Syntax, import, setup and harness failures never count. |
| R-8 | On the unmutated head, no error lies in any ledger theorem's span (baseline control). |
| R-9 | Every constructor of every refusal vocabulary — `GuardId`, `KelGroups.Vote.VoteError`, `KelGroups.ValidationError`, and any further error type an entry-point validator returns, discovered from the compiled environment — has at least one mutant on the guard that emits it. A guard mutant that kills nothing is published as `SURVIVED`, an assurance gap, by identity. |
| R-10 | Every check the runner performs has an executable negative control in CI that makes it fail for its own reason: a semantically neutral decoy mutant yields zero kills; a fabricated kill claim fails; a theorem without a row fails; a row naming an absent theorem fails; a stale rendered ledger fails. |
| R-11 | After the mutant run the working tree is byte-identical to the checked-out head, and CI asserts it. |
| R-12 | `lean/REACTIVEGAS-MUTANTS.md` and `lean/KELGROUPS-MUTANTS.md` are rendered from the machine-readable ledger; CI fails when the committed rendering differs. |
| R-13 | Published counts are by identity: DISCOVERED, EXCLUDED, HELPER, REQUIRED (= authored − HELPER), KILLED, OPEN, guard SURVIVED. The only ratio published is KILLED / REQUIRED. |
| R-14 | `lean/LEAN-CLARITY.md` records the historical experiment as VOID with its reason, the known ambiguities with evidence (at least: the "eighteen identities" / "8/10 split" prose against the 14-constructor guard vocabulary; the `Predicates.lean` design-page path; the `renounce` accept-and-no-op prose against V-5/S-12; OD74-S1-COMMENT at `Trace.lean` above `seedDenyPermissionRefunds`), and the protocol for a future isolated reader. No reconstruction of the void experiment. |
| R-15 | The mutant run executes in CI on every pull request and push to master. |

## Rejection behaviour

Any violation of R-1…R-12 or R-15 makes the CI step exit non-zero with a
message naming the violated rule and the identity. `OPEN` and `SURVIVED`
rows do not fail CI; they are published findings.

## Out of scope / fences

- No theorem statement, proof or production definition changes. The only
  permitted Lean-source edit is the stale doc comment named by OD74-S1-COMMENT.
- No edits to #92-owned mirror-checker paths (`scripts/check-lean-mirrors`
  and the Lean checker driver it runs), to the existing `just lean` recipe
  body, or to `docs/en/design/` (#71).
- S4, S5 and the finite-history correspondence are not discharged here.

## Success

The pushed head's CI is green with the mutant step executing; every
REQUIRED theorem is `KILLED` or `OPEN` with R-5 evidence; the `OPEN` and
`SURVIVED` sets are handed to the desk for per-identity disposition.

## Clarifications during implementation

- **R-9** applies to refusal constructors with a non-empty emitter set computed
  from the elaborated environment. A constructor no production definition emits
  is published as `UNEMITTED` by identity and counted in the summary; it does not
  fail CI. A control shows that an emitted constructor without a guard mutant does
  fail.
- **R-5** — a mutant of a type declaration (structure, inductive, abbreviation)
  is an admissible production mutant under R-6/R-7. An `OPEN` row with no
  attempted mutant is valid only when its computed statement closure contains no
  production definition with a computational body; the validator computes this
  and rejects any other such row.
- **D-3** — the statement closure includes the constructor types (structure
  fields included) of every project inductive it reaches.
