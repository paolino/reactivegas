# S4-B acceptance instrument — v1, parent-owned

**Author and owner: `claude-opus-5[1m]`, ticket/epic owner `%503`.**
**Not owned by the implementer. Not editable by the driver. Not reconstructible
at audit time.**

**Authored 2026-09-05, after the S4-B seat had journalled START and before any
dependent execution** (child spend at authoring: submissions 0/2, substantive
0/6, targeted 0/60; worktree clean, zero oleans, no edits). **This instrument did
not exist at dispatch — see the chronology correction in `STATUS.md`. Nothing is
backdated.**

Base: `4a6cd87fcbc3e4a536bbc9f240f5efe5704022af`.
Pre-existing tracked contract at base: `39d6aa4e2c0c0170` — that is the *base
contract*, **not** this instrument, and my brief conflated them.

## How to read this

**Requirement and control identities, and their expected failure classes, are
fixed here independently of any candidate.** A candidate does not get to define
what counts as passing.

**Actual declaration spellings are bound mechanically, with explicit versioned
evidence**, at the point the candidate exists — that binding is permitted; a
relaxation of a requirement is not.

## Requirements — fixed in advance

| id | requirement | expected failure class |
|---|---|---|
| R1 | finite mirror and correspondence obligations only; existing statements and definitions preserved | any edit to an existing statement or definition outside the fence |
| R2 | no new runtime monitor, no coordinator behaviour | a new production call site, guard, or monitor invocation |
| R3 | new modules only — `Reactivegas.Mirrors`, `KelGroups.Mirrors`, optionally `KelGroups.Vote.Mirrors` | a mirror or theorem placed in an existing module |
| R4 | **P01 and P07 existing expressions RELATED, not duplicated** | a new Bool duplicating an existing expression's computation |
| R5 | generic equality assumptions appear **only** in NEW counterpart/correctness statements; **no original theorem weakened** | an added hypothesis, or a changed statement, on any pre-existing theorem |
| R6 | threshold is a **callable policy parameter** | a chosen default, or a threshold whose equality must be decided |
| R7 | source → compiled inventory → counterpart/correctness relation reconciles **nonzero and per identity**, exceptions **separately named** | a count presented without identities; today's 23 presented as discovery; an unnamed exception |
| R8 | a newly introduced owned predicate with **counterpart absent** is detected **through the actual mandatory path** | the mandatory path passes; or detection only via a leaf probe |
| R9 | same, with **theorem absent** | as R8 |
| R10 | **checker-disable control** — disabling the checker is detected | disabling it leaves the mandatory path green |
| R11 | **per-identity falsification control**: intended definition **well-typed**, dependent theorem **fails to elaborate** | a control whose definition fails to compile; a module-level control standing in for identities |
| R12 | **no first-failure masking**; distinct mutations, observations and commands reported separately | one observed failure credited to several identities |
| R13 | proof-axiom and totality checks at the **actual final tree** | checks taken at an intermediate tree |
| R14 | finite reductions preserve lookup semantics on **arbitrary** states, including **duplicate keys** and **absent-key default balances** | an equivalence true only on well-formed or duplicate-free states |
| R15 | **no unruled well-formedness premise added to ease an equivalence** | a premise not traceable to a ruling, making the theorem narrower |
| R16 | exact fence honoured | any path outside the enumerated list |
| R17 | sibling `justfile` rule — own recipe lines only, no reformatting, no overwriting | any change to a line the lane did not add |
| R18 | spend enumerated against **6 substantive / 60 targeted**, failed and warm counted; **a wrapper that rebuilds the whole module list is substantive regardless of label** | a warm or failed call omitted; a whole build recorded as targeted |

## Controls — identities and expected failure classes, fixed in advance

| id | control | expected observation |
|---|---|---|
| C1 | clean mandatory path with mirrors and checker wired | exit 0 |
| C2 | introduced owned predicate, **counterpart absent** | mandatory path **exits non-zero**, naming the predicate |
| C3 | introduced owned predicate, **theorem absent** | mandatory path **exits non-zero**, naming the predicate |
| C4 | **checker disabled** | mandatory path **exits non-zero** |
| C5..C23 | **one per claimed correspondence identity** — intended definition well-typed, dependent theorem mutated to fail | **elaboration failure naming that identity**, distinct per row |
| C24 | proof-axiom check at final tree | only permitted axioms |
| C25 | totality / panic-string absence at final tree | no `PANIC at` in either stream |
| C26 | restored final full CI | exit 0 |

**A control that cannot fail does not close its row.** **An unexecuted control is
BLOCKED, never killed.**

## Prohibitions

- **No candidate-driven relaxation.** A requirement is not softened because the
  candidate cannot meet it; that is a finding.
- **No driver-owned acceptance edits.** The implementer does not modify this
  instrument. If it needs changing, it asks me.
- **No invisible gate reconstruction at audit time.** This file is the contract
  the auditor reads.
- **No row dropped, no row relabelled to fit.**

## Versioning

**v1.** Any change is a **new version with its own hash**, recorded with the
reason. Prior versions are preserved as evidence.
