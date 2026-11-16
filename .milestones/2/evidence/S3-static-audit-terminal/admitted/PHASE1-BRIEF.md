# S3 Phase 1 brief — required-extent discovery, receipt inventory, measured costing

**Authorized by desk NOTE-021.** Dispatch on the **accepted, landed S2 base** —
not before. No further desk checkpoint is needed once S2 lands.

**Phase 1 claims no coverage.** It produces the inputs from which phases 2..n are
costed and authorized. **Phases 2..n are unauthorized** and need explicit numeric
authorization from the measured proposal you return.

## Caps and bounds

| | |
|---|---|
| substantive builds | **3, maximum.** For the cost measurement only |
| production / model / theorem statement changes | **none** to the candidate or its sources. The fence is on the *repository*, not on your scratch space — see below |
| coverage claim | **forbidden.** Phase 1 may not assert any theorem row is covered |
| report delivery | **local only.** No gist, no push, no external artifact, no publication |
| seats | you are the commit owner; a fresh independent auditor of another family reads your packet, never `muse` auditing `muse` |

Full contract: `../handoffs/S3-MANDATE.md` (**revision 3** — read it, not the
superseded v1/v2 files beside it).

## What you produce

### P1-A — the required extent, by identity

Two sets **derived at the accepted base**, plus the relation between them.

**D1a, authored theorem inventory**, every identity classified
AUTHORED-STATEMENT / HELPER-FACT / COMPILER-GENERATED, with the classification
**rule** stated and excluded identities **listed**. `private` theorems are in.
The inventory is never quietly shrunk.

**Helper facts are characterized on their own terms.** A lemma about
`assocInsert` or list `Nodup` has no state-machine antecedent and **none is to be
invented** so a reachability column can be filled; its obligation is that its
hypotheses are satisfiable. Any exemption is **per identity with reasoning**.

**D1b, the semantic domain — re-derive it, do not seed from the table.** The
mandate lists entry points for `Reactivegas.Step`, `KelGroups.Fold`,
`KelGroups.Vote.Fold`, `KelGroups.Vote.Validate`, `KelGroups.Validate`,
`KelGroups.Integration` and `Reactivegas.Invariants`. **That table is not an
allowlist.** Its revision-2 form omitted `Integration` and most of `Validate`
entirely, which is exactly why you must derive the entry points **and their
reached hooks** from the sources yourself. **Disagreement with the table is a
finding about the table**, and I want it reported.

**The relation, and this is the part to get right: `REQUIRED-INPUT` is NOT the
Cartesian product.** Derive the **actual semantic ownership** — which theorem
properties a given guard or effect atom genuinely constrains. Every required
atom and every in-scope theorem property needs **relevant** coverage, not every
pairing. One atom may later close several rows, provided each row's
property-specific failure is actually observed. **Do not manufacture unrelated
pairings**; they inflate the denominator and mean nothing.

Historical figures **76, 29, 224, 1213, 1214** are **historical measurements
only** — never seeds, never validators, never bounds. Re-derive.

### P1-B — the receipt inventory

Every existing receipt gets a row binding **subject identity, the actual
mutation, the checker/gate, the fixture/input, the relevant transitive dependency
footprint with evidence, the toolchain, and the command**, then marked:

- **REUSABLE-BOUNDED** — context bound and demonstrably unchanged, usable for its
  stated scope;
- **STALE** — context moved, needs a fresh run;
- **UNUSABLE** — identity or provenance unrecoverable.

**Unchanged subject bytes alone do not preserve a receipt**, and a dependency
footprint must be **evidenced, not asserted**.

Input: the archived ledgers at
`/home/paolino/reactivegas-ms2-runtime-archive/ms2-runtime-20260905-0833.tar.gz`
(6489 files, 43 ledgers), keyed to `INV-` rows with explicit OPEN rows. They are
**input to be re-keyed to theorems**, never evidence to be re-labelled. **A
historical aggregate GREEN closes no theorem row.**

### P1-C — the measured cost model

**Measure; do not assume, and do not claim an arrangement exists without
evidence.** Revision 2 assumed one full rebuild per row without checking; that
assumption is withdrawn and this is its replacement.

**State which kind each measurement is** — full cold build, incremental
production rebuild, proof/check elaboration, or runtime replay. Those differ by
orders of magnitude; report them separately and **never average them together**.

**Isolation is preserved while costing.** Separately admitted single-atom
variants **may** share compilation infrastructure or be scheduled together. But
**several atoms mutated in one subject is one mutant with an ambiguous cause**,
not an independent kill of each — however many rows it appears to touch. A
costing shortcut that breaks isolation is not a saving, it is a false result.

### P1-D — the phase proposal

Numeric ceilings for phases 2..n, **derived from your measurements**, with every
required identity **preserved across phase boundaries** — a phase may defer an
identity, never drop it. Exploration budget **0** unless separately proposed.

## Scratch variants ARE allowed, and are how P1-C gets measured

**Corrected by desk NOTE-022.** The no-production-change fence means **no
candidate or source alteration**. It does **not** forbid **isolated single-atom
scratch variants built to measure actual compilation and check cost** — those are
allowed within the 3-build ceiling, retained as **costing evidence**, and
**never claimed as completed coverage**.

The reason is precise: "Phase 1 runs no mutants" must **not** force you into
timing an unchanged cache hit instead of the cost it proposes to measure. A
measurement of a no-op rebuild is not a measurement of a mutation rebuild, and
reporting the first as the second would make the whole cost model fiction.

So: build the scratch variants you need to get a real number, keep them isolated
from the candidate, retain them as evidence, and **claim nothing about coverage
from them**.

## Two things about mutation to carry forward, since they were wrong in revision 2

Your cost model depends on knowing what counts as a kill, so carry these:

- **A production mutation requires the mutated *definition* to remain
  well-typed — never the theorem meant to fail.** A theorem's proof failing to
  elaborate because the definition no longer has the claimed property **is the
  kill**. Only syntax/import/setup errors and unrelated elaboration failures are
  excluded. Revision 2 said the opposite and would have banned the main Lean
  falsification method.
- **Fixture, checker and production-definition mutations stay distinguishable and
  are never summed.** Only production-definition mutation can kill a theorem row.
  For an `always-true` checker mutant: a surrounding test can and should detect
  it, and **if it survives the required surrounding check, that survival is
  itself evidence of an assurance gap** — but it never shows the altered checker
  can reject.

## Submission

`handoffs/PHASE1-REPORT.md`: P1-A with identities, P1-B with its bindings, P1-C
with each measurement's kind and cost, P1-D as a costed proposal, the build spend
against the 3-build ceiling, and any limit you could not close, honestly stated.

**No coverage claim.** Missing required evidence, where you can already see it,
is recorded as an **owned finding with its owner** — not as a deliverable and not
as a gap to be closed by narrowing the denominator.

Do not park between routine steps.
