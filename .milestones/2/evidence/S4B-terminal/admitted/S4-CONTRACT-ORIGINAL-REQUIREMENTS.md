# S4 contract — revision 2

Revised per NOTE-027. v1 preserved at `S4-CONTRACT-v1-superseded.md`; the change
table is at the end. **Runtime preparation only. No build grant here. Not
dispatched.**

## What v1 got wrong, stated first because it shaped the whole taxonomy

v1 collapsed **decidability** and **correspondence** into one axis, and let a
required predicate vanish for want of a current consumer. Both are corrected
below.

- **Possessing `Decidable p` does not prove correspondence** with an
  independently implemented Bool consumer. It proves the `Prop` is decidable.
- **Definitional equality and a proved equivalence are different evidence**, and
  are now different classes.
- **Failure to synthesize a `Decidable` instance does not prove undecidability.**
- **An existing Bool mirror that needs its theorem does not need a second
  mirror.**

## Phase A — classify along two independent axes, and do not force a fit

### Axis 1 — the correspondence status of each `Prop`-valued declaration

| class | what it means | evidence required |
|---|---|---|
| **DEFINITIONAL-IDENTITY** | the `Prop` **is** the Bool equality; the correspondence closes by reduction | the reduction itself (e.g. an `Iff.rfl`), cited |
| **PROVED-EQUIVALENCE** | a theorem relates the `Prop` to an **independently implemented** Bool | that theorem, named |
| **LOGICAL-DECISION-EVIDENCE** | a `Decidable` instance exists | the instance. **This is not correspondence**, and **it is not runtime capability either** — a `Decidable` instance may itself be **noncomputable or classical**. Do not erase this evidence; do not count it as executability |
| **EXECUTABLE-DECISION** | a decision function **compiles and evaluates** | the successful compile/evaluation. This is the only class that establishes a **runtime** decision procedure |
| **MISSING-CORRESPONDENCE — mirror exists** | an independently implemented Bool consumer exists; **no theorem relates them** | the consumer, and the **equivalence theorem owed**. **No second mirror is owed** |
| **MISSING-CORRESPONDENCE — no mirror** | correspondence is required and no executable counterpart exists | the requirement's authority, and both the mirror and the theorem owed |
| **NOT-EXECUTABLE** | no decision procedure is available — `noncomputable`, or otherwise not executable | the **bounded evidence** for that, stated as bounded |
| **NOT-ESTABLISHED** | S4 could not determine the status | what was tried, and what would settle it |

**`UNDECIDABLE` is a claim, not a default.** It may be asserted **only** with a
proved reduction or a cited authority. **`Reach` is not undecidable because of
its name or because reachability is undecidable in general** — this concrete
`Reach` is an inductive predicate over this machine. Absent a proof, it is
**NOT-EXECUTABLE** or **NOT-ESTABLISHED**, with its bounded evidence.

**Do not force a conclusion to fit a box.** If an identity fits none, say so and
say why; the taxonomy serves the finding, not the reverse.

### Axis 2 — the consumer status, decided by authority

Independently of axis 1:

| class | means | decided by |
|---|---|---|
| **REQUIRED-CONSUMER-IMPLEMENTED** | the #66 / `system-design` contract requires an executable predicate here, and one exists | the contract text, cited |
| **REQUIRED-CONSUMER-UNIMPLEMENTED** | the contract requires one and **none exists** | the contract text, cited. **This is a finding, not an absence** |
| **NOT-REQUIRED** | no contract requires an executable counterpart here | the **authority** for that, cited |

**A required executable predicate does not disappear because no current consumer
exists.** v1's `NO-CONSUMER` class permitted exactly that and is withdrawn. The
distinction between **required-but-unimplemented** and **legitimately unneeded**
is settled by **authority — never by developer preference.** v1's phrase "or is
wanted" is withdrawn with it.

### Also in Phase A

Exact affected **source paths** per identity, and how **#68 / #69** changes
require rebind. Where a classification depends on them, record the dependency
rather than classifying around it.

**The historical figures 17 and 23 are measurements of a superseded tree.** Not
seeds, not bounds, not validators. S4 re-derives its own extent.

**Prohibited:** manufacturing decidability, and any mirror quota. The number of
mirrors S4 ships is whatever the MISSING-CORRESPONDENCE classes actually contain.

**Deliverable:** `handoffs/S4-CLASSIFICATION.md` — both axes per identity, with
evidence, source paths and #68/#69 dependencies. No implementation, no coverage
claim.

**Budget:** 3 substantive builds; **≤20 targeted queries/elaborations**,
separately counted, including failed setups.

## Phase B — implement only what Phase A proved is owed

For **MISSING-CORRESPONDENCE — mirror exists**: ship the **equivalence theorem
only**. For **MISSING-CORRESPONDENCE — no mirror**: the mirror and its theorem.
Nothing else.

**Budget proposed after Phase A, from its counts.** Numeric build and query
ceilings both, no "whatever it costs".

## Audit

**Fresh independent auditor from the inherited restricted family set — `codex`
or `grok`. Never `muse`, never `claude`.** Numeric build and targeted-query
ceilings set at commission. It validates **both axes' classification and the
proofs**: a misclassification that moves an identity out of
MISSING-CORRESPONDENCE, or out of REQUIRED-CONSUMER-UNIMPLEMENTED, is as much a
defect as a wrong proof and is easier to miss. Its completeness checks are
**authority-bound** — against the contract text, not against the classification's
own boundaries.

## Bounds

No model change, no theorem statement change, no statement strengthening. `#71`
alone writes `docs/en/design/`. Semantic questions route to the desk. Local-only
delivery; nothing is ever written into a human desk pane.

## Completion

**A classification table with named blocked rows does not finish #66's required
work.** Every REQUIRED-CONSUMER-UNIMPLEMENTED and MISSING-CORRESPONDENCE row
carries an **owned repair or dependency that remains on the milestone completion
map** until discharged. Genuinely open questions are permitted as *outcomes*;
they are not permitted as *closures*.

## Changes from revision 1

| # | v1 | v2 |
|---|---|---|
| 1 | `DEFINITIONALLY-PROVED` merged definitional identity, proved equivalence and `Decidable` availability | three separate classes. **`Decidable` is not correspondence** with an independent Bool consumer |
| 2 | `NEEDS-EQUIVALENCE` undifferentiated | split: **mirror exists** (theorem only) vs **no mirror** (both). No second mirror is ever owed |
| 3 | `UNIMPLEMENTABLE`, with `Reach` named as "the type case" | **`UNDECIDABLE` requires a proved reduction or cited authority.** Otherwise **NOT-EXECUTABLE** or **NOT-ESTABLISHED** with bounded evidence. Failure to synthesize proves nothing |
| 4 | `NO-CONSUMER`, and "or is wanted" | withdrawn. **Axis 2**, decided by **authority**: required-implemented / **required-unimplemented** / not-required. A required predicate cannot vanish for want of a current consumer |
| 5 | four boxes, implicitly exhaustive | **do not force a fit**; NOT-ESTABLISHED is a real outcome |
| 6 | "another family, never `muse`" | **inherited restricted set: `codex` or `grok`**; never `muse`, never `claude` |
| 7 | build budgets only | **numeric targeted-query ceilings** added; audit completeness checks **authority-bound** |
| 8 | ended at a classification plus mirrors | blocked rows **stay on the milestone completion map**; a table does not finish #66 |

## Change in revision 3 (NOTE-028)

`DECISION-AVAILABLE` is split. **A logical `Decidable p` instance can itself be
noncomputable or classical, so its mere existence does not make a runtime
decision procedure executable.** `LOGICAL-DECISION-EVIDENCE` records the
instance — real evidence, not to be erased — while `EXECUTABLE-DECISION`
requires a decision function that **actually compiles and evaluates**. Classical
proof evidence is preserved and is never counted as runtime capability.
