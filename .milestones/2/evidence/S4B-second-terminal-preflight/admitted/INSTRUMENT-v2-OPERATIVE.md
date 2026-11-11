# S4-B acceptance instrument — v2, parent-owned

**v1 preserved** at `S4B-ACCEPTANCE-INSTRUMENT-v1-preserved.md`, **including its
inaccurate C4 and C5–C23 statements**, which are quoted below rather than erased.

**Author and owner: `claude-opus-5[1m]` `%503`.** Not owned or editable by the
implementer. Not reconstructible at audit time.

## What this document is — and is not (NOTE-040)

**This is a REQUIREMENTS instrument. It is NOT, by itself, an executable frozen
gate**, and v1's framing invited that misreading.

- **Requirements prose fixes identities and expected failure classes.** That is
  its job and it does it.
- **The executable mandatory mechanism is `just lean` / `just ci` plus the
  parent-owned checker**, and **its evidence comes from running it** — never from
  this file.
- **Do not cite this document as a pre-proved executable gate.** Where a row
  needs executable evidence, the evidence is the run.

## Amendment 1 — C5–C23 tested the wrong thing

**v1 said:** *"dependent theorem mutated to fail"*. **Withdrawn.**

Mutating the **theorem** only tests a **different statement**. It does **not**
establish that the original correspondence **detects an implementation defect** —
which is the entire point of the row.

**Corrected requirement — sensitivity to the executable definition or input:**

| | |
|---|---|
| what is mutated | the **executable definition or input** the correspondence is about |
| the mutated definition | **stays well-typed** |
| the **production statement** | **preserved, never mutated** |
| the observation | the **original dependent theorem stops elaborating** |
| where | **isolated scratch, under a temporary control-only exception** |

**P01 and P07 specifically:** they **reuse existing expressions**. So **mutate
that expression or its operative definition** in isolated scratch, keep the
production statement intact, and observe the **original** dependent theorem
failing.

**Definitional-equality rows:** if a row genuinely needs a **different kind of
sensitivity evidence**, **state that accurately**. **Do not present theorem
mutation as a proof-strength claim.**

**The nineteen is not an allowlist.** Bind final spellings mechanically, but
**derive the actual required identities and reconcile** — a fixed extent copied
forward is the defect this milestone keeps catching.

## Amendment 2 — C4 tested a missing file, not a disabled checker

**v1 said:** *"checker disabled — mandatory path exits non-zero"*, and that was
read as **deleting the checker executable to get exit 127. Withdrawn.**

**Exit 127 proves missing-tool failure only.** It does **not** test:

- a **checker disabled to unconditional success**; nor
- **removal or bypass of the mandatory invocation**.

**Corrected requirement:** an **executable control that makes the checker or its
invocation ineffective — while present — and is detected by the permanent
mandatory mechanism, with correct failure attribution.**

The assurance being established is that **the actual invocation is operating**,
not that the pipeline breaks when a file vanishes.

**A missing-file 127 does not close this row.** **Green-but-disabled acceptance
is forbidden**, and the mechanism may be chosen freely but **not weakened to fit
an existing draft**.

## Requirements R1–R18

Unchanged from v1, except that **R11's sensitivity is now governed by Amendment 1**
and **R10's by Amendment 2**.

## Controls — corrected

| id | control | expected observation |
|---|---|---|
| C1 | clean mandatory path, mirrors and checker wired | exit 0 |
| C2 | introduced owned predicate, **counterpart absent** | mandatory path exits non-zero, naming it |
| C3 | introduced owned predicate, **theorem absent** | mandatory path exits non-zero, naming it |
| C4 | **checker or invocation made INEFFECTIVE WHILE PRESENT** | detected by the **permanent mandatory mechanism**, with **correct failure attribution**. **127 does not close it** |
| C5..C23 | per identity: **mutate the executable definition/input**, definition **well-typed**, **production statement preserved**, in isolated scratch | the **original** dependent theorem **stops elaborating**, distinct per row, **no first-failure masking** |
| C24 | proof-axiom check at **final** tree | only permitted axioms |
| C25 | totality / panic-string absence at **final** tree | no `PANIC at` in either stream |
| C26 | restored final full CI at the **final accepted base** | exit 0 |

**A control that cannot fail does not close its row. An unexecuted control is
BLOCKED, never killed. A control that fails for the wrong reason — a parse error,
a type error, a missing tool — closes nothing and is a counted setup failure.**

## Accepted-base integration — new in v2

`master` has advanced to **`d67032313acf3699cc50358a057391b88d002192`** (PR #87,
exporter), parent `4a6cd87`. **It changes `lakefile`, `justfile` and CI, and
expands the compiled inventory.**

- **Plan final accepted-base integration BEFORE the final independent audit.**
- **Preserve the full owned diff and controls** across it.
- **No expectation that 1213 or 23 remain constants.** They were observations of
  a superseded tree.
- **Account for incoming bytes** and **re-establish the mandatory controls at the
  actual final candidate.**
- **Return a measured remaining-command gap before cap overrun. No reset.**

## Versioning

**v2.** v1 preserved with its inaccurate statements intact. Any further change is
a new version with its own hash and reason.
