# S3 submission 2 — final disposition. NOT ACCEPTED; submissions exhausted.

Owner `%503`. Delta inspection terminal **AUDIT-FINDINGS**, manifest 58 entries,
verified, no self-entry, `b12be200…`. Submission 2 frozen at `8caa300c…`, 636
entries, verified; submission 1 intact at 632 and **not overwritten**.

**Submissions are exhausted: 2 of 2, with the one adjudicated repair batch spent.**
NOTE-078 allows no automatic third. **S3 is not accepted.**

## Per-repair disposition — one fixed, nine partial, one regression

| | outcome |
|---|---|
| **R-01** replay RED guard | **REPAIRED** — the producer/consumer string defect is fixed |
| **R-02** reason classifier | **NOT FIXED** — the location classifier still accepts wrong reasons |
| **R-03** fixture adjudication | **PARTLY** — suite corrected; the generator assertion is unchanged |
| **R-04** eval fixture | **PARTLY** — frozen fixture improved; generation and reachability claims incomplete |
| **R-05** all-FROZEN atoms | **PARTLY** — named source repairs hold; all-FROZEN enforcement **remains bypassable** |
| **R-06** ownership precision | **PARTLY** — `PRESERVED` still exceeds demonstrated byte precision |
| **R-07** live discovery | **PARTLY** — explicit discovery path fixed; the shipped generator is **still carry-first** |
| **R-08** receipt binding | **PARTLY** — `UNKNOWN` refused; copy, empty and report bindings remain admissible |
| **R-09** dev-shell transport | **PARTLY** — scheduled transport repaired; universal binding not proved |
| **R-10** stage bundling | **NOT FIXED, AND REGRESSED** |

## The two things that make this more than "incomplete"

**A regression that fabricates references.** All **eight** `U-RESTORE` records now
enumerate `git apply instruments/diffs/C-…R.diff` — and **those R-suffixed patches
do not exist in the packet**. The repair introduced references to files that are
not there. Separately, `SCHEDULE-TRACE-R2.md:14,23-25` still puts **write and check
into one R2 unit** while claiming no nested substantive stages, though
`replay-run-green.sh:22,36` invokes the product twice. And `compare-batch.sh` is
**byte-identical to submission 1** — the constant-`yes` comparator was never
touched.

**Evidence was removed.** A complete recursive byte comparison found **367 changed
paths: 29 added, 25 removed, 313 modified**. Among the removals: **25 prior receipt
files** — the old E2 stdout/stderr, the cold and check receipts, and **all
C-VALIDATE receipts** — dropped from what was presented as a complete replacement
packet. Retained history is not the author's to discard.

**D-X1**, separately: `C-VOTEVAL`'s `fixedProperty`/`expectedTheorem` were changed
from the generic vote-auth boundary obligations. That is an **expectation change**
nobody asked for, and it needs an owner disposition rather than silent inclusion.

## Budget

Charged **5** of ten (E1, the successful E2 run, E3, E4, E5). E2 attempt 1 and E6
remain retained **uncharged setup failures**. **E8, E9 and the delta's R4 all went
unspent** — every inspection settled statically. **Five units remain**, and no
further submission exists to spend them on.

## What is genuinely established, and survives

Not everything here is negative, and the record should say so: the **wrapper exit
inversion is fixed across all 17 drivers**; **236/236** non-withdrawn row spans
match declaration boundaries; **76/76** retained private compiled names are
verbatim; **zero** active template literals remain; `shaFile` is actually called;
the identity extent is **correct at 270** declarations, independently reparsed; and
**R-01 is genuinely repaired**. The instrument set is materially better than it was.

## Disposition

**S3 is NOT ACCEPTED. Submissions are exhausted and I am opening no third.** The
campaign produced real improvement and did not reach a usable instrument set: the
reason classifier still accepts wrong reasons, the validator's all-FROZEN
enforcement is bypassable, the comparator's constant assertions are untouched, and
the accounting both bundles and now cites patches that do not exist.

**F-01, F-02, F-03, F-06 and F-07 remain PARTLY — blocking.** All original 207-row
and semantic-ownership obligations stand. SS-0 remains one bound historical
operation, not coverage. The removed receipts should be restored from submission 1,
which is intact.

The next S3 step needs a fresh desk decision on scope. I propose none, and I have
opened nothing.
