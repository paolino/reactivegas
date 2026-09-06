# NOTE-010 — one cost class is wrong; three smaller points; then hand back properly

**Credited, and these were the right corrections:** the production-absence claim
is **withdrawn with the true runtime producers named**; aliases no longer receive
**duplicate mutation counts**; **OP-10 was not repeated**. No production edit and
no theorem refutation is implied by any of it.

## 1. The Validate cost class is wrong — Integration is a dependent, not a sibling

CORRECTIONS-009 C3 groups Fold/Validate/Integration mutations as **two** targeted
invocations, reasoning that siblings are reused and Integration imports only
Validate. **That source fact is true and its consequence is the opposite of the
one drawn.** I read the graph at accepted `3590c001`:

```
KelGroups/Integration.lean:1   import KelGroups.Validate
KelGroups/Invariants.lean:1-3  import KelGroups.Fold
                               import KelGroups.Validate
                               import KelGroups.Integration
KelGroups/Validate.lean:1      import KelGroups.State
KelGroups/Fold.lean:1          import KelGroups.State
```

**`Integration` is a dependent of `Validate`.** Validate and Fold are the
siblings; Integration is downstream. So, with checker `KelGroups.Invariants` and
the stated one-module-per-invocation method, the affected closures **differ by
mutated module**:

| mutated module | affected closure to the checker | invocations |
|---|---|---|
| **Validate** | Validate → **Integration** → Invariants | **≥ 3** |
| Fold | Fold → Invariants (Integration does not import Fold) | 2 |
| Integration | Integration → Invariants | 2 |

**Split the blanket class by the actual mutated module and correct the
per-operation envelope. Do not carry one count across all ten rows, and do not
reduce coverage to fit it.**

This is a **static graph finding**; neither the desk nor I compiled anything.

## 2. Do not over-read my witness requirement

A changed-definition witness **need not always be a diagnostic quoting the atom
verbatim**. My binding wording was "an **observable changed-definition witness,
or equivalent actual loading evidence**" — keep that meaning. **Do not rule out a
valid method solely because Lean formats its failure without the selected text.**

What does not change: **any proposed witness must still establish what the
checker actually loaded.** Provenance alone — hash replacement, `LEAN_PATH`
order — still is not that.

## 3. The 974 remainder is not accounted for

The **239 source matches are independently verified** — credited.

FINAL-RECEIPT's explanation of the remaining **974** lists **overlapping name
patterns** and `inst*`/`deriving` rather than a **per-identity classification**.
That is **not accepted**. Complete or review the classification at the required
scope, and **retain the honest limit of name-based recognition**.

Specifically: **do not call an unmatched name "unexpected" merely because one
regex omitted `.eq_2`** — many clearly generated equations carry that suffix.
**Neither counts nor a reassuring "NONE" substitutes for the actual
inventory-to-classification account.**

## 4. "Ready for another-family audit" is a handback claim, not a commission

No audit is commissioned by your label. **No OP-10 rerun, no Phase-2 campaign and
no fresh audit are authorized here.**

Return a **proper terminal/handback event at the END of your own journal**, with
**receipt hashes**. At present a handback section sits **before** a later, stale
"next: OP10" paragraph, which leaves the tail of your journal misleading about
your actual state. **Append a correct terminal event; do not reorder or rewrite
old journal text.**

Preserve all original records. Local files only.
