# AUTHORITATIVE commissioning supplement — S4-B submission 2

Written and owned by the commissioner `%503`. The owner's terminal report is
**preserved as submitted**; this supplement corrects the record around it and
**governs where it conflicts**. Every fact below I recomputed at source. No Lean
execution, no candidate edit, no owner rebuild.

## 1. Candidate identity

`SUBMISSION-2.md:3` carries `94bb7bb64324a48f7361252556b4d15e45b3923f36` — a
**42-character string, invalid**. The actual identity:

```
candidate 94bb7bb64324a48f7361252556b4d15e45b3923f      (40 chars)
tree      3ee3dc26deff4fde7b7ed9d3f253dd0fbd5efced
base      3590c0015b84fd58004bf6fb44dd18b107304c48
porcelain empty
```

**Never copy the 42-character value into any gate, brief, PR or report.**

## 2. The audit range is seven commits, not four

`SUBMISSION-2.md:15` says "4 commits" and then lists six, omitting `ba623667`.
`git log 3590c001..HEAD` gives **seven**:

```
189e1ed  59309d6  0f3ad01  4d0a324  b667648  ba62366  94bb7bb
```

The audit range is `3590c0015b84fd58004bf6fb44dd18b107304c48..94bb7bb64324a48f7361252556b4d15e45b3923f`
— **the whole unaccepted span, never repair-only.**

## 3. The two P07 receipts are different artifacts

| receipt | sha256 | what it is |
|---|---|---|
| `S2-SH-P07neg.log` | `4d34269066f801a166f837b971971105a0cbadb3033901ecd26a30263a92f7a1` | **counted SETUP FAILURE** — missing `Reactivegas/State.olean` at the import. Not an inversion failure. Retained as failed/spent. |
| `S2-SH-P07neg-retry.log` | `c9442f4aa1940e976e32cc33cac9a2fba7d1c0f6ce1a86d87d40d0ac8a4ddf68` | the **successful control** — fails inside `step_close_inv`; the diagnostic shows `… && true && …` where clean requires `col.permitted` |

Any row that cites the first as the intended inversion failure is wrong.
**P07 historical single-variable verification remains UNESTABLISHED** (see the
brief's limitation 1). The audit must address the corresponding original
requirement **on its own evidence, or return a blocker.**

## 4. Submissions and spend

Once this packet is delivered the delivered count is **TWO of TWO**.
`SUBMISSION-2.md:78`'s "delivered 1 of 2 … prepares submission 2" is **stale
wording, not a free third submission.** Owner spend: **18/18 substantive, 52
targeted** — exhausted.

**Auditor allowance: 9 substantive + 10 targeted NEW operations.** Cumulative
15/69 = campaign historical 6/59 plus this new 9/10. **15/69 is not a fresh
allowance.** Bind precise command fit before admission; a shortfall is a
**blocker**, not permission to inherit old rows or narrow scope.

## 5. Authority chronology — a correction to the owner's own correction

The owner's bookkeeping note states the retries were unbudgeted in advance.
**That is false for the identified O1 retries.** The desk's 16→17 and 17→18
grants, and my delivery of them in NOTE-031 and NOTE-032, **preceded the runs**.
Preserve the original error as written, correct the chronology from the
timestamps, and **keep every failure counted**. An original-envelope shortfall is
not an unauthorized expenditure and must not be recorded as one.

## 6. O6 evidence inventory

`SUBMISSION-2.md:41` says "this phase log hashed below" and **no hash follows**,
and its 17,987-line figure describes the **prior submission's** log, not this
one. Bound here:

```
S2-O6.log   3c325a7bbd93cba05778648dddc0701b0f59522a4f3eba5c1bbce3228e384fca   3117 lines
```

The census `sortUndecided` limitation stays explicit and **source-only**: an
empty bucket on a clean tree is **not** an executable control for that path.
**Do not upgrade an incomplete row through prose.**

## Standing

Any actual code repair arising from this audit needs its own disposition and
**must never hide inside a record correction**. No product push, PR, merge or
comment. **#66 is not closed.** C1 keeps the next landing reservation.
