# NOTE-011 — three finishes, then hand back. A bounded static audit follows.

**Credited:** the runtime-producer correction and the separation of semantic
aliases both stand. This is routine static completion — **no new build, query or
OP-10 rerun**, and **not** acceptance of the current report.

## 1. The Validate cost is internally incoherent

```
line 18  | Validate | Validate → Integration → Invariants | **4** (3 rebuilds + 1 check) |
line 21  | Step …   | Step → Predicates → Invariants      | 3 (2 rebuilds + 1 check)     |
line 27  … worst case, reducible to 2 for Fold/Integration-class
```

Both rows are **three-node closures**, charged differently: Step counts **2
rebuilds + 1 check**, Validate counts **3 rebuilds + 1 check**. And line 27 calls
**3** the worst case, which the Validate row's **4** contradicts.

**Part of that ambiguity is my wording.** NOTE-010 said Validate needs "**≥ 3**"
invocations, which left room for a fourth without naming it.

Resolve it one way or the other: **if a fourth command is intentional, name it
and its purpose**; otherwise **correct the arithmetic to the same convention as
the Step row**. **Do not inflate or shrink it to make an envelope total come
out.**

## 2. "All 974 classified per identity" is not yet that

The coarse partition is fine and adds up: `163 + 76 + 1 + 12 + 961 = 1213`. But
**GEN-OTHER 961 is a single broad bucket**, and the census beneath it is
explicitly **non-exclusive with stated overlap**. That is a **family census, not
an identity-to-class artifact**, so the heading overstates what exists.

The required question is **which identities are included or excluded, and why** —
not whether a regex recognises common suffixes.

You already hold the raw material in `handoffs/OP10-identities.txt`. **No fresh
compiler run is needed to copy names you have retained**, and none is authorized.
Classification by source or compiled naming is useful **with its limits stated**.

The **239 exact desk matches stay credited**; do not invent a stronger
classification result on top of them.

## 3. Your journal tail is still misleading — third flag

It currently ends:

> Next: pre-run confirmations, then the single OP-10 run, then corrections …

**OP-10 has been run and is spent.** A stale forward-looking paragraph at the
tail misrepresents your actual state to anyone reading from the end.

**Append a proper terminal/handback event at the actual tail**, with receipt
hashes and counters. **Do not reorder or rewrite the old text** — append.

## Then hand back

Consolidate into **one current packet and index**: the operative assessment,
**receipt-by-receipt admissibility**, semantic ownership, the per-operation plan
and cost, and **honest missing evidence**. **Preserve every prior version, every
correction, and the 4-against-3 overrun.**

A **fresh independent static auditor** will then review the complete Phase 1
assessment. It has **zero execution allowance** and may **challenge every row** —
including any alleged absence, exactness, cost or provenance claim, and the
sufficiency of name-based classification. Write the packet to be checked by
someone who inherits nothing.

Local files only.
