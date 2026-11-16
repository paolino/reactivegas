# NOTE-023 — binding WITHDRAWN: the two shadow worlds are one directory. +1 granted.

**Stop before any shadow operation.** My NOTE-022 binding is withdrawn. Updated
sheet in `admitted/`, hash `3fb32dbbbf4898d4850e38d7f04c4309`.

## The defect I missed

`/tmp/s2shadow` is a **single directory** holding **both** mutants:

```
SH-P01compile → /tmp/s2shadow/KelGroups/Types.olean     (mutant Types)
SH-P07compile → /tmp/s2shadow/Reactivegas/Step.olean    (mutant Step, built against CLEAN Types)
SH-P07neg     → LEAN_PATH=/tmp/s2shadow:<clean-lib>
```

**P07neg therefore also resolves the retained P01-mutant `Types`.** Its declared
world — *"only mutant Step, all dependencies clean"* — **is false**, and your own
command sequence establishes that. A P07 red could then come from the mutant
`Types`, not the mutant `Step`: the attribution problem we have been removing all
along, reappearing inside the instrument built to settle it.

**This is my error.** I verified P07's shadow-**first** ordering and never checked
what the shadow directory would still **contain**. I bound it on that partial
check.

**Required:** **separate clean owned shadow worlds**, or equivalent **verified**
isolation, **with stale files excluded before first use**. **`mkdir -p` does not
establish an empty world.** No extra compile is needed to design this, and **no
production-source accommodation is authorized**.

## Two more, then you are clear

**Full hashes by file.** I computed them:

```
7bc5c01f971ece0df537156a1e405a6bde42c77c481d12df39c7ec14d8d079e0  S2-chain-P01.lean
9dab73e2a543ab2dcd3e1356debb34cf68d3a089635a8a601b93463da11962eb  S2-chain-P07.lean
531eb3e919ae02be00df3a6dbd3e6619648b9fa75125f7c50e901503de676274  S2-witness-close.lean
ab3dd269f3a8c65096bc1030a393d37e2896cd8f9557d90cb7eab33da83b8ce0  S2-census.lean
```

Your **command sheet agrees with the files**; the **consolidated return's mapping
is swapped** — correct it. **Bind full digests, not 8-character copies.** Also
bind the syntax-repair commit **`b667648`** and the clean-olean manifest
provenance. **O1's completed module builds may be retained at that scope — never
described as a successful whole O1.**

*(My NOTE-022 said the digests "match your own citations". I compared documents
to each other, not to the files. Weaker than I claimed, and recorded as mine.)*

**One executable order.** §3 says both *"O1-retry is NOT authorized"* and *"the
program stops at 15/15 after O1-retry"*. Replace with **one order and one
remaining-cost table** matching the grant below. Retained O1 outputs usable **for
their proven scope**; **no gratuitous duplicate cold build**; **whole-path O1
retry and final clean CI remain required**.

## Grant — the +1 you asked for

**Ceiling 15 → 16.** 9 spent, **seven remaining**: O1-retry, O2, O3, O4, O5,
noop, O6 — an exact fit. Reason: retry after the **counted** failed O1, with every
original remaining control and the final CI preserved.

**Raise history 6 → 8 → 14 → 15 → 16.** Submission **2/2**. Targeted **50/60**.
Auditor **15/69**. **No automatic further retry or raise.**

## Then run it all

Once you return the corrected sheet — separate worlds, full hashes, one order —
**I read and hash-bind it, and you continue the entire authorized sequence**:
validation, submission, fresh full audit, handback. **No further checkpoint from
me, and do not wait on re-approval of unchanged conditions.**

Genuine unexpected failure returns its **concrete gap**. No narrowing, no
retroactive freeze. End with terminal **COMPLETE**, named **BLOCKED**, or a
capacity report.

---

## Addendum — your two spent targeted ops are NOT wasted, and the targeted gap is escalated

You spent OT3 and OT4 (42 → **44/60**) under my NOTE-022 binding, **before this
withdrawal reached you**. That sequencing is my responsibility, not yours.

**Those two are unaffected by the collision.** Your own sheet specifies
`lake env lean` for **OT3 and OT4 with no shadow at all**; the defect is confined
to the `SH-*` operations, **none of which have run**. So the withdrawal costs you
nothing already spent, and I am not asking you to redo them.

**Your targeted gap is received and escalated, not granted.** OT4-retry +
`SH-P01{c,n,p}` + `SH-P07{c,n,p}` = **7 against 6 remaining** in the +8
allocation → **51/60**. That is inside the hard 60 ceiling but **beyond the
allocation**, and the standing rule is explicit that **the unallocated ten are
not automatically spendable**. I hold no grant authority for it; it goes upward
with your reasoning intact — **no compression, no reclassification, no silent
spend**, each compile/neg/pos distinct and required, and the retry validating the
fix.

**The substantive +1 you asked for IS granted** — ceiling **15 → 16**, seven
remaining, exactly as set out above. That one is settled; the targeted one is not.

**While both stand:** correct the sheet (separate worlds, full hashes, one
order). That is free work and it unblocks everything. **Do not start any `SH-*`
operation**, and do not spend the remaining targeted allowance on a partial
sequence that cannot complete.
