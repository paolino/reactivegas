# #92 — final disposition: NOT ACCEPTED. Submissions exhausted, one unit unspent.

Owner `%503`. Delta inspection terminal **AUDIT-FINDINGS** 11:39:39Z, report
`1dc5db66…`, manifest `608cd159…`, **71 entries verified, no self-entry**, spend
**0/1**. Campaign **12/13 — the last unit is unspent.**

Submission 2 was ruled **the last submission**. **#92 is not accepted**, and I have
opened no further submission.

## My own overclaim, corrected first

I reported that X6 answered C/F001 *"by construction"*. **It does not, and I
verified the correction at source.**

The repair replaced a name-shape filter with an **awk text parser of the lakefile**
that handles only a **subset** of legitimate Lake spellings:

- **Dotted default root truncated.** At `:67` the substitution strips from the
  first non-alphanumeric character, so a dot cuts the name: `lean_lib Extra.Probe`
  yields **`Extra`**. The legitimate default root is lost *and replaced by a wrong
  name*.
- **Multi-line roots dropped.** At `:87` multi-line mode is entered **only if the
  assignment line itself carries the array opener**. So
  `roots :=` followed by ``#[`Extra.Probe]`` on the next line emits nothing, sets no
  multi flag, and the root is **silently dropped**.

Lake 4.25.0's own DSL syntax (`:273-288`) accepts both spellings. The inspector
built parser-only controls demonstrating each case.

**X6 demonstrates one recognized case, not the general requirement.** Its green
observation does not contradict the finding — it was written in the spelling the
parser happens to handle. This is the **same class of defect as F001, one level
up**: a filter that passes the cases it was tested against.

## F-DELTA-01 — blocking

P-01 is **not satisfied**. A properly registered declared root remains excludable,
so the acceptance bar's "no exclusion of legitimate project sources" is unmet.
Downstream Lean failure is **predicted, not executed** — the inspector is explicit
about that limit and spent nothing to manufacture it.

## F-DELTA-02 — provenance, not product behaviour

P-03 **improved materially**: every complete log now carries the full quality SHA,
the quality tree hash and the C1 SHA, and **X1, X5 and X6 carry the exact submitted
checker hash** in both header and runtime binding, linked by nonce and agreeing on
HEAD and tree.

What remains unbound is **fixture provenance**: X2's variant is reconstructible by
one exact removal and is established; **X3's** neutered variant has no inner
binding; **X4's** stated v2-plus-bogus-import relationship **cannot be verified**
because the variant bytes were not retained; and **X6's** modified lakefile and
staged probe bytes were likewise not retained.

This blocks exact control provenance, **not** the observation that X6 built and
reached a namespaced module — which the inspector credits substantially: the
compiled module list at X6 `55-86` is the C1 inventory **plus `Extra.Probe`, with
no `Extra` parent**, and C1's complete lexical import inspection contains no
`import Extra.Probe` or `import Extra`, with porcelain naming only the added files.

## What is genuinely established, and should not be lost

- **Ownership fence held**: exactly one tracked modification, `scripts/check-lean-mirrors`,
  no simulator-lane edit, **no driver deregistration**, no `docs/en/design/` write.
- **Semantics preserved**: correspondence, exception and promotion tables
  byte-equal to base.
- **Quota honest**: `tracked=31`, `29` and `32` all follow from discovery,
  independently derived.
- **Controls fire for their own reasons**: X2 RED naming `TraceTests`, X3 bypass
  caught by `RECEIPT-ABSENT`, X4 invalid import distinctly named.
- **X6 reached and built a namespaced root** — real progress over submission 1,
  even though it does not generalize.
- **Receipt binding is materially better** and three of six are exactly bound.

The repair is closer than submission 1 and still short of its own bar.

## Spend

**12 of 13.** Six original units (I1–I6), six raise units (X1–X6). **I7, I8 and the
delta's unit all went unspent** — every inspection settled statically. **One unit
remains and there is no submission left to spend it on.**

## Disposition

**#92 is NOT ACCEPTED.** F-DELTA-01 is blocking and submissions are exhausted. The
next step needs a fresh desk decision: the selector must read Lake's declared roots
rather than parse a subset of their spelling, and that is a further submission I am
not authorized to open.

**#92 and #66 remain open.** No push, PR, merge or issue comment was made. S3's
ledger and its separate `Q-001-SCHEDULE` block are untouched by this.
