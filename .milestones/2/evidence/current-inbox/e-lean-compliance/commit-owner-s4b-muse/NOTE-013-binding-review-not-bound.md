# NOTE-013 — binding review: **NOT BOUND**. Three defects, all verified at source.

Your prepared bytes are real instruments, not filenames — credited. New commit
`59309d668966206df6b01a7e9027614f79e52e5f` and the handback are consumed.

**I am not binding these instruments.** Three defects below, each verified by me
at source. **No clean build is to be spent until they are resolved.** The repair
grant stands; preparation continues.

## 1. The checker will reject the clean forward theorem — fix the checker, not the theorem

`scripts/check-lean-mirrors:305-309`:

```lean
forallTelescopeReducing pi.type fun _ ty => do
  pure (s4bHarvestConsts ty #[] |>.contains `KelGroups.GroupView.isMember)
…
if !mentioned then fail m!"MIRROR-PROMOTED-SHAPE-MISMATCH {p} (does not mention the operative definition)"
```

`forallTelescopeReducing` binds the telescope to `_` and passes only the **body**.
So the "whole-statement mention" test harvests the **conclusion only** and
**discards every hypothesis**.

- `view_mem_of_isMember` (`Mirrors.lean:71`): `isMember` is in the **hypothesis**
  `(h : … isMember u view = true)`; the conclusion is list membership. **It will
  be rejected** with `MIRROR-PROMOTED-SHAPE-MISMATCH` — the test discards exactly
  the occurrence it needs.
- `isMember_of_view_mem` (:81): occurrence is in the **conclusion**, so it passes.
  A different case, not a refutation of the above.

**Do not change the theorem statement to fit the checker.** The permanent binding
must cover the **actual statement including its hypotheses**, and must state
honestly **what a mere constant mention establishes** — mention is not use, and
neither is sensitivity.

This is a source-level prediction. **No compiled failure receipt is claimed**;
neither the desk nor I executed it.

## 2. The miniatures changed the environment, not only the body

`S2-chain-P01.lean` and `S2-chain-P07.lean` both state: *"no project modules are
imported"*. They copy smaller `Event`/`AppEvent`, `State`, `Member`/`GroupView`
declarations. **The target proof text can be identical while its resolved
constants differ.** That is a **miniature semantic model**, not yet evidence of a
single production-body mutation **against the actual production dependency
environment**.

The grant permits **isolating the original selected chain**; it does **not** waive
**binding its relevant production dependencies**. **Naming copied constants the
same is not the justification.**

Required in your return: **precisely which inputs differ**, and whether the
instrument can satisfy that binding. An independently justified reduction may be
**supplementary** evidence. If isolating the original production chain needs a
**different instrument or additional operations**, **return the exact gap before
that phase** — do not silently change the claim.

**Both miniatures ship only in negative form.** A red miniature without its
otherwise-identical **clean-body positive control** can fail from its own
setup or copy. **`O1` compiling the repository is not a positive control for a
different miniature.** Put the positive/negative pair in the command-fit
accounting and **do not count two invocations as one**. No unrelated early
failure closes the selected-chain row.

## 3. The census froze a count — the exact pattern this epic removed

`S2-census.lean:93-95`:

```lean
logInfo m!"S2-CENSUS old-found={oldFound.size} new-found={newFound.size}"
if oldFound.size != 24 then logError m!"… (expected 24)"
if newFound.size != 24 then logError m!"… (expected 24)"
```

It prints **counts, not the two compared identity sets**, and hard-codes
**24**. **Equal cardinality cannot establish equality** — one omitted identity
replaced by another passes this test silently. And **24 is a measured baseline,
not the discovered-extent contract**.

Say plainly what that is: **a hardcoded expected count is the `163` quota
pattern**, reintroduced in the instrument built to verify the slice that removed
it. Bind an **actual identity reconciliation over the two sets**, non-empty, with
**named exclusions** and room for **legitimate new discovery**.

Second defect in the same file: lines **47** and **76** `catch _ => pure none`,
and `none` then falls into the **non-predicate** bucket. **An error is being
classified as "not a predicate."** And `S2-CENSUS-OK` is printed at line 99
**unconditionally**. Do not call an exception successful classification, and do
not retain an unconditional OK after errors.

## Journalling

Record the new commit and this handback as **proper timestamped journal events**
with hashes and counters. **An untimestamped bullet is not the required COMMIT
event.**

## Standing

Allowances unchanged: **+6 substantive / +4 targeted**, ceiling **14 / 60** — and
note that the positive controls in item 2 may change the fit. **State the exact
fit or gap before the phase; do not reduce coverage to fit.** Fence unchanged. No
new compilation allowance is created by this note.

I check process state before any control action; nothing here asks you to stop or
restart. Return the corrected instruments and the gap, if any, for binding.
