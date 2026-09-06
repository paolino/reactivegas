# NOTE-008 — R5: three source-verified corrections, plus OP-10 authorized

**Credited:** demoting t57 and the GUARDS/FENCE rows to **unestablished reuse**
is right — it distinguishes missing dependency evidence from demonstrated change.
Remaining claims still need **their individual supporting hunks**; a count is not
one.

Three corrections before Phase 1 can be accepted, then one bounded operation you
may actually run.

## 1. `comune_not_a_member_of_reach` — premise, not runtime producer

R5 assigns it to Validate's direct-admission guard and production boot. **That is
wrong**, and I verified it at source:

```lean
-- Invariants.lean:1141
theorem comune_not_a_member_of_reach {s : State} (hr : Reach view auth s) :
    comune_not_a_member view := by
  induction hr with
  | boot h => exact h
  | trans _ _ ih => exact ih

-- Predicates.lean:97+
| boot (h : comune_not_a_member view) : Reach view auth State.empty
```

`Reach` fixes **`view` as a parameter** and stores `comune_not_a_member view` as
the **boot premise**. The theorem simply **transports that premise** through the
induction — `boot h => exact h`, `trans _ _ ih => exact ih`. Its statement and
proof **never touch** the Validate admission guard or production boot.

This is exactly the **premise versus runtime-producer** distinction. Correct the
semantic ownership relation, and **do not manufacture a guard-mutant pairing for
a fixed-parameter transport theorem**.

Any resulting **statement-to-production gap is recorded for S5**. Do **not**
claim this true theorem is false, and do **not** broaden the current
implementation fence.

## 2. Alias rows and the "14 economic atoms"

OP-32..38 proposes **breaking alias statements or their dependence while leaving
the counterpart body unchanged**. A **changed theorem statement is not a
production definition mutation**, and cannot test the original claim. Alias
surface or checker tests may be classified **separately** where required, but
**cannot be counted as production kills**.

Map each alias to its **actual semantic dependencies** and **keep its original
statement**.

Likewise **"14 economic atoms" is not a demonstrated property-to-atom relation**.
Show **which relevant guard or effect matters to each property**. No
one-per-constructor quota, no forced irrelevant pairing.

## 3. M-elab may be testing the baseline while claiming a mutant

As written, M-elab copies the baseline compiled library, changes a source atom,
then elaborates a theorem module — **without rebuilding the modified `Step`/`Fold`
dependency before the checker imports its `.olean`**. So the probe can load the
**baseline** artifact and report a result attributed to a mutant.

**Name and count** the actual dependency **rebuild**, **artifact replacement**
and **proof check**. **The instrument needs evidence that it loaded the changed
definition** — not an assumption that it did.

Also: the stated **≤10 s upper bound is unsupported**. "Expected faster,
unmeasured" does not follow from a *different* incremental rebuild's timing.
**Keep measurement classes separate and state estimates as estimates, not
bounds.**

The ranged OP rows still carry approximate family extents and "exact lines
re-read at execution time". **That is not the claimed completed exact plan.** Fix
the semantic ownership and the loading method **first**, then derive the finite
proposal from them. **Do not produce another numbered restatement.**

## OP-10 — AUTHORIZED, one bounded discovery operation

Establish the **actual compiled identity set** with the **accepted, unmodified**
`scripts/check-lean-axioms` on your isolated worktree at **exactly**
`3590c0015b84fd58004bf6fb44dd18b107304c48`. **Confirm the worktree, pins and
script identity before the run.**

Authorized entry, from repository root:

```
nix develop --quiet -c bash scripts/check-lean-axioms
```

**Charging:** the internal `lake build` counts as **ONE additional substantive**
operation; the generated driver elaboration as **ONE additional targeted**
operation.

- The historical **4-against-3 overrun remains an overrun in its original
  campaign** — this does not retroactively authorize it. This prospective grant
  makes the **substantive total ceiling 5**, with **one new operation available**.
- Historical **targeted** usage was never given a comparable numeric ceiling in
  the Phase-1 brief. **Record the discovered history, or UNKNOWN — never invent
  zero.** An explicit **one-operation targeted allowance** is opened for this
  enumeration, and **no further query grant is implied**.

**Preserve:** actual stdout and stderr, exit code, source identity, distinct
names, **private-name mapping**, and full hashes.

**Reconcile** the source inventory against those compiled identities and
classifications. **Neither 239 nor any historical compiled count is a quota.**
**Unexpected names or missing source identities are findings to account for, not
entries to filter away.**

This is **current identity discovery** — not re-running the lost cold-cost log,
and not a fresh cold-provenance claim.

**No mutation campaign and no Phase 2 execution is authorized.** A failed setup
**spends the operation and returns a concrete gap** — never an automatic retry.

Do the three static corrections **alongside** this one bounded operation. Local
files only.
