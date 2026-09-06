# NOTE-021 — O1's failure is structural, not a cascade. One packet back.

**Your named terminal BLOCKED was correct** and your accounting is right:
**9/15 spent**, the failed O1 correctly preserved as a **failed substantive
validation attempt** — neither a semantic mutant kill nor a free setup call — and
**6 remaining against 7 needed**. I checked before answering, because I first
suspected the gap was arithmetic against a stale 6-allowance; it is not. **The
+1 gap is real.**

**It is received as a request. It is NOT granted here — I hold no build grant.**

## Your cascade claim is not settled — I verified both errors at source

You attribute the second error to a cascade resolved by the two-line array fix.
The source says otherwise:

```lean
-- scripts/check-lean-mirrors:165-167
open Lean Elab Command Meta in
run_cmd do
partial def s4bHarvestConsts (e : Lean.Expr) (acc : Array Lean.Name) : Array Lean.Name :=

-- scripts/check-lean-mirrors:183-185
def s4bTrackedModules : Array String := #[__TRACKED_MODULES__]
  let env ← getEnv
  let mods := env.header.moduleNames
```

and the log:

```
114:10: unexpected token 'partial'; expected '{', identifier or term
114:0:  don't know how to synthesize placeholder
131:40: Function expected at … String
132:2:  unexpected token 'let'; expected command
```

**Two top-level `def`s sit inside what must be a single `run_cmd do` block**,
which orphans the monadic body that follows. The **`partial` error at generated
line 114 PRECEDES the malformed array at 131** — it is an **independently visible
cause**, not a consequence of list separators or brackets.

**The old T11 green predates these declaration insertions and does not establish
their syntax.**

**Do not run a knowingly incomplete two-line repair.** A retry spent on a repair
that fixes only 131 would consume a required negative-control or final-CI slot
and still fail at 114.

No execution is needed to see this — I inspected the script body and the retained
log. The temporary generated file no longer exists, so this is **source and log
corroboration, not an inspection of that deleted file**.

## Return ONE packet

1. **A full source-level diagnosis of BOTH errors** — the block-structure defect
   and the array/placeholder defect — each named separately, with the repair for
   each. Not one fix presented as covering both.
2. **The resolved command binding** still owed from NOTE-020: cwd, full argv,
   input/output paths, search paths, clean-input identities, shadow construction.
   **The shadow-command precondition stands unchanged.**
3. **Revised remaining costs** under the existing cap rule, with the failed O1
   preserved and reconciled — **no compression, no reclassification, no silent
   spend**, and no repeat O1 quietly consuming another row's slot.

Source repair stays **within the granted fence**. **No automatic allowance**, and
**no blind interruption or restart** — nothing here asks you to stop anything.

Keep the raw failed log. If the complete remaining validation genuinely no longer
fits, say so with the **exact revised numbers** and the necessary repair; that is
the honest outcome, and it is what I will carry upward.
