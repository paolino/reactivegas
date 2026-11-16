# NOTE-009 — the premise-transport correction must not become a runtime-absence claim

Your fixed-view premise-transport correction is **right and credited**. But
CORRECTIONS-008 then asserts:

> production never ESTABLISHES `comune_not_a_member view` for a view

**That does not follow, and it is contradicted by the actual production
boundary.** I read the bodies before classifying, and so must you:

```lean
-- Step.lean:357
def productionWellFormed (gs : KelGroups.GroupState State) : Bool :=
  !KelGroups.GroupView.isMember comuneId (KelGroups.groupView gs)

-- Step.lean:366   boot
  if productionWellFormed gs then some gs else none

-- Step.lean:381+  apply
  if productionWellFormed gs then
    match KelGroups.applyIntegratedEvent … with
    | .ok result => if productionWellFormed result.state then .ok result

-- KelGroups/Validate.lean:145   validateDirectAdmission
  if target = reserved then .error (.reservedKey target)
```

`productionWellFormed` **is** the negated canonical `isMember comuneId` check.
`boot` returns the aggregate **only if it succeeds**. `apply` checks it **before
the integrated fold and again on the result**. `validateDirectAdmission`
**rejects admission of the reserved key**.

So production **establishes and maintains** the property. **Do not convert "this
proof transports a premise" into "no production implementation establishes it".**
Those are different claims, and the second is false here.

**Part of this is on my wording.** NOTE-008 said "any resulting
statement-to-production gap is recorded for S5" without requiring that the gap be
**established first**. Tightened now:

**The only warranted S5 question is whether, and how, the current theorem
statements connect to those actual runtime producers across the claimed scope. An
absence of such a connection must itself be established — it is never inferred
from a theorem's proof scope.** Correct the proposed S5 finding so it carries no
false runtime-absence claim into another slice. **No production change is
requested**, and this true theorem is still not false.

## Two planning limits

**Alias rows.** A called counterpart's proof failing **does not make the alias a
second independent mutation**. Classify the **shared semantic dependency**
without mutating the counterpart's original statement. And **do not require every
transport or helper fact to own an unrelated runtime guard just to fill a row** —
a fact whose obligation is satisfiability of its hypotheses is characterised on
its own terms.

**M-elab loading evidence.** "2 targeted invocations per op" is justified **only
for a direct changed-module → checker dependency**. With **intermediate imports**
you must **rebuild the actual affected dependency closure and count its
invocations** — copying an old intermediate `.olean` **can retain the old
definition**, which is exactly the baseline-as-mutant failure already flagged.

**Hash replacement and `LEAN_PATH` order are provenance. They are not by
themselves evidence of what a compiled checker loaded.** Bind the selected defect
with an **observable changed-definition witness**, or equivalent actual loading
evidence.

## OP-10 status

The grant is **unchanged and already spent** — its raw output now shows
`axiom-theorems count=1213` and `axiom-gate: ok`. **Do not re-run it for this
note.**

The final receipt, counters and source reconciliation are **still to be read**,
so **the report is not accepted**. Complete the finite static corrections and
return the **actual inventory reconciliation against the current model scope**.

Local files only.
