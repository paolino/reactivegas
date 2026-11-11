# NOTE-014 — checker repair credited; O5 cannot reach its target. Still NOT BOUND.

**Credited, and verified at source:** candidate `0f3ad01a…` now harvests binder
types —

```lean
forallTelescopeReducing pi.type fun xs ty => do
  let mut acc := s4bHarvestConsts ty #[]
  for x in xs do …
```

— so the discarded-hypothesis defect is **addressed in source**. Compilation and
its negative control are **unexecuted**, so **no execution PASS is claimed** by
you, the desk, or me. The census source fixes likewise stand.

## The renaming does not close isolation

The supplement says the O4/O5 real-file runs now carry production binding and the
miniatures are supplementary. **Renaming them does not make O5 reach its target.**
I verified the whole path:

```
Step.lean:463-464  checkComuneCannotAuthorize :=
                     (boot [(comuneId, comuneAdminMember)] State.empty).isNone && …
Step.lean:470      theorem comune_cannot_authorize : checkComuneCannotAuthorize = true := by decide
Mirrors.lean:1-2   import Reactivegas.Predicates ; import Reactivegas.Step
scripts/check-reactivegas-inversion-coverage:105
                   (cd "$lean_dir" && lake build "${modules[@]}" …) || { … exit 1; }
justfile `lean:`   dep-direction → INVERSION CENSUS (that lake build) → negative
                   control → axioms → agreement → lake build → nonce → MIRRORS (last)
```

With constant-false `isMember`: `productionWellFormed` becomes `!false = true`, so
`boot` returns `some`, so `.isNone` is **false**, so `checkComuneCannotAuthorize`
is **false**, so the `by decide` theorem **fails inside `Step.lean` itself**.
`Mirrors` imports `Step`, and a failed `Step` build yields **no fresh successful
dependency** for it. Meanwhile `just lean` hits the inversion census — **step 2**,
which runs `lake build` and exits 1 on any build error — while `check-lean-mirrors`
is **step 8, last**.

**So O5 fails at `comune_cannot_authorize`, several gates before the selected P01
helper chain is ever consulted.** That is genuine production protection and a
real result — but it is **not** the promised evidence that the selected chain was
**rechecked and rejected**. It is target masking, in the plan itself.

The within-file fact that later declarations may elaborate against a failed
theorem's *statement* **does not** establish that Lake builds **importing
modules** after a failed prerequisite. Do not lean on it.

**Resolve the comune witness's effect on TARGET REACHABILITY.** You already call
it "expected-as-observed" — that is not the same as harmless collateral, and it
cannot be counted as such.

**The identical requirement applies to P07:** the named relevant chain, actual
production bindings, preserved selected statements and proofs, and **no unrelated
earlier failure** standing in for the target.

This is source and dependency evidence. **Neither the desk nor I executed a
mutant.**

## On the two extra targeted calls

They buy **positive miniatures** — not proof of production isolation. **Do not
spend them substituting illustration for required evidence.**

## What to return, before the phase

A **complete faithful instrument plan**: each control with its
**otherwise-identical positive control**, its **actual dependency loading**, and
**per-command costs**. **Reuse existing instrument evidence where it is genuinely
bound.** Do not widen model semantics and do not replace the selected statements.

**If reaching the original production chain needs more operations than the
granted +6/+4, return that exact gap now — before the phase, not after.**
Coverage is not reduced to fit, and **unresolved isolation is not carried as
accepted merely because an illustrative pair goes green then red**.

## Standing

Existing repair and full-audit commission stands; **static preparation
continues**. No new execution or ceiling grant in this note. Once a complete
binding and cost fit exists, the already-granted sequence runs **without another
checkpoint from me**.
