Reactivegas' Lean is substantial — 163 theorems across 27 files, 14/14 event inversions, zero `sorry`, zero `admit` — but it was built before `system-design` became the governing quality standard. This issue brings it into compliance, or records where our shape legitimately diverges.

## Measured gap, against master `e6c5924`

| required by `system-design` | state |
|---|---|
| `lean/<MACHINE>-MUTANTS.md` — one mutant per guard, the theorem owning it | missing |
| `LEAN-CLARITY.md` — the simulation creator's record | missing |
| `decisions.md` / rulings in-repo, dated, cited by doc comments | missing |
| `<Machine>Goals.lean` — theorems numbered by the design record | missing (theorems live in `Invariants.lean`) |
| `#print axioms` | present, 23 sites across 5 files |

## The row that matters most

`system-design` requires `#print axioms` to be taken on a **fresh `.lake`**: a mutation campaign that built in the same worktree leaves oleans behind that change the answer. Our audits ran mutation campaigns in their worktrees.

**Were any axiom receipts taken on a contaminated tree?** If so, the zero-`sorry` claim rests on a measurement that cannot support it and must be re-taken.

## Also in scope

- **Mutant coverage per theorem.** "A theorem no mutant reddens constrains nothing." Quantify over the discovered set of 163 theorems, never a hand-listed subset.
- **Statements-before-proofs.** The standard requires statement audit between statements and proofs. We proved as we went; say whether that leaves specific statements unaudited, or any theorem true but narrower than its name.
- **The clarity measurement is void.** Our simulator was built with explanation, not from the Lean alone, so `LEAN-CLARITY.md` cannot be reconstructed honestly — record the void, start the file for future slices, do not fabricate it.
- Refusal names reconciled to guard hypotheses; any `Prop` without a decidable mirror.

## Acceptance

Assessment report first, slices after. A finding that our shape legitimately diverges is a valid outcome stated with its reason. Compliance is not the goal — a Lean we can trust is, and this standard is the current best statement of what that means.


---

## Ruling RG-S4-REACH-20260906 — `Reach` consumer boundary

Issued 2026-09-06. Recorded here as the standing warrant for the `Reach` mirror
exemption; it was not pre-existing authority.

An arbitrary-state decision procedure for `Reach view auth s` is **NOT-REQUIRED**
for this milestone. The required executable observable is **validation of a
supplied finite history**, with its initial-state and fixed view/auth premises
established. Logical `Reach` premises remain legitimate proof inputs. This is not
an undecidability claim, and not an inference from absent callers or instances.

Finite replay and `Reach` are **not unconditionally the same object**:
`Trace.initial` may be arbitrary and `TraceResult.refused` retains the state,
whereas `Reach.boot` requires `State.empty` plus comune exclusion and
`Reach.trans` covers only successful `stepEvent` under a fixed view and auth.
Integrated `apply` histories can change the view and fall outside that predicate.

The **finite-history correspondence remains OPEN** as an owned S5
statement-completeness obligation, retaining the #75 replay and #71 reporting
dependencies. It is **not waived** to let S4 land: this issue and the milestone
remain incomplete until it is discharged. Any bridge must establish its genesis,
view/auth premises and refusal preservation, and validation of one supplied
history must never masquerade as deciding existential reachability.

