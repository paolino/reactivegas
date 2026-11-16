# S4-B repair/acceptance packet v3 (supersedes v2; v1 and v2 retained)

v1 and v2 are preserved as history. **Dated corrections to my prior claims are
recorded here rather than silently edited away.** No build, edit, second-audit or
merge grant is assumed. Every number is a request.

---

## DATED CORRECTION 2026-09-06 — my "no existing theorem reads closePurchase" was FALSE

v2 stated: *"No existing theorem reads `closePurchase` (searched: no theorem or
lemma mentions it)."* **That is false at the candidate**, and the error was in my
search method.

I ran `grep "closePurchase" … | grep -E "theorem|lemma"`, which requires both
tokens **on the same line**. `close_permission_to_close` declares its name on
line 647 and its `closePurchase` hypothesis on line **648** — a continuation
line. **A same-line conjunction grep cannot find a theorem that consumes a term
in its hypothesis.** I then asserted the false negative as established fact.

**The theorem I proposed to write already exists**, `lean/Reactivegas/Invariants.lean:647`:

```lean
theorem close_permission_to_close {s s' : State} {a : KelGroups.Key} {c : CollId}
    (h : stepEvent view s (.closePurchase a c) auth = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧ permissionToClose col := by
  obtain ⟨col, rest, hpull, hg, _⟩ := step_close_inv h
  obtain ⟨_, _, hperm, hempty⟩ := close_guard_inv hg
  exact ⟨col, rest, hpull, hperm, hempty⟩
```

**The proposed new auxiliary theorem is withdrawn entirely.** No new theorem for
a count.

### Citation corrections, also mine

| v2 said | source says |
|---|---|
| `CollectionId` | **`CollId`** — `lean/Reactivegas/Types.lean:15`, `abbrev CollId := Nat` |
| `permissionToClose` in `Mirrors.lean` | **`lean/Reactivegas/Predicates.lean:50`** |
| a new theorem is needed | `close_permission_to_close` already exists |

**Adapter binding verified, not inferred.** `stepEvent` (`Step.lean:147`) is the
legacy Event→AppEvent adapter and delegates to `step` (`step view s signer app
auth`, with `.closePurchase a c ↦ go a (.closePurchase c)`). So a hypothesis on
`stepEvent` **does** bind the production body; the old Event-shaped theorem is
**not** unrelated merely because my draft named `step`.

---

## P07 — reuse, no new theorem, no edit to `Invariants.lean`

**Selected proof chain, named before execution:**

```
close_permission_to_close   Invariants.lean:647
  ← step_close_inv          Invariants.lean:305   (binds pullCollection c; derives the guard)
  ← close_guard_inv         Invariants.lean:178   (splits permitted / pending.isEmpty)
```

**Intended guard failure:** mutating the `closePurchase` permission atom in
`Step.lean:126-132` breaks the `col.permitted` conjunct that `step_close_inv`
derives and `close_guard_inv` splits, so `close_permission_to_close` **fails at
its own selected chain**.

Its declaration and proof dependency chain can be **permanently bound and tested
without changing `Invariants.lean`** — the binding is a tabled row in
`scripts/check-lean-mirrors`, not a source edit.

**Two distinct things are required and neither substitutes for the other:**

1. **Proof sensitivity** — the selected chain fails under the mutant.
2. **A compiled mutant behaviour witness** establishing the defect itself: the
   unpermitted collection actually closes. Proof failure alone does not show the
   defect; the witness does.

Also required: a **valid authorized close** (satisfiable successful inputs) and
the **unpermitted case** the audited bypass mutant wrongly accepts. **No broader
equivalence with all close guards.**

## P01 — reuse candidate, but sensitivity must be executed

`view_mem_of_isMember` (`Mirrors.lean:71`) and `isMember_of_view_mem` (:81) were
read by the desk and by me and are relevant to canonical-data membership.
**Their constant-false mutant sensitivity still requires execution — a
source-only reading is not a PASS**, and I do not claim one.

## Isolation — my O4/O5 split was necessary but not sufficient

**Two full `just lean` invocations do not isolate either theorem**: both commands
can stop earlier, at any preceding gate or proof. Splitting them removed one
masking mode and left another.

The instrument must **exhibit the selected theorem/dependency failure without
masking** — the first semantic failure **in the explicitly selected relevant
chain**, which is different from first-error masking by an unrelated theorem.
Requirements:

- name the chain and the intended guard failure **before** executing (done above
  for P07);
- **preserve exact statement and proof bytes**;
- **name any extracted minimal proof chain** used to isolate;
- **never claim a synthetic theorem with a different statement is the original
  theorem.**

## F01 — unchanged from v2, with its own control

Total `ConstantInfo` classification in `scripts/check-lean-mirrors` (`:165-169`,
`:268`), every kind predicate-bearing or **excluded with a named reason**, and
**fail-closed on an unclassified kind**. Control R3′ is an isolated
classifier-omission mutant whose expected diagnostic is the **classifier's own**
unclassified-kind message — not a Lean type error. No toolchain upgrade. The
current 24-entry census was **not** shown incomplete.

## Auditor plan — ALL affected controls, not an F01/F02-only review

**F01 changes the discovery checker.** Therefore the original **new-predicate**,
**missing-theorem**, **present-but-disabled checker** and **invocation** controls
**cannot be called unchanged-input merely because some Lean sources are
unchanged** — the checker they exercised has moved.

Each such control gets either an **actual re-verification** or a **precise
retained-evidence binding**, with its cost named:

| control class | disposition | cost |
|---|---|---|
| new-predicate rejection (S02) | **re-verify** — checker changed | 1 substantive |
| missing-theorem / orphan-mirror (S03) | **re-verify** — checker changed | 1 substantive |
| present-but-disabled checker | **re-verify** — checker changed | 1 substantive |
| invocation/receipt-nonce controls | **re-verify** | shares the above runs; 2 targeted |
| Lean-source-only rows untouched by F01 | retained evidence, **explicit byte identity** | targeted |

**A full audit permits challenging every prior PASS.** The 78 CLOSED are not
protected by their earlier label.

## Command plan and numeric requests

**Owner** — O1 cold `just lean`; O2 F01 specimen; O3 R3′ classifier-omission;
O4 P07 chain isolation + behaviour witness; O5 P01 constant-false sensitivity;
O6 cold `just ci` at final SHA. Targeted: census before/after, promoted-row
identities, P07 chain footprint, behaviour-witness evaluation.
**Request: 6 substantive + 4 targeted.**

**Auditor** — A1 cold `just ci`; A2 F01; A3 R3′; A4 P07 isolated; A5 P01
isolated; A6–A8 the three re-verified checker-dependent controls; A9 reserve for
one setup failure. Targeted: identity probes, the 11 PARTLY rows, retained-evidence
byte identity, restoration checks.
**Request: 9 substantive + 10 targeted.**

| party | spent | requested |
|---|---|---|
| owner | 8/8 substantive, 42/60 targeted | **+6 substantive, +4 targeted** |
| auditor | 6/8 substantive, 59/60 targeted | **+9 substantive, +10 targeted** |
| parent | none | none |
| submissions | 1/2 | submission 2 |
| ceiling raises | one (owner 6→8) | retained |

The auditor request rose from v2 because the F01-affected controls must be
re-verified rather than inherited — an omission in v2 that this correction closes.

## Bounds

No model edit, no new theorem for a count, no changed current theorem statement,
no candidate acceptance, no new budget taken. No push, PR, merge, `#66` closure,
`#68` interruption, other-slice change, or `docs/en/design/` write.
