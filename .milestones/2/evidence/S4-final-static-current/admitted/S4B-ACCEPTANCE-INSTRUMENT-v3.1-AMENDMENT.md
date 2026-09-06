# S4-B acceptance instrument — v3.1 amendment (P01/P07 only)

Supersedes **v3** (`S4B-ACCEPTANCE-INSTRUMENT-v3-AMENDMENT-superseded.md`,
preserved verbatim). v2 remains authority where not amended; v1 is superseded
history. Scoped to **P01 and P07 only**.

## SUPERSEDED TEXT — explicitly withdrawn before this hash is admitted

v3 contained, and this amendment **withdraws in full**:

- the claim *"No existing theorem reads `closePurchase` (searched: no theorem or
  lemma mentions it)"* — **false at the candidate**; and
- the proposal of a **new auxiliary theorem**
  `closePurchase_requires_permissionToClose`, with its "exact type" using
  `CollectionId` and citing `permissionToClose` in `Mirrors.lean`.

**None of that text is authority.** It is retained only in the superseded file
as history. The original audit findings are likewise preserved unchanged.

Cause of the false claim, recorded: a `grep closePurchase | grep -E
"theorem|lemma"` requires both tokens on **one line**, while the theorem name is
at `Invariants.lean:647` and its `closePurchase` hypothesis at `:648`. Such a
grep cannot find a theorem that consumes a term in its hypothesis.

## Why v2's demand on the two existing theorems is impossible

Verified at source, and unchanged from v3:

- `Mirrors.lean:125-128` (**P01**) unfolds `comune_not_a_member`, then **cases on
  the value** of `KelGroups.GroupView.isMember comuneId view` and closes both
  branches by `simp`. It never inspects `isMember`'s definition, so it holds for
  **every** implementation, including the audited constant-false mutant.
- `Mirrors.lean:186-189` (**P07**) destructures `Collection` and relates
  `permissionToClose` to an **inline field expression**; it never reads
  `closePurchase` (`Step.lean:126`).

No unreferenced added theorem can make either proof fail; an artificial
dependency would test the dependency, not the theorem. **The original statements
and proofs remain valid and unchanged. The executed survivors remain survivors.**

## The v3.1 replacement — two separated obligations

For **P01 and P07 only**, v2's body-sensitivity demand on the *existing*
theorems is **replaced**. Not retroactive closure of C5/C11; not disclosure
accepted as coverage.

### Obligation 1 — existing statements, accurately scoped

They stand, with their **accurate logical scope recorded**: each relates a
predicate to an expression and is **value-parametric / definitionally inline**,
not sensitive to the production implementation. Their relatum controls stand and
are **never called implementation kills**.

### Obligation 2 — an additional body/consumer obligation that does fail

Semantically relevant, **permanently checked on the mandatory path**, original
statement and proof **frozen before its single-body mutant**, and it **does fail**
when the relevant production implementation is broken. Each control **isolates
its own target theorem**.

**P01 — reuse.** `lean/Reactivegas/Mirrors.lean:71` `view_mem_of_isMember`
unfolds `isMember u view` to `(KelGroups.assocLookup u view.members).isSome =
true`; `:81` `isMember_of_view_mem` concludes `isMember u view = true` from
`(u, v) ∈ view.members` and is therefore **unprovable under a constant-false
`isMember`**. Promote both to tabled mandatory-path obligations. **Their
sensitivity still requires execution — no source-only PASS.**

**P07 — reuse the existing chain. No new theorem.**

```lean
-- lean/Reactivegas/Invariants.lean:647
theorem close_permission_to_close {s s' : State} {a : KelGroups.Key} {c : CollId}
    (h : stepEvent view s (.closePurchase a c) auth = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧ permissionToClose col := by
  obtain ⟨col, rest, hpull, hg, _⟩ := step_close_inv h
  obtain ⟨_, _, hperm, hempty⟩ := close_guard_inv hg
  exact ⟨col, rest, hpull, hperm, hempty⟩
```

Chain: `close_permission_to_close` (:647) ← `step_close_inv` (:305) ←
`close_guard_inv` (:178). Correct citations: type **`CollId`**
(`Types.lean:15`); `permissionToClose` defined at **`Predicates.lean:50`**.

**Adapter binding, verified:** `stepEvent` (`Step.lean:147`) is the legacy
Event→AppEvent adapter delegating to `step view s signer app auth`, with
`.closePurchase a c ↦ go a (.closePurchase c)`. A `stepEvent` hypothesis
therefore **does** bind the production body.

Required for P07: exact collection binding (via `pullCollection c`), a
**successful authorized close witness**, and the **unpermitted mutant behaviour
witness** — the defect itself, not only proof failure. **No broader equivalence
with all close guards.** **No new theorem duplicating this chain.**

## Bounds

No new runtime semantics. **No edit to any existing statement, and no
`Invariants.lean` edit.** All remaining original S4 and v2 obligations stay
required; the denominator is **discovered, not frozen at 89**. Any further true
logical conflict returns with its **exact statement**.
