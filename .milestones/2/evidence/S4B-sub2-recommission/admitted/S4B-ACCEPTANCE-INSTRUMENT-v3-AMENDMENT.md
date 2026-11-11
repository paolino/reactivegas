# S4-B acceptance instrument — v3 amendment (P01/P07 only)

**Versioned before any repair submission or auditor START.** v2 remains authority
for everything it is not amended on here; v1 stays superseded history. This
amendment is scoped to **P01 and P07 only**.

## Why v2's demand on the two old theorems is impossible — conceded, with the source

My submission-2 proposal (a) claimed an added bridge could make the existing
proofs stop elaborating under a body mutant. **That claim is wrong and I withdraw
it.** The desk's argument is correct and I verified it at source:

```lean
-- lean/Reactivegas/Mirrors.lean:125-128   (P01)
theorem comune_not_a_member_corr (view : KelGroups.GroupView) :
    comune_not_a_member view ↔ ((!KelGroups.GroupView.isMember comuneId view) = true) := by
  unfold comune_not_a_member
  cases KelGroups.GroupView.isMember comuneId view <;> simp
```

After `unfold`, the proof **cases on the value** of `isMember … view` and closes
both branches by `simp`. It never inspects `isMember`'s definition, so it holds
for **every** implementation — including the audited constant-false mutant.

```lean
-- lean/Reactivegas/Mirrors.lean:186-189   (P07)
theorem permissionToClose_corr (col : Collection) :
    permissionToClose col ↔ ((col.permitted && col.pending.isEmpty) = true) := by
  obtain ⟨id, ref, perm, acc, pend⟩ := col
  cases perm <;> cases pend <;> simp [permissionToClose]
```

It relates `permissionToClose` to an **inline field expression** and never reads
`closePurchase` (`Step.lean:126`).

Therefore **no unreferenced added theorem can make either proof fail**, and an
artificial dependency introduced solely to force failure would test the
dependency, not the theorem. **The original proofs remain valid and unchanged;
the executed survivors remain survivors in the old report.**

## The v3 replacement — two separated obligations

For **P01 and P07 only**, v2's body-sensitivity demand on the *existing* theorems
is **replaced** by this pair. This is a replacement, **not** retroactive closure
of C5/C11 and **not** disclosure accepted as coverage.

### Obligation 1 — existing statements, accurately scoped

The existing correspondence statements and proofs stand **with their accurate
logical scope recorded**: each relates a predicate to an expression and is
**value-parametric / definitionally inline**, not sensitive to the production
implementation. Their existing relatum controls stand **and are never to be
called implementation kills**.

### Obligation 2 — an additional body/consumer obligation that does fail

A **semantically relevant** body/consumer obligation, **permanently checked on
the mandatory path**, whose **original statement and proof are frozen before its
single-body mutant**, and which **does fail** when the relevant production
implementation is broken. Each new sensitivity control **isolates its own target
theorem**; an earlier unrelated failure does not close it.

**P01** — connect membership to **actual canonical member data**, with positive
**and** negative cases able to distinguish the audited constant-false mutant.
Restating `isMember` on both sides is insufficient.

*Reuse candidate, preferred over any new theorem.* `lean/Reactivegas/Mirrors.lean`
already carries two private theorems that genuinely read the definition —
`view_mem_of_isMember` (:71), which unfolds `isMember u view` to
`(KelGroups.assocLookup u view.members).isSome = true`, and
`isMember_of_view_mem` (:81), which concludes `isMember u view = true` from
`(u, v) ∈ view.members`. The latter is **unprovable under a constant-false
`isMember`**, which is exactly the required sensitivity. The revised packet must
confirm by inspection that these meet Obligation 2 and, if so, **promote them to
tabled, mandatory-path-checked obligations rather than adding a new theorem for a
count**.

**P07** — connect **successful production `closePurchase`** to
`permissionToClose` for the **actual selected collection**, with the
event/collection binding and **satisfiable successful inputs** established.
Include a valid authorized close **and** the unpermitted case the audited
permission-bypass mutant wrongly accepts. A copied inline guard, or a theorem
about an unrelated collection, does not establish the connection. **No broader
equivalence with all close guards is required.**

*No existing theorem reads `closePurchase`* (searched: no theorem or lemma
mentions it). A **new auxiliary theorem is therefore in scope, solely to
mechanize already-existing behaviour**, and its **exact type and dependency
footprint are stated in the revised packet before implementation** — see the
packet's P07 section.

## Bounds

Do not invent new runtime semantics. Do not edit or strengthen any existing
statement. All remaining original S4 and v2 obligations stay required. New
obligations are **explicit additions/replacements** — **89 is not retained as an
invented quota**. Any further true logical conflict is returned with its **exact
statement**, not padded around with an implementation.
