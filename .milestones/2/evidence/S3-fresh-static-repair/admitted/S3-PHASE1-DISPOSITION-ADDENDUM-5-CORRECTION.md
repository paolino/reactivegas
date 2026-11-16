# S3 disposition — addendum 5: OP-25 reclassified, and Addendum 4's title corrected

Owner `%503`. Append-only; Addenda 1-4 preserved unchanged. Static, no execution.

## 1. OP-25 `credit_pledges_step` — my (a) verification was wrong

Addendum 4 listed it as a verified **(a)** ground. **It is not.** Verified at source:

- `Step.lean:82-83`, donate arm: `conti := bump s.conti comuneId v`. The mutation
  drops **only the comune credit**.
- The theorem (`Invariants.lean:890`) concludes
  `∀ u, u ≠ comuneId → bal s'.conti u ≥ 0` — it **explicitly excludes the comune**.
- Its donate proof (`:1107-1109`) is `rw [bal_bump_ne hwne]; exact hcred w hwne` —
  it rewrites `bal_bump_ne` precisely **to remove** the comune bump from the goal,
  then closes from `hcred`.

With the bump dropped, `bal s'.conti u = bal s.conti u` for every `u ≠ comuneId`,
so the property **still follows from `hcred` and `hamt`**. The statement stays
true. What breaks is the `rw`: there is no bump left to rewrite.

**That is proof-shape sensitivity (P), not (a).**

My error precisely: I saw `bal s'.conti` in the conclusion and called the mutated
field projected. But the mutated part is *the comune's* entry, and the conclusion's
`u ≠ comuneId` guard excludes exactly that. Projecting `bal s'.conti` in general is
not projecting the changed entry. Worse, my "donate-arm case" note shows I *saw*
the guard and then reasoned backwards from it — treating it as a hint that the
effect must surface elsewhere, when it is the thing that excludes the effect
entirely. **Do not demand an effect surface on another balance when the mutation
changes none.**

Corrected tally: verified (a) grounds **2**, not 3 — `step_grant_inv` and
`step_close_inv` stand. `credit_pledges_step` is **reclassified as proposed (P)**,
and it goes to the repair owner as an input, **not** as a waiver of the original
mutation criteria. No executed kill is claimed for it by me or by the desk.

## 2. Addendum 4's title overstated its own limit

Its heading reads *"remaining classes reviewed"*. That is **broader than its own
stated content**, which reports **94 rows explicitly not reviewed** — 51 (a)
grounds and 43 GREEN-justified ELAB-STATIC rows. The body was accurate; the title
was not, and a title is what survives a skim.

Read Addendum 4 as: **"remaining classes surveyed by kind; 94 rows not reviewed."**
The exact limit stands as written in its §7, and this is the second time in this
assessment that a heading of mine claimed more than the text under it.

## 3. Status of Phase 1

**Addendum 4 is a valid capacity handoff. It is not a full accepted assessment and
not Phase 1 acceptance.** Phase 1 remains **unaccepted**; no Phase 2 grant exists.
My five-CLOSED / three-PARTLY labels are **my disposition**, and they are **not
inheritable** by any later audit — the fresh auditor covers the complete returned
artifact and the original mandate, including those labels.
