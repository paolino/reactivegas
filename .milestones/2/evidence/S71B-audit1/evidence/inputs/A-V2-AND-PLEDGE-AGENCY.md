# Operator rulings — 2026-09-05

Two decisions taken by the operator. Both bind the Lean and everything
transcribing it.

## V-2 — the proposer does not count as an assent

**Ruling: a proposal opens at zero assents. The proposer's own signature is not
an assent.**

Today `KelGroups.majority = (adminCount + 1) / 2` and the proposer is counted,
so at two admins one carries a proposal alone, and at three a single admin can
build a leadership of three.

The arithmetic is **not** changing — `(adminCount + 1) / 2` stays. What changes
is that the proposer no longer supplies one of the required assents.
Consequences:

| admins | required | who must agree |
|---|---:|---|
| 1 | 1 | the founder alone — unavoidable, and unchanged |
| 2 | 1 | **someone other than the proposer** |
| 3 | 2 | proposer + 2 others, or 2 others |
| 5 | 3 | 3 others |

So every decision above n=1 now requires assent from someone who did not
propose it. The n=2 anomaly disappears as a consequence rather than as a
special case.

This aligns the base channel with the vote machine, which already opens
questions with **empty tallies** and does not credit the proposer
(`applyVoteEvent`, `Fold.lean:77`, annotated "divergenza deliberata dal
legacy"). The two channels stop disagreeing about what proposing means.

**Not authorized:** changing the majority formula. `floor(n/2)+1` was offered
and declined.

## Pledge agency — free while pending, referente after acceptance

**Ruling: a member may change or withdraw their own pledge freely while it is
`pending`. Once the referente has accepted it, changes require the referente.**

Rationale, in the operator's terms: before acceptance the money is still the
member's and nobody has relied on it. After acceptance the referente may have
committed to the supplier, and a unilateral withdrawal strands them.

Consequences for `Step.lean`:

- **`pledge`** — authorize `signer == u`. It currently demands
  `isResponsabile view signer`, so a member cannot pledge for themselves at
  all. This is #48's central ruling and it has never landed.
- **`correctPledge`** — two regimes, not one:
  - pledge in `col.pending`: authorize `signer == u`. `v' = 0` is withdrawal;
    the escrow return `bump s.conti u (v - v')` is already correct.
  - pledge in `col.accepted`: keep the existing referente authorization.
    Unchanged.
- keep the solvency guard `bal s.conti u + (v - v') ≥ 0` — it already makes an
  increase affordable;
- `closePurchase` already demands `col.pending.isEmpty`, so nothing about
  closure changes.

**The UI must make the two regimes legible.** A member seeing "you can change
this" and later "you cannot" without knowing why is the two-stage election flow
all over again. Pending versus accepted must be visible *before* it matters.

## Scope note

Neither ruling is authorized for anticipation. They land as their own slices,
against current master, with fresh audits. Until then the simulator keeps
transcribing the Lean verbatim — the oracle pinning `bgMajority` at n=2 → 1
stays correct and must not be softened in advance.
