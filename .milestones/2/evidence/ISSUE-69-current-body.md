Operator ruling, 2026-09-05: **a member may change or withdraw their own pledge freely while it is `pending`. Once the referente has accepted it, changes require the referente.**

Rationale in the operator's terms: before acceptance the money is still the member's and nobody has relied on it. After acceptance the referente may have committed to the supplier, and a unilateral withdrawal strands them.

## What is wrong today

Verified on master:

```lean
| .pledge u c v => do
    demand (isResponsabile view signer && GroupView.isMember u view && …)
```

**A member cannot pledge for themselves at all** — `pledge` requires a responsabile signer. That is #48's central "sovereign members" ruling and it has never landed.

And `correctPledge` demands `isResponsabile view signer && col.referente == signer` and reads only `col.accepted`. So a member can neither create, change, nor withdraw their own pledge at any point in its life. The operator found this by hand: *"once a membro si è impegnato it's impossible to change the amount (or retract)."*

## The change

- **`pledge`** — authorize `signer == u`.
- **`correctPledge`** — two regimes:
  - pledge in `col.pending`: authorize `signer == u`. `v' = 0` is withdrawal; the escrow return `bump s.conti u (v - v')` is already correct.
  - pledge in `col.accepted`: keep the existing referente authorization, unchanged.
- keep the solvency guard `bal s.conti u + (v - v') ≥ 0` — it already makes an increase affordable.
- `closePurchase` already demands `col.pending.isEmpty`, so closure is undisturbed.

## This supersedes part of #48

#48's departure design describes app events that #62 deleted (`addUser`, `electResponsabile`, `removeResponsabile`, `removeMember`). That part is superseded and must not be implemented. What survives of #48 is exactly this: member sovereignty over one's own pledge.

## UI obligation

The two regimes must be legible **before** they matter. A member told "you can change this" and later "you cannot", without being shown why, repeats the two-stage election flow the operator hit with Carlo and Enzo.


## Acceptance consequence: pending withdrawal

A pending correction to `v'=0` refunds the pledged amount and removes the pending entry, without creating an accepted entry. Keeping a zero-valued pending row would leave `closePurchase` blocked by `pending.isEmpty`, contrary to withdrawal with the closure guard unchanged. Removing the final pending pledge removes that blocker only; all other close guards still apply. The existing accepted-pledge correction behavior remains unchanged.

Acceptance needs reachable signed production-root journeys for self-service creation, affordable correction, withdrawal, and the transition to referente-only correction after acceptance, plus impersonation, unaffordable-increase and negative-amount controls. Conservation, solvency and uniqueness must retain their stated scope. The simulator UI handoff remains part of the issue: the pending/accepted distinction must be visible before the action.
