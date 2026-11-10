# Pending and accepted pledges on the page, slice D — #70

Authority: [issue 70](https://github.com/paolino/reactivegas/issues/70) "Open work";
issue 69 "UI obligation" (the two regimes legible before they matter); desk
NOTE-D08 (2026-11-10) objective D; #69 handoff (guard table as built in PR 114).
Base: master `72bde8a`. Model position: Below — the page offers and labels what the
core accepts; it changes no Lean and no core guard.
Ceiling: this artifact 90 lines / 6500 bytes. Every row is BLOCKING.

## Stories

- Before acting, a member sees for each of their pledges whether it is pending
  (they may raise, lower or withdraw it themselves) or accepted (only the
  referente may correct it).
- While their pledge is pending, its owner can correct it or withdraw it from
  the page; once accepted, the page offers correction to the referente only.
- The page never offers a pledge or correction that the core would refuse for who
  signs it, and never hides one the core would accept.

## Acceptance

| ID | Must hold |
|---|---|
| D-REGIME-SHOWN | wherever the page lists a pledge (the purchase pile and the referente panel), it states its regime — pending (owner may correct or withdraw) or accepted (referente only) — and the stated regime equals the core state's (`pending` vs `accepted` row) |
| D-OWNER-CORRECT | for a pending pledge, the page offers its owner a correction (new amount > 0) signed by the owner; the step the page emits is `correctPledge` with the owner as signer and the core accepts it |
| D-OWNER-WITHDRAW | for a pending pledge, the page offers its owner a withdrawal; the step emitted is `correctPledge` with amount 0 signed by the owner; the core removes the pending row |
| D-ACCEPTED-REFERENTE | for an accepted pledge, correction is offered with the collection's referente as the only enabled signer; the owner and others are shown disabled with the guard named |
| D-SIGNER-SOUND | over the fixed set of generated histories (G1 seeds) and the four hand-written scenarios, at every state: every enabled (signer, pledge, correction/withdrawal/pledge) choice the page offers is accepted by the core, and every signer the core accepts for `correctPledge` and `pledge` on an existing collection is offered enabled; the pledge dialog never enables an author other than the pledging member |
| D-CANFAIL | mutants of the page (owner offered on an accepted pledge; referente offered on a pending pledge; withdrawal hidden; regime label swapped; a responsabile enabled as author of another member's pledge) are each RED inside `just simulator` |
| D-READ-ONLY | expected values come from the core state and the core's own verdict on each candidate step; the check computes no guard itself |
| D-DEFECT | a disagreement on the unmutated page between offer and core verdict is a page defect to fix in this slice only when it is a page offer; a core guard that looks wrong is a question to the ticket owner |
| D-KEEP | every existing gate and `--selftest` control still passes and still fails where it failed; the core is unchanged |
| D-BUDGET | the new checks fit a stated budget the gate enforces inside "Run the simulator gates" |
| D-CI | hosted CI green on every job of the pushed head |
| D-PREVIEW | PR preview job green; live preview bytes equal the tracked `economics-simulator.html` |

## Invariants

| ID | Observable truth |
|---|---|
| INV-D-ONE-CORE | the page's verdict on every offered step is the embedded core's; no second guard table |
| INV-D-CANFAIL | each mutant mutates the built page the check reads and is shown failing at RED |

## Non-goals

- NG-1 any change to `economics-simulator-core.mjs` or a core guard (D-STALL stays a recorded design choice).
- NG-2 `lean/**`, Haskell, Voci, `docs/en/design`.
