# Functions model — #48 inversion coverage amendment

Artifact ceiling: 90 lines / 7 KiB. Only new theorem/check signatures and
signature-level constraints are listed.

## `Reactivegas.Invariants`

- `step_open_inv` — successful `openPurchase` step; concludes the complete
  open guard and exact successor state.
- `step_deposit_inv` — successful `deposit` step; concludes its complete guard
  and exact successor state.
- `step_withdraw_inv` — successful `withdraw` step; concludes its complete
  guard and exact successor state.
- `step_transferCassa_inv` — successful `transferCassa` step; concludes its
  complete guard and exact successor state.
- `step_donate_inv` — successful `donate` step; concludes its complete guard
  and exact successor state.
- `step_backdonate_inv` — successful `backdonate` step; concludes its complete
  guard, including caller-supplied authorization, and exact successor state.

Every theorem quantifies the existing view, state, event arguments,
authorization evidence, and successful successor needed by its branch. No
existing theorem statement changes.

## Permanent coverage checker

- normal mode derives constructor and theorem-event sets, reports counts and
  gaps, and exits nonzero unless coverage is exact;
- negative-control mode dynamically withholds derived coverage, requires the
  same comparison to fail, and exits nonzero if the defect is not detected;
- the checker accepts no authoritative hand-written constructor list.
