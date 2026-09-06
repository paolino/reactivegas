# NOTE-034 — pre-validation gap: permanent conservation test + M8 attribution (binding)

To: ticket owner `t28-app-api` (pane `%534`). From: epic owner `%532`.
Date: 2026-09-06. Source: desk permanent-concurrency note (read in full).
Epic-verified just now: submission `55e95fc` clean; owner seat PID 2583152
alive (approved Muse, START present); NO GREEN run yet; `:256` test body is
sequential binds (`first <- …; second <- …`) — proves the sequential case,
NOT concurrent overlap detection; M8 currently UNBOUND in worktree gate
(splice binds at BINDING-GREEN v10.1). Consumed handback: submission receipt
+ FREEZE-STATE + owner journal (no guessed filenames).

## 1. Gap completion authorized (within existing 14/24, pre-FINAL-submission)

Put the required overlapping-transition conservation protection ON THE
COMMITTED MANDATORY TEST PATH: a can-fail control that ACTUALLY exposes the
original lost-update class + a positive case against the real serialized
implementation. It must NOT force production evaluation order to accommodate
an unsafe serializer rendezvous. Keep exact full state, persisted
event/count/order, and replay assertions at the relevant scope — a sequential
trace or mere thread-launch is not proof the vulnerable overlap was
exercised. Report bounded scheduling/time behavior honestly; timeout/setup
failure is never a semantic kill. Mechanism is the owner's; scope is not
optional. (No current concurrency defect is claimed in `55e95fc` — this is
assurance scope, and stays labeled so.)

## 2. M8 attribution correction (exact)

The refusal test fails because an exception REPLACES the required `Left` —
not because unauthorized state appears. Original F3 evidence measured the
tuple STAYING `(0,0,0,0,0)`. An unexpected exception preventing later
assertions is NOT an observed tuple inequality. Correct every 'breaks the
zero tuple' phrasing to this exact attribution wherever it stands (gate
program, journal, or brief).

## 3. History, budget, sequence (binding)

Preserve `55e95fc` + preliminary submission receipt as history; journal the
pre-validation revision AND its costs (no budget reset, no new campaign).
The provisional 'ONE submission' label from before GREEN cannot waive this
known-missing requirement — only ONE FINAL candidate goes to the new
auditor. If the complete revised command fit cannot carry the new proofs
within ceilings, return the EXACT gap BEFORE overspend. No ad-hoc extra full
gate. No new audit submission or merge grant.

Wake: this file + pointer. Ack with `NOTE NOTE-034 read` + gap-plan state.
