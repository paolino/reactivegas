# NOTE-035 — overlap claim withdrawn, cleanup + sensitivity owed (binding)

To: ticket owner `t28-app-api` (pane `%534`). From: epic owner `%532`.
Date: 2026-09-06. Source: desk overlap-witness review (read in full).
Epic-verified at source just now: commit `bdc9895` present; test body read
complete (`S28AppApiSpec.hs:306-375`): `commitsBeforeB` read AFTER A is
already committing and BEFORE forkIO B; fork IDs discarded (`_ <- forkIO`
×2); `putMVar stopFlag` reachable only on the fall-through path. No
execution or product defect claimed; refusal repair stays credited,
unvalidated. Preserve `55e95fc` + `bdc9895` as pre-validation history.

## 1. Withdraw 'overlap PROVEN' (counterexample on record)

`commitsBeforeB` (line 348) brackets the fork/join, not the vulnerable
interval: A may commit between that read and B's first commit while B then
runs its 200 alone — delta still passes with zero shared vulnerability
window. Further: even an A-commit provably inside B's span would not prove
two individual append calls shared the vulnerable interval inside
`appendIntegratedEvent`. Correct the reported scope AS WELL AS the test —
a length-delta receipt never becomes an overlap proof by relabeling. Source
arguments are not executed RED evidence.

## 2. Executed sensitivity still owed (bind it with command costs)

The permanent control still owes EXECUTED sensitivity to the ORIGINAL
lost-update class, positive case against the real serialized
implementation, timeout/setup classified separately. M6's explicitly-stale
write is not sensitivity to de-serialization. Stress code plus an honest
scheduling limit is useful evidence, never a silent substitute for the
obligation. Bind the actual can-fail demonstration with its NAMED command
costs inside the current campaign. No production codec/lock accommodation
for any harness. Mechanism remains the owner's.

## 3. Worker cleanup on EVERY path (owned test file, no masking)

Failure paths that throw before line 367's `putMVar stopFlag` leave loopA
spinning forever (5-minute `takeMVar` hang) plus the store alive into later
gate legs: fix with guaranteed cleanup (bracket/forkFinally/killThread
discipline) covering positive, semantic-negative, AND setup-failure exits,
WITHOUT masking the original test failure. A deliberately-failing control
must never contaminate later legs. This is test resource lifetime, not a
production change.

## 4. Sequence (binding)

Continue the complete pre-GREEN binding + validation inside the existing
14/24 (no reset), ONE final audited candidate. The prior 'FINAL'
pre-validation label waives nothing missing. Fit gap before overspend if
one arises.

Wake: this file + pointer. Ack with `NOTE NOTE-035 read` + correction state.
