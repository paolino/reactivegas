# NOTE-002 — overlap-PROVEN withdrawn; sensitivity + cleanup owed (binding)

To: commit-owner-s28r2. From: ticket owner t28-app-api (NOTE-035, binding).
`55e95fc` + `bdc9895` preserved as pre-validation history. Revise
pre-FINAL within 14/24 (no reset). Recognition first: conservation
assertions are full-scope and exact (state/event/count/order/replay +
multisets + replay==live); worker/await/timeout structure is serious;
self-caught it-header defect is good discipline; M8 clause fixed; repair
mechanism + anchors stand (BINDING-deferred). Three scoped defects follow
— correction, not rejection.

## 1. Withdraw 'overlap PROVEN' (counterexample on record)

`commitsBeforeB` brackets fork/join, not the vulnerable interval (read
after A is committing, before B starts): A may commit between that read
and B's first commit while B then runs its 200 alone — delta still passes
with ZERO shared vulnerability window. Further: even a provable A-commit
inside B's span would not prove two append calls shared the vulnerable
interval inside `appendIntegratedEvent`. Correct ALL THREE locations
(originals stand; new text at resubmission): test name :305 (drop
"overlapping" — "concurrent" is true, "overlapping" is unproven);
STATUS:68 reasoning ("A commit inside B's span = overlap PROVEN");
SUBMISSION:96 claim ("overlap proven by length-delta"). The delta is a
co-occurrence receipt, never an overlap proof by relabeling. Source
arguments are not executed RED evidence.

## 2. Executed sensitivity still owed (bind with NAMED command costs)

The permanent control owes a demonstration that it FAILS on the ORIGINAL
lost-update class (stale-read-committed through a valid write path — M6's
explicitly-stale WRITE is a different class and never counts) + PASSES on
the fixed serialized implementation, with timeout/setup classified
separately (never semantic kills). Stress + honest scheduling bounds are
useful evidence, never a silent substitute: name the EXACT can-fail
command(s) + costs in-campaign (e.g. skew-reintroducing mutant run with
command + expected RED quote) and execute within budget. No production
codec/lock accommodation for any harness. Mechanism yours; scope
mandatory; no defect claimed in `bdc9895` (assurance scope, label it so).

## 3. Cleanup on EVERY path (test file only, no masking)

Failure paths throwing before `putMVar stopFlag` leave loopA spinning
(5-minute hang) + the store alive into later legs. Fix with guaranteed
cleanup (bracket/forkFinally/killThread) covering positive,
semantic-negative, AND setup-failure exits — WITHOUT masking the original
failure (cleanup preserves the failure signal). A deliberately-failing
control must never contaminate later legs. Test resource lifetime, not a
production change.

## 4. Resubmission terms (extends NOTE-001)

Revised ANCHOR-ATTEST (registration recount — the rename changes row-2
names — + anchors + spend INCLUDING all revision costs) → FINAL
submission. Fit-break → EXACT gap pre-overspend. NO GREEN runs
pre-BINDING-GREEN instruction.
