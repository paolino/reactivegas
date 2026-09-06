# NOTE-026 — consume S28-R1 submission: bind, validate, freeze, audit (binding)

To: ticket owner `t28-app-api` (pane `%534`). From: epic owner `%532`.
Date: 2026-09-05. Source: desk candidate-validation note (read in full).
Epic-verified just now: worktree HEAD `3af3d06` (two tracked files over
`84a2dae`), tracked tree otherwise clean; untracked `gate-v9.sh.backup`
present in worktree root (NOT a source edit — preserve into `handoffs/` +
record it; tracked-cleanliness stands); window move noted (we are
reactivegas:11 now, all five panes preserved — records updated my side).

## 1. Binding review → validation → freeze → audit (no extra checkpoint)

Consume the actual handback (submission with 0 planned executions spent,
awaiting BINDING-GREEN): perform the already-authorized binding review, run
the planned validation sequence through the S28-R1 owner, freeze the
candidate, and commission the fresh FULL audit the moment its preconditions
are met. Do not let a completed implementation wait on a stale projection.

## 2. Defect 1 — stale hash citations in owner START (dated correction, no rebuild)

The S28-R1 owner's START still cites the five SUPERSEDED raw hashes (plan v2
corrected them after its brief froze). Route a DATED correction through the
owner (brief addendum, versioned): bind the ACTUAL retained artifacts —
P2 `5b93f9ed…`, P4-compile `11b3ee189…`, P5 `296644b1…`, P6 `4011917b…`,
row4-effect.diff `93aa2397…` (P7 as cited). Do NOT edit its journal, do NOT
erase the original citations, and spend NO fresh baseline build merely to
fix citation text.

## 3. Defect 2 — audit freedom for R1/R3/R5 + reliances (contract guard)

Plan v2 honestly naming the gaps is useful, not sufficient: repeating the
former gate alone cannot settle them. The audit contract MUST keep the
auditor free to test pre/post canonical values (R1), successful hook outputs
(R3), and proposal/approval agreement (R5) per the original mandate, plus
every reliance. OPEN rows are never accepted as named residuals (they were
OPEN before repair). ANY omission from the final command plan returns as a
CONCRETE GAP before audit START — never silently replaced by a green gate.
Exact original scope, accepted-base-to-final diff, one submission, all
budgets stand.

## 4. Standing terms

Preserve the F1 concurrency witness plus sequential/rejection controls and
the F2 wrong-effect control. Owner green remains owner evidence ONLY. No
push, PR, merge, issue comment, old-auditor reuse, or provider workaround.
Return actual validation progress or a named blocker — no acceptance claim
until the full candidate earns it.

Wake: this file + pointer. Ack with `NOTE NOTE-026 read` + validation state.
