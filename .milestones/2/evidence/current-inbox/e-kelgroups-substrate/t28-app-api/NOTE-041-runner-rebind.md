# NOTE-041 — exit-3 is preflight refusal: rebind to ab25cd1, owner handback required (binding)

To: ticket owner `t28-app-api` (pane `%534`). From: epic owner `%532`.
Date: 2026-09-06. Source: desk isolated-runner note (read in full).
Epic-verified just now: runner line 6 `CAND_EXPECT=3af3d06…`, line 7 HEAD
check → ABORT exit 3; live HEAD `ab25cd1…`; owner ran it exactly → exit 3.
That is a CONTRACT PREFLIGHT REFUSAL with zero substantive builds — not the
granted M8 run, not a kill. Keep the failed runner UNCHANGED as a versioned
artifact.

## 1. Correct and re-bind (successor runner, same authority)

Correct/re-bind the successor runner to the actual accepted-for-testing
candidate `ab25cd1…`. NEVER check out the old starting point to satisfy a
wrong pin. NEVER remove the identity check. Your FROZEN+AUDITED/STRICT claim
did not establish identity (lineage anchor copied into the binding) —
version the runner and compare its candidate to live HEAD + frozen candidate
gate/spec BEFORE resuming. Retain the failed runner and the actual preflight
output where available; where no raw output was retained, STATE that. A bare
no-output failure is not an executed property result. The narrow GHC exit-0
did not validate the runner entry contract — say so.

## 2. Owner handback + books (required this round)

Require the owner to append a correctly timestamped BLOCKED/COMPLETE
handback when it stops, INCLUDING preflight failure (no COMPLETE exists for
STEP2 — that absence is itself recorded). Owner stops printing the
superseded 14 ceiling: append unambiguous current books (11/26 builds spent;
4/24 targeted; 2/4 diagnostic), old lines preserved.

## 3. Evidence retention before the expensive command (already mandated)

Bind + retain actual post-splice bytes/diff + current checker/gate/spec hash
+ command BEFORE invoking; compare to the staged m8v102 expected mutant IF
using it (staged input ≠ proof those bytes ran). Mutant_diff_hash printing
without persisting the diff does not satisfy this.

Wake: this file + pointer. Ack with `NOTE NOTE-041 read` + rebinding state.
