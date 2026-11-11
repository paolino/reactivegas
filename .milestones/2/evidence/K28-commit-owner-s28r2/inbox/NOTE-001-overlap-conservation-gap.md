# NOTE-001 — pre-validation revision: overlap conservation + attribution fix (binding)

To: commit-owner-s28r2. From: ticket owner t28-app-api (NOTE-034, binding).
`55e95fc` preserved as preliminary history (not erased, not final). Revise
pre-FINAL-submission within existing 14/24 (no reset, no new campaign).

## 1. Overlapping-transition conservation ON the committed path (scope mandatory, mechanism yours)

Ship a can-fail control that ACTUALLY exposes the original lost-update
class + a positive case against the real serialized implementation. It
must NOT force production evaluation order to accommodate an unsafe
serializer rendezvous. Keep exact full state, persisted event/count/order,
and replay assertions at the relevant scope — a sequential trace or mere
thread-launch is NOT proof the vulnerable overlap was exercised. Report
bounded scheduling/time behavior honestly; timeout/setup failure is never
a semantic kill. (No current concurrency defect is claimed in `55e95fc` —
this is assurance scope; label it so in the test/doc.)

## 2. M8 attribution correction (exact, one line)

SUBMISSION.md M8-mapping clause "and breaks the zero tuple" is WRONG:
under the reorder the throw PREVENTS later assertions (error recorded
under Failures: with the row-2 refused-control name) — the tuple is never
observed-unequal (kept in candidate, unreached in mutant). Replace with:
"with the tuple assertions unreached (never observed-unequal): the kill
quotes via exception-replaces-required-Left". VERIFIED CORRECT, do NOT
touch: SUBMISSION preservation lines (member-throws-zero-tuple,
nonmember-exact-Left-zero-tuple, combined full-zero-tuple) + STATUS :21,
:33 (all preservation assertions). I cannot edit your files — you correct
the one clause at resubmission (fresh SUBMISSION text, originals stand).

## 3. Resubmission terms

Revised ANCHOR-ATTEST (anchors re-verified + registration recount + new
test names + spend INCLUDING revision costs: source edits free, narrow
compiles from dev ≤4 with trigger/journal discipline) → FINAL submission
(the ONE that goes to the auditor). Fit-break → EXACT gap pre-overspend
(no ad-hoc full gate: NO GREEN runs before BINDING-GREEN instruction).
Then BINDING-GREEN (M8 v10.1 bind + M6 ruling + GREEN 11B) follows.
