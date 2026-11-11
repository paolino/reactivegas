# NOTE-031 — F3 repair proposal + cost accounting (binding, static only)

To: ticket owner `t28-app-api` (pane `%534`). From: epic owner `%532`.
Date: 2026-09-06. Source: desk F3-handback note (read in full).
Epic-verified just now: terminal report `24252ef1…` (FINDINGS incomplete,
F3 BLOCKING on R2, F1/F2 credited, zero OPEN rows, candidate unaccepted);
F3 site `Store.hs:618-627` (`evaluate payloadText` before `withMVar` +
`applyIntegratedEvent` — faulting codec throws before the required
nonmember refusal; comment documents the rendezvous motive); P2 raw output
as stated (counts `(0,0,0,0,0)`, changed refusal behavior, NOT unauthorized
append or state corruption); auditor NOT terminally sealed (no COMPLETE/
manifest yet — finalization proceeds uncoached, no extra executions).

## 1. ONE concrete F3 repair proposal (original FULL scope — static prep only)

Preserve validate/refuse behavior AND concurrent conservation together — a
fix breaking one to satisfy the other is rejected in advance. Assess the
rendezvous dependency head-on (production comment: early encoding
accommodates the auditor serialization rendezvous): the test harness must
observe the intended production property WITHOUT forcing production
evaluation order merely to make its scheduling trick work. No locking
implementation prescribed from anywhere. Required acceptance (state
verbatim): existing full original rows + faulting-codec accepted/refused
controls with state/count checks + concurrent conservation + lock release +
appropriate can-fail controls. Prior PASS remains evidence, never an
inherited verdict at new bytes. No source edits, investigation, tests, or
builds under this note.

## 2. Complete cost proposal BEFORE any new repair execution

Command-level OWNER costs + FRESH FULL AUDITOR costs + remaining
submission/raise accounting, all explicit: current R1 one-submission is
SPENT by this report (unused builds do NOT imply a new submission);
historical owner34 + audit9/7, R1 owner13 + audit10/16, and the zero-build
invalid admission stay separately visible. No automatic new submission, cap
reset, smaller audit, push/PR/merge. Static proposal preparation authorized;
execution awaits ruling.

## 3. Auditor finalization (hands off, eyes on)

Let the auditor complete its terminal seal uncoached. Consume its terminal
packet on own COMPLETE + manifest (verify hashes + outcomes yourself);
supervise to that event with a tested COMPLETE-capable wait. Your journal
missed 00:13–00:42 execution entirely — record the gap and the fix (read
journals every round).

Wake: this file + pointer. Ack with `NOTE NOTE-031 read` + proposal path when ready.
