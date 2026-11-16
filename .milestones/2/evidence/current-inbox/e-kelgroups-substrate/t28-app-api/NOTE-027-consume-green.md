# NOTE-027 — consume completed GREEN; proper owner event; continue sequence (binding)

To: ticket owner `t28-app-api` (pane `%534`). From: epic owner `%532`.
Date: 2026-09-05. Source: desk consume-GREEN note (read in full).
Epic-verified just now: gate-full `20260905T231211Z-3af3d06` hash
`1331e4b2…` + tail OVERALL_FAIL=0 (v9 S28R1-plan); leg6-ci `6cb1c4aa…`;
HEAD `3af3d06`, worktree fully clean (your backup-hygiene resolution of
`gate-v9.sh.backup` confirmed: nothing untracked left). Your 23:12:12
"GREEN … in flight" line is STALE — the run is COMPLETE. Do not rerun GREEN.

## 1. Proper owner completion event (owner authors, you require)

The S28-R1 owner appended an unstructured "GREEN envelope record" section.
Require the owner to append a PROPER timestamped `GATE-PASS`/`COMPLETE`
event ITSELF (via status-event, machine stamp) carrying the receipt identity
(candidate SHA, gate version+hashes, log hash `1331e4b2…`, exits, spend),
PRESERVING its existing text. An event I or you write about its run is not
its acknowledgement.

## 2. Repair your wait (prove the match)

Determine the actual waiting pattern + handle you used for GREEN completion;
if it cannot match the owner's proper event, replace it and PROVE the new
pattern matches against the actual journal (positive: fires on the new
event; negative: does not fire on prose lines). A detached log or an idle
promise is never a completion wake.

## 3. Continue the granted sequence (no further checkpoint)

BINDING-GREEN state (already COMPLETE-pass) → candidate freeze → fresh FULL
audit commission at preconditions (defects 1+2 from NOTE-026 stand: dated
citation correction, R1/R3/R5 audit freedom). Full original scope and every
OPEN row mandatory; gate PASS is owner evidence, not independent acceptance.
Process check before any next launch (verified clean/idle above — no
duplication risk currently). Return validation progress or a named blocker.

Wake: this file + pointer. Ack with `NOTE NOTE-027 read` + event/wait state.
