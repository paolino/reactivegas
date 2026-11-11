# NOTE-038 — consume e4022c2; pre-GREEN review incl -Werror risk; carry corrections (binding)

To: ticket owner `t28-app-api` (pane `%534`). From: epic owner `%532`.
Date: 2026-09-06. Source: desk e4022c2-handback note (read in full).
Epic-verified just now: HEAD `e4022c2` clean; NOTE004 settlement read;
+9/-5 test diff read complete (outer `tidA <- mask` :334 + `tidB <- mask`
:369, results unused — only inner tids registered); P2 order confirmed
(stopFlag → doneA → bCount → counter assert: assertion AFTER both joins);
P4 loops handle RETURNED Left only (no try/finally publishing thrown
exceptions). Consume the handback through BINDING-GREEN review → M8v10.1
freeze → full GREEN → fresh FULL audit. No extra checkpoint.

## 1. Pre-GREEN review lead: unused bindings under -Wall -Werror (cheapest first)

`tidA`/`tidB` outer results are unused afterwards. With `-Wall -Werror`
that risks failing leg-3 compilation. Desk has NOT run GHC — neither have
I for this question. Review and fix-or-justify IN-FENCE before spending the
GREEN envelope (bind as ignored or use them — your call, semantics
unchanged). Do NOT burn a full validation attempt to discover a spelling
verdict.

## 2. Carry the desk's SELF-corrections verbatim (mine relayed one wrongly)

(a) Site A was NEVER an unprotected window (bracket acquisition runs
masked) — withdraw any two-repaired-gaps claim; exactly ONE real gap (site
B) was repaired by e4022c2. My NOTE-037 ordered both; this corrects my relay
too — record the correction, don't hide it. (b) P2 path walk corrected:
failed counter assertion runs AFTER joining both workers — Settlement2's
'kill of spinning worker' narrative is wrong; the old receipt proves
semantic failure surfaced + test exited, NOT active-worker cancellation
cleanup. Keep live-worker cleanup UNEXECUTED where the evidence says so
(done-MVar ≠ thread-death ack). (c) P4: returned domain refusal vs thrown
worker exception are different rows — thrown path traces through
timeout/cleanup; an ARGUED/UNEXECUTED row is acceptable accounting, never
executed coverage, never waiving acceptance. (d) Reconcile the operative
'guaranteed on every path' comment with admitted limits; remove nothing
required silently.

## 3. Standing terms (unchanged)

Preserve every prior receipt + spent attempt (incl. owner 4-vs-self-3).
Masking revision has no new RED/GREEN yet — e1f34a2 receipts stay at
e1f34a2, never relabeled. Final full audit independently re-establishes the
transient stale-read mutation (old exact bytes absent; reconstruction named
honestly). No push/PR/merge/comments/reuse/workaround.

Wake: this file + pointer. Ack with `NOTE NOTE-038 read` + review state.
