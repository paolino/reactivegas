# NOTE-006 — handback owed + rerun successor isolated M8 (binding)

To: commit-owner-s28r2. From: ticket owner t28-app-api (NOTE-041, binding).
The v1 runner pinned the START SHA, not the candidate (my binding slip —
owned in ticket STATUS; its own preflight correctly refused exit-3 with
zero builds). Successor `handoffs/isolated-m8-runner-v2.sh` is bound to
`ab25cd1` (full SHA) with logged strengthened preflight (HEAD-full +
clean + gate-norm + M8-presence), persisted M8 diff, staged-compare
(BLOCKER on live-vs-staged mismatch), and checker/gate/command records.

## 1. Handback required this round (timestamped, child-authored)

Append a handback event via status-event covering STEP1 (narrow exit-0,
exact command) + STEP2 (v1 exit-3 preflight refusal — no COMPLETE exists
for STEP2; record that absence, don't forge it). Use BLOCKED (preflight
stopped the run) or COMPLETE (for what completed) with the AUDIT-S28R1
marker discipline you already hold. Old lines preserved.

## 2. Books unambiguous (stop printing the superseded 14 ceiling)

Append current books with old lines preserved: builds 11/26 spent (GREEN
11B; SLIM 3B + isolated 1B + gate 11B newly available = 15: EXACT fit,
zero margin); targeted 4/24; diagnostic 2/4 (P-narrow spent). The failed
v1 attempt cost ZERO builds (refused pre-mutation) — books 11, not 12.

## 3. Rerun (1B) on instruction

Run the v2 runner (ONE substantive build). Report exit + ISO-LOG +
PRELOG paths + post-HEAD/tree + spend. Its result does NOT replace M8 in
the full gate. No other builds.
