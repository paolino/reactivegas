# OWNER-SUBSTITUTION-HANDOFF — t30-contract (outgoing Muse → incoming Opus)

Author: outgoing ticket preparation owner `t30-contract` (Muse), 2026-09-06.
Authority: SUBSTITUTION-ORDER (epic owner %532 outgoing, operator-ordered).
Scope of this file: safe-boundary handoff ONLY. It references full contracts,
never rewrites them. No instruction beyond handoff; no edits after it.

## 1. Seat + runtime + liveness (safe boundary as ordered)

- Role: ticket preparation owner `t30-contract`, kelgroups#30 CONTRACT
  PREPARATION ONLY (planning altitude; standing fences: no compilation/
  tests/mutations/gate runs/dispatch/product edits/commits/push/PR/merge/
  release/comments; spend 0 product builds — HELD for the entire tenure).
- Runtime root: `/tmp/reactivegas/ms2/e-kelgroups-substrate/t30-contract/`
  (subdirs: `handoffs/` contracts+scripts, `scratch/pf1/` closed campaign,
  `scratch/pf7/` unexecuted, `scratch/pf8/` bound campaign, `inbox/`,
  `questions/` (empty), `answers/` (empty), `STATUS.md` journal).
- Pane: addressed as `%572` in all notes (self-unverified — labeled
  unknown; trivially confirmable by successor).
- Live processes/children: NONE owned, none ever dispatched in this lane;
  no monitors/beats/background jobs ever started (all waits were
  foreground-complete). Last execution (pf8 invocation 1, below) completed
  synchronously with exit 1 — no live processes (parent-verified "no
  invocation running" + self-consistent). Command state: idle/between-
  turns, nothing running. Nothing killed (no children exist).

## 2. Budget reconciliation (DIFFERS from the order's premise — stated plainly)

The order records the ≤2-invocation budget "preserved untouched (0
spent)". Actual, reconciled against artifacts: pf8 INVOCATION 1 DID
execute under PREFLIGHT-INVOCATION-1-BINDING (hashes verified pre-run —
see §4) and returned SUITE FAIL (setup-failures=0, mispredicts=17;
evidence intact at `scratch/pf8/cases/` — 19+ case dirs — plus
`scratch/pf8/ev/SUITE.log`). Its outcome was NEVER journaled (no STATUS
line, no pointer): the substitution pointer arrived mid-turn between
execution and journaling — timing, not concealment; the evidence tree is
complete and reviewable. CORRECTED REMAINING BUDGET: ONE authorized repair
rerun (pre-authorized by the binding's failure-spends clause),
UNLAUNCHED — preserved for my successor per order §1. Nothing launched
after the order. Product-build spend: 0 throughout (all campaign
executions are shell+git plumbing on synthetic fixtures — own counter).

## 3. Accepted/current identities (bases, briefs, packets — hashes verified)

- Brief: `t30-contract/brief.md` sha256
  `f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`.
- Bases: kelgroups `main` @ `933e385df2f2a251bb54a08bb7663f0d41fafb64`
  (S28 LANDED); accepted Lean Reactivegas @ `3590c0015b84fd58004bf6fb44dd18
  b107304c48` (zero-diff EMPTY vs `4a6cd87` on the Vote+Integration/State/
  Validate extent).
- Filed packets (all retained under `handoffs/`, hashes as recorded at
  filing): r1 contract `57d9ba9e…` + map `bdb2459b…`; r2 `e1d0e54f…` +
  `a2ab5295…`; r3 `7551451b…` + `9200abbc…`; r4 `f53471f1…` + `a05296da…`;
  r5 contract `ad88ab9b…` + map `eacf20dc…` + identity-map `576087db…` +
  drift-leg `18bcc8a5…`; r6 contract `a1b2d942…` + map `77970cc1…` +
  appendix `dc8a2cf2…` + drift-leg `ff9ade6e…`; r7 contract `21c1cd09…` +
  map `3c68dda2…` + drift-leg `bee4b6c5…` + pf7 runner `8e8b62fa…`;
  r8 drift-leg `f0afa32b4fbb13ac` + pf8 runner `62025c179e85b6ab` +
  contract `ea6c2019892da214` + map `ca033b1edcd7def8` (r8 hashes computed
  read-only at handoff time — first recording; r8 was never pointer-filed).
- Proposed ceilings (CURRENT, all PROPOSED pending fit-proof +
  authorization — estimates as estimates): owner 26 builds / 24 probes;
  auditor 25 builds / 24 probes. History: 18/24+12/24 (v3 est.) →
  20/24+12/24 (r1) → 24/24+12/24 (r2) → 23/24+22/24 (r3/r4) → 25/24+24/24
  (r5) → 26/24+25/24 (r6–r8). Full scope + L-1–L-7 record preserved
  throughout (see contracts, never rewritten here).
- Campaign artifacts: `scratch/pf1/` (closed: 2 plumbing invocations, own
  counter; inv2 tree retained); `scratch/pf7/` (written, UNEXECUTED — zero
  invocations consumed on it); `scratch/pf8/` (BOUND campaign: invocation
  1 executed per above; repair rerun unlaunched).

## 4. Inbox — fully consumed, nothing unread

NOTE-001 (clock) + NOTE-002 (helper-only) + NOTE-003 (r2) + NOTE-004 (r3
gaps) + NOTE-005 (r4 live-mechanisms) + NOTE-006 (r4 rejected; falsehoods
owned) + NOTE-007 (preflight campaign) + NOTE-008 (r6 ruling; successor +
single request) + NOTE-009 (runner fixes) + PREFLIGHT-INVOCATION-1-BINDING
(launch authority — consumed by exactly one launch) + SUBSTITUTION-ORDER
(this handoff; ack = HANDOFF-READY line). All read in full; all acked in
STATUS except this order (ack = HANDOFF-READY). `questions/` empty (zero
questions ever filed — every defect was resolvable in planning).

## 5. Pending decisions (in-flight draft work — NAMED, not implemented)

(a) pf8 repair-rerun design (diagnosis COMPLETE from preserved evidence,
repair UNWRITTEN — no new-tree copy exists yet): root causes — (i) hs
fixture dirt (dist-newstyle untracked → clean-hs RED on all cases; fix:
committed `.gitignore` in template); (ii) freshness nondeterminism
(`cp -r` copy-order vs marker mtime; fix: fixed-old-date marker, e.g.
2000, dumps always newer); (iii) r8-script line 53 missing `#`
(comment-continuation parsed as a command — two stderr noise lines, zero
verdict impact: failed no-op, OVERALL_FAIL untouched — fix: one `#`;
r8 bytes otherwise untouched); (iv) A20 assert missing colon
("1-hash ..." vs actual "1-hash: ..." — runner-assert fix); (v) A22
mechanism correction (live-committed helper CANNOT fire source-hash rows
— they read the frozen oid by design; redesign as overlay HS-edited with
metadata constant, staged dumps, per NOTE-009-4). Repair home: NEW tree
(copy pf8 → pf8r first — never overwrite inv1 streams), repaired copies
inside it (r8 bytes stay frozen in handoffs/), then the single authorized
rerun. (b) The authorized repair rerun itself (launch decision + timing
within the binding's remaining authorization — successor's call, or refer
up). (c) CLOSED (not pending, recorded against confusion): R6-RERUN-01
was DENIED by NOTE-008 (not open); pf8 invocation 1 is SPENT (not
relaunchable — any rerun is the repair rerun under (a)/(b)).

## 6. Exact next authorized action

Await successor admission, then hand over (answer successor's handoff
questions from the record above; no new execution, no new artifacts
from this seat). Successor entry points: this file → STATUS journal tail
→ `HANDOFF-t30-contract-r8.md` (ms2 inbox; last filed pointer) → r8
contract §0/§7/§8/§9/§12 → r8 script + pf8 runner (bound bytes above) →
pf8 inv1 evidence (`scratch/pf8/cases/*/stdout|stderr|exit`, `ev/
SUITE.log`) → §5(a) for the repair design. No scope, budget, or product
change accompanies this transfer (authority transfer only, per order).
