# OWNER-SUBSTITUTION-HANDOFF — design record (#71), muse %516 → Opus successor

Ordered by operator (ROLE-SUBSTITUTION-OPUS-20260906, POINTER-1788673822-3348773):
ticket-ownership transfer, not a submission, acceptance, or scope/budget change.
Successor pin: `claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high`,
fresh conversation and runtime. Desk compiles the successor; this lane launches nothing.
Written 2026-09-06 at a safe boundary (lane parked, nothing in flight).

## 1. Exact scope (frozen; successor inherits, does not re-decide)

- paolino/reactivegas#71, design-record rewrite, successor campaign S71-B,
  Round-B repair terminal VERIFIED and PARKED. Mandate: specs/71-design-record
  at planning commit 90dae99 (R71-01..12 + claim-syntax definition + T71-06/07/08).
- NO accepted candidate exists. Unaccepted history: 36666dc REJECTED (b5d3199f),
  67877b1 REJECTED (a6a0d9f5 F-01/02/03), 77f8be6 UNACCEPTED Round-B terminal
  (full v8 GREEN receipt 2af22b6e, frozen diff 460411b2 + manifest, pushed).
- Standing prohibitions: no Lean/model/theorem edits to make prose true; no
  merge/comments/publication without desk grant; upward reporting local-only
  (never type into %510 or any human composer); #68 rebind only after landing
  (handoff + precision rule held); #75/#76 rows deferred with rationale.

## 2. Runtime roots, panes, processes (verified live at handoff)

- Ticket runtime: /tmp/reactivegas/ms2/t71-design-record (brief.md, STATUS.md,
  RESUME.md, evidence/ incl. gates v1–v8 + batteries + attribution + witness
  seed/output, handoffs/ incl. SUCCESSOR-PROPOSAL.md, commit-owner-s2/,
  .archived/commit-owner-s1/, .archived/audit-s1/, .archived/audit-s2-invalid/,
  .archived/audit-s2r/, .archived/audit-b1/).
- Window: reactivegas:11 `design-wait-model` (@156; renamed by root
  reorganization from reactivegas:7 — same window, no pane moved by this lane).
- Live: %516 (this owner, pid 614577, muse-spark-1.3-contributor/xhigh, idle
  post-handoff); %542 (commit owner, pid 1296754, same identity, PARKED
  write-idle, no live children — verified by ps, nothing to SIGSTOP).
  Retired/closed earlier: %518 (S71-A owner), %521 (audit-s1), %533 (invalid),
  %537 (audit-s2r), %546 (audit-b1). Unknown: none — every historical pane in
  this window is accounted above or in retained-history (desk-owned).
- Worktree: /code/reactivegas-issue-71, branch docs/71-design-record, HEAD ==
  origin == 77f8be6, clean. PR #77 draft (hold draft).

## 3. Base movement during park (successor's first reconciliation)

- At Round-B terminal: origin/master d670323 (PR87). OVERNIGHT it advanced to
  3590c00 `fix(lean): total axiom gate over discovered extent with
  resolved-olean ownership (#66 S2R) (#88)` — exactly the reserved S2R landing.
  This lane performed NO rebase after fa50613; worktree stays on d670323 line.
- Known consequences (from the landing sequence, not yet measured here): S2R
  changes Invariants source/line identities and the justfile path; required
  anchor lines + gate PIN + discovery count + full CI must be re-established
  against 3590c00; #68/#69 still unmerged (no anticipation). Do NOT treat the
  old pins as current.

## 4. Gate/brief versions (exact)

- Frozen gate: worktree ./gate.sh == evidence/gate-v7.sh v7 plus NOTE-012/015
  deltas — NO. Precisely: current frozen instrument is v8
  sha256 7aa3f2b5c3f4b23447a9e32e5ddecf2510a10ec978af843d0b885a4512fa7939
  (backup evidence/gate-v8.sh identical); v1–v7 backups + batteries preserved
  in evidence/. (If any hash here mismatches the files, the FILES win —
  re-hash before use; this line is an index, not authority.)
- Briefs: this root brief.md (lane), commit-owner-s2/brief.md + inbox
  NOTE-001..005 and answers A-Q001/A-Q002 (child contract history).
- Full contracts (referenced, not rewritten): LEDGER.md, orchestrator-contract,
  ticket-orchestrator, resolve-ticket, worker-protocol, gate-script skills.

## 5. Cumulative counters and ceilings (reconcile live before spending)

- Owner S71-B (%542): 2/4 full builds + targeted per its journal (≈7+; recount
  from commit-owner-s2/STATUS.md lines before spending). S71-A owner: 4/6 (closed).
- Auditors ticket-wide: 2/3 builds + 20/40 targeted spent; reserve 2 builds +
  20 targeted untouched for the final FULL audit. T.O. static battery work is
  ledgered in STATUS/evidence (0 builds) and consumes no seat cap.
- Ceiling raises: 0/2. No submission reset: S71-B used 1 of max 2 submissions;
  exactly one repair→re-audit cycle remains within this ticket.
- Recorded mistakes (do not repeat): leg-12 authorized after NOTE-018 withheld
  it (spent within cap, owned in journal); grok-seat contract breach on
  audit-s2 (terminalized, archived); stale gate hash in one admission line
  (corrected, old line preserved).

## 6. Unconsumed inbox: NONE.

- Ticket inbox NOTE-001..008 + artifacts NOTE-009..019 + UPWARD-REPORTING +
  SEQ + RELEASE + this substitution order: all read, journaled, and either
  acted or parked with a named wake. No open question in any root.
- Child inbox NOTE-001..005 (incl. pause): all read/acknowledged.

## 7. Pending decisions (none are the successor's to take alone)

- (a) Wake announcement (desk): accepted final model/quality base for final
  validation + fresh FULL audit. Nothing else releases the park.
- (b) Merge/publication of any candidate (desk grant only).

## 8. Exact next authorized action (in order, stop conditions included)

1. On wake: verify base (origin/master vs d670323), rebase lane branch iff the
   announced base differs (abort + escalate on conflict), re-read required
   anchor lines + discovery count at the new PIN, re-verify gate end-to-end,
   version/freeze the gate delta if the base moved it.
2. Then: final full validation + fresh FULL independent audit (reserved 2+20;
   row-level AUTH/pending/claim truth; local-only; codex-or-grok restricted
   set, grok cap exhausted ticket-wide) on the rebased candidate.
3. Then: accept/handback packet for merge authorization, or one bounded repair
   + re-audit within the remaining submission cap, or re-cut conversation.
4. Stop and escalate on: scope contradiction, missing honest citation, cap
   pressure (exact gap, never silent overrun/narrowing), base ambiguity.

## 9. Standing warnings for the successor

- Upward delivery is LOCAL FILES + own STATUS only. Never paste, send keys, or
  address %510 or any human composer. The operator already corrected this
  tree once (UPWARD-REPORTING-LOCAL-ONLY.md).
- Wait-status patterns must match the two-space tag column; preflight every
  wait with a non-zero grep count; keep blocking calls ≤60s.
- Bare `grep -o` counts in `$()` under pipefail need `|| true`; `out=$(failing)`
  bare under set -e exits silently — capture with `|| rc=$?`.
- Do not wake the parked commit owner for acknowledgements it does not owe;
  do not restart terminal historical owners; do not anticipate #68/#69.
