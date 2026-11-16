# Brief — commit-auditor-s28r1b (replacement FULL independent audit, S28-R1)

Worker: commit-auditor-s28r1b. Seat: fresh pane IN reactivegas:11 (ticket
window — admission precondition, answers Q-001 of the terminal run: the
prior seat correctly CONTRACT-BLOCKED on window separation; verify
co-location yourself at START via `tmux display-message -p '#S:#I'` (expect
reactivegas:11) + record it, else BLOCK). Family: Codex `gpt-6-astra`,
effort high (pinned; no substitute). draft=NONE. Authority: ticket owner
t28-app-api per NOTE-024/026/028/029. Delivery: local files only
(`commit-auditor-s28r1b/handoffs/` + own STATUS). No push/PR/merge/comment/
remote write/provider workaround. NEVER reuse terminal %566/root/context.

## Subject (complete range, nothing inherited as verdict)

Accepted base `368b596fef0b6d393c2ac7afc631d236c55d86d1` → final candidate
`3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4` (40-char; parent `84a2dae…`,
Store.hs F1 repair + spec F2/F1-regression tests). Checkout (read-only for
review; leg-5 mutates + hash-verified reverts there):
`/code/kelgroups-audit-3af3d06` (detached HEAD, verify + record on START;
0/0 spent there — pristine). Gate: `G28-1 v9 (S28R1-plan)`, normalized
`3c433eff…`, full `dcbc8c2b…`. Owner evidence ONLY (never acceptance):
GREEN `1331e4b2…` (OVERALL_FAIL=0, seven kills) + SLIM seat logs (131/0,
build recipe-matched, ci clean) + SUBMISSION.md + ticket BINDING record.
Prior runs (context, never verdicts): S28-1 terminal `b7b793a3…` + ledger
`b2860a4f…` + retained instruments (StoreProbe.hs, Row4Probe.hs,
row4-shadow/, P-logs with plan-v2 corrected identities) + s28r1 terminal
`22c79c04…` (CONTRACT-BLOCKED admission, 0/0 spent — read its Q-001).

## Scope (every row open to challenge; gate never closes anything)

R1/R3/R5 OPEN, R2/R4/R6 BLOCKED (F1/F2 inside), all five reliances with
ledgered limits — reassess ALL against repaired bytes. Inherited kills
never verdicts. FREEDOM (binding): challenge ALL rows beyond the gate;
the concrete probe contracts below are MANDATORY coverage (conform, then
extend at will within budget). OPEN rows NEVER residuals. Report coverage
AND limits honestly (UNCOVERED with shown effort is legitimate).

## Fit-first concrete probe contracts (mandatory; author programs, cover cases)

Compile narrowly (`ghc --make`, S28-1 P1/P3/P4 precedent — exact commands
in retained `command-receipts.jsonl`; each compile 1 probe, each run 1):
- R1-C1 view-values (2): scripted 3-step trace (founding-add → member-add →
  role-change) asserting EXACT pre/post GroupViews per step (auditor
  hand-computes expectations — independence); + nonmember attempt leaves
  views exactly unchanged.
- R3-C1 hook-outputs (2): recording-hook trace asserting EXACT success-side
  (pre/post views + payload) and refusal-side outputs + prestate restoration.
- R5-C1 lifecycle-agreement (2): propose→approve→enact trace asserting
  stepwise validate-then-fold == foldIntegrated throughout + replay equality.
- MAJ-C franchise/enactment (2): mid-vote membership change; quorum of
  CURRENT franchise; successful enactment commits effects.
- P1' StoreProbe-class vs repaired API (1) + P2' extended pairs (4
  inherited + 4 new: (5,11),(42,43),(1000,7),(0,999)) + controls + ≤1
  stress (≤3 total) + P3' Row4Probe-class (1) + P4' shadow compile (1) +
  P5'/P6'/P7' runs (3).
- Reliances without new probes (concrete commands + named limits):
  HIST-FOLD (leg-4 historical suites + base→candidate historical-diff review;
  beyond-suites UNJUDGED); CESR (leg-4 key tests; decoder-domain UNJUDGED);
  APPFOLD-SHAPE (leg-3/4 compile; semantics UNJUDGED); MAJORITY (MAJ-C +
  leg-4 pending-entry test).

## Budget (binding; ONE terminal report)

12 builds / 24 targeted, separately identified. Envelope 10B: gate v9 legs
3,4 + M1-M7 + 6 via `./gate.sh` (refuse-closed; hash-verified restore;
quote every kill). Discretionary ≤2B, named ONLY: infra-flake
disambiguation re-runs. Probes ≤16 (exact: P1'(1)+P2'(≤2)+P3'(1)+P4'(1)+P5'/P6'/P7'(3)+R1-C1(2)+R3-C1(2)+R5-C1(2)+MAJ-C(2)). Recon/reads charge 0. Gap (any row
without an executable command): STOP, return CONCRETE GAP as BLOCKER
pre-spend (never green-gate substitution). Journal EVERY attempt/spend
(child-authored). Questions → `commit-auditor-s28r1b/questions/`.

## Returns + stops + tags

`AUDIT-RESULT verdict=AUDIT-PASS|AUDIT-FINDINGS …` + `COMPLETE …`
+ `handoffs/AUDIT-REPORT.md` + `REQUIREMENT-LEDGER.md` + `EVIDENCE-INVENTORY`
(+ .json) + receipts, hash-bound, one terminal report. Tag discipline
(binding, wakes supervision): terminal COMPLETE + AUDIT-RESULT with marker
AUDIT-S28R1; stops BLOCKED + reason. Stops: pause/stop, overruns (STOP
before 12/24), restriction-blocks-work (record + stop), provider issues
(record WITHOUT diagnosis, no switches/workarounds, work stops).

Acknowledge: verify co-location + checkout HEAD + gate hashes; read mandate
(plan v2) + gate + owner evidence + both terminal records; journal `START
commit-auditor-s28r1b` (worker ID + ledgers 0/12+0/24) via status-event.
