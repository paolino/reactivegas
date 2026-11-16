# Successor campaign proposal — #71 design record (re-cut, not submission 3)

Prepared by ticket owner t71-design-record (%516, muse) per desk NOTE-009
(POINTER-1788607523-1214434). Runtime-only preparation: 0 builds spent, no
worktree writes, no dispatches, no product ruling invented. Desk can dispatch
without a new product ruling: every content ruling the repair needs exists.

## Why a successor (not acceptance, not a third submission)

- Submission-2 candidate 67877b1 NOT accepted: AUDIT-FINDINGS a6a0d9f5,
  blocking F-01 (economic prose), F-02 (AUTH strength), F-03 (missing citation)
  despite full v4 GREEN and twelve representative kills. Campaign 2/2 exhausted;
  next-submission FORBIDDEN stands. Old kills are evidence below, never
  acceptance carried forward.
- No Lean semantic or theorem change is authorized (or needed) to make the
  prose true: every finding is false documentation, and the rulings that settle
  the corrected content already exist (operator V-series, Q-001 chronology,
  NOTE-016/A-Q001, A-V2-AND-PLEDGE-AGENCY, #66-S1 #79).

## Preserved inputs (exact paths; read-only for the successor)

- Submission-1 report b5d3199fafad5cd1683177839df71818e1d6740397cd55035f732866a6e9707a
  + ledger 46e3752b2c… + instruments, at .archived/audit-s1/.
- Invalid-seat record (no verdict): .archived/audit-s2-invalid/ (terminal,
  input stats, cold captures, 0 builds).
- Submission-2 report a6a0d9f548abf18108027bed0c4514b33b789cecd2d447969a621cfc11bfd243
  + ledger 24251ff160ff… + ROW-REVIEW.md fc7517b19c… + ARTIFACTS.sha256 7a0d6ae4…,
  at .archived/audit-s2r/ (just archived).
- Immutable gates v1 bc245c0e… / v2 0781cfd5… / v3 cb899594… / v4 b9fef1c7…,
  falsification batteries, v4 attribution log, owner RED/GREEN receipts
  (d3bfb249 / 5768d517 / 84513410 / 47846196 / d0ee3183), RELIANCE.md —
  all under evidence/ and commit-owner/handoffs/.
- Base/pins: e6c5924 (planning) → S1 #79 4a6cd87f (current origin/master,
  verified at proposal time; re-verify at dispatch, re-pin if moved).
- PR #77 stays draft (branch docs/71-design-record at pushed 67877b1, in-sync);
  no merge, no comments, no publication.

## Historical accounting (spend is history, not authority)

- Owner full validations: 4 total — v1 GREEN (1), v2 GREEN (1), v3 GREEN (1),
  v4 GREEN (1); all RED/negative-control runs spent 0. Old per-submission 6-cap
  is closed with its campaign; remaining allowance is NOT submission authority.
- Audit building (cap 3, ticket-wide): audit-s1 1 + invalid seat 0 (no refund)
  + audit-s2r 1 = spent 2/3. Remaining 1 is NOT authority for the successor.
- Proposed fresh allowances (for desk authorization, not assumed):
  owner ≤2 full validations (1 v5 GREEN + 1 contingency; static battery free
  but recorded); auditor ≤2 builds (1 full gate + 1 contingency). Campaign
  contract as before: max 2 submissions, 1 repair bounce, fresh auditor per
  submission, ceiling raises 0/2 reset under the new campaign.

## Exact file fence (successor)

- WRITE: `docs/en/design/state-machine.md` ONLY — the three findings, the
  malformed pending row, and consequential marker/anchor updates.
- VERIFY-AND-CARRY: `docs/en/design/kelgroups-vote-machine.md` (byte-identical
  through 67877b1; rewrite only on measured drift re-derived at PIN).
- PLANNING-ONLY: `specs/71-design-record/` — one amendment commit recommended:
  promote R71-11 with the claim-syntax definition (§4 below) and the row-truth
  duties (brief-level in S71-A) to mandate rows, so the successor auditor judges
  mandate, not brief prose. No spec change to R71-01..12 substance.
- READ-ONLY: `lean/**` (no semantic/theorem edit to make prose true),
  Haskell/simulator sources, gate.sh (successor-ticket-owner-versioned),
  PR metadata beyond factual body refresh.
- FORBIDDEN: deleting a required claim or rephrasing it out of detection to
  avoid citing it (F-03 hardening target; auditor watches claim inventory).

## Content repair (prose conforms to model; model untouched)

- F-01: L4 must describe the actual cash-box DEBIT (cassa 30→0, conti unchanged,
  escrow 30→0 on the auditor's journey; cite Invariants.lean:679 for what it
  proves — a decrease — not as an account-credit law). L5 deposit boundary as
  the −1/0/+1 triplet (refused/accepted/accepted): zero deposits permitted.
  Stored-zero distinction: accepted zero deposit from empty stores `(u,0)`,
  `(a,0)` rows (read vs stored). Re-verify every other economic row against its
  cited statement the same way, not only these examples.
- F-02: AUTH section states the proof's actual scope (Predicates.lean:74 proves
  responsabile-authorship for all fourteen events; ignores state/args — show
  the admin-to-absent-member illustration as the scope limit, not as a
  Lean defect). Grant/deny rows name the collection-lookup prerequisite first
  (Step.lean:53/57 pullCollection must succeed; absent id refused, succeeds
  after open). Re-verify all authorization rows against statements.
- Pending row: repair the malformed #81/V-5 row (4 data cells under 5 headers;
  re-pin text back in its source-ruling column) while in the document.
- All other rows keep their accepted content; current-vs-ruled table and
  re-pin hook retained and refreshed to dispatch-time pins.

## Executable property classes (gate v5; static falsification, 0 builds)

- Witness legs (post-CI, 0 extra builds — reuse leg-12 outputs via `lake env
  lean` exactly as the auditor's DocWitness.lean did): W-1 close-30 journey
  (conti Δ0, referente-cassa −30, escrow −30); W-2 deposit triplet (−1 refused,
  0 accepted, +1 accepted); W-3 zero-store rows present after accepted zero
  deposit; W-4 grant-absent-id refused / succeeds-after-open. Each with pinned
  expected outputs; each shown RED by reverting its prose sentence to the
  F-01/F-02 wording (documentary legs below must also RED — paired proof).
- Documentary legs for the corrected sentences (credit→debit wording with
  `cassa` + decrease/debit tokens; triplet table; stored-zero sentence; AUTH
  scope-limitation sentence naming Predicates.lean:74's limit; lookup
  prerequisite tokens at grant/deny rows; well-formed pending table: every row's
  cell count == header count). Each leg carries its F-01/F-02 revert mutant.
- Claim-syntax definition (new deliverable, itself falsified): admitted
  declaration-like claim = a backticked Lean-thing co-located with a `lean:`
  marker in the same claim block (block = list item or paragraph; definition
  frozen in the gate header with its boundary tests). Domain = discovered Lean
  extent at PIN. Controls against the REAL checker: missing (delete one marker,
  keep claim), malformed, unknown, and ADDED-UNCITED-CLAIM (append a new
  sentence using a real Lean name with no marker — must RED; this is the class
  no marker-quantifier can catch). Larger hardcoded anchor minimums are
  explicitly NOT the mechanism (F-03); the v4 required table stays as the
  mandate minimum underneath.
- Boundary stated in gate and record: the parser enforces claim↔marker
  association, never natural-language truth — semantic claim-to-statement
  review remains human/auditor work (row-level duties below, unchanged).
- Falsification standard as v4 (every control asserts its intended reason;
  SYNTH-style satisfiability subjects; auditor-exact mutant replays where
  applicable), all static.

## Successor audit subject and seats

- Subject: the FULL unaccepted design record (successor candidate) against
  actual accepted model pins at dispatch, including ALL original row duties
  (R71-01..12 + NOTE-001 qualifications + AUTH/pending row-level truth +
  V-7/S5 caveat + #75/#76 planned status). Old kills are evidence inputs only.
- Seats: fresh Muse commit owner (same operator-authorized exception as S71-A)
  + fresh FULL Codex auditor (grok cap exhausted ticket-wide; explicit
  model+effort argv pins; restricted codex-or-grok set; local-only delivery).
  Fresh roots/panes/detached worktrees; no context reuse from S71-A seats.
- Mandate/gate/audit packet compiled by the successor ticket owner at dispatch;
  budgets per §Historical (proposed allowances above).

## What the desk must supply (nothing product-new)

- Campaign authorization + fresh build allowances (owner ≤2, auditor ≤2) +
  seat authorization (muse owner exception carries over unless revoked; codex
  auditor standing restricted set). No product ruling: content rulings exist;
  no Lean change; no merge authorization (PR77 stays draft through the campaign).
