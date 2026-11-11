# AUDIT-DISPOSITION — handback only, no verdict (binding NOTE-021)

Ticket owner `t28-app-api`. Lane state at disposition: candidate
`84a2dae…` clean, gate v8 frozen (`f5796d1e…`/`7a7a99e3…`), owner campaign
34/34 SPENT, auditor 9/12 + 7/24 spent, all seats parked/idle, zero live
builds. NO verdict language exists anywhere in this record — by design
(verdicts come only from an auditor's own hand; none was rendered).

## (a) What completed (with hashes, all re-verified this turn)

- Full gate v8 exit 0 on `84a2dae` (`…/evidence/20260905T145247Z-84a2dae-
  gate-full.log`, full sha `ca77e793…`, tail `OVERALL_FAIL=0`): legs 1–4/6/7
  green (23/23 executed, 0 failures, no pendings; `just ci` exit 0; Trivial
  present-uncounted) + all six LEG-PASS with witness-quoted kills (M1
  unification, M2 rejecting-step, M3 atomic-hook, M4 exhaustiveness, M5
  agreement, M6 authority) + hash-verified restore + clean tree.
- Targeted P1–P7 (all journaled with exits/receipts; logs retained):
  P1-compile, P2 exit 1 (compiled StoreProbe RED — evidence
  `5b93f9ed…`, file hash recomputed equal), P3 exit 0, P4 exit 0 (receipts
  transcribed from existing tool results post-refusal — provenance labeled
  in-journal, not re-run), P5 exit 1 / P6 exit 0 / P7 exit 0.
- Instruments retained: `StoreProbe.hs`, `Row4Probe.hs`, M1–M6 diffs,
  `base-candidate.diff`, `command-receipts.jsonl`, `gate-exit.json`
  (`code:0, spent:9`), per-leg logs, registration lists — all under
  auditor `handoffs/evidence/` (handback-confirmed complete).
- Pre-commitment proofs: leg-2 identity/self-hash/ancestry/pins (v8 run) +
  this ticket's freeze/dry-run records. SLIM-final 3 green on the identical
  envelope (pre/post HEAD/tree/status equal) — spent within owner 34.

## (b) What remains unaudited (explicitly open)

- Terminal verdict + acceptance decision: DOES NOT EXIST (no auditor
  rendered one; this record must not be read as one).
- F1 bounded-additional plan (seat-journaled: P3–P6 row-4-effect-mutant +
  controls, within-24-cap, no-new-substantive-build): PLANNED, unexecuted
  beyond P3–P6 as already run (those runs ARE the plan's first instances;
  any further F1 work is future scope below).
- F2 follow-up (row-4-effect coverage: shipped properties that let the
  mutant survive + strengthened properties + kill re-proof): UNPLANNED
  detail, future scope.

## (c) Artifact support per finding (exact)

- F1 (append-conservation/replay-ordering, state-lost-update): SUPPORTED —
  P2 receipt `5b93f9ed…` (exit 1, 395ms, 590 bytes, controls pass;
  journal + file agree) + compiled StoreProbe instrument retained.
- F2 (row-4-effect mutant survived shipped 6-examples; independent
  negative=1 / candidate-positive=0): SUPPORTED — P5 log (exit 1), P6 log
  (exit 0), P7 log (exit 0), all present; Row4Probe instrument retained;
  shipped-suite 6-examples evidence in leg-4 log (0 failures WITH mutant
  per P7 design — survival demonstrated, not assumed).
- Refusal record (triple, observation-only, trigger UNESTABLISHED — no
  diagnosis): (i) seat reports NO direct observation (its words, handback
  journal); (ii) epic reports provider safety refusal with process parked
  (their record); (iii) ticket owner observed refusal-notice text
  (`This content can't be shown / cybersecurity / Trusted Access`) in
  `%557` transcript capture pre-pause. No inference recorded about triggers.

## (d) Permissible next actions (bounded, spend-stated, ALL UNCOMMITTED)

- D0 (null): close as evidence-only audit input (findings feed design
  record / future tickets). Spend 0. Always permissible.
- D1 (F1 follow-through): production repair (requires EXPLICIT repair
  authorization with files + charge — NOT granted) + bounded re-proof per
  F1's plan. Does NOT fit residual (repair voids identical-envelope; full
  re-gate + re-audit needed) → returns for ruling with numbers. UNCOMMITTED.
- D2 (F2 follow-up): new/strengthened row-4 properties + kill (owned test
  surface = new-submission mechanics under one-submission rule) → ruling +
  budget required. UNCOMMITTED.
- D3 (fresh seat for verdict): requires commissioning ruling + budget
  (current seat nonterminal-parked; terminal verdict from THIS seat would
  need an explicit wake + its own protocol, unrequested). UNCOMMITTED.
- Residual caps (no further spend authorized): owner 34/34 SPENT; auditor
  3/12 + 17/24 remain but are NOT approved for duplicate validation toward
  a nicer outcome (standing rule). Any execution beyond parking requires a
  new ruling with exact invocation set + bearing cap FIRST.

## Stops + freezes in force

No candidate edit/reset/merge/bypass; no resubmission/rewording to evade;
no model switch; no safeguard disable; no forced PASS. All roots/panes/
worktrees/evidence preserved in place (no moves, no retirements, no
teardown — lane frozen as it stands pending ruling).

## Corrections (2026-09-05, NOTE-022 — dated; originals retained above)

(a) F1 bullet: "F1 bounded-additional plan (P3–P6 …)" is AMENDED — P3–P6
as executed belong to ROW-4/F2 groundwork, NOT to a remaining F1 execution
plan. Remaining unassessed for F1 beyond verdict/acceptance: NONE — stated
exactly (no new probe invented from the P3–P6 labels).
(b) F2 support: survival evidence is P7 SPECIFICALLY (mutant-shipped-
property exit 0, with P5 exit-1 proving the mutant CAN fail and P6 exit-0
proving baseline works; Row4Probe retained). The generic leg-4 mention is
DEMOTED to baseline context (leg-4 ran the unmutated tree — NOT the
mutation-survival receipt).
(c) Verdict-language precision: every "no verdict" phrase above means
precisely "no TERMINAL auditor verdict was rendered". The gate results
(v8 exit-0 legs) + finding-support judgments (F1/F2 as journaled with
receipts) STAND as evidence assessments under this correction.
