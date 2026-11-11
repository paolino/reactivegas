# #30 MANDATE v3 — substrate vote interface + closure evidence (FOR RULING)

Epic owner, 2026-09-06. SUPERSEDES v2 (retained unmodified; v1 likewise).
Companion: `T30-REQUIREMENT-MAP-v3.md`. PROPOSED — no execution, build,
push, merge, or release granted. Corrections in this version: (1) honesty
fix below; (2) ruled/unruled lifecycle split; (3) source rebind 3590c001 +
zero-diff; (4) COMPLETE row-to-command mapping (envelopes retained ONLY as
labeled estimates); (5) qualified links.

## 0. Honesty correction (stated plainly)

v2 §0's 'narrowed' phrasing implied v1's client-UI exclusion already meant
only Reactivegas views. FALSE: v1 banned client work broadly; v2 CORRECTED
it to kelgroups-client additions under adapt-only discipline. The correction
is recorded here, not backdated into v1 (unchanged).

## 1. Objective + scope (v2 text stands, with R30-10 split + R30-10U added)

Nondegenerate app opens questions, casts ballots, observes verdicts +
closure records through the integrated boundary, refusals first, replay
equal. Rows R30-1–R30-8 + R30-12 + R30-14 now; R30-9 rebind after
paolino/reactivegas#68; R30-10a content via paolino/reactivegas#81 (which
needs #76 for refund); R30-10U + R30-5-producing explicitly UNSCHEDULED
(preserved boundary, no promise); R30-11 → #76; R30-13 Lean-owned where
ruled, nonexistent where unruled. Fences R30-X hold.

## 2. Row-to-command/control mapping (binding pre-execution contract shape)

- RED-equivalence (1–2 builds): failing-first properties vs absent Haskell
  Vote API (zero-extent control with positive control on LANDED S28
  surface; exact absent names quoted).
- Implementation (inside GREEN envelope): Haskell mirrors of
  Vote/{Types,State,Event,Validate,Fold} + Integration wiring
  (`BaseProposal` payload reading, `proposalMutation`, `digest`,
  hook composition) + client Api additions.
- GREEN envelope commands (each whole-project invocation = 1 build):
  cold `build` 1B + full test suite 1B + per-mutant build/test 1B each +
  full CI 1B. Mutant set (DISCOVERY-BOUNDED 8–10, verified at freeze from
  Lean equation sites minus shared-step dedup):
  [openQuestion-nonresponsabile-refusal; cast-nonresponsabile;
  cast-unknown-question; ballot-switch-moves + idempotent-recast;
  sweep-tally-positive; sweep-dissent-negative; sweep-franchiseChange;
  verdictOf-permission-ignores-tally; closure-drop-silently;
  pendingBase-admits (compile-RED expected, type-level absence claim);
  hook-refusal-discards-enacted (atomicity); client-propose-roundtrip].
  Each: apply + build/test + revert; kill must name its row's witness.
- Interface-existence claims (narrow, honest): the pendingBase-compile-RED
  establishes ONLY 'no admission constructor is encodable' (type-level
  absence), never behavioral refusal — labeled as such.
- SLIM identical-envelope 3B (legs 1,2,2b,3,4,6,7 analog).
- R1/R3/R5-style vote rows: R1 = pre/post GroupView values + nonmember
  unchanged; R3 = hook success/error outputs + restoration + reopen;
  R5 = integrated-validator stepwise agreement on SAME aggregate/event/
  signer + founding replay + reopen (never historical validateEvent, never
  same-wrapper-twice).
- Cold validation: first compile is the cost (stated, not optimized away).
- Final CI + clean-state obligations: full `just ci` (Haskell + Lean +
  client suites), tracked-clean before/after, Trivial degenerate-only.
- Fit arithmetic (BOTTOM-UP, caps pending fit-proof): RED 2 + GREEN
  (2 + 8–10 + 1) + SLIM 3 = 16–18 → PROPOSED owner 18/24; targeted counted
  per row (charge-0 recon enumerated); auditor 12/24 with exact command
  table pre-dispatch (mandate rows + reliances + R30-9 rebind check if #68
  landed). Caps remain PROPOSALS until fit is established + authorized; fit
  failure returns exact gap, never trimmed scope.
- Ticket owner authors the final immutable gate (this epic contract ≠ that
  instrument).

## 3. Team + fences + acceptance (v2 stands, links qualified)

Muse owner pair + fresh Codex `gpt-6-astra/high` auditor; `draft=NONE`;
signed commits; issue-body updates + draft PR post-GREEN only; exact-SHA
merge at desk. Fences R30-X. Acceptance per v2 §5 (threshold-parameterized
verdicts, retained closures, refusal-first, replay equality, no expiry, no
dormant refusals, client CI, Trivial intact), bounded claims only.
