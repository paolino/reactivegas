# #30 MANDATE v2 — substrate vote interface + closure evidence (FOR RULING)

Epic owner, 2026-09-06. SUPERSEDES `handoff/T30-MANDATE-DRAFT.md` (v1,
retained unmodified as history). Companion: `T30-REQUIREMENT-MAP.md`
(row-level evidence/dependency authority). Status: PROPOSED — NO
implementation/auditor execution, build, push, merge, or release granted.
Dated corrections below reconcile the stale 2026-08-25 issue body against
accepted Lean + later rulings.

## 0. Dated corrections to prior inputs (binding)

- v1 §'no rejection/expiry' WITHDRAWN as ambiguous, replaced by the map's
  triad: event-validation refusals EXIST; negative verdicts EXIST and stay
  deliverable; EXPIRY does not exist and is unruled. Nothing here erases
  dissent or existing validation errors.
- v1 'client UI' exclusion NARROWED: it banned only Reactivegas browser
  views; kelgroups-client proposing/voting support (R30-12) is INCLUDED
  under adapt-only discipline. Broader views + wasm stay #84/#82.
- v1 'mirror measured S28-R2 actuals' budget justification WITHDRAWN in
  full (false: actuals are owner 26 + 4 + 2 diagnostic, auditor 11 + 22;
  and analogy never fits a different slice). Envelope below is bottom-up
  or it is nothing.
- v1 'V-2 state at dispatch' blanket DELETED as a dependency statement;
  replaced by row R30-9 with its concrete boundary (#68 landing →
  approve-path rebind; freeze against current base meanwhile).

## 1. Objective (one observable)

A nondegenerate application opens an app-scoped assent question, casts
ballots as the franchise, and observes the verdict (positive/negative with
explicit cause) plus its closure record through the integrated boundary —
with refusals before durable effects and replay equality. Test-only proving
instance (as S28); the runnable user demo is a #29 follow-on, not this
ticket.

## 2. Scope (rows R30-1–R30-8 + R30-12 + R30-14 now; R30-9–R30-11 as
explicit dependencies, not scope)

Implement the accepted Vote-subtree interface in kelgroups Haskell against
the LANDED S28 `Integration` surface (`BaseProposal` param,
`proposalMutation → BaseMutation`, `digest`, `pendingBase`, `GroupView`,
`commitBaseChange` hook): vocabulary, validation, placement, sweep/closure
with retention, threshold-parameterized verdicts, franchise reads, negative
delivery observable at the boundary, route separation, plus kelgroups-client
proposing/voting additions. Fences (R30-X): no expiry, no theta default, no
votable admission, no second store/fold, no Reactivegas UI/wasm, no
Reactivegas economics, no unilateral Lean edits.

## 3. Team (PROPOSED, standing pattern)

Muse ticket owner + distinct Muse commit owner (override pattern) + fresh
Codex `gpt-6-astra/high` auditor (never Muse/GLM/Claude); visible tmux
workers; `draft=NONE`. Local signed commits; factual issue-body updates +
draft PR only after full local CI GREEN; exact-SHA merge authority stays at
the desk.

## 4. Bottom-up envelope (PROPOSED CAPS — fit proved pre-execution, else gap)

Named rows R30-1–R30-8 + R30-12 with per-row witnesses + can-fail mutants
(positive/refusal/atomicity/replay where applicable, N/A with reason);
RED-equivalence failing-first vs absent API (Lean spec exists, Haskell
absent — zero-extent control, ~1–2 builds); implementation of new
Vote/State/Event/Validate/Fold mirrors + integration + client additions;
GREEN envelope (build + full test + mutants + CI); SLIM identical-envelope;
fresh FULL audit (mandate incl. R1–R4-style vote rows + reliances + R30-9
rebind check post-#68 if landed). Mutant count DISCOVERY-BOUNDED 6–10
(Lean equation sites: placeBallot 2, sweepStep/sweep, effectedState 3,
validate 3, verdictOf 2, minus shared-step dedup — verified at freeze).
Fitted plan: RED 2 + GREEN 9–11 + SLIM 3 = 14–16 → PROPOSED owner 16/24;
auditor 12/24 with exact command table pre-dispatch. Probes counted per
row; charge-0 recon enumerated. THESE ARE CAPS PENDING FIT-PROOF AND
AUTHORIZATION, not grants. If fit fails: exact gap, never trimmed scope.

## 5. Acceptance (binding when commissioned)

Executable controls on the candidate SHA; threshold-parameterized verdicts
(exhibits never defaults); explicit closure records retained; refusal
before durable effects (state+log unchanged); replay equality; validate/
fold agreement; negative delivery observable; no expiry anywhere; no
dormant-constructor refusals; client additions covered by client CI;
Trivial intact (degenerate presence only). Bounded claims only.
