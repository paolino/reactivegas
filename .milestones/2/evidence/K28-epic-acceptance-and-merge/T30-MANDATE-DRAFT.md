# #30 MANDATE DRAFT — substrate vote interface + closure evidence (PREP ONLY)

Status: DRAFT for desk ruling. NO implementation/auditor execution grant.
No budget granted (figures below PROPOSED for ruling). Epic: kelgroups#29.
Depends: S28-R2 LANDED (`933e385d`); V-2 rule state per accepted base at
dispatch (re-verify #68 landing; never anticipate candidate semantics).

## Objective (one observable)

A nondegenerate application can open an app-scoped assent question, cast
ballots as the franchise, and observe the verdict (positive/negative with
cause) plus its closure record through the integrated boundary — with
refusals before durable effects and replay equality. (Reactivegas purchase
gating etc. is #76 consumer binding — NOT this ticket.)

## Accepted sources (frozen at dispatch; stale bodies corrected per
EPIC-MAP 2026-09-05: no single-admin immediate enactment, no generic
proposals, no rejection/expiry, no theta default, no dormant-constructor
refusals)

- `KelGroups.Vote/Types`: Verdict (+/-/open), Threshold PARAM
  (legacyThreshold/zeroThreshold exhibits, NOT defaults), Ballot,
  QuestionKind (collective / permission-designee), ClosureCause
  (tally/franchiseChange + carried proposerDeparted/renounced),
  Question/Ballot placement one-position (placeBallot).
- `Vote/{State,Event,Validate,Fold}`: VoteState (openQuestions + closed
  log), openQuestion/cast/renounce vocabulary, validateVoteEvent boundary,
  applyVoteEventChecked/effectedState/sweepClosures (same-step close with
  record, R-51/R-61), GroupView franchise reads, NO clock/expiry (R-54).
- S28-R2 interface as landed: `Integration` bundle (`BaseProposal` param +
  `proposalMutation → BaseMutation` + `digest`), `pendingBase`, `GroupView`,
  `commitBaseChange` atomicity (post-enactment consequences ride here).
- V-2 (zero-open, proposer-not-assent, n=1 separate approve, arithmetic
  unchanged) as LANDED at dispatch — explicit rebind step, never
  anticipation.

## Out of scope (hard fences)

Reactivegas economics/composition wiring (#76); shipped theta default;
voter-refusal policy from dormant `notDesignee`/`notProposer` (zero
construction sites — forward declarations, not authority); expiry/time
semantics; client UI; publication/merge (separate grants).

## Proposed team + budgets (FOR RULING, not granted)

Muse ticket owner + distinct Muse commit owner (standing override pattern)
+ fresh Codex `gpt-6-astra/high` auditor (never Muse/GLM/Claude); visible
tmux workers; draft=NONE. Proposed: owner 14/24 + auditor 12/24, ONE
submission, zero auto-raises, command-plan-first with named gate-author
ledger, gap-first (mirrors measured S28-R2 actuals). Gate: frozen v11
family re-cut to landed base with per-row witnesses + can-fail mutants
(positive/refusal/atomicity/replay where applicable, N/A stated), full CI,
fresh FULL audit, no inheritance. Acceptance packet + disposition, same
shape as S28-R2.

## Acceptance (binding when commissioned)

Executable controls on the candidate SHA; refusal-before-effect with
state+log unchanged; tentative-change + failing-hook rollback where hooks
compose; validate/fold agreement; replay equality; threshold-parameterized
(not defaulted) verdicts; explicit closure records retained (never silently
dropped). Bounded claims only.
