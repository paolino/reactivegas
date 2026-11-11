# S4 Phase A report — CORRECTED v3 (NOTE-003 / desk NOTE-035)

Supersedes `PHASE-A-REPORT-v2-superseded.md` (`399cdb…`; v1 `c52d96…` and
NOTE-001 `f289f6…` likewise preserved). Probes/outputs archived
(`artifacts/MANIFEST.sha256`); new probes under `probes/`; fixture under
`fixture/`; `TIMESTAMP-CORRECTION.md` owns the time repair. Failed probes
preserved and cited.

Base `4a6cd87` (accepted `master`, START-verified). Worktree
`/code/reactivegas-66-s4`, branch `chore/66-s4-phase-a`, HEAD unchanged. No
implementation, no coverage claim, no acceptance, **no Phase B START**, no
model/guard/statement/`#71` edits, no push/PR/merge, no invented rulings.
Nothing from `chore/66-s2-axiom-gate` is authority.

v2 repairs are credited and stand (result-sort boundary proof, imported-path
fixture, timestamp correction, archival). This v3 fixes the two NOTE-003
limits. Authority applied throughout: system-design SKILL.md (“a `Prop`
with no decidable mirror is unexecutable — ship the Bool mirror with its
correctness theorem, or the simulator twin is unproved”) + #66 (“any `Prop`
without a decidable mirror”). Assurance obligation, not product behaviour:
no monitor/guard/new-live-invocation is required to prove correspondence.

## 1. Equality correctness — `decide` + `DecidableEq`, never bare `BEq`

v2 scoped Enacts (and by implication other mirrors) to “`DecidableEq`/`BEq`
α”. Corrected: an arbitrary `BEq` can return true for unequal values or
false for equal ones, so **availability ≠ correctness**. Specification rule
for every mirror below: use **`decide` with `DecidableEq` instances**
(correctness via core `decide` API as already used in-tree, e.g.
`decide_eq_true_eq` in `no_expiry`, `Vote/Invariants.lean:893`), and where a
`==` shape is ever preferred, its `LawfulBEq` law (`a == b ↔ a = b`) must be
established first — **no such law is claimed here** (Q13’s `BEq GroupState
Unit` is demoted to availability-only non-evidence and is not relied upon).

Justified instance chain (QUERY-14 `probes/probe14-deceq.lean`, exit 0):
`DecidableEq` resolves for `Member`, `PendingProposal`, `GroupState Unit`,
`Enactment Unit`, `StepResult Unit`, `Vote.Question`, `Vote.VoteState`, and
— under an explicit `[DecidableEq α]` binder — `GroupState α` and
`Enactment α`. Derivings in source: `KelGroups/State.lean:9,23,30`,
`KelGroups/Fold.lean:27,32`, `KelGroups/Vote/State.lean:36,46,54`,
`KelGroups/Types.lean` (`Member`/`Role`), `Reactivegas/State.lean:19,31`.
Function-field audit (free reads): none of the 19 statement types below
quantifies over function-valued fields (`RoleDef`’s `α → Bool` lives in
`GroupConfig`, which no Prop here mentions; `BackdonateAuth` appears only in
`Reach`, excepted). Hence every general-α scope below honestly carries
`[DecidableEq α]` (concrete payloads need nothing new), and **no original
theorem is weakened to hide it** — assumptions stay in the new statements.

## 2. Axis 1 — final (table ≡ text; 2 + 1 + 2 + 17 + 1 = 23)

DEFINITIONAL V4, P11 · EXECUTABLE R0 · MISSING-exists P01, P07 (theorem each,
no new runtime) · MISSING-no-mirror 17 (V1–V3, K1–K4, P02–P04, P05/P06,
P08–P10, P12, K5) · NOT-EXECUTABLE P13 (bounded; absence ≠ proof) · PROVED 0
(Q7 statement inspection) · NOT-ESTABLISHED 0. K5 moved here from
NOT-EXECUTABLE per the §4-style bounded analysis credited in NOTE-003 §1
(single Option-existential over total `tryEnactDetailed`, Q13 eval).
V4’s definitional identity and the bounded `Reach` exception stand; R0/P11
(and V4, P01/P07 existing expressions) need no redundant implementation.
The 23 is today’s observed inventory, never a future quota.

## 3. Exact correspondence statements (the 19 owed; scoping, nothing landed)

`B` names are Phase B scope (new fenced modules §6); P01/P07 use existing
expressions (no new mirror). `[DecidableEq α]` assumed wherever `α` occurs.

- P01: `∀ view, comune_not_a_member view ↔ ((!isMember comuneId view) = true)`; plus rfl-projection `productionWellFormed gs = !isMember comuneId (groupView gs)` (definitional, `Step.lean:357`).
- P07: `∀ col, permissionToClose col ↔ ((col.permitted && col.pending.isEmpty) = true)`.
- V1: `∀ q, QuestionClean q ↔ questionCleanB q = true`.
- V2/V3: `∀ view gs, SweepReady view gs ↔ sweepReadyB view gs = true`; `∀ θ view gs, VoteWellFormed θ view gs ↔ voteWellFormedB θ view gs = true`.
- K1–K4: `∀ p, PendingWellFormed p ↔ pendingWellFormedB p = true`; `∀ gs, MembersCoherent gs ↔ membersCoherentB gs = true`; `∀ gs, PendingCoherent gs ↔ pendingCoherentB gs = true`; `∀ gs, WellFormed gs ↔ wellFormedB gs = true`.
- P02: `∀ s, conservation s ↔ conservationB s = true`.
- P03/P04: `∀ view s, solvent view s ↔ solventB view s = true`; `∀ view s, insolvent view s ↔ insolventB view s = true` (finite reductions §4).
- P05/P06: `∀ col, uniquePledges col ↔ uniquePledgesB col = true`; `∀ s, allUniquePledges s ↔ allUniquePledgesB s = true`.
- P08: `∀ col u v, escrowHeld col u v ↔ escrowHeldB col u v = true` (via `decide ((splitUser u col.pending) = some …)`-shape with `DecidableEq`, or `isSome`-of-decided-equality — decided in Phase B within the same statement; no `==` without law).
- P09: `∀ u s', governanceEnacts u s' ↔ governanceEnactsB u s' = true`.
- P10: `∀ s s' a u v, doubleEntry s s' a u v ↔ doubleEntryB s s' a u v = true`.
- P12: `∀ view s, canCloseGroup view s ↔ canCloseGroupB view s = true`.
- K5: `∀ gs pid result, Enacts gs pid result ↔ enactsB gs pid result = true` with `[DecidableEq α]` (decide-equalities over `Option (Enactment α)` and `GroupState α`, §1 chain).

## 4. Finite-domain reductions with lookup support (per ∀/∃ row)

- P03: `∀ u, isMember u view → bal s.conti u ≥ 0` ≡ all over
  `view.members` (keys `Prod.fst`); balances via `bal`/`assocLookup`
  (`Reactivegas/State.lean`, `KelGroups/Types.lean`); second conjunct already
  over `s.collections` lists. P04 dually via `any`.
- P12: `∀ u …` over members, `∀ r …` over `s.casse` keys
  (`s.casse.map Prod.fst`); middle conjunct `collections = []` direct.
- K2: `∀ key member ∈ gs.members` (already a finite list quantification).
- P09: `∀ c ∈ s'.collections` (finite); P08: single `splitUser` equality;
  P05/P06: `accepted ++ pending` lists; V1–V3/K1/K3/K4: tally/pending/open/
  closed lists with `Nodup`/`assocLookup` lemmas already in-tree.
- Lookup support cited, not re-implemented: `assocLookup`, `lookupMember`,
  `bal`, `memberKeys`/`GroupView.admins`, `decide` + §1 chain.

## 5. Costed Phase B proposal — BOUNDED ENGINEERING ESTIMATE (labelled)

Status of inputs: MEASURED — 3 exact statements elaborate in 1 command
(QUERY-15 `probes/probe15-statements.lean`, exit 0 with sorry-warnings only;
stubs opaque, scratch, discarded; proof cost explicitly NOT measured).
Everything below about proofs is therefore an **estimate with risk**, not an
observation. Risk: induction-heavy carriers (V2/V3/K4) and equality-dense
rows (K5/P08/P10) may each exceed the per-theorem allowance; any overrun is
reported with a cost gap **before** exceeding (no silent consumption, no
fractional shares — all counts are whole commands).

- Mirrors (17 new `B` defs, §6 fence): estimate 4 per query (shape + eval +
  fix; basis halved from the measured 6-eval/query Q8 pattern for new-code
  risk) → `ceil(17/4)` = **5 queries**.
- Correctness proofs (19 theorems, §3): estimate 2 per query (attempt + fix;
  statement cost measured §Q15, proof cost estimated) → `ceil(19/2)` =
  **10 queries**.
- Mandatory verification + controls (actual commands): cold project build
  (1 build) + result-sort walk (1 query) + statement capture batch (1 query)
  + single-definition negative controls, 1 invocation per mirror module
  (mutated def stays well-typed, dependent theorems expected to FAIL
  elaboration with each name in the error output; 2 modules → **2 queries**,
  expected exit 1 each) + omission control (minus-one free diff, in-walk
  empty control) = **1 build + 4 queries**.
- Base Phase B ceilings: **3 builds** (1 cold + 1 warm per mirror module),
  **19 queries** (5 + 10 + 4). No Phase B START here; builds need
  authorization (0 remain).
- Rebind envelopes (per landing of #68 and/or #69, only if it lands during
  Phase B): cold build (1 build) + result-sort walk (1 query) + affected-row
  statement re-capture (1 query) + affected-row mirror re-eval batch
  (1 query) = **1 build + 3 queries per landing**; worst case both landings:
  2 builds + 6 queries additional, authorized only on landing.

## 6. Fence, deps, bounds, ledger

Fence: new modules only — `Reactivegas.Mirrors` (P02–P12 mirrors+theorems),
`KelGroups.Mirrors` (K1–K5 + V1–V4 theorems/mirrors as applicable; vote
carriers may split into `KelGroups.Vote.Mirrors` only via a cost-gap request for +1 build if shaping shows review size demands it). Existing files imported, never edited;
controls/scratch never in the repo; existing expressions (R0/P11/V4
`decide`/coerced Bools, P01 projection, P07 subexpression) counted, never
duplicated; no coordinator behaviour. Deps: #68 rebinds K1/K3/K4/K5 (+Enacts
threshold reading), #69 rebinds P05/P06/P07/P11/P03/P04; any accepted change
to `Step`, `Vote.Fold/Validate`, `Predicates`, `State`, `Invariants` rebinds
affected rows per the §5 envelopes.

Ledger (grant 3 builds / 20 queries TOTAL; failed+warm count; reads, greps,
diffs, version checks, helper appends free): BUILD-1 exit 0; fixture
BUILD-2/BUILD-3 exit 1 + 1 (lakefile roots; at cap — **0 builds remain**);
queries Q1 exit 1, Q2/Q5/Q6/Q7/Q8/Q9B/Q11/Q12/Q13/Q14/Q15 exit 0, Q3/Q4/Q9
exit 1 → **13 → 15/20 after Q14/Q15; 5 queries remain** for desk follow-ups.
NOTE-003 receipt agreed (3/3, 13/20); no disagreement to report. Fresh FULL
acceptance (reconciled audit budget, exact candidate) remains separately
required; this work does not substitute for it.
