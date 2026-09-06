# Spec — #71 rewrite the design record to the merged model, with a citation checker that fails closed

Base: origin/master e6c59242ccf9b388053626c24446faaa2d7417fd.
Issue: paolino/reactivegas#71 (re-cut of #47/#59, which are closed because bases/subjects no longer exist).
Parent: milestone desk %510. Lane: sole writer of `docs/en/design/` for this rewrite.

## Requirements

- R71-01 — Current State mapping: `Reactivegas.State` is exactly
  conti/casse/collections/votes (`State.lean:23`); membership and roles live only
  in `KelGroups.GroupState.members`, read through immutable `GroupView`; no
  payload-local member/admin copy; three lists plus a vote state, not four
  association lists. Stale `users`/`responsabili` rows removed.
- R71-02 — Current Event mapping: `Reactivegas.Event` 14 constructors
  (openPurchase, grantPermission, denyPermission, deposit, withdraw,
  transferCassa, donate, backdonate, pledge, acceptPledge, refusePledge,
  correctPledge, closePurchase, failPurchase); `AppEvent` 17 (same 14 authorless
  + openQuestion/cast/renounce); retired `addUser/electResponsabile/
  removeResponsabile/Reactivegas.Event.removeMember` named with namespace
  (KelGroups Proposal.removeMember and BaseMutation.removeMember remain live);
  counts fixed everywhere including inside conservation/solvency claims.
- R71-03 — Current Route mapping: `Composition.route` 11 `direct` + 0
  `baseEnacted` + 3 `appDecided` (grantPermission, denyPermission, backdonate);
  `Route.baseEnacted` constructor survives for the accepted historical theorem
  but no event routes to it; sealed `baseHook` with memberAdmitted (no economic
  consequence) / memberRemoved (stall-refused else windUpAdmin+absorbConto) /
  rolesChanged (stall-refused on admin loss else windUpAdmin), exhaustive over
  three BaseChange constructors; step signatures with GroupView/signer/Auth
  (`step`, `stepEvent`).
- R71-04 — Current Authority mapping: per-event authorized signer + guard at
  current source; sovereign-members vs isResponsabile tension stated honestly;
  pledge currently demands isResponsabile (member cannot pledge for self);
  correctPledge currently referente-only over accepted; V-2 and pledge-agency
  rulings recorded as ruled-not-yet-implemented (see R71-10).
- R71-05 — Law-versus-finite-witness distinction in every claim people read:
  universally quantified laws vs finite oracles whose statement is `check…=true`;
  29 short names are a syntactic category, NOT a total census (`majority_table`
  and others use different syntax); no unproved model-completeness claims;
  root `State`/`Event` are not automatically `Reactivegas.State`/`Reactivegas.Event`.
- R71-06 — Explicit unimplemented runtime composition: Composition.lean is a leaf
  (imports Types/Fold/Invariants/Vote.Fold, not Step; only importer is library
  aggregator); production root cannot reach route/voteDerived/appVerdictAllows
  (delete-file control builds clean); `appDecided_verdict_exhaustive`
  reachability/target/polarity links unbound (standalone theorem over unrelated
  parameters); status `enforced: PROVED-IN-MODEL` until substrate mirrors it.
- R71-07 — Explicit vote-lifecycle limits: bare `step` returns none for
  openQuestion/cast/renounce (run inside appFold via voteApply); integrated
  renounce by responsabile succeeds and changes nothing (Fold identity, three
  rfl inversions); notDesignee/notProposer declared but zero usages (Slice B
  placeholders, not corpus gaps); VOTE_TRACES_V1 drives VoteState standalone
  (15 signed events, reusable not substitute); #74 economic/integrated corpora
  carry zero signed vote events (integrated V-3 franchise closure credited);
  threshold θ open (legacyThreshold vs zeroThreshold exhibits, not defaults;
  #68 must not be read across as vote default).
- R71-08 — Voci non-goal with fact/ruling/reason/cost-curve/open-question:
  21 Voci/ modules + ImpegnoVincolato/CorrezioneImpegnoVincolato; Lean models
  none (pledge carries bare (user,c,v)); out of scope for milestone 2 outcome
  test; recorded not omitted (unmodelled subsystem = unenforced contract);
  cheapest now, corpus re-freeze + fold rewrite after D3; operator question
  whether GAS reaches outcome test without catalogue.
- R71-09 — Precise dated operator authority in-repo: Q-001 chronology appended,
  later rulings supersede earlier assertions; V-1..V-7 (2026-08-27), Q-6/cassa
  comune/stall/total-stall/committed-vs-available (2026-08-26), V-2 + pledge
  two-regimes (2026-09-05), NOTE-016/A-Q001/NOTE-031 composition chain;
  issue title alone never supplies a missing ruling.
- R71-10 — Current-vs-ruled behavior written explicitly for every pending merge:
  #66 S1 (six UNPROVED bytes → resolved pins; corpus re-freeze before D3),
  #68 (proposer opens at zero; arithmetic unchanged; n=2 needs other assent),
  #69 (pledge signer==u while pending with v'=0 withdrawal; referente after
  acceptance; solvency guard + closePurchase unchanged; UI legibility);
  exact source pins refreshed after each merge; honest current record may land
  before semantics, but implemented-new-behavior claims wait for their merge.
- R71-11 — Immediate citations verified against resolvable pinned Lean
  declarations: every declaration-like claim carries its marker block; gate
  discovers actual cited extent (never a hand list); malformed/unknown/missing
  citations RED through an executable negative control; fails closed when Lean
  prerequisite fails on a cold tree; freshness quantifies over discovered set.
  Claim-syntax definition (successor binding): a declaration-like claim is any
  claim block (a list item, a table row, or a paragraph) containing a backticked
  span whose inner text equals a Lean declaration basename (or qualified name)
  from the discovered extent at PIN, plus the explicit required semantic claim
  blocks named in this spec (AUTH table rows, pending-table rows, law/witness
  caveat, composition limits, vote-lifecycle limits, Voci fact, closure
  classification). Every such claim needs a co-located `lean:` marker in the
  same block resolving to that declaration; an added claim using a real Lean
  name with no marker REDs as added-uncited. The parser enforces
  claim↔marker association only — never natural-language truth, which stays
  human/auditor row-level duty. No required claim may be deleted or rephrased
  to evade detection.
- R71-12 — canCloseGroup classification: `Predicates.lean:85` orphan (zero
  usages), three conjuncts stated; record classifies against product intent as
  missing guarantee or justified non-goal; no deletion/implementation by this
  lane; no invented theorem.

## Rejection behavior

Stale counts/names/signatures refused by the gate; unresolvable citations
refused; claims of implemented pending behavior refused until their merge;
second membership store / second constructor list / second manifest refused
as scope violations.

## Non-goals

No Lean model/proof/semantic changes; no simulator/Haskell changes; no
Goals.lean/decisions.md filename obligation (substance only); no threshold θ
default; no composition implementation (T-B belongs elsewhere); no corpus
emulation beyond citing #74 wrapper discipline.
