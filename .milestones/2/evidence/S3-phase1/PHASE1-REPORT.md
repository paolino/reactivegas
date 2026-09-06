# S3 Phase 1 report — required-extent discovery, receipt inventory, measured costing

Local only. No push, PR action, comment, gist, publication, deployment or merge.
No `docs/en/design/` writes. No coverage claim. Phases 2..n unauthorized.

## 0. Base, seat, spend — reconciled first

| | |
|---|---|
| Accepted base (re-derived at worktree, not trusted) | HEAD `3590c0015b84fd58004bf6fb44dd18b107304c48` — MATCH |
| single parent | `d67032313acf3699cc50358a057391b88d002192` — MATCH |
| tree | `44a1f0bce4796c63203070e23b96172a7774956e` — MATCH |
| landed via | PR #88 squash, `closingIssuesReferences []`, AUDIT-PASS 32/32 (per S2R-MERGE-RECEIPT) |
| #66 | OPEN — this report does not close it and implies nothing that does |
| worktree | `/code/reactivegas-66-s3-phase1`, detached (`symbolic-ref` fails, `branch --show-current` empty), `git status --porcelain=v1` clean at START and at delivery (`.lake/` ignored per `.gitignore:8`) |
| seat | agent PID/PGID `2401092` (`pi --provider opencode-go --model muse-spark-1.3-contributor --thinking xhigh --approve`, tty `pts/107`, pane `%558`) — see STATUS.md NOTE-002 correction; START-sampled `2404583` is SUPERSEDED tool-shell, kept visible there |
| contract | `../handoffs/S3-MANDATE.md` operative **revision 3** |
| brief | `brief.md` sha256 `77c9d6bf8425afdde42be2cd3fa779901f0664a0de88fcfa3c58c808caabdac2` (prefix `77c9d6bf8425afdd` as dispatched) |

### Known artifact defect (disclosed, not repaired)

The mandate file opens *"S3 contract — revision 2"* and its preservation note
mentions only v1, although `S3-MANDATE-v2-superseded.md` exists. Header lags
content. Read as revision 3 per brief and NOTE-001. Frozen artifact not edited.

### Build spend — 3/3, ceiling exhausted, gap stated now

| # | kind (never averaged) | command (cwd) | wall | exit | evidence |
|---|---|---|---|---|---|
| 1 | **full cold build** | `lake build` (`lean/`, zero oleans, toolchain `leanprover/lean4:v4.25.0`) | **19 s** (`date +%s` before/after; full log NOT retained — tail only — honest limit below) | 0, `Build completed successfully (27 jobs)` | oleans 25 post-build; `Reactivegas.Trace` 2.3 s, `Reactivegas.Invariants` 8.4 s noted in tail |
| 2 | **incremental production rebuild, single-atom scratch variant, RED** | `lake build` (`lean/`) after one-atom `Step.lean:80` `decide (0 < v)` → `decide (0 < v + 1)` (donate guard admits `v=0`; diff retained) | **10 s** | 1, genuine property failure (not syntax): `Reactivegas/Invariants.lean:407` `step_donate_inv` — `g : (isResponsabile view a && decide (0 < v + 1)) = true` vs expected `(… && decide (0 < v)) = true` in `And.intro g`; `Reactivegas.Step` itself rebuilt OK (1.2 s), dependent theorem failed | `handoffs/P1C-scratch-variant-donate.diff` (`0bdf4e…`), `handoffs/P1C-build2-incremental.log` (`6dedd2…`, 11340 B) |
| 3 | **incremental production rebuild, single-file restore, GREEN** | `lake build` (`lean/`) after restoring `Step.lean` to HEAD | **3 s** | 0, `Build completed successfully (27 jobs)` (`Reactivegas.Step` 1.3 s, `Reactivegas.Invariants` replayed) | `handoffs/P1C-build3-restore.log` (`43fae2…`, 10430 B) |
| — | non-substantive invoc error (NOT counted) | `lake build` from repo root (no `lakefile` there) | 1 s, exit 1, `no configuration file` | — | excluded with reason: no Lean compilation attempted |

One `lake build` from the wrong cwd failed before compiling; it is not a
substantive build and is not charged. All other `lake` invocations below are
`lake env lean` elaborations (no build, no charge).

**Gap:** none outstanding against the 3-build ceiling — reconciled spend fits
exactly (3/3). No further builds are available in Phase 1; anything needing a
fourth build is recorded as an owned Phase-2 cost (see P1-D and §5).

### Non-build measurements (free; kinds kept separate)

| kind | command | wall | result |
|---|---|---|---|
| runtime replay | `lake env lean Reactivegas/CorpusGate.lean` | 2 s | exit 0, `true` (`handoffs/P1C-corpusgate.out` `a17fcf…`) |
| proof/check elaboration (43 `decide` checks + inventory) | `timeout 600 lake env lean Reactivegas/TraceTests.lean` | 11 s | exit 0, `TRACE-INVENTORY ctors=14 covered=14 missing=0`, `TRACE-TEST-SUMMARY checks=43 failures=0` (`handoffs/P1C-tracetests-summary.txt` `4d4bfc…`) |

`TRACE-INVENTORY 14/14` is a **binding** count (accepted-inversion shape +
name/mention rule), not exactness — per `ONWARD-68-INV-01`, three of the eight
non-required bindings are shown inexact elsewhere. No exactness claimed here.

## P1-A — required extent by identity, derived at this base

Historical figures 76, 29, 224, 1213, 1214 were never seeds, validators or
bounds. What follows is re-derived. Where a re-derived number coincides with a
historical one (224 distinct source names), that is reported as a comparison,
not a derivation from it.

### D1a — authored theorem inventory

**Classification rule (stated first, applied uniformly):**

- **AUTHORED-STATEMENT** — the statement mentions transition/invariant
  vocabulary: `step`/`stepEvent`/`fold`/`validate`/`enact`/`apply`/`Reach`/
  `solvent`/`insolvent`/`conservation`/`WellFormed`/`VoteWellFormed`/
  `SweepReady`/`GuardId`/`authorized`/`governance`/`permission`/`doubleEntry`/
  vote/group/proposal/member/threshold/franchise/verdict/tally/ballot/question/
  closure as *machine* properties, or is a `checkX = true by decide` executable
  witness over model checks. Obligation: relevant production-mutation kill.
- **HELPER-FACT** — a mathematical/structural lemma about `assocInsert`/
  `assocLookup`/`assocErase`/`assocAdjust`/`setInsert`/list `Nodup`/`erase`/
  `bal`/`bump`/`sumBal`/`splitUser`/`pullCollection`/`stripCollections`/
  `refundAll`/`demand`/`option.bind`/bool plumbing, true independently of this
  model **even when its data types (`Collection`, `Pledge`) are domain types**.
  Obligation on its own terms: hypotheses satisfiable (e.g. `Nodup`,
  membership, `splitUser … = some`). **No state-machine antecedent is invented
  for any helper row so a reachability column can be filled.**
- **COMPILER-GENERATED** — `.eq_def`, `.eq_N`, `.injEq`, `.sizeOf_spec`,
  `match_N.eq_N`, `Reach.below`, deriving output, `_private_`-mangled
  elaborator internals beyond the source `private theorem` itself. Excluded by
  construction. **Rule stated; source-level matches listed (zero); environment
  enumeration deferred with cost (see limit L2).**
- **Excluded, listed:** 32 anonymous `example`s (no identity; cannot be killed
  by identity), all `def`/`abbrev` (not theorems), the Lake config.
  `private theorem` is **IN** (76 occurrences) and classified like any other.

**Source derivation (commands an auditor can re-run without a build):**

- `git ls-files -- 'lean' | grep -E '\.lean$'` → 28 files incl. `lakefile.lean`
- `grep -rn --include="*.lean" -E "^(private\s+)?(theorem|lemma)\b" lean/` →
  **239 occurrences**, **224 distinct names** (15 names occur twice — mirror
  declarations in `Reactivegas/Invariants.lean` and
  `Reactivegas/TraceTests.lean`, listed below), **76 `private` occurrences /
  163 non-private**, zero `lemma` (all are `theorem`), per-file:
  `KelGroups/Invariants` 49, `KelGroups/Types` 4, `KelGroups/Vote/Invariants`
  58, `Reactivegas/Composition` 3, `Reactivegas/Invariants` 89,
  `Reactivegas/State` 20, `Reactivegas/Step` 3, `Reactivegas/Trace` 1,
  `Reactivegas/TraceTests` 12.
- Full occurrence list retained: `handoffs/P1A-theorems-grep.txt` (`9784ca…`).
- 15 duplicated names (each 2 occurrences, distinct count −15):
  `app_members_preservation_holds`, `app_members_preservation_mutant_caught`,
  `approvals_nodup`, `base_change_can_close_without_ballot`,
  `base_departure_applies_cleanup`, `base_recompute_reachable_holds`,
  `direct_admission_only_holds`, `enact_implies_threshold_met`,
  `majority_not_strict_on_even`, `majority_table`, `member_key_coherent`,
  `members_change_implies_enacted`, `proposer_mem_approvals`,
  `sweep_idempotent_mutant_caught`, `sweep_idempotent_witness`.

**Classification outcome (heuristic + per-identity review of borderlines):**

- HELPER-FACT, retained with own obligation (no exemption claimed): the
  `assoc*`/`setInsert*`/`Nodup`/`erase`/`Lookup`/`mem_*` family
  (`assocLookup_erase_of_none`, `assocLookup_adjust_of_none`,
  `assocLookup_insert_of_none`, `assocLookup_insert_self`,
  `assocErase_sublist[_']`, `assocErase_keys_nodup[_']`,
  `assocErase_key_absent[_']`, `assocInsert_keys_nodup[_']`,
  `assocAdjust_keys[_']`, `assocAdjust_keys_nodup'`, `assocAdjust_property`,
  `assocErase_property`, `assocInsert_property`, `assocLookup_some_mem[_']`,
  `mem_assocLookup_some'`, `assocErase_other_lookup`,
  `assocInsert_other_lookup`, `assocInsert_mem_cases`,
  `mem_map_fst_erase_of_ne`, `mem_map_fst_insert`, `setInsert_mem_cases`,
  `setInsert_nodup[_']`, `nodup_append_mem`, `mem_erase_inv`, `nodup_erase`,
  `closed_guard_absent`, `assoc_entries_key_unique`, `filterMap_keys_nodup`,
  `option_bind_inv`, `bool_not_true`, `bool_and_left`, `bool_and_right`,
  `bal_cons`, `bal_bump`, `bump_sum`, `sumBal_cons`, `bal_bump_ne_lemma`)
  **plus** the domain-op structural family, explicitly judged helper despite
  domain data types (`splitUser_*`, `pullCollection_*`, `stripCollections_*`,
  `refundAll_*`, `sumPledges_append`, `escrowSum_cons`, `not_mem_users_of_splitUser_none`,
  `demand_eq_true_of_some`, `demand_none_of_ne_true`, `user_absent_of_any_false`,
  `stripCollections_referente_ne`, `unique_mem_cons_inv`,
  `uniquePledges_pend_cons`). Each row's obligation is its hypotheses'
  satisfiability; none is given a `Reach` antecedent.
- AUTHORED-STATEMENT: everything else named (≈184 distinct), i.e. all
  `*_preserves_wellFormed` / `foldGroup_wellFormed` / `Enacts` / threshold /
  majority / admission / departure / hook / `step_*_inv` (14) /
  `conservation_preserved` / `step_authorized` / `governance_enacts_windUpAdmin` /
  escrow / double-entry / `solvent_*` / `reach_solvent` /
  `pledge_rejected_when_member` / `pledge_preserves_allUnique` /
  vote `SweepReady`/`VoteWellFormed`/`sweep*`/`placeBallot_*`/`effectedState_*`/
  `applyVoteEvent_*`/`foldVote_*`/`ballots_nodup_disjoint`/`open_questions_are_open`/
  `questions_partition`/`no_expiry`/`inadmissible_is_noop`/
  `nonresponsabile_event_noop`/`unfranchised_cast_noop`/`franchise_of_tallies`/
  `sweepStep_of_open`/`sweep_filterMap_of_swept`/`filter_open_idem`/
  `sweepClosures_idempotent`/`sweepDuplicating_duplicates` /
  composition `voteDerived_iff_not_direct`/`baseEnacted_threshold_met`/
  `appDecided_verdict_exhaustive` / `stepDetailed_erases` / all
  `checkX = true by decide` witnesses (`base_departure_applies_cleanup`,
  `base_change_can_close_without_ballot`, `direct_admission_only_holds`,
  `base_recompute_reachable_holds`, `sweep_idempotent_witness`,
  `sweep_idempotent_mutant_caught`, `integrated_theorem_witness_holds`,
  `canonical_economy_holds`, `exhaustive_inventories_hold`, `i57_*`,
  `admissionPreservation_holds`, `productionWellFormed_holds`,
  `comuneThresholdSanity_holds`, `all_checks_pass`,
  `frozen_checks_faithful`, `frozen_inventory_faithful`,
  `frozen_corpus_faithful`, `app_members_preservation_holds`,
  `app_members_preservation_mutant_caught`, `comune_cannot_authorize`).
  `all_checks_pass` is authored (witness over the 43 model checks), not helper.
- Zero exemptions: every helper is **retained** in the required extent with its
  own obligation. No per-identity denominator removal is claimed, so no desk
  disposition is owed for one.

**224 distinct source names (alphabetical, auditor-checkable against the
retained grep file):** `admissionPreservation_holds`, `all_checks_pass`,
`appDecided_verdict_exhaustive`, `app_members_preservation_holds`,
`app_members_preservation_mutant_caught`, `applyApprove_preserves_wellFormed`,
`applyEvent_preserves_wellFormed`, `applyPropose_preserves_wellFormed`,
`applyVoteEvent_preserves_wellFormed`, `approvals_nodup`,
`approvePending_wellFormed`, `assocAdjust_keys`, `assocAdjust_keys'`,
`assocAdjust_keys_nodup'`, `assocAdjust_property`, `assocErase_key_absent`,
`assocErase_key_absent'`, `assocErase_keys_nodup`, `assocErase_keys_nodup'`,
`assocErase_other_lookup`, `assocErase_property`, `assocErase_sublist`,
`assocErase_sublist'`, `assocInsert_keys_nodup`, `assocInsert_keys_nodup'`,
`assocInsert_mem_cases`, `assocInsert_other_lookup`, `assocInsert_property`,
`assocLookup_adjust_of_none`, `assocLookup_erase_of_none`,
`assocLookup_insert_of_none`, `assocLookup_insert_self`,
`assocLookup_some_mem`, `assocLookup_some_mem'`, `assoc_entries_key_unique`,
`auth_referente_guard_inv`, `bal_bump`, `bal_bump_ne`, `bal_bump_ne_lemma`,
`bal_cons`, `ballots_nodup_disjoint`, `baseHook_votes`,
`base_change_can_close_without_ballot`, `base_change_recomputes_votes`,
`base_change_runs_hook`, `base_departure_applies_cleanup`,
`base_recompute_reachable_holds`, `baseEnacted_threshold_met`,
`bool_and_left`, `bool_and_right`, `bool_not_true`, `bump_sum`,
`canonical_economy_holds`, `close_guard_inv`, `close_permission_to_close`,
`close_spends_referente`, `closed_guard_absent`, `commitBaseChange_members`,
`commitBaseChange_ok`, `comune_not_a_member_of_reach`, `comune_cannot_authorize`,
`comuneThresholdSanity_holds`, `conservation_preserved`,
`credit_pledges_of_reach` (private, retained), `demand_eq_true_of_some`,
`demand_none_of_ne_true`, `deposit_double_entry`, `direct_admission_only_holds`,
`direct_admission_requires_admin`, `emptyState_wellFormed`,
`emptyVoteState_sweepReady`, `emptyVoteState_wellFormed`,
`enact_implies_threshold_met`, `enact_preserves_wellFormed`,
`enactMutation_preserves_absence`, `exhaustive_inventories_hold`,
`fail_guard_inv`, `filterMap_keys_nodup`, `filter_open_idem`,
`finishEnact_preserves_wellFormed`, `foldGroup_wellFormed`,
`foldVote_wellFormed`, `franchise_of_tallies`, `frozen_checks_faithful`,
`frozen_corpus_faithful`, `frozen_inventory_faithful`,
`governance_enacts_windUpAdmin`, `i57_auth_holds`, `i57_boundary_holds`,
`i57_disjoint_holds`, `i57_disjoint_mutant_caught`, `i57_exhaustive_holds`,
`i57_franchise_holds`, `i57_franchise_mutant_caught`, `i57_noexpiry_holds`,
`i57_noop_holds`, `i57_nostale_holds`, `i57_partition_holds`,
`i57_policyfree_holds`, `i57_policyfree_mutant_caught`, `i57_r45_holds`,
`inadmissible_is_noop`, `integrated_theorem_witness_holds`,
`majority_not_strict_on_even`, `majority_table`, `member_key_coherent`,
`members_change_implies_enacted`, `membership_growth_is_direct_admission`,
`mem_assocLookup_some'`, `mem_erase_inv`, `mem_map_fst_erase_of_ne`,
`mem_map_fst_insert`, `nodup_append_mem`, `nodup_erase`,
`non_admin_admission_is_noop`, `nonresponsabile_event_noop`,
`not_insolvent_of_reach`, `not_mem_users_of_splitUser_none`,
`no_expiry`, `open_questions_are_open`, `option_bind_inv`,
`placeBallot_clean` (private, retained), `placeBallot_tally` (private,
retained), `pledge_escrow_debit`, `pledge_guard_inv`,
`pledge_preserves_allUnique`, `pledge_rejected_when_member`,
`productionWellFormed_holds`, `proposer_mem_approvals`,
`pullCollection_det` (private, retained), `pullCollection_id`,
`pullCollection_mem`, `pullCollection_sublist`, `pullCollection_sum`,
`questions_partition`, `reach_solvent`, `refundAll_bal_ge`, `refundAll_sum`,
`setInsert_mem`, `setInsert_mem_cases`, `setInsert_nodup`,
`setInsert_nodup'`, `solvent_init`, `solvent_preserved`, `splitUser_amount`,
`splitUser_sublist`, `splitUser_sum`, `step_accept_inv`, `step_authorized`,
`step_backdonate_inv`, `step_close_inv`, `step_correct_inv`, `step_deny_inv`,
`step_deposit_inv`, `step_donate_inv`, `step_fail_inv`, `step_grant_inv`,
`step_open_inv`, `step_pledge_inv`, `step_refuse_inv`, `step_transferCassa_inv`,
`step_withdraw_inv`, `stepDetailed_erases`, `stripCollections_amount_lemma`
(private, retained), `stripCollections_referente`,
`stripCollections_referente_ne`, `stripCollections_sublist`,
`stripCollections_sum`, `sumBal_cons`, `sumPledges_append`,
`sweepClosures_idempotent`, `sweepDuplicating_duplicates`,
`sweep_filterMap_of_swept`, `sweepStep_of_open`, `sweep_idempotent_mutant_caught`,
`sweep_idempotent_witness`, `tryEnactBase_preserves_absence`,
`tryEnactBase_runs_hook`, `tryEnact_eq_of_enacts`,
`tryEnact_preserves_wellFormed`, `unfranchised_cast_noop`,
`uniquePledges_pend_cons`, `user_absent_of_any_false`,
`validateDirectAdmission_ok`, `verdictOf_threshold_congr`,
`voteDerived_iff_not_direct`, `withdraw_double_entry`
(plus private `*_lemma`/`foldEvents_preserves_wellFormed`/
`tryEnactDetailed_enactment_threshold_met`/`sweepStep_key`/
`sweepClosures_open_mem`/`sweepClosures_closed_mem`/
`sweepClosures_sweepReady`/`sweepClosures_wellFormed`/
`effectedState_sweepReady`/`foldFrom_preserves_wellFormed`/`foldVote_append`/
`sweepClosures_preserves_qid`/`effectedState_preserves_qid`/
`applyVoteEvent_preserves_qid`/`foldFrom_preserves_qid`/
`sweepClosures_tallyKeys`/`tallyKeysOfState_erased_le`/
`tallyKeysOfState_insert_cases`/`effectedState_tally_growth`/
`tally_keys_franchised_from`/`comune_not_a_member_step`/
`credit_pledges_step`/`unique_mem_cons_inv`/`pullCollection_mem_lemma`/
`pullCollection_id_lemma`/`pullCollection_sum_lemma`/
`pullCollection_sublist_lemma`/`splitUser_sum_lemma`/
`splitUser_sublist_lemma`/`sumBal_foldl_bump`/`bal_foldl_bump_ge`/
`splitUser_amount_lemma`/`refundAll_bal_ge_lemma`/
`stripCollections_sublist_lemma`/`eq_nil_of_isEmpty` — all retained as listed
in the grep file; any name an auditor cannot find there is a finding about
this report).

### D1b — semantic domain, re-derived from sources (table NOT used as seed)

S (source modules): 27 (`handoffs/P1A-S-modules.txt` `f445c4…`;
tracked `git ls-files` == filesystem walk; `lakefile.lean` excluded).
S grew by one vs the S2 audit's 26: `Reactivegas.CorpusExport` (parent #87
"export verified Lean trace corpora"). Re-derived, not inherited.

**Hardcoded quota gone — verified, not trusted:** `grep -rn "163"` over
`scripts/` finds no quota constant; `grep -rn "expectedDeclarations"` finds
nothing; both gates derive S from `git ls-files` + walk agreement and print
identities. Agreement with NOTE-001's sentence; the evidence is the grep, not
the sentence.

**Mandate-table verification (each entry checked to exist at this base):**

| module | mandate entries | found |
|---|---|---|
| `Reactivegas.Step` | `step`, `stepEvent`, `voteApply`, `appFold`, `apply` | all five at `Step.lean:44,147,172,181,377` |
| `KelGroups.Fold` | `enact`, `tryEnact`, `applyEvent`, `applyEventDetailed` | all four at `Fold.lean:9,43,84,75` |
| `KelGroups.Vote.Fold` | `effectedState`, `sweepClosures`, `applyVoteEvent`, `foldVote` | all four at `Vote/Fold.lean:87,74,118,126` |
| `KelGroups.Vote.Validate` | `validateVoteEvent` | `Vote/Validate.lean:54` |
| `Reactivegas.Invariants` | `voteApplyHardPolicy` | `Invariants.lean:2248` |
| `KelGroups.Validate` | `validateProposal`, `validateApproval`, `validateDirectAdmission`, `validateBaseMutation`, `validateBaseApproval`, `validateBase`, `validateEvent` | all seven at `Validate.lean:107,116,142,153,163,173,180` |
| `KelGroups.Integration` | `applyIntegratedEvent`, `foldIntegrated`, `tryEnactBase`, `commitBaseChange`, `admitMemberInto`, `enactMutation`, `BaseHook`/`IntegratedAppFold` | all at `Integration.lean:172,217,148,139,118,124,66,59` |

**No disagreement with the rev-3 table at this base.** Additionally derived
(non-allowlist) production-adjacent defs exist and are listed so the table
cannot go stale silently: `KelGroups.Fold`: `finishEnact`, `tryEnactDetailed`,
`applyPropose[D compr.]`, `approvePending`, `applyApprove[D compr.]`,
`foldGroup`; `KelGroups.Vote.Fold`: `placeBallot`, `sweepStep`,
`applyVoteEventChecked`, `foldFrom`; `Reactivegas.Step`: `demand`,
`findCollection`, `isResponsabile`, `memberKeys`, `absorbConto`, `windUpAdmin`,
`economicCleanup`, `baseHook`, `proposalDigest`, `proposalMutation`,
`validateProposal` (distinct from `KelGroups.Validate.validateProposal`),
`integration`, `productionWellFormed`, `boot`; `KelGroups.Integration`:
`mutationChange`, `IntegratedEvent`/`IntegratedError`/`Integration`.
Reached hooks: `AppFold`, `IntegratedAppFold`, `BaseHook`.

**Guard/effect/error axis (derived):** `Event` 14 ctors
(`Reactivegas/Types.lean:41-69`); `AppEvent` 14+3 (`Types.lean:75-94`: 14
economic + `openQuestion`/`cast`/`renounce` vote passthrough); `GuardId` 14
(`Trace.lean:51-54`) with total `guardOf` (`Trace.lean:59-73`);
`VoteEvent` 3 (`Vote/Event.lean`); `VoteError` 4 (`notResponsabile`,
`questionNotFound`, `notDesignee`, `notProposer` — the latter two declared for
Slice B, produced by nothing in Slice A per `Vote/Validate.lean` header);
`ValidationError` 11 (`Validate.lean:7-19`); `StepError.rejected` single
(`Types.lean:100`); `ProductionError` (`Step.lean:368`); `BaseChange` 3,
`BaseMutation` 2, `DirectCommand.admitMember` 1 (`KelGroups/Event.lean`);
`Proposal` 2 (`Reactivegas/Types.lean` `departure`/`changeRoles`).
`allGuardIds` hand-lists 14 (`TraceTests.lean:193-196`), reconciled live
against elaboration-discovered `traceInventory.ctors` (14/14 at this base).

### The relation — ownership, NOT the Cartesian product

REQUIRED-INPUT is two sets plus the relevant-ownership relation. The Cartesian
product (≈224 × ≈40 ≈ 9000 pairs) is explicitly NOT claimed. Relevant ownership
derived from statements (property-specific failure must be observed per row):

- Each `step_<c>_inv` (14) is owned by its constructor's guard atom + effect
  atom (e.g. `step_donate_inv` ↔ `donate` guard `isResponsabile && 0 < v` +
  `casse`/`comune` effect; proven relevant by Build 2: mutating the donate
  guard broke exactly this proof).
- `conservation_preserved`, `step_authorized`, `solvent_preserved`,
  `reach_solvent`, `not_insolvent_of_reach`, `comune_not_a_member_of_reach`
  are owned by **all 14** economic atoms (cross-cutting; one atom's relevant
  failure does not close the row — each row needs its own property failure).
- Vote rows (`ballots_nodup_disjoint`, `open_questions_are_open`,
  `questions_partition`, `no_expiry`, `franchise_of_tallies`,
  `inadmissible_is_noop`, `nonresponsabile_event_noop`,
  `unfranchised_cast_noop`, sweep/idempotence family) are owned by the three
  `VoteEvent` atoms + franchise/threshold atoms, never by economic guards.
- Substrate rows (`enact_*`, `applyPropose/Approve/Event*`, `foldGroup_*`,
  `approvals_nodup`, `proposer_mem_approvals`, threshold/majority,
  admission/departure/hook rows) are owned by base/validate/integration atoms,
  never by economic `step` guards.
- `checkX = true by decide` witnesses are owned by the atoms their check
  evaluates (e.g. `sweep_idempotent_witness` ↔ sweep atoms;
  `direct_admission_only_holds` ↔ direct-admission atoms).
- Helper rows are owned by **no guard atom**; their obligation is satisfiable
  hypotheses. No pairings are manufactured for them.

## P1-B — receipt inventory (43 campaign ledgers re-keyed, zero reused)

Scope: the 43 `*campaign-ledger*` files in
`ms2-runtime-20260905-0833.tar.gz` (6489 files). The 22 further `*ledger*`
files (ceiling/build/LEDGER notes) are context, not receipts, and are named
here so none is silently absorbed.

**Binding rule per receipt:** subject identity · actual mutation · checker/gate
· fixture/input · transitive dependency footprint **with evidence** · toolchain
· command → marking. **Unchanged subject bytes alone preserve nothing.**

| ledger family (archive paths abridged) | INV rows / subject area | kinds present (rev-3 defs) | checker/gate · toolchain/command as recorded | marking at base `3590c00` |
|---|---|---|---|---|
| `t48-inversions` (4: `campaign-ledger.md`, 3 auditor `campaign-ledger.md`/`mutation-campaign-ledger.md`) | INV-48-I-* (10 rows: SURFACE/GUARDS/CANFAIL/AXIOMS/REGRESSION/FENCE/EVENT-SYNTAX/INV-HYP-SYNTAX/STEP-ITE/STEPEVENT-DELEGATE); inversion coverage | FIXTURE + CHECKER + PRODUCTION mixed; old `6/224` summed unlike units (withdrawn) | `check-reactivegas-inversion-coverage` ± negative control; Lean 4.25/4.27 instruments; RED logs hashed per row | **STALE** — context moved (new tree, S 26→27, gate rewritten for axioms/provenance); footprint not evidenced at this base; aggregate GREEN closes no row |
| `e-lean-compliance/auditor-choices-codex/evidence/inputs/t48-campaign-ledger.md`, `t54-campaign-ledger.md` (2) | copies of the above INV-48 + INV-54 rows as auditor inputs | as above | as above | **STALE** (same reason; copies, not fresh evidence) |
| `e43/t54*` + `t54-vote-coverage/*` + `t54-composition-owner-codex/*` (6) | INV-54-* (PARTITION/DISJOINT/NOSTALE/FRANCHISE/NOEXPIRY/POLICYFREE); vote machine | PRODUCTION (partition silent-deletion, opposite-tally erase, unfranchised-recast, legacyThreshold hard-code) + FIXTURE comparisons | slice/frozen/ticket gates; Lean 4.25.0 typecheck rung unmetered; RED logs per row | **STALE** — subjects predate #87/#88 (no `CorpusExport`, old gate); footprint not evidenced here |
| `e43/.archived/t62-owner-codex*` (8 incl. a011/a013/s1/s2) | S62 membership/role model | PRODUCTION + FIXTURE | campaign + build/ceiling ledgers; mixed toolchains | **STALE** (substrate moved; S62-B vocabulary `DirectCommand`/`BaseMutation` re-cut since) |
| `e43/.archived/t48-owner-codex/*` (7 tsv/md) | #48 docs/emitter/pledge/inversions slices | CHECKER + FIXTURE heavy | auditor campaign TSVs | **STALE**; several rows are checker-sensitivity, never production kills under rev-3 kinds |
| `e43/.archived/t57-owner-codex`, `t59-owner-claude/*`, `e43/.archived/t47`, `t48-bootstrap` (5+) | earlier slices/pipelines | mixed, provenance thin | various | **STALE** or **UNUSABLE** where row→identity binding unrecoverable from the TSV alone |
| `t-simulator-fable/handoffs/*` (5: geometry-permalinks/strips/batch2/fidelity/fidelity-recut) + `.archived/commit-auditor-grok-batch1-s1` + `.archived/t-simulator/*` (3) | simulator geometry/fidelity, no theorem identity | n/a (not Lean mutations) | browser/gate logs | **UNUSABLE** for S3 D2 — wrong subject (no theorem identity, no production-definition mutation); retained, not re-labelled |
| `t-toolchain-contract/handoffs/*` (2) + `.archived/auditor-s1-corrected` (1) | toolchain/ceiling | n/a | `check-lean-toolchain`, ceiling TSVs | **UNUSABLE** for D2 (no theorem row); toolchain pin `v4.25.0` noted as context |
| `e-haskell-impl/t74-corpus-exporter/evidence/campaign-ledger.md` (1) | Haskell exporter | n/a | Haskell gate | **UNUSABLE** for D2 (wrong subject) |
| `t-release-pipeline/.archived/recut-s001-auditor` (1) | release recut | n/a | release gate | **UNUSABLE** for D2 |

**Totals: REUSABLE-BOUNDED 0 · STALE ≈30 · UNUSABLE ≈13 · historical aggregate
GREEN closes 0 theorem rows.** No receipt is re-labelled as D2 evidence.
Per-mutant re-keying to current theorem identities (with evidenced footprints)
is deferred to Phase 2 as owned work (owner: Phase 2; cost: P1-D).

## P1-C — measured cost model (measure; do not assume)

Kinds are reported separately and **never averaged**:

| kind | measurement | cost | what it establishes |
|---|---|---|---|
| full cold build | Build 1: `lake build` from zero oleans | **19 s** wall (27 jobs; `Invariants` 8.4 s, `Trace` 2.3 s) | ceiling for a from-scratch row |
| incremental production rebuild, RED (true mutant) | Build 2: single-atom donate-guard variant; `Step` rebuilt 1.2 s, `Invariants` failed 7.6 s at `step_donate_inv:407` with the expected guard-type mismatch; definition stayed well-typed, theorem proof failed = admitted RED per rev-3 | **10 s** wall | per-row mutant cost when the owning theorem is downstream of `Step` (representative, not universal — see limits) |
| incremental production rebuild, GREEN (restore) | Build 3: restore `Step.lean`, `Step` 1.3 s, dependents replayed | **3 s** wall | lower bound for a single-file touch that re-verifies |
| proof/check elaboration | `lake env lean Reactivegas/TraceTests.lean` (43 checks + inventory, no build) | **11 s** wall | cost of the executable check harness, distinct from compilation |
| runtime replay | `lake env lean Reactivegas/CorpusGate.lean` → `true` | **2 s** wall | cost of corpus replay, distinct from everything above |

**Isolation preserved:** the variant mutated exactly one atom
(`Step.lean:80` donate guard); the RED names exactly the owning theorem
(`step_donate_inv`). No multi-atom subject was built; no ambiguous-cause mutant
is claimed as multiple kills. Shared-infrastructure/batched scheduling across
**separately admitted** single-atom variants was **not measured** within the
3-build ceiling and is **not claimed** — any batching saving in P1-D is marked
as hypothesis with its validation cost, not as evidence.

## P1-D — phase proposal with numeric ceilings derived from measurements

Every required identity is preserved across phases (defer, never drop).
Exploration budget 0. No coverage claimed in Phase 1.

| phase | scope | ceiling (derived) | basis |
|---|---|---|---|
| P2-a | D3 footprint binding + per-mutant re-keying of STALE Lean rows to the 224 current identities (no kills) | **8 substantive builds + 40 `lake env lean` elaborations** | ≤30 STALE ledgers × spot re-elaboration; elaborations are free of the build cap but bounded here so the auditor can check pacing; builds reserved for footprint disputes only |
| P2-b | production-definition kills for the 14 `step_*_inv` + owning guard/effect atoms (one admitted single-atom mutant per relevant row, isolation as in P1-C) | **20 substantive builds** | 14 rows × 10 s RED + margin for 6 cross-cutting rows' property-specific failures; cold 19 s used only if incremental state is ever untrusted |
| P2-c | vote/substrate authored rows (≈60 machine-property rows) + helper satisfiability ledger (hypotheses exhibited, no mutants) | **25 substantive builds** | same per-row RED rate; helpers cost elaborations only (bounded inside P2-a's 40) |
| P2-d | `checkX = true by decide` witness rows (≈30) — each row's check re-elaborated at final candidate + surrounding-check sensitivity for `*_mutant_caught` pairs | **6 substantive builds + 20 elaborations** | 11 s harness + 2 s replay per witness family, batched per file; builds only for witness-fixture disputes |
| total 2..n | | **59 substantive builds + 60 elaborations, exploration 0** | sum of phase ceilings; batching savings NOT subtracted (unmeasured — see P1-C) |

Batching validation (optional, separately authorized if wanted): permit at most
**4 additional builds** to test shared-infrastructure scheduling of separately
admitted single-atom variants; any ceiling reduction from it requires that
evidence first. Not included above.

## §5. Limits not closed — owned findings, not deliverables

- **L1 — environment T not re-elaborated in Phase 1.** Source T is 239
  occurrences / 224 distinct; environment T (≈1213 distinct incl.
  compiler-generated, per gate header) was not re-derived by elaboration here:
  that costs a gate execution (a build) we no longer hold (3/3 spent).
  Owner: Phase 2 (P2-a). The denominator for any future ratio is the D1
  REQUIRED-INPUT derived here, never executed-mutant counts.
- **L2 — compiler-generated identities listed by rule only.** Source matches
  for `.eq_def`/`.eq_N`/`.injEq`/`sizeOf_spec`/`match_N.eq_N`/`Reach.below`
  are zero; the environment list is deferred with L1. No row is closed by
  narrowing past them.
- **L3 — Build 1 full log not retained.** Wall 19 s and tail (27 jobs,
  key module times) recorded; full stdout not hashed. Re-measurable in Phase 2
  if the ceiling grants it. Owner: Phase 2.
- **L4 — per-row costs vary by dependency depth.** 10 s RED is measured for a
  `Step`→`Invariants` edge; vote/substrate rows may differ. P1-D ceilings carry
  margin for this; no average is used.
- **L5 — historical `checkCanonicalEconomyMutant`-class fixture comparisons
  remain fixtures.** They show check sensitivity (and surviving surrounding
  checks show assurance gaps) but never production kills. Not summed.
- **L6 — D6 items observed, not repaired here:** OD74-S1-COMMENT
  (`Trace.lean:357-359` doc *"exercises an `UNPROVED` claim row"* — stale since
  S1 resolved that refusal to `step_withdraw_inv`; doc-vs-definition
  disagreement, not model defect); *"eighteen identities"* + *"8/10 split"*
  prose (`TraceTests.lean:804,816,828`) against 14-constructor `allGuardIds`;
  `Predicates.lean` design-page path; `renounce` accept-and-no-op vs V-5
  (routed to #81). `docs/en/design/` routing via desk to #71. Owner: D6 final
  pass / #71 per mandate.

## §6. Reconciliation (Phase 1 claims no coverage — by identity, nothing absorbed)

- `DISCOVERED` (source): 224 distinct named identities (239 occurrences) + 27
  source modules + guard/effect/error axis above.
- `REQUIRED-INPUT`: same 224 identities (zero exemptions) + relevant owned
  atoms per §P1-A-relation. No Cartesian pairs.
- `EXECUTED`/`KILLED`/`SURVIVED`/`BLOCKED`: **not claimed** in Phase 1
  (forbidden). The single scratch RED (`step_donate_inv` vs donate variant) is
  **costing evidence only**, retained in `handoffs/`, never a coverage kill.
- Every REQUIRED-INPUT identity is accounted for as: kill pending (Phase 2),
  helper-satisfiability pending (Phase 2), or D6-ambiguity noted (L6) — none
  silently absorbed, none counted as covered.

*End of Phase 1 report. Auditor of another family: the packet is
`handoffs/` (this report + retained evidence files with hashes above) plus
`STATUS.md`. Nothing else is offered and nothing is hidden.*
