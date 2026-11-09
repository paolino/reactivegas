# Unconstrained theorems at the start of the statement-triage slice

Base: master `d51d473`. Derived from `lean/mutants/ledger.json` at that commit: every OPEN row whose reason begins `unconstrained:`, that is, no attempted mutant of its statement closure breaks it, nor a theorem its proof uses. 83 rows, each with the one disposition it has in the ledger now:

- **KILLED**: a catalogued mutant of its statement closure breaks it.
- **INTENDED**: no mutant can fairly break it; the reason is stored in the ledger row and rendered beside it.
- **MASKED**: a mutant now breaks a theorem its proof only cites, so it rests on a killed theorem.

| theorem | mutants attempted at the base | disposition |
|---|---|---|
| `CompositionTests.l7_antecedent_of_check` | 144 | INTENDED (reason in the ledger row) |
| `CompositionTests.l7_mint_mutant_rejected` | 145 | INTENDED (reason in the ledger row) |
| `CompositionTests.l7_refund_mutant_rejected` | 145 | INTENDED (reason in the ledger row) |
| `KelGroups.Vote.assocAdjust_keys'@KelGroups.Vote.Invariants` | 2 | KILLED by `assocAdjust-deletes` |
| `KelGroups.Vote.assocAdjust_keys_nodup'@KelGroups.Vote.Invariants` | 2 | MASKED, its proof uses `KelGroups.Vote.assocAdjust_keys'@KelGroups.Vote.Invariants` (killed by `assocAdjust-deletes`) |
| `KelGroups.Vote.assocErase_other_lookup@KelGroups.Vote.Invariants` | 2 | KILLED by `assocErase-erases-others` |
| `KelGroups.Vote.filter_open_idem` | 5 | INTENDED (reason in the ledger row) |
| `KelGroups.Vote.foldVote_append@KelGroups.Vote.Invariants` | 31 | KILLED by `foldFrom-ignores-initial` |
| `KelGroups.Vote.inadmissible_is_noop` | 30 | KILLED by `applyVoteEvent-rejected-resets` |
| `KelGroups.Vote.sweepDuplicating_duplicates` | 11 | INTENDED (reason in the ledger row) |
| `KelGroups.Vote.sweepStep_key@KelGroups.Vote.Invariants` | 11 | KILLED by `sweepStep-open-arm-closes` |
| `KelGroups.Vote.sweepStep_of_open` | 11 | KILLED by `sweepStep-open-arm-closes` |
| `KelGroups.Vote.sweep_filterMap_of_swept` | 11 | MASKED, its proof uses `KelGroups.Vote.sweepStep_of_open` (killed by `sweepStep-open-arm-closes`) |
| `KelGroups.Vote.unfranchised_cast_noop` | 30 | KILLED by `applyVoteEvent-rejected-resets` |
| `KelGroups.applyEvent_app_lists@KelGroups.Invariants` | 12 | KILLED by `applyEvent-app-clears-members` |
| `KelGroups.approvals_nodup` | 2 | INTENDED (reason in the ledger row) |
| `KelGroups.approvePending_appr@KelGroups.Invariants` | 1 | KILLED by `approvePending-ignores-signer` |
| `KelGroups.assocAdjust_keys@KelGroups.Invariants` | 2 | KILLED by `assocAdjust-deletes` |
| `KelGroups.basePendingCoherent_corr` | 2 | INTENDED (reason in the ledger row) |
| `KelGroups.baseUpdate_appr@KelGroups.Invariants` | 1 | INTENDED (reason in the ledger row) |
| `KelGroups.emptyState_strong@KelGroups.Invariants` | 1 | KILLED by `emptyState-has-pending-proposal` |
| `KelGroups.emptyState_strongBase@KelGroups.Invariants` | 1 | KILLED by `emptyState-has-pending-base` |
| `KelGroups.enact_pendingBase@KelGroups.Invariants` | 5 | KILLED by `enact-remove-clears-pending-base` |
| `KelGroups.enact_pendingProposals@KelGroups.Invariants` | 5 | KILLED by `enact-remove-clears-pending-proposals` |
| `KelGroups.enacts_corr` | 10 | INTENDED (reason in the ledger row) |
| `KelGroups.majority_not_strict_on_even` | 3 | KILLED by `majority-strict` |
| `KelGroups.member_key_coherent` | 2 | INTENDED (reason in the ledger row) |
| `KelGroups.pendingCoherent_corr` | 2 | INTENDED (reason in the ledger row) |
| `KelGroups.proposer_absent_above_one` | 2 | INTENDED (reason in the ledger row) |
| `KelGroups.traceAdmissible_corr` | 23 | INTENDED (reason in the ledger row) |
| `KelGroups.tryEnact_eq_of_enacts` | 10 | KILLED by `tryEnact-returns-input` |
| `KelGroups.wellFormed_corr` | 2 | INTENDED (reason in the ledger row) |
| `Reactivegas.Composition.appDecided_verdict_exhaustive` | 1 | KILLED by `appVerdictAllows-open-allowed` |
| `Reactivegas.Composition.closeProposerQuestions_keeps_closed` | 4 | KILLED by `closeProposer-forgets-closed` |
| `Reactivegas.Composition.effectedState_keeps_closed` | 13 | KILLED by `renounce-forgets-closed` |
| `Reactivegas.Composition.targetPresent_pruneAuth` | 8 | KILLED by `pruneAuth-drops-collections` |
| `Reactivegas.Lifecycle.foldVoteWith_production` | 31 | KILLED by `applyVoteEvent-rejected-resets` |
| `Reactivegas.Lifecycle.l1_renounce_noop_caught` | 137 | INTENDED (reason in the ledger row) |
| `Reactivegas.Lifecycle.l2_depart_none_caught` | 137 | INTENDED (reason in the ledger row) |
| `Reactivegas.Lifecycle.l3_depart_positive_caught` | 137 | INTENDED (reason in the ledger row) |
| `Reactivegas.Lifecycle.l3_renounce_positive_caught` | 137 | INTENDED (reason in the ledger row) |
| `Reactivegas.Lifecycle.l4_depart_tally_caught` | 137 | INTENDED (reason in the ledger row) |
| `Reactivegas.Lifecycle.l4_renounce_tally_caught` | 137 | INTENDED (reason in the ledger row) |
| `Reactivegas.Lifecycle.l5_depart_discard_caught` | 137 | INTENDED (reason in the ledger row) |
| `Reactivegas.Lifecycle.l5_renounce_discard_caught` | 137 | INTENDED (reason in the ledger row) |
| `Reactivegas.Lifecycle.l6_depart_all_caught` | 137 | INTENDED (reason in the ledger row) |
| `Reactivegas.Lifecycle.l6_renounce_all_caught` | 137 | INTENDED (reason in the ledger row) |
| `Reactivegas.Lifecycle.l6b_also_unrelated_caught` | 137 | INTENDED (reason in the ledger row) |
| `Reactivegas.Lifecycle.order_identity_first_caught` | 137 | INTENDED (reason in the ledger row) |
| `Reactivegas.Lifecycle.r1_any_renouncer_caught` | 137 | INTENDED (reason in the ledger row) |
| `Reactivegas.Lifecycle.r2_any_caster_caught` | 137 | INTENDED (reason in the ledger row) |
| `Reactivegas.Lifecycle.v3_no_sweep_caught` | 137 | INTENDED (reason in the ledger row) |
| `TraceTests.frozen_corpus_faithful` | 36 | INTENDED (reason in the ledger row) |
| `TraceTests.frozen_inventory_faithful` | 1 | INTENDED (reason in the ledger row) |
| `approvals_nodup` | 2 | INTENDED (reason in the ledger row) |
| `auth_referente_guard_inv` | 4 | INTENDED (reason in the ledger row) |
| `close_guard_inv` | 4 | INTENDED (reason in the ledger row) |
| `comune_not_a_member_corr` | 4 | INTENDED (reason in the ledger row) |
| `comune_not_a_member_of_reach` | 35 | INTENDED (reason in the ledger row) |
| `comune_not_a_member_step@Reactivegas.Invariants` | 34 | INTENDED (reason in the ledger row) |
| `conservation_corr` | 3 | INTENDED (reason in the ledger row) |
| `doubleEntry_corr` | 1 | INTENDED (reason in the ledger row) |
| `escrowHeld_corr` | 2 | INTENDED (reason in the ledger row) |
| `escrowSum_cons` | 2 | KILLED by `escrowSum-drops-head` |
| `fail_guard_inv` | 4 | INTENDED (reason in the ledger row) |
| `governance_enacts_windUpAdmin` | 7 | KILLED by `windUpAdmin-keeps-collections` |
| `majority_not_strict_on_even` | 3 | MASKED, its proof uses `KelGroups.majority_not_strict_on_even` (killed by `majority-strict`) |
| `member_key_coherent` | 2 | INTENDED (reason in the ledger row) |
| `not_mem_users_of_splitUser_none` | 2 | KILLED by `splitUser-matches-other-users` |
| `pledge_guard_inv` | 6 | INTENDED (reason in the ledger row) |
| `pullCollection_det@Reactivegas.Invariants` | 1 | INTENDED (reason in the ledger row) |
| `pullCollection_id` | 1 | MASKED, its proof uses `pullCollection_id_lemma@Reactivegas.State` (killed by `pullCollection-ignores-id`) |
| `pullCollection_id_lemma@Reactivegas.State` | 1 | KILLED by `pullCollection-ignores-id` |
| `pullCollection_mem` | 1 | MASKED, its proof uses `pullCollection_mem_lemma@Reactivegas.Invariants` (killed by `pullCollection-returns-modified`) |
| `pullCollection_mem_lemma@Reactivegas.Invariants` | 1 | KILLED by `pullCollection-returns-modified` |
| `solvent_init` | 5 | KILLED by `bal-absent-negative` |
| `splitUser_amount` | 2 | MASKED, its proof uses `splitUser_amount_lemma@Reactivegas.Invariants` (killed by `splitUser-matches-other-users`) |
| `splitUser_amount_lemma@Reactivegas.Invariants` | 2 | KILLED by `splitUser-matches-other-users` |
| `stepDetailed_erases` | 35 | KILLED by `stepDetailed-applies-old-state` |
| `stripCollections_referente` | 2 | KILLED by `stripCollections-keeps-referente` |
| `stripCollections_referente_ne` | 2 | KILLED by `stripCollections-keeps-referente` |
| `stripCollections_sublist` | 2 | MASKED, its proof uses `stripCollections_sublist_lemma@Reactivegas.Invariants` (killed by `stripCollections-keeps-modified`) |
| `stripCollections_sublist_lemma@Reactivegas.Invariants` | 2 | KILLED by `stripCollections-keeps-modified` |

