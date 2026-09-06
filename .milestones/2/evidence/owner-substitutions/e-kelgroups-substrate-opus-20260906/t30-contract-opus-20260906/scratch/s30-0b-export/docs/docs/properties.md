# Properties Catalog

This page catalogs every proven invariant and QuickCheck property in the
kelgroups project. Each entry links a Lean theorem to its corresponding
QuickCheck test, ensuring the Haskell implementation matches the formal
specification.

## Static Invariants

Lean file: `KelGroups/Invariants.lean` | QC file: `InvariantsSpec.hs`

These properties hold for any `GroupState`, independent of how it was
reached.

| # | Property | Lean theorem | QC test |
|---|---|---|---|
| S1 | Bootstrap mode iff zero admins | `bootstrap_iff_zero_admins` | `bootstrap mode implies zero admins` |
| S2 | Normal mode iff positive admins | `normal_iff_positive_admins` | `normal mode implies positive admins` |
| S3 | Empty state is bootstrap | `empty_is_bootstrap` | `empty state is in bootstrap mode` |
| S4 | majority(0) = 0 | `majority_zero` | `majority of 0 admins is 0` |
| S5 | majority(1) = 1 | `majority_one` | `majority of 1 admin is 1` |
| S6 | majority(2) = 1 | `majority_two` | `majority of 2 admins is 1` |
| S7 | majority(3) = 2 | `majority_three` | `majority of 3 admins is 2` |
| S8 | majority(n) <= n | `majority_le` | `majority n <= n` |
| S9 | majority(n) > 0 when n > 0 | `majority_pos` | `majority of positive admin count is positive` |
| S10 | Removing all members triggers bootstrap | `remove_all_triggers_bootstrap` | `removing all members triggers bootstrap` |
| S11 | State with admin member is normal | `admin_member_means_normal` | `state with at least one admin is in normal mode` |

## Transition Invariants

Lean file: `KelGroups/TransitionInvariants.lean` | QC file:
`TransitionInvariantsSpec.hs`

These properties describe how transition functions (`enact`,
`applyPropose`, `foldGroup`) preserve or change state.

| # | Property | Lean theorem | QC test |
|---|---|---|---|
| T1 | Introducing admin makes adminCount > 0 | `enact_introduce_admin_exits_bootstrap` | `introducing admin makes adminCount > 0` |
| T2 | Introducing admin increases adminCount by 1 | `enact_introduce_admin_count` | `introducing admin increases adminCount by 1` |
| T3 | Introducing non-admin preserves adminCount | `enact_introduce_nonadmin_count` | `introducing non-admin preserves adminCount` |
| T4 | Enact only touches members, not proposals | `enact_preserves_pendingProposals` | `enact only touches members` |
| T5 | Folding empty list yields emptyState | `foldGroup_nil` | `folding empty list yields emptyState` |
| T6 | Bootstrap proposal is immediately enacted | `bootstrap_proposal_immediately_enacted` | `bootstrap proposal has no pending after apply` |
| T7 | Single admin proposal enacted immediately | `single_admin_proposal_enacted` | `single admin proposal is enacted immediately` |
| T8 | Removing member with >= 2 admins keeps normal | `enact_remove_preserves_normal_if_enough_admins` | `adminCount >= 2 and remove keeps adminCount >= 1` |

## Fold Invariants

Lean file: `KelGroups/FoldInvariants.lean` | QC file: `FoldSpec.hs`

These properties describe the separation between base and app events in
the fold, and the effect of individual operations on state shape.

| # | Property | Lean theorem | QC test |
|---|---|---|---|
| F1 | App event preserves members | `applyEvent_app_preserves_members` | `app event preserves members` |
| F2 | App event preserves pendingProposals | `applyEvent_app_preserves_pendingProposals` | `app event preserves pendingProposals` |
| F3 | Approve of non-existent proposal is identity | `applyApprove_nonexistent_is_identity` | `approve nonexistent is identity` |
| F4 | ChangeRoles preserves member count | `enact_changeRoles_preserves_member_count` | `changeRoles preserves member count` |
| F5 | IntroduceMember increases member count by 1 | `enact_introduce_increases_member_count` | `introduce increases member count` |

## Validation Invariants

Lean file: `KelGroups/ValidateInvariants.lean` | QC file:
`ValidateSpec.hs`

These properties describe the accept/reject behavior of event
validation. The Lean formalization covers base-system checks only (no
`GroupConfig` role preconditions).

### Bootstrap mode

| # | Property | Lean theorem | QC test |
|---|---|---|---|
| V1 | IntroduceMember with Admin passes bootstrap | `bootstrap_accepts_admin_intro` | `bootstrap accepts admin intro` |
| V2 | IntroduceMember without Admin fails bootstrap | `bootstrap_rejects_nonadmin_intro` | `bootstrap rejects non-admin intro` |
| V3 | RemoveMember fails bootstrap | `bootstrap_rejects_remove` | `bootstrap rejects remove` |
| V4 | ChangeRoles fails bootstrap | `bootstrap_rejects_changeRoles` | `bootstrap rejects changeRoles` |

### Normal mode: authentication

| # | Property | Lean theorem | QC test |
|---|---|---|---|
| V5 | Non-admin signer rejected for proposals | `normal_proposal_requires_admin` | `non-admin proposal rejected` |
| V6 | Existing member key rejected for introduce | `normal_introduce_existing_rejected` | `introduce existing rejected` |
| V7 | Non-member key rejected for remove | `normal_remove_nonmember_rejected` | `remove nonmember rejected` |

### Normal mode: approval constraints

| # | Property | Lean theorem | QC test |
|---|---|---|---|
| V8 | Non-existent proposal rejected for approve | `normal_approve_missing_rejected` | `approve missing rejected` |
| V9 | Already-approved signer rejected | `normal_double_approve_rejected` | `double approve rejected` |
| V10 | Admin with valid proposal accepted | `normal_valid_admin_proposal_accepted` | `valid admin proposal accepted` |

### QC-only properties (no Lean counterpart)

These test Haskell-specific aspects involving `GroupConfig` or the
`App` event branch, which are not formalized in Lean.

| # | Property | QC test |
|---|---|---|
| V11 | Non-member cannot sign proposals | `non-member proposal rejected` |
| V12 | Non-member app event rejected | `non-member app event rejected` |
| V13 | Member app event accepted | `member app event accepted` |
| V14 | Valid approval accepted | `valid approval accepted` |

## Store-Through Invariants

QC file: `StoreInvariantsSpec.hs` (no Lean counterpart)

These replay the above invariants through the SQLite store (JSON
serialization round-trip), verifying that serialization does not break
any invariant.

| # | Property | Mirrors |
|---|---|---|
| ST1 | bootstrap iff zero admins (store) | S1 |
| ST2 | normal iff positive admins (store) | S2 |
| ST3 | admin member means normal (store) | S11 |
| ST4 | enact introduce admin exits bootstrap (store) | T1 |
| ST5 | enact introduce admin count (store) | T2 |
| ST6 | enact introduce nonadmin count (store) | T3 |
| ST7 | enact preserves pendingProposals (store) | T4 |
| ST8 | enact remove preserves normal (store) | T8 |
| ST9 | bootstrap proposal immediately enacted (store) | T6 |
| ST10 | single admin proposal enacted (store) | T7 |
| ST11-13 | Store mechanics (roundtrip, fold, length) | -- |
