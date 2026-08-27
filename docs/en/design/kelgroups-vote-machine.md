# Kelgroups vote-machine model

This Lean 4 model reproduces the vote machine in `kelgroups` commit `368b596`.
Its status is deliberately narrow: the state transition and validation rules
are proved and executed in the model. The model is not connected to the
Reactivegas economic machine in this slice, and it does not claim runtime or
cryptographic enforcement.

`FAITHFUL` means the declaration reproduces the cited Haskell behavior.
`DIVERGENT` means a ruled abstraction replaces an external implementation;
the port consequence is stated in the row. `EXTENSION` names behavior absent
from Haskell and adds no semantics.

## Fidelity matrix

| Requirements | Status | Lean declaration | Haskell anchor | Fidelity and later-port consequence |
|---|---|---|---|---|
| R-1, R-4 | FAITHFUL | lean:KelGroups.GroupState | `State.hs` `GroupState` | The separate default Lake target roots and elaborates the complete model. |
| R-2, R-2b, R-3, R-5 | FAITHFUL | lean:KelGroups.AppFold | `Types.hs` type parameter `a` | The substrate uses only an abstract application type; the tracked checker enforces the one-way boundary. |
| R-6 | FAITHFUL | lean:KelGroups.applyPropose | `Fold.hs` `applyPropose` | A proposal installs the signer as proposer and sole approver. |
| R-7 | FAITHFUL | lean:KelGroups.applyPropose | `Fold.hs` `Map.insert` | Reusing a digest replaces the pending entry and discards approvals. |
| R-8, R-11 | FAITHFUL | lean:KelGroups.tryEnact | `Fold.hs` `tryEnact` | Propose and approve call enactment after updating pending state; the threshold is read before the payload effect. |
| R-9 | FAITHFUL | lean:KelGroups.majority | `State.hs` `majority` | Natural division computes `(admins + 1) / 2`; lean:KelGroups.majority_table and lean:KelGroups.majority_not_strict_on_even pin the weak even case. |
| R-10 | FAITHFUL | lean:KelGroups.applyPropose | `State.hs` `majority`; `Fold.hs` `applyPropose` | Threshold zero enacts a bootstrap proposal immediately. |
| R-12 | FAITHFUL | lean:KelGroups.applyApprove | `Fold.hs` `applyApprove` | An unknown digest returns the state unchanged. |
| R-13 | FAITHFUL | lean:KelGroups.setInsert | `Fold.hs` `Set.insert` | Duplicate approval does not change cardinality, although enactment is retried. |
| R-14 | FAITHFUL | lean:KelGroups.finishEnact | `Fold.hs` `Map.delete` | Only the enacted digest is erased; sibling approvals remain stale. |
| R-15 | FAITHFUL | lean:KelGroups.enact | `Fold.hs` `enact` | Introduction overwrites, removal of an absent key is inert, and role adjustment never inserts. |
| R-16 | FAITHFUL | lean:KelGroups.member_key_coherent | `Fold.hs` `Member { memberKey = pubKey }` | Introduced members store the association-list key. |
| R-17 | FAITHFUL | lean:KelGroups.applyEvent | `Fold.hs` `App` branch | Application folding discards the signer and preserves membership and pending proposals. |
| R-18 | FAITHFUL | lean:KelGroups.validateProposal | `Validate.hs` `validateBootstrapProposal` | Bootstrap checks the introduced key and admin role but never the signer. |
| R-19 | FAITHFUL | lean:KelGroups.validateProposal | `Validate.hs` `validateNormalProposal` | Admin authentication precedes payload checks in Haskell order. |
| R-20 | FAITHFUL | lean:KelGroups.validateApproval | `Validate.hs` `validateApproval` | Admin, proposal existence, and duplicate approval fail distinctly and in order. |
| R-21 | FAITHFUL | lean:KelGroups.validateEvent | `Validate.hs` `validateEvent` `App` | Application events require membership only. |
| R-22 | FAITHFUL | lean:KelGroups.ValidationError | `Validate.hs` `ValidationError` and `Either` sequencing | The ten errors short-circuit in the same modeled order. |
| R-23 | FAITHFUL | lean:KelGroups.validateProposal | `Validate.hs` `checkRole`, `checkRemoval` | Admin roles and unknown application role names bypass predicates. |
| R-24 | FAITHFUL | lean:KelGroups.foldGroup | `Fold.hs` `applyEvent` | The total fold never calls validation. |
| VI-1 | FAITHFUL | lean:KelGroups.approvals_nodup | `State.hs` `Set Text` | Pending approval cardinality is duplicate-free. |
| VI-2 | FAITHFUL | lean:KelGroups.proposer_mem_approvals | `Fold.hs` singleton proposer approval | Every well-formed pending proposal contains its proposer. |
| VI-3 | FAITHFUL | lean:KelGroups.enact_implies_threshold_met | `Fold.hs` `tryEnact` guard | The production detailed fold step emits enactment provenance only from the successful branch, and the theorem derives the approval bound against that recorded pre-enactment state. No-op payloads are included. |
| VI-4 | FAITHFUL | lean:KelGroups.members_change_implies_enacted | `Fold.hs` `applyEvent` | A fold-step membership change has an enactment witness. |
| VI-5 | FAITHFUL | lean:KelGroups.member_key_coherent | `Fold.hs` `IntroduceMember` | Member keys remain coherent under modeled transitions. |
| VI-6 | FAITHFUL | lean:KelGroups.stalePendingWitness | `Fold.hs` deletion of only the enacted proposal | Executed production fold over an event list validated at every intermediate state: lowering admin count leaves a sibling at or above the new threshold. |
| VI-7 | FAITHFUL | lean:KelGroups.bootstrapNonMemberWitness | `Validate.hs` bootstrap signer omission | Executed production fold over a validated bootstrap event; its detailed step records the non-member proposer approval that immediately enacted and disappeared from pending. |
| EP-DIGEST | DIVERGENT | lean:KelGroups.applyPropose | `Fold.hs` `proposalDigest` | The model threads an abstract digest. The later port must supply Blake2b SAID and prove injectivity wherever a theorem requires it. |
| EP-CESR | DIVERGENT | lean:KelGroups.validateEvent | `Validate.hs` `requireValidCesrKey` | The model threads a Boolean predicate. The later port must connect it to CESR Ed25519 decoding. |
| EP-ROLE-PRED | FAITHFUL | lean:KelGroups.RoleDef | `Types.hs` `RoleDef` | Addition/removal predicates remain abstract functions of the application fold. |
| EP-LAST-ADMIN | EXTENSION | lean:KelGroups.authMode | `Bootstrap.hs` `authMode`; `Validate.hs` admin bypass | No prevention is invented. Removing the last admin returns the model to bootstrap, and the later port must retain or separately rule that behavior. |
| EP-REDUNDANT-LOOKUP | EXTENSION | lean:KelGroups.validateProposal | `Validate.hs` `validateRoleChanges` | The redundant second member lookup is retained; its error branch remains unreachable after the first check. |
| EP-DENY | EXTENSION | lean:KelGroups.BaseEvent | `Event.hs` `BaseEvent` | There is no dissent, rejection, expiry, or withdrawal event. Slice 2 cannot derive a deny verdict until this gap is ruled. |
| R-25, R-26 | FAITHFUL | lean:KelGroups.Enacts | This matrix | Status is scoped to the model and every ruled abstraction names its port consequence. |
| R-27, R-28 | FAITHFUL | lean:KelGroups.majority_table | Lean proof declarations | The model has no custom axioms, `sorry`, or `native_decide`; gate evidence prints each named theorem's axiom set. |
| R-29 | FAITHFUL | lean:KelGroups.Tests.proposerAutoApproval | `Fold.hs` point behavior | Forty elaboration-time guards are imported by the default target and fail the build when false. |

## Named gaps

The most important absent behavior is EP-DENY: the source vocabulary contains
only propose and approve. This model therefore stops before permission-verdict
composition. EP-DIGEST and EP-CESR are explicit parameters, not axioms;
EP-LAST-ADMIN and EP-REDUNDANT-LOOKUP retain existing behavior rather than
repairing it.

The proposed `CI-54-BOOTSTRAP-NO-PENDING` invariant is false. The executed
lean:KelGroups.bootstrapPendingWitness first reaches the VI-6 stale sibling,
then uses validated proposals to remove the remaining admins. The model returns
to bootstrap mode with that sibling still pending.
