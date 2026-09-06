# Campaign ledger — reactivegas #54 slice 1, auditor s1

builds_budget: 12
builds_spent: 5
stopped: none
state: open

| ID | Severity | Verdict | Row state | Killer / note |
|---|---|---|---|---|
| R-1 | BLOCKING | PASS | OPEN | root + imports; G2 existence in gate log |
| R-2 | BLOCKING | PASS | KILLED | gate G1 + G1b injected `import Reactivegas.Types` made `just lean` exit 1 |
| R-2b | BLOCKING | PASS | KILLED | same wiring control; tracked checker invoked before `lake build` |
| R-3 | BLOCKING | PASS | OPEN | legal-direction count 0, not rejected |
| R-4 | BLOCKING | PASS | KILLED | G2b: clean copy built, then type error in Types.lean made `lake build` exit 1 |
| R-5 | BLOCKING | PASS | OPEN | no `Reactivegas.*` in KelGroups types/theorems |
| R-6 | BLOCKING | PASS | OPEN | `proposerAutoApproval` |
| R-7 | BLOCKING | PASS | OPEN | `proposeReplacesPending` (5 admins so replace is observable) |
| R-8 | BLOCKING | PASS | OPEN | propose/approve call `tryEnact`; no other callers of `finishEnact` |
| R-9 | BLOCKING | PASS | OPEN | `majority_table` 0..5 + `majority_not_strict_on_even` via `omega` |
| R-10 | BLOCKING | PASS | OPEN | `zeroAdminsEnactImmediately` |
| R-11 | BLOCKING | FAIL | OPEN | code uses pre-state `majority gs`; shipped tests survive post-state swap |
| R-12 | BLOCKING | PASS | OPEN | `approveUnknownIsNoOp` |
| R-13 | BLOCKING | PASS | OPEN | `duplicateApprovalIsIdempotent`; re-attempt is structural |
| R-14 | BLOCKING | PASS | OPEN | `enactDeletesOnlySelected` |
| R-15 | BLOCKING | PASS | OPEN | overwrite + two no-ops guarded |
| R-16 | BLOCKING | PASS | OPEN | `member_key_coherent` |
| R-17 | BLOCKING | PASS | OPEN | `applicationSignerDiscarded` |
| R-18 | BLOCKING | PASS | OPEN | `bootstrapAcceptsUninspectedSigner` |
| R-19 | BLOCKING | PASS | OPEN | current-admin accept/reject + payload order |
| R-20 | BLOCKING | PASS | OPEN | approval errors distinct |
| R-21 | BLOCKING | PASS | OPEN | `errorNotAMember` |
| R-22 | BLOCKING | PASS | OPEN | ten constructors; bootstrap invalid-key-first |
| R-23 | BLOCKING | PASS | OPEN | admin bypass + unknown app role |
| R-24 | BLOCKING | PASS | OPEN | `foldDoesNotValidate` |
| R-25 | BLOCKING | PASS | OPEN | 29 `lean:` anchors exist; G5 checked them |
| R-26 | BLOCKING | PASS | OPEN | no end-to-end enforcement claim |
| R-27 | BLOCKING | PASS | OPEN | no `sorry` / `axiom` |
| R-28 | BLOCKING | PASS | OPEN | live `Classical.em` control; axiom sets ⊆ {propext, Classical.choice, Quot.sound}; no `native_decide` |
| R-29 | BLOCKING | PASS | KILLED | `#guard false` in Tests.lean made `lake build` exit 1 |
| VI-1 | BLOCKING | PASS | OPEN | `approvals_nodup` from `WellFormed` preservation |
| VI-2 | BLOCKING | PASS | OPEN | `proposer_mem_approvals` |
| VI-3 | BLOCKING | FAIL | OPEN | theorem is a destructor of `Enacts`, not a fold-step property |
| VI-4 | BLOCKING | PASS | OPEN | `members_change_implies_enacted` |
| VI-5 | BLOCKING | PASS | OPEN | `member_key_coherent` |
| VI-6 | BLOCKING | PASS | OPEN | `stalePendingWitness` is a real 3-admin removal trace; `#guard` requires ≥ current threshold |
| VI-7 | BLOCKING | FAIL | OPEN | `bootstrapNonMemberWitness` is a record literal, not a fold trace |
| EP-DENY | BLOCKING | PASS | OPEN | `BaseEvent` = propose \| approve |
| EP-DIGEST | BLOCKING | PASS | OPEN | `digest` is a parameter; no injectivity axiom |
| EP-CESR | BLOCKING | PASS | OPEN | `validKey` is a parameter; no CESR axiom |
| EP-LAST-ADMIN | BLOCKING | PASS | OPEN | `authMode` only; no prevention |
| EP-ROLE-PRED | BLOCKING | PASS | OPEN | abstract `RoleDef` |
| EP-REDUNDANT-LOOKUP | BLOCKING | PASS | OPEN | second `lookupMember` retained in `validateRoleChanges` |
