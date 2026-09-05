# Spec — #68 proposer signature is not an assent (V-2), as ruled in A-001

Base: origin/master e6c59242. Ruling: questions/A-V2-AND-PLEDGE-AGENCY.md (V-2),
issue #68 body, answers/A-001-proposer-approval-and-n1-semantics.md (binding
interpretation). Desk chose option A; B is killed by counterexample and kept as
a negative witness; C leaves the n=2 defect.

## Requirements

- R68-01 — Proposal creation records ZERO approvals on both paths:
  `applyProposeDetailed` (historical `pendingProposals`) and `.propose`
  (integrated `pendingBase`). Never auto-enact by crediting the proposal
  signature, including for one admin.
- R68-02 — Enactment consumes only recorded assents against the UNCHANGED
  `majority = (adminCount+1)/2` computed from canonical membership at
  enactment time (admin-count changes between propose and enact move the
  threshold).
- R68-03 — Arithmetic theorems byte-identical in meaning: `majority_table`
  (0,1,1,2,2,3), `majority_not_strict_on_even`, `majorityZero/One/Two`.
- R68-04 — With more than one current admin, the proposer cannot supply a
  counted assent on their own proposal. The self-approval is REFUSED AT THE
  BOUNDARY: historical `validateApproval` and integrated
  `validateBaseApproval` (hence `applyIntegratedEvent`). Every counted
  approval is someone else's. The refusal carries a NEW error variant derived
  in-ticket; it MUST NOT be labelled `alreadyApproved` (different meaning:
  first approval by that signer, barred by proposer identity, not duplication).
- R68-05 — With exactly one current admin, a SEPARATE explicit approval by
  that sole admin enacts (propose then approve). No threshold special case,
  no auto-assent. "Preserved" = the founder still acts alone, not one-event
  enactment (incompatible with "opens at zero").
- R68-06 — Enactment sets: n=2 needs 1 OTHER assent; n=3 needs 2 OTHER
  assents; n=5 needs 3 OTHER assents. Proposing is the proposer's
  participation; assents come from the others. In particular n=3
  proposer+1-other stays PENDING (this is the B-counterexample, kept as a
  negative witness: length=2 with a non-proposer present must NOT enact).
- R68-07 — Well-formedness restated: `Nodup` approvals kept;
  `proposer ∈ approvals` REPLACED by: proposer ∉ approvals whenever
  adminCount > 1 at the approving transition; sole-admin self-approval
  permitted. BOUNDARY QUALIFICATION (F-01 resolution under A-001, desk
  NOTE-008): preservation holds for every boundary-admitted transition —
  validated historical traces (`TraceAdmissible`) and successful
  integrated transitions. The validation-free raw fold preserves the
  structural guarantees unconditionally (member/pending key uniqueness,
  member-key coherence, approval Nodup where applicable,
  enactment-threshold evidence shape, app-payload isolation,
  duplicate-approval idempotence — each by named theorem + step/fold
  induction cited in the repair receipt; any claim failing proof is
  exhibited and reported, never narrowed away). The count-indexed
  governance predicate is conditional on the raw domain. The excluded
  raw-self-approval class is exhibited by the retained 7-event witness
  (instrument sha 3b4229fc, run sha 0a2799b7) and refused at the boundary
  (`proposerSelfApproval`). No originally promised reachable guarantee is
  otherwise withdrawn.
- R68-08 — Every theorem depending on the old regime restated and re-proved:
  `proposer_mem_approvals` family (both namespaces) becomes the ruled
  non-membership/exception pair; threshold-met evidence
  (`tryEnactDetailed_enactment_threshold_met`, `baseEnacted_threshold_met`
  family); fold preservation proofs; Reactivegas wrappers; #66 S1 inversion
  repair consumed, not relitigated.
- R68-09 — Reachable executable witnesses at reachable-call level (folds and
  `applyIntegratedEvent`, not helpers alone), wired so they RUN:
  - positive: n=2 unilateral propose stays pending; n=3 proposer+1-other
    stays pending (B-killer); n=5 two-others stays pending;
  - negative (correct behavior): n=2 other-approval enacts; n=3 two-others
    enact; n=5 three-others enact; n=1 propose pends then sole-admin
    self-approve enacts; self-approval above n=1 refused with a
    non-`alreadyApproved` error;
  - admin-count change: propose at n=2, admit third admin via direct route,
    one other-approval still pending (threshold now 2), second other-approval
    enacts.
- R68-10 — Mutation control: a mutant restoring `approvals := [signer]`
  (EACH path) REDs the frozen gate for the intended semantic reason, proved
  through the REAL INTEGRATED path; candidate tree left clean.

## Rejection behavior (unchanged unless ruled)

Non-admin propose/approve refused; duplicate approval refused
(`alreadyApproved`, meaning intact); unknown proposal refused; direct
admission stays the only admission route; sealed hook runs on every committed
base change; economic semantics untouched.

## Non-goals

#69 pledge sovereignty; vote-machine threshold default (no new theta);
assenso-composition; docs/en/design/ (cited handoff only); unrelated quality
repair; any anticipation by the simulator before this slice lands.
