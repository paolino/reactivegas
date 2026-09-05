# Spec — #68 proposer signature is not an assent (V-2)

Base: origin/master e6c59242. Ruling: questions/A-V2-AND-PLEDGE-AGENCY.md (V-2)
plus issue #68 body. Semantics Q-001 answered as: <pending — gate v2 binds it>.

## Requirements

- R68-01 — `.propose` (integrated, `pendingBase`) and `applyProposeDetailed`
  (historical, `pendingProposals`) open with EMPTY approvals. No `[signer]`.
- R68-02 — Enactment consumes only recorded assents against the UNCHANGED
  `majority = (adminCount+1)/2`. No credit for the propose event itself.
- R68-03 — Arithmetic theorems byte-identical in meaning: `majority_table`
  (0,1,1,2,2,3), `majority_not_strict_on_even`, `majorityZero/One/Two`.
- R68-04 — Above n=1, no enactment without a non-proposer assent (mechanism
  per A-001: approval-route bar with sole-admin exception, or enactment
  guard — desk rules).
- R68-05 — n=1 preserved: the sole founder carries a decision alone (two-step
  propose+self-approve per recommendation, or as ruled in A-001).
- R68-06 — Well-formedness restated: pending entries keep `Nodup` approvals;
  `proposer ∈ approvals` is REPLACED by the ruled regime (bar or guard +
  exception), proved preserved by every transition including approve/propose/
  enact on BOTH paths.
- R68-07 — Every theorem depending on the old regime restated and re-proved:
  `proposer_mem_approvals` family (both namespaces), threshold-met evidence
  (`tryEnactDetailed_enactment_threshold_met`, `baseEnacted_threshold_met`
  family), integrated fold theorems, Reactivegas wrappers; six-inversion
  repair owned by #66 S1 is consumed, not relitigated.
- R68-08 — Reachable executable witnesses, wired so they RUN (not merely
  defined): positive — n=2 Anna-proposes-demote-Bruno stays pending with zero
  further assents; negative — n=2 proposer+other-approve enacts; n=1 founder
  propose+self-approve enacts; self-approval above n=1 refused.
- R68-09 — Mutation control: a mutant restoring `approvals := [signer]` (each
  path) REDs the frozen gate for the intended semantic reason; candidate tree
  left clean.

## Rejection behavior (unchanged unless ruled)

Non-admin propose/approve refused; duplicate approval refused
(`alreadyApproved`); unknown proposal refused; direct admission stays the only
admission route; sealed hook and economic semantics untouched.

## Non-goals

#69 pledge sovereignty; vote-machine threshold default; assenso-composition;
docs/en/design/ (handoff only); unrelated quality repair; anticipation in any
other lane (simulator oracle pin stays verbatim until this lands).
