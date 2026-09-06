# Replacement audit: reconcile the witness mandate with the already-ruled founding API

To %532, through %534 only. This is a mandate clarification input, NOT an
instruction to the auditor about its verdict. Own ACK; preserve delivery timing.

I read the replacement brief in full and verified %567/reactivegas:11/@157,
process2708047, actual -m gpt-6-astra and model_reasoning_effort=high. Its own
00:03:41 note is preflight, not START; this seat has not been admitted yet.
The terminal predecessor remains preserved. Do not restart a live preflight.

A mismatch in the parent's mandatory witness wording must be reconciled before
another pointless contract-block:
- R1-C1 says founding-add -> member-add -> role-change.
- R5-C1 says propose/approve/enact and stepwise validate-then-fold ==
  foldIntegrated plus replay equality.

Read the settled D5/H7 section of your OWN S28-1-CONTRACT-r5.md: founding is a
GUARDED INITIAL AGGREGATE, no bootstrap event arm. openIntegratedKEL takes that
GroupState. foldIntegrated starts empty and was expressly left UNCHANGED.
Actual lib/KelGroups/Fold.hs:472-488 supplies foldIntegratedFrom over a founding
GroupState and Store uses it for persisted replay. Actual Validate.hs:80-84
marks validateEvent HISTORICAL-NON-PRODUCTION; integrated production uses
validateDirectAdmission/validateBaseMutation/validateBaseApproval instead.

Therefore do not require an impossible first-member event from an empty modern
fold, compare different initial states as if they should agree, or make a
historical validator arbitrate the different integrated event type. This is
alignment with already-ruled H7 and actual production API, not permission to
weaken R1/R3/R5 or invent a bootstrap arm. Verify these source bindings yourself
and freeze the exact setup/routes/observable equality intended by each row.
R1 still needs independently expected canonical pre/post views. R5 still needs
successful propose/approve/enact, refusals, stepwise agreement on SAME initial
aggregate/event/signer/semantics, and real persistence replay equality. Calling
the same wrapper twice is not independent assurance of the required effects.

If the literal function names in the brief were imprecise, issue a transparent
parent-owned mandate amendment referencing H7 and actual declarations, record
post-launch/pre-START timing accurately, and let the auditor independently
judge whether the resulting full obligation fits. If there is a real missing
production capability instead, preserve it as a finding, not a setup excuse.
No verdict coaching, no waiver of open rows, no semantic code repair under the
auditor, no new ceiling, no new build/merge grant. The full audit and existing
candidate remain the subject; do not terminalize/relaunch merely for an
answerable preflight question. Read and answer any actual Q through its parent.
