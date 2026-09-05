# Functions model — #68 (A-001 reading)

New/changed signatures only: explicit argument names, argument/result types,
signature-level constraints. No bodies, no control flow.

- `applyProposeDetailed (digest : Proposal → ProposalId) (gs : GroupState α) (signer : Key) (proposal : Proposal) : StepResult α`
  — constraint: resulting pending entry carries `approvals = []`.
- `applyPropose (digest) (gs) (signer) (proposal) : GroupState α` — same.
- `validateApproval (gs : GroupState α) (signer : Key) (proposalId : ProposalId) : Except ValidationError Unit`
  — new refusal: `signer = pending.proposer` while `adminCount gs > 1`.
- `validateBaseApproval (gs) (signer) (proposalId) : Except ValidationError Unit`
  — same new refusal on the integrated path.
- `tryEnactDetailed (gs) (proposalId) : StepResult α`,
  `tryEnactBase (integration) (gs) (proposalId)` — unchanged threshold
  reading (`approvals.length ≥ majority gs`); behavior changes only via the
  emptied input.
- `proposer_mem_approvals` family — REPLACED by the ruled pair:
  non-membership above n=1 + sole-admin exception; same binders otherwise.
- Worker `#guard` theorems in `Tests.lean` (exact names owned by the worker,
  coverage mandated by the packet): empty-open both paths, n=1 two-step both
  paths, n=2 unilateral-pends + other-enacts, n=3 killer + two-others, n=5
  two-pend/three-enact, admin-count-change, refusal-distinctness both
  validation functions.
- Ticket-owner oracle `witness-t68.lean` (13 guards, fixed): the independent
  executable specification; worker must satisfy it, never edit it.
