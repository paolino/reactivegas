import KelGroups.State

namespace KelGroups

abbrev AppFold (α : Type) := α → α → α

variable {α : Type}

def enact (gs : GroupState α) (proposal : Proposal) : GroupState α :=
  match proposal with
  | .introduceMember key email roles =>
      { gs with members := assocInsert key { key, email, roles } gs.members }
  | .removeMember key =>
      { gs with members := assocErase key gs.members }
  | .changeRoles key roles =>
      { gs with members := assocAdjust key (fun member => { member with roles }) gs.members }

def finishEnact (gs : GroupState α) (proposalId : ProposalId)
    (pending : PendingProposal) : GroupState α :=
  let enacted := enact gs pending.proposal
  { enacted with pendingProposals := assocErase proposalId enacted.pendingProposals }

structure Enactment (α : Type) where
  proposalId : ProposalId
  pending : PendingProposal
  preState : GroupState α
deriving DecidableEq, BEq, Repr

structure StepResult (α : Type) where
  state : GroupState α
  enactment : Option (Enactment α)
deriving DecidableEq, BEq, Repr

def tryEnactDetailed (gs : GroupState α) (proposalId : ProposalId) : StepResult α :=
  match lookupPending proposalId gs with
  | none => { state := gs, enactment := none }
  | some pending =>
      if pending.approvals.length ≥ majority gs then
        { state := finishEnact gs proposalId pending
          enactment := some { proposalId, pending, preState := gs } }
      else { state := gs, enactment := none }

def tryEnact (gs : GroupState α) (proposalId : ProposalId) : GroupState α :=
  (tryEnactDetailed gs proposalId).state

def applyProposeDetailed (digest : Proposal → ProposalId) (gs : GroupState α)
    (signer : Key) (proposal : Proposal) : StepResult α :=
  let proposalId := digest proposal
  let pending : PendingProposal := { proposal, proposer := signer, approvals := [signer] }
  let proposed := { gs with
    pendingProposals := assocInsert proposalId pending gs.pendingProposals }
  tryEnactDetailed proposed proposalId

def applyPropose (digest : Proposal → ProposalId) (gs : GroupState α)
    (signer : Key) (proposal : Proposal) : GroupState α :=
  (applyProposeDetailed digest gs signer proposal).state

def approvePending (signer : Key) (pending : PendingProposal) : PendingProposal :=
  { pending with approvals := setInsert signer pending.approvals }

def applyApproveDetailed (gs : GroupState α) (signer : Key)
    (proposalId : ProposalId) : StepResult α :=
  match lookupPending proposalId gs with
  | none => { state := gs, enactment := none }
  | some pending =>
      let approved := approvePending signer pending
      let updated := { gs with
        pendingProposals := assocInsert proposalId approved gs.pendingProposals }
      tryEnactDetailed updated proposalId

def applyApprove (gs : GroupState α) (signer : Key)
    (proposalId : ProposalId) : GroupState α :=
  (applyApproveDetailed gs signer proposalId).state

def applyEventDetailed (digest : Proposal → ProposalId) (appFoldFn : AppFold α)
    (gs : GroupState α) (signer : Key) (event : GroupEvent α) : StepResult α :=
  match event with
  | .base (.propose proposal) => applyProposeDetailed digest gs signer proposal
  | .base (.approve proposalId) => applyApproveDetailed gs signer proposalId
  | .app appEvent =>
      { state := { gs with appFold := appFoldFn gs.appFold appEvent }
        enactment := none }

def applyEvent (digest : Proposal → ProposalId) (appFoldFn : AppFold α)
    (gs : GroupState α) (signer : Key) (event : GroupEvent α) : GroupState α :=
  (applyEventDetailed digest appFoldFn gs signer event).state

def foldGroup (digest : Proposal → ProposalId) (appFoldFn : AppFold α)
    (initial : α) (events : List (Key × GroupEvent α)) : GroupState α :=
  events.foldl
    (fun gs signed => applyEvent digest appFoldFn gs signed.1 signed.2)
    (emptyState initial)

end KelGroups
