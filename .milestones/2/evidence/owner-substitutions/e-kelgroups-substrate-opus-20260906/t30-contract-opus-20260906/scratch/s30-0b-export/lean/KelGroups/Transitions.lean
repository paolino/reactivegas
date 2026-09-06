/-
  KelGroups.Transitions — State transition functions

  Lean counterpart of the Haskell `Fold.hs`. Defines how
  base events modify the group state: enact proposals,
  manage pending approvals, fold an event list.
-/
import KelGroups.Basic

namespace KelGroups

-- ============================================================
-- Helper functions for member list operations
-- ============================================================

/-- Remove the first member matching the given id. -/
def eraseMember (mid : MemberId) (members : List Member)
    : List Member :=
  members.eraseP (fun m => m.id == mid)

/-- Update roles of the first member matching the given id. -/
def updateMemberRoles
    (mid : MemberId)
    (roles : List Role)
    (members : List Member)
    : List Member :=
  members.map fun m =>
    if m.id == mid then { m with roles := roles } else m

-- ============================================================
-- enact — apply a proposal to the member list
-- ============================================================

/-- Enact a proposal by modifying the member list.
Corresponds to Haskell `enact`. -/
def enact (gs : GroupState) (p : Proposal) : GroupState :=
  match p with
  | .introduceMember mid roles =>
    { gs with members := ⟨mid, roles⟩ :: gs.members }
  | .removeMember mid =>
    { gs with members := eraseMember mid gs.members }
  | .changeRoles mid roles =>
    { gs with members := updateMemberRoles mid roles gs.members }

-- ============================================================
-- tryEnact — check majority threshold and enact if met
-- ============================================================

/-- Try to enact a pending proposal if it has reached admin
majority. Majority is computed on the pre-enactment state,
but the proposal is removed from the post-enactment state. -/
def tryEnact (gs : GroupState) (pid : Nat) : GroupState :=
  match gs.pendingProposals.lookup pid with
  | none => gs
  | some pp =>
    if pp.approvals.length >= majority (adminCount gs) then
      let gs' := enact gs pp.proposal
      { gs' with pendingProposals :=
          gs'.pendingProposals.eraseP (fun p => p.1 == pid) }
    else gs

-- ============================================================
-- applyPropose / applyApprove — event handlers
-- ============================================================

/-- Apply a propose event. The proposer auto-approves.
The pid is passed explicitly (abstracting over the hash). -/
def applyPropose
    (gs : GroupState)
    (signer : MemberId)
    (prop : Proposal)
    (pid : Nat)
    : GroupState :=
  let pp : PendingProposal :=
    { proposal := prop
    , proposer := signer
    , approvals := [signer]
    }
  let gs' := { gs with
    pendingProposals := (pid, pp) :: gs.pendingProposals }
  tryEnact gs' pid

/-- Apply an approve event. Adds signer to approval list,
then tries to enact. -/
def applyApprove
    (gs : GroupState)
    (signer : MemberId)
    (pid : Nat)
    : GroupState :=
  match gs.pendingProposals.lookup pid with
  | none => gs
  | some pp =>
    let pp' := { pp with approvals := signer :: pp.approvals }
    let gs' := { gs with pendingProposals :=
      gs.pendingProposals.map fun p =>
        if p.1 == pid then (pid, pp') else p }
    tryEnact gs' pid

-- ============================================================
-- applyBase / applyEvent / foldGroup — top-level fold
-- ============================================================

/-- Apply a single base event. -/
def applyBase (gs : GroupState) (evt : BaseEvent) : GroupState :=
  match evt with
  | .propose signer pid prop => applyPropose gs signer prop pid
  | .approve signer pid => applyApprove gs signer pid

/-- Apply a group event. App events are no-ops in the base
formalization. -/
def applyEvent {α : Type} (gs : GroupState) (evt : GroupEvent α) : GroupState :=
  match evt with
  | .base b => applyBase gs b
  | .app _ => gs

/-- Fold a list of events starting from empty state. -/
def foldGroup {α : Type} (events : List (GroupEvent α)) : GroupState :=
  events.foldl applyEvent emptyState

end KelGroups
