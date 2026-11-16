/-
  KelGroups.Validate — Validation functions for group events

  Formalizes the pure validation logic for base-system
  checks. Application-level role preconditions (GroupConfig)
  are not modeled — only membership, admin status, and
  bootstrap constraints.
-/
import KelGroups.Basic

namespace KelGroups

/-- Validation errors for group events. -/
inductive ValidationError where
  | notAMember : MemberId → ValidationError
  | notAnAdmin : MemberId → ValidationError
  | bootstrapRequiresAdmin : ValidationError
  | memberAlreadyExists : MemberId → ValidationError
  | memberNotFound : MemberId → ValidationError
  | proposalNotFound : Nat → ValidationError
  | alreadyApproved : MemberId → Nat → ValidationError
  deriving DecidableEq, Repr

/-- Validate a proposal during bootstrap mode.
Only IntroduceMember with an admin role is accepted. -/
def validateBootstrapProposal (p : Proposal)
    : Option ValidationError :=
  match p with
  | .introduceMember _ roles =>
    if hasAdmin roles then none
    else some .bootstrapRequiresAdmin
  | .removeMember _ => some .bootstrapRequiresAdmin
  | .changeRoles _ _ => some .bootstrapRequiresAdmin

/-- Validate a proposal during normal mode (base checks).
Requires signer to be admin, checks member existence. -/
def validateNormalProposal
    (gs : GroupState) (signer : MemberId) (p : Proposal)
    : Option ValidationError :=
  if ¬ isAdmin signer gs then some (.notAnAdmin signer)
  else match p with
  | .introduceMember mid _ =>
    if isMember mid gs then some (.memberAlreadyExists mid)
    else none
  | .removeMember mid =>
    if ¬ isMember mid gs then some (.memberNotFound mid)
    else none
  | .changeRoles mid _ =>
    if ¬ isMember mid gs then some (.memberNotFound mid)
    else none

/-- Validate a proposal against current state. Dispatches
on authentication mode: bootstrap or normal. -/
def validateProposal
    (gs : GroupState) (signer : MemberId) (p : Proposal)
    : Option ValidationError :=
  match authMode gs with
  | .bootstrap => validateBootstrapProposal p
  | .normal => validateNormalProposal gs signer p

/-- Validate an approval against current state.
Requires admin, proposal must exist, no double approval. -/
def validateApproval
    (gs : GroupState) (signer : MemberId) (pid : Nat)
    : Option ValidationError :=
  if ¬ isAdmin signer gs then some (.notAnAdmin signer)
  else match gs.pendingProposals.lookup pid with
  | none => some (.proposalNotFound pid)
  | some pp =>
    if pp.approvals.any (· == signer)
    then some (.alreadyApproved signer pid)
    else none

end KelGroups
