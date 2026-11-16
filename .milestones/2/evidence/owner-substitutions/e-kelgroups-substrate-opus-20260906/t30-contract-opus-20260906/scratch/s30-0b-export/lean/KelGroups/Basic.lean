/-
  KelGroups.Basic — Core types for KEL-based group management

  Formalizes the base types: roles, members, group state,
  events, and the fold function.
-/

namespace KelGroups

/-- A member identifier (abstract, stands for a public key). -/
abbrev MemberId := Nat

/-- Admin visibility: public admins expose their email
to non-members, private admins do not. -/
inductive Admin where
  | publicAdmin : Admin
  | privateAdmin : Admin
  deriving DecidableEq, Repr

/-- A role is either an admin role (public or private)
or an application-defined role identified by name. -/
inductive Role where
  | adminRole : Admin → Role
  | appRole : String → Role
  deriving DecidableEq, Repr

/-- A member with their identifier and set of roles. -/
structure Member where
  id : MemberId
  roles : List Role
  deriving Repr

/-- Check if a role list contains any admin role. -/
def hasAdmin (roles : List Role) : Bool :=
  roles.any fun r => match r with
    | .adminRole _ => true
    | _ => false

/-- A proposal for a group change. -/
inductive Proposal where
  | introduceMember : MemberId → List Role → Proposal
  | removeMember : MemberId → Proposal
  | changeRoles : MemberId → List Role → Proposal
  deriving Repr

/-- A pending proposal with its approvals. -/
structure PendingProposal where
  proposal : Proposal
  proposer : MemberId
  approvals : List MemberId
  deriving Repr

/-- Base events for group management. -/
inductive BaseEvent where
  | propose : MemberId → Nat → Proposal → BaseEvent
  | approve : MemberId → Nat → BaseEvent
  deriving Repr

/-- A group event: base or application. -/
inductive GroupEvent (α : Type) where
  | base : BaseEvent → GroupEvent α
  | app : α → GroupEvent α
  deriving Repr

/-- The group condition, derived from folding the KEL. -/
structure GroupState where
  members : List Member
  pendingProposals : List (Nat × PendingProposal)
  deriving Repr

/-- Empty group state. -/
def emptyState : GroupState :=
  { members := [], pendingProposals := [] }

/-- Count admins in the group. -/
def adminCount (gs : GroupState) : Nat :=
  gs.members.filter (fun m => hasAdmin m.roles) |>.length

/-- Compute required majority: ⌈n/2⌉. -/
def majority (n : Nat) : Nat :=
  (n + 1) / 2

/-- Authentication mode. -/
inductive AuthMode where
  | bootstrap : AuthMode
  | normal : AuthMode
  deriving DecidableEq, Repr

/-- Determine auth mode from group state. -/
def authMode (gs : GroupState) : AuthMode :=
  if adminCount gs == 0 then .bootstrap else .normal

/-- Check if a member id is an admin. -/
def isAdmin (mid : MemberId) (gs : GroupState) : Bool :=
  match gs.members.find? (fun m => m.id == mid) with
  | some m => hasAdmin m.roles
  | none => false

/-- Check if a member id exists. -/
def isMember (mid : MemberId) (gs : GroupState) : Bool :=
  gs.members.any (fun m => m.id == mid)

end KelGroups
