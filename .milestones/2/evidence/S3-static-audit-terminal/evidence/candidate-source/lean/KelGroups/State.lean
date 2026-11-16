import KelGroups.Event

namespace KelGroups

structure PendingProposal where
  proposal : Proposal
  proposer : Key
  approvals : List Key
deriving DecidableEq, BEq, Repr

/-- A voted base mutation awaiting its approval threshold.  This is the
**integrated** pending store: its payload is `BaseMutation`, which cannot
express admission, so nothing here can ever insert a member (R62-06,
INV-62-DIRECT-ONLY).

`PendingProposal` above is the historical store, read only by
`KelGroups.Fold` and the accepted `baseEnacted_threshold_met` evidence.  The
Reactivegas production root does not reach it. -/
structure PendingBase where
  mutation : BaseMutation
  proposer : Key
  approvals : List Key
deriving DecidableEq, BEq, Repr

structure GroupState (α : Type) where
  members : List (Key × Member)
  pendingProposals : List (ProposalId × PendingProposal)
  pendingBase : List (ProposalId × PendingBase)
  appFold : α
deriving DecidableEq, BEq, Repr

variable {α : Type}

def emptyState (initial : α) : GroupState α :=
  { members := [], pendingProposals := [], pendingBase := [], appFold := initial }

def lookupMember (key : Key) (gs : GroupState α) : Option Member :=
  assocLookup key gs.members

def lookupPending (proposalId : ProposalId) (gs : GroupState α) : Option PendingProposal :=
  assocLookup proposalId gs.pendingProposals

/-- The voted base mutation pending under this id, if any. -/
def lookupPendingBase (proposalId : ProposalId) (gs : GroupState α) : Option PendingBase :=
  assocLookup proposalId gs.pendingBase
def adminCount (gs : GroupState α) : Nat :=
  gs.members.foldl
    (fun count entry => if hasAdmin entry.2.roles then count + 1 else count) 0

def majority (gs : GroupState α) : Nat := (adminCount gs + 1) / 2

def isAdmin (pubKey : Key) (gs : GroupState α) : Bool :=
  match lookupMember pubKey gs with
  | some member => hasAdmin member.roles
  | none => false

def isMember (pubKey : Key) (gs : GroupState α) : Bool :=
  (lookupMember pubKey gs).isSome

inductive AuthMode where
  | bootstrap
  | normal
deriving DecidableEq, BEq, Repr

def authMode (gs : GroupState α) : AuthMode :=
  if adminCount gs == 0 then .bootstrap else .normal

/-- **The canonical projection.** The sole route from the writable aggregate to
the read-only view every app fold, base hook and vote observation consumes. It
copies nothing that can be written back: `GroupView` has no path to a
`GroupState`. -/
def groupView (gs : GroupState α) : GroupView := { members := gs.members }

end KelGroups
