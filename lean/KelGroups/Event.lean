import KelGroups.Types

namespace KelGroups

inductive Proposal where
  | introduceMember (key : Key) (email : Email) (roles : List Role)
  | removeMember (key : Key)
  | changeRoles (key : Key) (roles : List Role)
deriving DecidableEq, BEq, Repr

inductive BaseEvent where
  | propose (proposal : Proposal)
  | approve (proposalId : ProposalId)
deriving DecidableEq, BEq, Repr

inductive GroupEvent (α : Type) where
  | base (event : BaseEvent)
  | app (event : α)
deriving DecidableEq, BEq, Repr

/-- The direct base-command vocabulary (R62-06): exactly one constructor, member
admission.  The signer is supplied separately by the integrated transition, so
no command carries a second author identity. Its admission *semantics* are
T6220 in S62-B; this slice fixes only the vocabulary. -/
inductive DirectCommand where
  | admitMember (key : Key) (email : Email) (roles : List Role)
deriving DecidableEq, BEq, Repr

/-- The observable base-change vocabulary (R62-09, R62-12).  A committed
substrate membership or role effect is exactly one of these three, each naming
the affected key, so an exhaustive post-base hook cannot ignore a future
substrate membership effect: adding a fourth stops the hook compiling. -/
inductive BaseChange where
  | memberAdmitted (key : Key)
  | memberRemoved (key : Key)
  | rolesChanged (key : Key)
deriving DecidableEq, BEq, Repr

end KelGroups
