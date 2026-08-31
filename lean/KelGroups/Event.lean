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

/-- The **voted** base vocabulary (R62-07, INV-62-DIRECT-ONLY): removal and
role change, and nothing else.  Admission is not representable here, so no
pending approval — in any aggregate, however it was supplied — can enact one.
That is a structural exclusion rather than a guard: adding an admission
constructor stops the exhaustive enactment matching compiling.

It is deliberately *not* the historical `Proposal`, which still carries
`introduceMember` for the accepted #54 evidence. -/
inductive BaseMutation where
  | removeMember (key : Key)
  | changeRoles (key : Key) (roles : List Role)
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
