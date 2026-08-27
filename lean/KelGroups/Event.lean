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

end KelGroups
