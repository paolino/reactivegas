import KelGroups.Vote.Types

/-!
# Required vote machine — event vocabulary

The events the required machine folds. The signer is supplied separately by
the fold, matching Slice 1's `(Key × GroupEvent α)` shape. `admitMember`
carries no question id and no threshold — that absence is R-66: admitting a
member is a plain state event, never a question and never a vote.
-/

namespace KelGroups.Vote

inductive VoteEvent where
  | openQuestion (questionId : QuestionId) (kind : QuestionKind)
  | cast (questionId : QuestionId) (ballot : Ballot)
  | renounce (questionId : QuestionId)
  | admitMember (key : Key) (email : Email) (roles : List Role)
  | removeMember (key : Key)
  | setRoles (key : Key) (roles : List Role)
deriving DecidableEq, BEq, Repr

end KelGroups.Vote
