import KelGroups.Vote.Types

/-!
# Required vote machine — event vocabulary

The events the required machine folds: questions only. The signer is supplied
separately by the fold, matching the integrated `(Key × IntegratedEvent)`
shape, so no event carries a second author identity.

**There is no membership event here at all** (T6222, R62-11). `admitMember`,
`removeMember` and `setRoles` are gone — not refused, not inert, not
representable. Membership has exactly one writable store and one insertion
path, and neither is in this machine; a base transition tells this machine
what changed by handing it the post-transition canonical view.

That removal takes the old bootstrap admission capability with it: the
founding admin arrives through the application's guarded initial aggregate,
so an empty group can no longer insert its own first member by vote.
-/

namespace KelGroups.Vote

inductive VoteEvent where
  | openQuestion (questionId : QuestionId) (kind : QuestionKind)
  | cast (questionId : QuestionId) (ballot : Ballot)
  | renounce (questionId : QuestionId)
deriving DecidableEq, BEq, Repr

end KelGroups.Vote
