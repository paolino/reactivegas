import KelGroups.Vote.Invariants

namespace GateNoExpiryMember

open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]

def preEvents : List (Key × VoteEvent) :=
  [("a", .admitMember "a" "a@gate.test" adminRoles),
   ("a", .admitMember "b" "b@gate.test" adminRoles),
   ("a", .admitMember "c" "c@gate.test" adminRoles),
   ("a", .openQuestion "q" .collective),
   ("a", .cast "q" .assent)]

def before : VoteState := foldVote legacyThreshold preEvents
def event : VoteEvent := .admitMember "observer" "observer@gate.test" []
def after : VoteState := applyVoteEvent legacyThreshold before "a" event

#guard
  franchise before == franchise after &&
    lookupQuestion "q" before == lookupQuestion "q" after &&
    (match lookupQuestion "q" after with
      | some q =>
          q.proposer == "a" && q.assents == ["a"] && q.dissents == [] &&
            verdictOf legacyThreshold after q == .open
      | none => false)

example : PreservesQuestionSemantics legacyThreshold before "a" event "q" := by
  decide

end GateNoExpiryMember
