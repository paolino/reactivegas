import KelGroups.Vote.Invariants

namespace AuditR45ObservedImpact

open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]

def before : VoteState :=
  foldVote legacyThreshold
    [("a", .admitMember "a" "a@audit.test" adminRoles),
      ("b", .admitMember "b" "b@audit.test" adminRoles),
      ("c", .admitMember "c" "c@audit.test" adminRoles),
      ("a", .openQuestion "q" .collective),
      ("a", .cast "q" .assent)]

def after : VoteState :=
  applyVoteEvent legacyThreshold before "stranger" (.removeMember "b")

#guard
  isResponsabile "stranger" before == false &&
    validateVoteEvent legacyThreshold before "stranger" (.removeMember "b") == .ok () &&
    lookupQuestion "q" before != none &&
    lookupQuestion "q" after == none &&
    (match after.closed with
      | [record] =>
          record.questionId == "q" && record.verdict == .positive &&
            record.question.assents == ["a"]
      | _ => false)

end AuditR45ObservedImpact
