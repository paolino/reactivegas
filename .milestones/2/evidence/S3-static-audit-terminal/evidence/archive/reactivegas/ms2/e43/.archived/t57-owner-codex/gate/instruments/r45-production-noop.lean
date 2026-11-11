import KelGroups.Vote.Invariants

namespace GateR45Production

open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]

def preEvents : List (Key × VoteEvent) :=
  [("a", .admitMember "a" "a@gate.test" adminRoles),
    ("a", .admitMember "b" "b@gate.test" adminRoles),
    ("a", .admitMember "c" "c@gate.test" adminRoles),
    ("a", .openQuestion "q" .collective),
    ("a", .cast "q" .assent)]

def before : VoteState := foldVote legacyThreshold preEvents
def removal : VoteEvent := .removeMember "b"
def after : VoteState := applyVoteEvent legacyThreshold before "stranger" removal

#guard
  franchiseSize before == 3 && isResponsabile "stranger" before == false &&
    (match lookupQuestion "q" before with
      | some q => q.assents == ["a"] && verdictOf legacyThreshold before q == .open
      | none => false)

#guard
  validateVoteEvent legacyThreshold before "stranger" removal ==
    Except.error VoteError.notResponsabile

#guard after == before

end GateR45Production
