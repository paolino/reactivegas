import KelGroups.Vote.Invariants

namespace AuditR45Candidate

open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]

def preEvents : List (Key × VoteEvent) :=
  [("a", .admitMember "a" "a@audit.test" adminRoles),
    ("b", .admitMember "b" "b@audit.test" adminRoles),
    ("c", .admitMember "c" "c@audit.test" adminRoles),
    ("a", .openQuestion "q" .collective),
    ("a", .cast "q" .assent)]

def before : VoteState := foldVote legacyThreshold preEvents
def removal : VoteEvent := .removeMember "b"
def after : VoteState := applyVoteEvent legacyThreshold before "stranger" removal

-- Value controls: the question is genuinely open at threshold 2 before the
-- event, and the signer is not in the franchise.
#guard
  isResponsabile "stranger" before == false &&
    (match lookupQuestion "q" before with
      | some q => q.assents == ["a"] && verdictOf legacyThreshold before q == .open
      | none => false)

-- R-45 admissibility oracle. The candidate currently returns `.ok ()`, so
-- this guard must go red.
#guard
  validateVoteEvent legacyThreshold before "stranger" removal ==
    Except.error VoteError.notResponsabile

-- Complete-state no-op oracle. The unauthorised removal lowers the franchise
-- and closes q positive, so this also must go red.
#guard after == before

end AuditR45Candidate
