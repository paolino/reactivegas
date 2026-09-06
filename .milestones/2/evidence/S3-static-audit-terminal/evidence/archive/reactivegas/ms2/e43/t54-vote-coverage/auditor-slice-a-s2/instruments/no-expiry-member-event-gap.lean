import KelGroups.Vote.Invariants

namespace AuditNoExpiryGap

open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]

def preEvents : List (Key × VoteEvent) :=
  [("a", .admitMember "a" "a@audit.test" adminRoles),
    ("b", .admitMember "b" "b@audit.test" adminRoles),
    ("c", .admitMember "c" "c@audit.test" adminRoles),
    ("a", .openQuestion "q" .collective),
    ("a", .cast "q" .assent)]

def before : VoteState := foldVote legacyThreshold preEvents
def event : VoteEvent := .admitMember "observer" "observer@audit.test" []
def after : VoteState := applyVoteEvent legacyThreshold before "stranger" event

-- This member event changes neither the target ballots nor the franchise nor
-- the proposer's standing, and the production step preserves the open
-- question with its non-degenerate assent tally.
#guard
  franchise before == franchise after &&
    lookupQuestion "q" before == lookupQuestion "q" after &&
    (match lookupQuestion "q" after with
      | some q => q.proposer == "a" && q.assents == ["a"] && q.dissents == []
      | none => false)

-- Yet the shipped theorem's premise rejects every admitMember event
-- categorically, so no_expiry cannot be instantiated for this preserving
-- production transition.
#guard !(decide (EventPreservesQuestion before "stranger" event "q"))

end AuditNoExpiryGap
