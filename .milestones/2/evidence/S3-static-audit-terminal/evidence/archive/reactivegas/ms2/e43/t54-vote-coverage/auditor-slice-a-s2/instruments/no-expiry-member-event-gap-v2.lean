import KelGroups.Vote.Invariants

namespace AuditNoExpiryGapV2

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

-- Non-degenerate value control: this production member event changes neither
-- franchise, target ballots, nor proposer standing, and the target stays open.
#guard
  franchise before == franchise after &&
    lookupQuestion "q" before == lookupQuestion "q" after &&
    (match lookupQuestion "q" after with
      | some q =>
          q.proposer == "a" && q.assents == ["a"] && q.dissents == [] &&
            verdictOf legacyThreshold after q == .open
      | none => false)

-- Coverage oracle: the public no_expiry premise reduces to False for this
-- preserving production event, so the theorem cannot be instantiated here.
example : ¬ EventPreservesQuestion before "stranger" event "q" := by
  simp [EventPreservesQuestion, event]

end AuditNoExpiryGapV2
