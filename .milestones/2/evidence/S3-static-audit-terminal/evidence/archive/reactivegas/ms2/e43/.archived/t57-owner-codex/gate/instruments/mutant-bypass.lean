import KelGroups.Vote.Invariants

namespace GateMutantBypass
open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]
def before := foldVote legacyThreshold
  [("a", .admitMember "a" "a@gate.test" adminRoles),
   ("a", .admitMember "b" "b@gate.test" adminRoles),
   ("a", .admitMember "c" "c@gate.test" adminRoles),
   ("a", .openQuestion "q" .collective),
   ("a", .cast "q" .assent)]
def bypassApply (threshold : Threshold) (gs : VoteState) (signer : Key)
    (event : VoteEvent) : VoteState := sweepClosures threshold (effectedState gs signer event)
def after := bypassApply legacyThreshold before "stranger" (.removeMember "b")

#eval IO.println "MUTATION-APPLIED:BYPASS"
#guard after != before && lookupQuestion "q" after == none
#guard after == before
end GateMutantBypass
