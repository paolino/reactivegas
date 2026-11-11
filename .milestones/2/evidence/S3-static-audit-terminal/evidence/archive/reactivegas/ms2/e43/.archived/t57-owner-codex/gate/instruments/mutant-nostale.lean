import KelGroups.Vote.Invariants

namespace GateMutantNoStale
open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]
def mutantApply (threshold : Threshold) (gs : VoteState) (signer : Key)
    (event : VoteEvent) : VoteState :=
  match validateVoteEvent threshold gs signer event with
  | .error _ => gs
  | .ok () =>
      let changed := effectedState gs signer event
      match event with
      | .cast _ _ => sweepClosures threshold changed
      | _ => changed
def mutantFold (threshold : Threshold) (events : List (Key × VoteEvent)) : VoteState :=
  events.foldl (fun gs e => mutantApply threshold gs e.1 e.2) emptyVoteState
def final := mutantFold legacyThreshold
  [("a", .admitMember "a" "a@gate.test" adminRoles),
   ("a", .admitMember "b" "b@gate.test" adminRoles),
   ("a", .admitMember "c" "c@gate.test" adminRoles),
   ("a", .openQuestion "q" .collective),
   ("a", .cast "q" .assent),
   ("a", .removeMember "c")]

#eval IO.println "MUTATION-APPLIED:NOSTALE"
#guard
  match lookupQuestion "q" final with
  | some q => verdictOf legacyThreshold final q == .positive
  | none => false
#guard
  match lookupQuestion "q" final with
  | some q => verdictOf legacyThreshold final q == .open
  | none => true
end GateMutantNoStale
