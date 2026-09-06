import KelGroups.Vote.Invariants

namespace GateMutantFranchise
open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]
def badEffect (gs : VoteState) (signer : Key) (event : VoteEvent) : VoteState :=
  match event with
  | .cast qid ballot =>
      match lookupQuestion qid gs with
      | some q => { gs with openQuestions := assocInsert qid (placeBallot signer ballot q) gs.openQuestions }
      | none => gs
  | _ => effectedState gs signer event
def mutantApply (threshold : Threshold) (gs : VoteState) (signer : Key)
    (event : VoteEvent) : VoteState := sweepClosures threshold (badEffect gs signer event)
def mutantFold (threshold : Threshold) (events : List (Key × VoteEvent)) : VoteState :=
  events.foldl (fun gs e => mutantApply threshold gs e.1 e.2) emptyVoteState
def final := mutantFold legacyThreshold
  [("a", .admitMember "a" "a@gate.test" adminRoles),
   ("a", .admitMember "b" "b@gate.test" adminRoles),
   ("a", .admitMember "c" "c@gate.test" adminRoles),
   ("a", .admitMember "d" "d@gate.test" adminRoles),
   ("a", .openQuestion "q" .collective),
   ("a", .cast "q" .assent),
   ("a", .setRoles "a" []),
   ("a", .cast "q" .dissent)]

#eval IO.println "MUTATION-APPLIED:FRANCHISE"
#guard
  match lookupQuestion "q" final with
  | some q => q.assents == [] && q.dissents == ["a"]
  | none => false
#guard
  match lookupQuestion "q" final with
  | some q => q.assents == ["a"] && q.dissents == []
  | none => false
end GateMutantFranchise
