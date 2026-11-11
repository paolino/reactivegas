import KelGroups.Vote.Invariants

namespace GateMutantDisjoint
open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]
def badPlace (voter : Key) (ballot : Ballot) (q : Question) : Question :=
  match ballot with
  | .assent => { q with assents := setInsert voter q.assents }
  | .dissent => { q with dissents := setInsert voter q.dissents }
def badEffect (gs : VoteState) (signer : Key) (event : VoteEvent) : VoteState :=
  match event with
  | .cast qid ballot =>
      match lookupQuestion qid gs with
      | some q => { gs with openQuestions := assocInsert qid (badPlace signer ballot q) gs.openQuestions }
      | none => gs
  | _ => effectedState gs signer event
def mutantApply (threshold : Threshold) (gs : VoteState) (signer : Key)
    (event : VoteEvent) : VoteState :=
  match validateVoteEvent threshold gs signer event with
  | .ok () => sweepClosures threshold (badEffect gs signer event)
  | .error _ => gs
def mutantFold (threshold : Threshold) (events : List (Key × VoteEvent)) : VoteState :=
  events.foldl (fun gs e => mutantApply threshold gs e.1 e.2) emptyVoteState
def final := mutantFold legacyThreshold
  [("a", .admitMember "a" "a@gate.test" adminRoles),
   ("a", .admitMember "b" "b@gate.test" adminRoles),
   ("a", .admitMember "c" "c@gate.test" adminRoles),
   ("a", .openQuestion "q" .collective),
   ("a", .cast "q" .dissent),
   ("a", .cast "q" .assent)]

#eval IO.println "MUTATION-APPLIED:DISJOINT"
#guard
  match lookupQuestion "q" final with
  | some q => q.assents.contains "a" && q.dissents.contains "a"
  | none => false
#guard
  match lookupQuestion "q" final with
  | some q => !(q.assents.contains "a" && q.dissents.contains "a")
  | none => false
end GateMutantDisjoint
