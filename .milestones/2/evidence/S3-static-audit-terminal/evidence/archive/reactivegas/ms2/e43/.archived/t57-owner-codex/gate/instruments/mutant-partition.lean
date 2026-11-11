import KelGroups.Vote.Invariants

namespace GateMutantPartition
open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]
def dropSweep (threshold : Threshold) (gs : VoteState) : VoteState :=
  { gs with
    openQuestions := gs.openQuestions.filter (fun e => verdictOf threshold gs e.2 = .open),
    closed := gs.closed }
def mutantApply (threshold : Threshold) (gs : VoteState) (signer : Key)
    (event : VoteEvent) : VoteState :=
  match validateVoteEvent threshold gs signer event with
  | .ok () => dropSweep threshold (effectedState gs signer event)
  | .error _ => gs
def mutantFold (threshold : Threshold) (events : List (Key × VoteEvent)) : VoteState :=
  events.foldl (fun gs e => mutantApply threshold gs e.1 e.2) emptyVoteState
def final := mutantFold zeroThreshold
  [("a", .admitMember "a" "a@gate.test" adminRoles),
   ("a", .openQuestion "q" .collective)]

#eval IO.println "MUTATION-APPLIED:PARTITION"
#guard lookupQuestion "q" final == none && final.closed == []
#guard
  (final.openQuestions.map Prod.fst).contains "q" !=
    (final.closed.map (fun record => record.questionId)).contains "q"
end GateMutantPartition
