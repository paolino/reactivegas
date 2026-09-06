import KelGroups.Vote.Invariants

namespace AuditFranchiseMutant

open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]

def badEffectedState (gs : VoteState) (signer : Key) (event : VoteEvent) : VoteState :=
  match event with
  | .cast questionId ballot =>
      match lookupQuestion questionId gs with
      | some question =>
          { gs with
            openQuestions :=
              assocInsert questionId (placeBallot signer ballot question) gs.openQuestions }
      | none => gs
  | _ => effectedState gs signer event

def mutantApply (threshold : Threshold) (gs : VoteState) (signer : Key)
    (event : VoteEvent) : VoteState :=
  sweepClosures threshold (badEffectedState gs signer event)

def mutantFold (threshold : Threshold) (events : List (Key × VoteEvent)) : VoteState :=
  events.foldl
    (fun current signed => mutantApply threshold current signed.1 signed.2)
    emptyVoteState

def trace : List (Key × VoteEvent) :=
  [("a", .admitMember "a" "a@audit.test" adminRoles),
    ("b", .admitMember "b" "b@audit.test" adminRoles),
    ("c", .admitMember "c" "c@audit.test" adminRoles),
    ("d", .admitMember "d" "d@audit.test" adminRoles),
    ("a", .openQuestion "q" .collective),
    ("a", .cast "q" .assent),
    ("a", .setRoles "a" []),
    ("a", .cast "q" .dissent)]

def final : VoteState := mutantFold legacyThreshold trace

-- Mutation-applied control: the formerly franchised caster moved to dissent
-- after losing standing.
#guard
  match lookupQuestion "q" final with
  | some q => q.assents == [] && q.dissents == ["a"]
  | none => false

-- Complete-tally no-op oracle: this must go red for the unfranchised-recast
-- mutant and is green in the shipped production fold.
#guard
  match lookupQuestion "q" final with
  | some q => q.assents == ["a"] && q.dissents == []
  | none => false

end AuditFranchiseMutant
