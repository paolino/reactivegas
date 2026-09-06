import KelGroups.Vote.Invariants

namespace AuditPartitionMutant

open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]

def dropSweep (threshold : Threshold) (gs : VoteState) : VoteState :=
  { gs with
    openQuestions :=
      gs.openQuestions.filter (fun entry => verdictOf threshold gs entry.2 = .open),
    closed := gs.closed }

def mutantApply (threshold : Threshold) (gs : VoteState) (signer : Key)
    (event : VoteEvent) : VoteState :=
  dropSweep threshold
    (match validateVoteEvent threshold gs signer event with
      | .ok () => effectedState gs signer event
      | .error _ => gs)

def mutantFold (threshold : Threshold) (events : List (Key × VoteEvent)) : VoteState :=
  events.foldl
    (fun current signed => mutantApply threshold current signed.1 signed.2)
    emptyVoteState

def trace : List (Key × VoteEvent) :=
  [("a", .admitMember "a" "a@audit.test" adminRoles),
    ("a", .openQuestion "q" .collective)]

def final : VoteState := mutantFold zeroThreshold trace

-- Mutation-applied control: the zero-threshold question was removed and no
-- closure record was appended.
#guard lookupQuestion "q" final == none && final.closed == []

-- Property oracle: an ID opened by the production-shaped trace must remain
-- in exactly one final container. This must go red for the silent-deletion
-- mutant above.
#guard
  (final.openQuestions.map Prod.fst).contains "q" !=
    (final.closed.map (fun record => record.questionId)).contains "q"

end AuditPartitionMutant
