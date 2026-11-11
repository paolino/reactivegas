import KelGroups.Vote.Invariants

namespace AuditorStaleRejectionBypassRed

open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]

def staleQuestion : Question :=
  { kind := .collective, proposer := "a", assents := ["a"], dissents := [] }

def staleState : VoteState :=
  { members :=
      [("a", Member.mk "a" "a@audit.test" adminRoles),
       ("b", Member.mk "b" "b@audit.test" adminRoles)],
    openQuestions := [("q", staleQuestion)], closed := [] }

def oldBoundaryShape (threshold : Threshold) (gs : VoteState) (signer : Key)
    (event : VoteEvent) : VoteState :=
  sweepClosures threshold
    (match validateVoteEvent threshold gs signer event with
      | .ok () => effectedState gs signer event
      | .error _ => gs)

def mutated :=
  oldBoundaryShape legacyThreshold staleState "stranger" (.cast "q" .dissent)

#eval IO.println "MUTATION-APPLIED:STALE-REJECTION-SWEEP"
#guard verdictOf legacyThreshold staleState staleQuestion == .positive
#guard mutated != staleState
#guard mutated == staleState

end AuditorStaleRejectionBypassRed
