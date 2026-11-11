import KelGroups.Vote.Invariants

namespace AuditorStaleRejectionCandidateGreen

open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]

def staleQuestion : Question :=
  { kind := .collective, proposer := "a", assents := ["a"], dissents := [] }

def staleState : VoteState :=
  { members :=
      [("a", Member.mk "a" "a@audit.test" adminRoles),
       ("b", Member.mk "b" "b@audit.test" adminRoles)],
    openQuestions := [("q", staleQuestion)], closed := [] }

def candidateAfter :=
  applyVoteEvent legacyThreshold staleState "stranger" (.cast "q" .dissent)

#guard verdictOf legacyThreshold staleState staleQuestion == .positive
#guard sweepClosures legacyThreshold staleState != staleState
#guard
  validateVoteEvent legacyThreshold staleState "stranger" (.cast "q" .dissent) ==
    Except.error VoteError.notResponsabile
#guard candidateAfter == staleState

end AuditorStaleRejectionCandidateGreen
