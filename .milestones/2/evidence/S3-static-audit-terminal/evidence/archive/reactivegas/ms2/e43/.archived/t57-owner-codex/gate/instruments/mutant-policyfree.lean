import KelGroups.Vote.Invariants

namespace GateMutantPolicyFree
open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]
def gs : VoteState :=
  foldVote legacyThreshold [("a", .admitMember "a" "a@gate.test" adminRoles),
    ("a", .openQuestion "q" .collective)]
def q : Question := { kind := .collective, proposer := "a", assents := [], dissents := [] }
def badVerdictOf (_threshold : Threshold) (state : VoteState) (question : Question) : Verdict :=
  verdictOf legacyThreshold state question

#eval IO.println "MUTATION-APPLIED:POLICYFREE"
#guard badVerdictOf zeroThreshold gs q == .open
#guard badVerdictOf zeroThreshold gs q == verdictOf zeroThreshold gs q
end GateMutantPolicyFree
