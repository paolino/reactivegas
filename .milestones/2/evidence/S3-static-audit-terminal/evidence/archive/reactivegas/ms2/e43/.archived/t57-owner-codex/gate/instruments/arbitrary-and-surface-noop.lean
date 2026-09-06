import KelGroups.Vote.Invariants

namespace GateArbitraryNoop

open KelGroups KelGroups.Vote

def adminRoles : List Role := [.adminRole .publicAdmin]

def staleQuestion : Question :=
  { kind := .collective, proposer := "a", assents := ["a"], dissents := [] }

def arbitraryPreState : VoteState :=
  { members :=
      [("a", Member.mk "a" "a@gate.test" adminRoles),
       ("b", Member.mk "b" "b@gate.test" adminRoles)],
    openQuestions := [("q", staleQuestion)], closed := [] }

#guard verdictOf legacyThreshold arbitraryPreState staleQuestion == .positive
#guard
  validateVoteEvent legacyThreshold arbitraryPreState "stranger" (.cast "q" .dissent) ==
    Except.error VoteError.notResponsabile
#guard
  applyVoteEvent legacyThreshold arbitraryPreState "stranger" (.cast "q" .dissent) ==
    arbitraryPreState

def bootstrapped : VoteState :=
  foldVote legacyThreshold
    [("a", .admitMember "a" "a@gate.test" adminRoles),
     ("a", .openQuestion "q" .collective)]

def everyCurrentEvent : List VoteEvent :=
  [.openQuestion "new" .collective,
   .cast "q" .assent,
   .renounce "q",
   .admitMember "x" "x@gate.test" adminRoles,
   .removeMember "a",
   .setRoles "a" []]

#guard franchiseSize bootstrapped == 1
#guard
  everyCurrentEvent.all (fun event =>
    validateVoteEvent legacyThreshold bootstrapped "stranger" event ==
      Except.error VoteError.notResponsabile &&
    applyVoteEvent legacyThreshold bootstrapped "stranger" event == bootstrapped)

end GateArbitraryNoop
