import KelGroups.Vote.Fold

open KelGroups
open KelGroups.Vote

def outsiderOpen : VoteState :=
  foldVote zeroThreshold [("stranger", .openQuestion "q" .collective)]

#guard lookupQuestion "q" outsiderOpen == none
#guard outsiderOpen.closed.length == 1
#guard
  match outsiderOpen.closed with
  | [record] =>
      record.questionId == "q" &&
        record.question.proposer == "stranger" &&
        record.verdict == Verdict.positive &&
        record.question.assents == [] &&
        record.question.dissents == []
  | _ => false
