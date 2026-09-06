import KelGroups.Vote.Fold

namespace ProbeExtract
open KelGroups KelGroups.Vote

def preservesQuestionDecide (threshold : Threshold) (gs : VoteState)
    (signer : Key) (event : VoteEvent) (questionId : QuestionId) : Bool :=
  match lookupQuestion questionId gs with
  | none => true
  | some q =>
      decide (lookupQuestion questionId
          (applyVoteEvent threshold gs signer event) = some q) &&
        decide (franchise (applyVoteEvent threshold gs signer event) =
          franchise gs) &&
        decide (isResponsabile q.proposer
            (applyVoteEvent threshold gs signer event) =
          isResponsabile q.proposer gs)

example (θ : Threshold) (pre : List (Key × VoteEvent)) (signer : Key)
    (event : VoteEvent) (questionId : QuestionId) (q : Question)
    (hopen : assocLookup questionId (foldVote θ pre).openQuestions = some q)
    (hpres : preservesQuestionDecide θ (foldVote θ pre) signer event questionId = true) :
    lookupQuestion questionId
        (applyVoteEvent θ (foldVote θ pre) signer event) = some q ∧
      franchise (applyVoteEvent θ (foldVote θ pre) signer event) = franchise (foldVote θ pre) ∧
      True := by
  have hdec : preservesQuestionDecide θ (foldVote θ pre) signer event
      questionId = true := hpres
  simp only [preservesQuestionDecide] at hdec
  rw [lookupQuestion, hopen] at hdec
  simp only [Bool.and_eq_true, decide_eq_true_eq] at hdec
  exact ⟨hdec.1, hdec.2.1, trivial⟩

end ProbeExtract
