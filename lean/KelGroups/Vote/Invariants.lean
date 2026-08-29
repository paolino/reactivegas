import KelGroups.Vote.Fold

/-!
# Required vote machine — invariants over the production fold

Every theorem here is about the production fold `foldVote` (R-68): no
property is discharged against a hand-written record literal or a state a
test-only constructor built. Reachable means: the state `foldVote` produces
from some event list.

The well-formedness carrier `VoteWellFormed` is the conjunction the named
theorems stand on; it holds at the empty state and is preserved by every
`applyVoteEvent` step. The franchise invariant is a property of the whole
trace (a tally key may have lost standing after casting, V-3), so it is
stated with the cast-time prefix as its witness rather than as a final-state
predicate.

Axiom sets of the contractual theorems are printed at the bottom; the frozen
gate reads the printed lines.
-/

namespace KelGroups.Vote

/-! ## Proof-side vocabulary -/

/-- VC-1 (R-57): both tallies are duplicate-free and mutually disjoint. -/
def QuestionClean (q : Question) : Prop :=
  q.assents.Nodup ∧ q.dissents.Nodup ∧ ∀ k, k ∈ q.assents → k ∈ q.dissents → False

/-- Every key recorded in any tally of the state, open or closed questions
alike. The franchise theorem is stated over this. -/
def tallyKeysOfQuestion (q : Question) : List Key := q.assents ++ q.dissents

def tallyKeysOfState (gs : VoteState) : List Key :=
  (gs.openQuestions.map (fun entry => tallyKeysOfQuestion entry.2)).flatten ++
    (gs.closed.map (fun record => tallyKeysOfQuestion record.question)).flatten

/-- The state-side shape the sweep needs and every branch preserves: the
representation invariants of the carrier minus the no-stale-open property,
which is exactly what the sweep alone re-establishes. -/
structure SweepReady (gs : VoteState) : Prop where
  membersNodup : (gs.members.map Prod.fst).Nodup
  openNodup : (gs.openQuestions.map Prod.fst).Nodup
  closedNodup : (gs.closed.map (·.questionId)).Nodup
  openClosedDisjoint : ∀ qid, qid ∈ gs.openQuestions.map Prod.fst →
    qid ∉ gs.closed.map (·.questionId)
  openClean : ∀ qid q, assocLookup qid gs.openQuestions = some q → QuestionClean q
  closedClean : ∀ c, c ∈ gs.closed → QuestionClean c.question

/-- The well-formedness carrier: `SweepReady` plus VC-4 (R-52), no stale open
question under the current franchise and the threshold parameter. The
threshold is carried explicitly because VC-4 is about *that* threshold —
nothing hard-codes a policy (R-46). -/
structure VoteWellFormed (θ : Threshold) (gs : VoteState) extends SweepReady gs : Prop where
  opensOpen : ∀ qid q, assocLookup qid gs.openQuestions = some q →
    verdictOf θ gs q = Verdict.open

/-! ## The carrier over the production fold -/

theorem emptyVoteState_sweepReady : SweepReady emptyVoteState := by
  sorry

theorem emptyVoteState_wellFormed (θ : Threshold) : VoteWellFormed θ emptyVoteState := by
  sorry

/-- Verdicts depend on the threshold only through its value at the current
franchise size — the executable form of INV-54-POLICYFREE (R-46): no verdict
path reads a threshold other than the one passed in, at the count it is
passed for. -/
theorem verdictOf_threshold_congr (θ θ' : Threshold) (gs : VoteState) (q : Question)
    (h : θ (franchiseSize gs) = θ' (franchiseSize gs)) :
    verdictOf θ gs q = verdictOf θ' gs q := by
  sorry

/-- Every step preserves the carrier: branch by branch the effected state is
`SweepReady`, and the unconditional sweep (R-51) re-establishes VC-4 against
the current franchise — which is why a question sitting at or above threshold
while still open is unreachable (R-52, the deliberate negation of Slice 1's
refuted VI-6 about the *faithful* machine). -/
theorem applyVoteEvent_preserves_wellFormed (θ : Threshold) (gs : VoteState)
    (signer : Key) (event : VoteEvent) (h : VoteWellFormed θ gs) :
    VoteWellFormed θ (applyVoteEvent θ gs signer event) := by
  sorry

private theorem foldFrom_preserves_wellFormed (θ : Threshold)
    (events : List (Key × VoteEvent)) (initial : VoteState)
    (h : VoteWellFormed θ initial) :
    VoteWellFormed θ (foldFrom θ initial events) := by
  sorry

/-- The contractual carrier theorem: every reachable state is well formed. -/
theorem foldVote_wellFormed (θ : Threshold) (events : List (Key × VoteEvent)) :
    VoteWellFormed θ (foldVote θ events) := by
  sorry

/-! ## INV-54-DISJOINT (R-57, VC-1) -/

/-- In every reachable state each question's tallies — open questions and
closure records alike — are duplicate-free and mutually disjoint. The
"just vote no" escape is therefore always available. -/
theorem ballots_nodup_disjoint (θ : Threshold) (events : List (Key × VoteEvent)) :
    (∀ qid q, assocLookup qid (foldVote θ events).openQuestions = some q →
      QuestionClean q) ∧
    (∀ c, c ∈ (foldVote θ events).closed → QuestionClean c.question) := by
  sorry

/-! ## INV-54-NOSTALE (R-52, VC-4) -/

/-- In every reachable state, every open question is open under the *current*
franchise and the threshold the fold was run with. A question sitting at or
above threshold while still open is unreachable. -/
theorem open_questions_are_open (θ : Threshold) (events : List (Key × VoteEvent))
    (questionId : QuestionId) (q : Question)
    (h : assocLookup questionId (foldVote θ events).openQuestions = some q) :
    verdictOf θ (foldVote θ events) q = Verdict.open := by
  sorry

/-! ## INV-54-PARTITION (R-61, VC-3) -/

/-- In every reachable state the open set and the closure log partition every
question ever opened — the ids are separate and each side is duplicate-free —
and every closure record carries a verdict that is never `open`. Closing is
removal plus an appended record, as one operation; no question is ever
silently deleted. This is the escrow invariant. -/
theorem questions_partition (θ : Threshold) (events : List (Key × VoteEvent)) :
    ((foldVote θ events).openQuestions.map Prod.fst).Nodup ∧
    ((foldVote θ events).closed.map (·.questionId)).Nodup ∧
    (∀ qid, qid ∈ (foldVote θ events).openQuestions.map Prod.fst →
      qid ∉ (foldVote θ events).closed.map (·.questionId)) ∧
    (∀ c, c ∈ (foldVote θ events).closed → c.verdict ≠ Verdict.open) := by
  sorry

/-! ## INV-54-NOEXPIRY (R-54) -/

/-- An open question stays open, with its tallies untouched, under any event
that changes neither its ballots, nor the franchise, nor its proposer's
standing. The theorem's event is a cast on a *different* question: a cast
mutates no membership, so neither the franchise nor anyone's standing moves,
and the target question is not touched. Nothing in the state can age a
question out — there is no field any transition could read (R-54). -/
theorem no_expiry (θ : Threshold) (gs : VoteState) (signer : Key)
    (questionId otherId : QuestionId) (ballot : Ballot) (q : Question)
    (hform : VoteWellFormed θ gs)
    (hopen : assocLookup questionId gs.openQuestions = some q)
    (hdist : questionId ≠ otherId) :
    assocLookup questionId
        (applyVoteEvent θ gs signer (.cast otherId ballot)).openQuestions = some q ∧
      verdictOf θ (applyVoteEvent θ gs signer (.cast otherId ballot)) q = Verdict.open := by
  sorry

/-! ## INV-54-FRANCHISE (R-44, R-45, VC-5) -/

/-- A cast by a non-responsabile is a no-op in the fold: no key enters or
leaves any tally because of it. (Its distinct rejection lives in
`validateVoteEvent`; this is the fold-side guarantee that makes the
franchise theorem below provable without validation hypotheses.) -/
theorem unfranchised_cast_noop (θ : Threshold) (gs : VoteState) (signer : Key)
    (questionId : QuestionId) (ballot : Ballot)
    (h : isResponsabile signer gs = false) (k : Key) :
    (k ∈ tallyKeysOfState (applyVoteEvent θ gs signer (.cast questionId ballot))) ↔
      k ∈ tallyKeysOfState gs := by
  sorry

private theorem tally_keys_franchised_from (θ : Threshold) (initial : VoteState) :
    ∀ (events : List (Key × VoteEvent)) (k : Key),
      k ∈ tallyKeysOfState (foldFrom θ initial events) →
      k ∈ tallyKeysOfState initial ∨
        ∃ pre suffix : List (Key × VoteEvent),
          events = pre ++ suffix ∧
          isResponsabile k (foldFrom θ initial pre) = true := by
  sorry

/-- INV-54-FRANCHISE: every key in any tally of a reachable state was a
responsabile at the moment it cast. The witness is the prefix at whose end
the key's ballot was placed; `isResponsabile` there is the cast-time
franchise check the fold's cast branch performs. The key may have lost
standing since — tallies are counted as recorded (V-3, R-53). -/
theorem franchise_of_tallies (θ : Threshold) (events : List (Key × VoteEvent)) (k : Key)
    (hk : k ∈ tallyKeysOfState (foldVote θ events)) :
    ∃ pre suffix : List (Key × VoteEvent),
      events = pre ++ suffix ∧
      isResponsabile k (foldVote θ pre) = true := by
  sorry

end KelGroups.Vote

/- Axiom evidence for the contractual theorem names; the frozen gate reads
the printed lines, so a theorem that vanishes takes its evidence with it. -/
#print axioms KelGroups.Vote.foldVote_wellFormed
#print axioms KelGroups.Vote.ballots_nodup_disjoint
#print axioms KelGroups.Vote.open_questions_are_open
#print axioms KelGroups.Vote.questions_partition
#print axioms KelGroups.Vote.no_expiry
#print axioms KelGroups.Vote.franchise_of_tallies
#print axioms KelGroups.Vote.verdictOf_threshold_congr
