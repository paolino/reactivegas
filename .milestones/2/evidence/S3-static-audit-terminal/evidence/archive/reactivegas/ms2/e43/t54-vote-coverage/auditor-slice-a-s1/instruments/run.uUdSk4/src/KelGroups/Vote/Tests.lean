import KelGroups.Vote.Fold
import KelGroups.Vote.Validate

/-!
# Required vote machine — executed witnesses

Every witness here is produced by executing the production fold `foldVote`
from the empty state over a signed trace, and each trace is checked
admissible by `validateVoteEvent` at every step (R-68, R-69).

The two V-2 witnesses below are consequences of an **unruled** policy choice
(R-48): the operator has deliberately left open whether a tie passes under
`legacyThreshold` and whether `zeroThreshold` passes everything instantly.
They are delivered as executed facts about those named instances only —
never as machine-wide truths — and freezing either as the product answer is
a rejection reason.

Deliberate divergence from legacy, recorded here: legacy seeds a collection
with the proposer; these traces open questions with empty tallies, so a
zero-threshold pass really is a pass with no ballot cast at all.
-/

namespace KelGroups.Vote

/-! ## Trace helpers -/

private def witnessTraceValidFrom (θ : Threshold) (gs : VoteState) :
    List (Key × VoteEvent) → Bool
  | [] => true
  | (signer, event) :: rest =>
      (validateVoteEvent θ gs signer event == Except.ok ()) &&
        witnessTraceValidFrom θ (applyVoteEvent θ gs signer event) rest

private def witnessTraceValid (θ : Threshold) (events : List (Key × VoteEvent)) : Bool :=
  witnessTraceValidFrom θ emptyVoteState events

/-- One admission event: the member arrives with an admin role, so the
franchise grows by one responsabile. -/
def vAdmit (key : Key) : Key × VoteEvent :=
  (key, .admitMember key (key ++ "@vote.test") [.adminRole .publicAdmin])

/-- `openQuestion` issued by `who`. -/
def vOpen (who : Key) (questionId : QuestionId) : Key × VoteEvent :=
  (who, .openQuestion questionId .collective)

/-- `cast` issued by `who`. -/
def vCast (who : Key) (questionId : QuestionId) (ballot : Ballot) : Key × VoteEvent :=
  (who, .cast questionId ballot)

/-- The one closure record of a state whose question `questionId` has closed,
if the log holds exactly one record for it. -/
private def soleClosure (gs : VoteState) (questionId : QuestionId) : Option ClosureRecord :=
  match gs.closed with
  | [record] => if record.questionId == questionId then some record else none
  | _ => none

/-! ## R-48a — under the legacy policy, a tie passes (unruled consequence) -/

/-- Four responsabili, two assents, no dissent. `legacyThreshold 4 = 2`, so
the second assent reaches the threshold: the question closes positive with no
dissent recorded. Executed through `foldVote`. -/
def tieEvents : List (Key × VoteEvent) :=
  [vAdmit "a", vAdmit "b", vAdmit "c", vAdmit "d",
    vOpen "a" "q",
    vCast "a" "q" .assent,
    vCast "b" "q" .assent]

def tiePassesUnderLegacyThreshold : VoteState := foldVote legacyThreshold tieEvents

#guard
  witnessTraceValid legacyThreshold tieEvents &&
    lookupQuestion "q" tiePassesUnderLegacyThreshold == none &&
    (match soleClosure tiePassesUnderLegacyThreshold "q" with
      | some record =>
          record.verdict == Verdict.positive &&
            record.cause == ClosureCause.tally &&
            record.question.assents.length == 2 &&
            record.question.assents.contains "a" &&
            record.question.assents.contains "b" &&
            record.question.dissents == []
      | none => false)

/-! ## R-48b — under the zero policy, everything passes instantly (unruled
consequence) -/

/-- `zeroThreshold` maps every responsabile count to `0`, so a question that
opens with empty tallies already meets the threshold: it opens and closes
positive in the same `openQuestion` event, with no ballot cast at all. -/
def zeroEvents : List (Key × VoteEvent) :=
  [vAdmit "a", vOpen "a" "q"]

def zeroThresholdPassesWithNoBallot : VoteState := foldVote zeroThreshold zeroEvents

#guard
  witnessTraceValid zeroThreshold zeroEvents &&
    lookupQuestion "q" zeroThresholdPassesWithNoBallot == none &&
    (match soleClosure zeroThresholdPassesWithNoBallot "q" with
      | some record =>
          record.verdict == Verdict.positive &&
            record.question.assents == [] && record.question.dissents == []
      | none => false)

/-! ## R-53 — a question can pass because a responsabile left -/

/-- Five responsabili, two assents: under `legacyThreshold 5 = 3` the question
is open. Removing assent caster `a` leaves the tallies exactly as recorded,
shrinks the franchise to four, and `legacyThreshold 4 = 2` carries the stale
tally past the threshold: the question closes **positive**. The closure cause
is the franchise route: the deciding side records a key the current franchise
no longer counts. Required behaviour, not a bug. -/
def departureEvents : List (Key × VoteEvent) :=
  [vAdmit "a", vAdmit "b", vAdmit "c", vAdmit "d", vAdmit "e",
    vOpen "a" "q",
    vCast "a" "q" .assent,
    vCast "b" "q" .assent,
    ("a", .removeMember "a")]

def departureCarriesStaleAssents : VoteState := foldVote legacyThreshold departureEvents

#guard
  witnessTraceValid legacyThreshold departureEvents &&
    lookupQuestion "q" departureCarriesStaleAssents == none &&
    !isResponsabile "a" departureCarriesStaleAssents &&
    (match soleClosure departureCarriesStaleAssents "q" with
      | some record =>
          record.verdict == Verdict.positive &&
            record.cause == ClosureCause.franchiseChange &&
            record.question.assents.length == 2 &&
            record.question.assents.contains "a" &&
            record.question.assents.contains "b"
      | none => false)

/-! ## V-7 — the "just vote no" escape (one position per responsabile) -/

/-- A responsabile who dissents and then assents moves between the lists:
after the switch they are counted once, on the assent side, and the dissent
tally is back to zero. Legacy left the other list untouched and double-counted
switchers; this machine cannot. -/
def switchEvents : List (Key × VoteEvent) :=
  [vAdmit "a", vAdmit "b", vAdmit "c",
    vOpen "a" "q",
    vCast "b" "q" .dissent,
    vCast "b" "q" .assent]

def switchLeavesOneList : VoteState := foldVote legacyThreshold switchEvents

#guard
  witnessTraceValid legacyThreshold switchEvents &&
    (match lookupQuestion "q" switchLeavesOneList with
      | some question => question.assents == ["b"] && question.dissents == []
      | none => false)

/-- Two dissents of three responsabili reach the same threshold: the question
closes negative. The dissent side is first-class; assent is not the only path
to a verdict. -/
def dissentEvents : List (Key × VoteEvent) :=
  [vAdmit "a", vAdmit "b", vAdmit "c",
    vOpen "a" "q",
    vCast "a" "q" .dissent,
    vCast "b" "q" .dissent]

def dissentReachesThresholdClosesNegative : VoteState :=
  foldVote legacyThreshold dissentEvents

#guard
  witnessTraceValid legacyThreshold dissentEvents &&
    lookupQuestion "q" dissentReachesThresholdClosesNegative == none &&
    (match soleClosure dissentReachesThresholdClosesNegative "q" with
      | some record =>
          record.verdict == Verdict.negative &&
            record.cause == ClosureCause.tally &&
            record.question.dissents.length == 2 &&
            record.question.dissents.contains "a" &&
            record.question.dissents.contains "b" &&
            record.question.assents == []
      | none => false)

/-! ## R-61 — a decided question stays decided -/

/-- Re-issuing `openQuestion` for an id that has already closed is a no-op:
the closure log keeps exactly its one record and no open question revives.
Every question ever opened is in exactly one of the two places. -/
def noReviveEvents : List (Key × VoteEvent) :=
  tieEvents ++ [vOpen "a" "q"]

def decidedQuestionStaysDecided : VoteState := foldVote legacyThreshold noReviveEvents

#guard
  witnessTraceValid legacyThreshold noReviveEvents &&
    lookupQuestion "q" decidedQuestionStaysDecided == none &&
    decidedQuestionStaysDecided.closed.length == 1 &&
    (match soleClosure decidedQuestionStaysDecided "q" with
      | some record => record.verdict == Verdict.positive
      | none => false)

/-! ## Admissibility point tests -/

/-- A state with one responsabile and one open collective question, produced
by the production fold. -/
def votePointState : VoteState :=
  foldVote legacyThreshold [vAdmit "a", vOpen "a" "q"]

-- A cast by a non-member is rejected with the franchise error (R-44).
#guard
  validateVoteEvent legacyThreshold votePointState "stranger" (.cast "q" .assent) ==
    Except.error VoteError.notResponsabile

-- A cast by a member without an admin role is rejected with the same
-- franchise error (R-44): the franchise tracks admin roles, not membership.
#guard
  validateVoteEvent legacyThreshold
    (foldVote legacyThreshold
      [vAdmit "a", ("b", .admitMember "b" "b@vote.test" []), vOpen "a" "q"])
    "b" (.cast "q" .assent) == Except.error VoteError.notResponsabile

-- A question opening by a non-responsabile is rejected (R-45: no path by
-- which a non-responsabile influences the machine).
#guard
  validateVoteEvent legacyThreshold votePointState "stranger"
    (.openQuestion "r" .collective) == Except.error VoteError.notResponsabile

-- A cast on an unknown question id is rejected with the lookup error.
#guard
  validateVoteEvent legacyThreshold votePointState "a" (.cast "missing" .assent) ==
    Except.error VoteError.questionNotFound

-- An admissible cast is accepted.
#guard
  validateVoteEvent legacyThreshold votePointState "a" (.cast "q" .assent) ==
    Except.ok ()

-- Renunciation's proposer-only restriction is Slice B (R-58); in this slice
-- an existing-question renounce validates and folds to no effect.
#guard
  validateVoteEvent legacyThreshold votePointState "a" (.renounce "q") == Except.ok ()

end KelGroups.Vote
