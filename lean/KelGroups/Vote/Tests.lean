import KelGroups.Vote.Fold
import KelGroups.Vote.Validate
import KelGroups.Vote.Invariants

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

/-- One admission event, signed by an existing responsabile `who`: the
member arrives with an admin role, so the franchise grows by one
responsabile. Only the first seeder signs with the arriving key itself,
from the empty state where the empty-franchise bootstrap capability applies
(R57-04). -/
def vAdmit (who key : Key) : Key × VoteEvent :=
  (who, .admitMember key (key ++ "@vote.test") [.adminRole .publicAdmin])

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
  [vAdmit "a" "a", vAdmit "a" "b", vAdmit "a" "c", vAdmit "a" "d",
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
  [vAdmit "a" "a", vOpen "a" "q"]

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
  [vAdmit "a" "a", vAdmit "a" "b", vAdmit "a" "c", vAdmit "a" "d", vAdmit "a" "e",
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
  [vAdmit "a" "a", vAdmit "a" "b", vAdmit "a" "c",
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
  [vAdmit "a" "a", vAdmit "a" "b", vAdmit "a" "c",
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
  foldVote legacyThreshold [vAdmit "a" "a", vOpen "a" "q"]

-- A cast by a non-member is rejected with the franchise error (R-44).
#guard
  validateVoteEvent legacyThreshold votePointState "stranger" (.cast "q" .assent) ==
    Except.error VoteError.notResponsabile

-- A cast by a member without an admin role is rejected with the same
-- franchise error (R-44): the franchise tracks admin roles, not membership.
#guard
  validateVoteEvent legacyThreshold
    (foldVote legacyThreshold
      [vAdmit "a" "a", ("a", .admitMember "b" "b@vote.test" []), vOpen "a" "q"])
    "b" (.cast "q" .assent) == Except.error VoteError.notResponsabile

-- A question opening by a non-responsabile is rejected (R-45: no path by
-- which a non-responsabile influences the machine).
#guard
  validateVoteEvent legacyThreshold votePointState "stranger"
    (.openQuestion "r" .collective) == Except.error VoteError.notResponsabile

-- R-45 on the production fold: a non-responsabile opening is a complete
-- no-op, including under `zeroThreshold` where a successful empty-tally open
-- would close positive. Auditor instrument nonresponsabile-open.lean
-- (sha256 1f7aa80a) against 757dac98 is the complementary mutant.
#guard
  let gs := foldVote zeroThreshold [("stranger", .openQuestion "q" .collective)]
  lookupQuestion "q" gs == none && gs.closed == [] && gs.members == []

-- A cast on an unknown question id is rejected with the lookup error.
#guard
  validateVoteEvent legacyThreshold votePointState "a" (.cast "missing" .assent) ==
    Except.error VoteError.questionNotFound

-- An admissible cast is accepted.
#guard
  validateVoteEvent legacyThreshold votePointState "a" (.cast "q" .assent) ==
    Except.ok ()

-- INV-54-FRANCHISE: after losing standing, a recast cannot switch position.
-- Four responsabili so that one assent stays open after the caster drops
-- admin (legacyThreshold 3 = 2).
def lostStandingEvents : List (Key × VoteEvent) :=
  [vAdmit "a" "a", vAdmit "a" "b", vAdmit "a" "c", vAdmit "a" "d",
    vOpen "a" "q",
    vCast "a" "q" .assent,
    ("a", .setRoles "a" []),
    vCast "a" "q" .dissent]

#guard
  match lookupQuestion "q" (foldVote legacyThreshold lostStandingEvents) with
  | some question => question.assents == ["a"] && question.dissents == []
  | none => false

-- Renunciation's proposer-only restriction is Slice B (R-58); in this slice
-- an existing-question renounce by a responsabile validates and folds to no
-- effect. A non-responsabile renounce is inside the universal rejection
-- class (see the R-45 section below).
#guard
  validateVoteEvent legacyThreshold votePointState "a" (.renounce "q") == Except.ok ()

/-! ## R57-04 universal class — every constructor rejected and inert after
bootstrap (T5712) -/

/-- All six current constructors, signed by a non-responsabile, from a
production-reachable bootstrapped state (one responsabile, one open
question). The three member/role events are inside the universal class, not
exceptions to it. -/
def strangerRejectedEvents : List VoteEvent :=
  [.openQuestion "r" .collective,
    .cast "q" .assent,
    .renounce "q",
    .admitMember "x" "x@vote.test" [.adminRole .publicAdmin],
    .removeMember "a",
    .setRoles "a" []]

#guard
  strangerRejectedEvents.all (fun event =>
    validateVoteEvent legacyThreshold votePointState "stranger" event ==
        Except.error VoteError.notResponsabile &&
      applyVoteEvent legacyThreshold votePointState "stranger" event ==
        votePointState)

-- Explicit no-op oracles for the three franchise-changing events
-- (risk-ledger control: authorization must not repair only `removeMember`).
#guard
  applyVoteEvent legacyThreshold votePointState "stranger"
    (.admitMember "x" "x@vote.test" [.adminRole .publicAdmin]) == votePointState
#guard
  applyVoteEvent legacyThreshold votePointState "stranger"
    (.removeMember "a") == votePointState
#guard
  applyVoteEvent legacyThreshold votePointState "stranger"
    (.setRoles "a" []) == votePointState

/-! ## R57-05 — the retained R-45 oracle on the production fold -/

/-- Three responsabili, question `q` open on one assent. A stranger's
`removeMember` is rejected with the franchise error and leaves the entire
state unchanged: no membership change, no threshold drop, no closure, no
verdict. -/
def r45PreEvents : List (Key × VoteEvent) :=
  [vAdmit "a" "a", vAdmit "a" "b", vAdmit "a" "c",
    vOpen "a" "q", vCast "a" "q" .assent]

def r45Before : VoteState := foldVote legacyThreshold r45PreEvents

def r45After : VoteState :=
  applyVoteEvent legacyThreshold r45Before "stranger" (.removeMember "b")

#guard
  franchiseSize r45Before == 3 &&
    isResponsabile "stranger" r45Before == false &&
    validateVoteEvent legacyThreshold r45Before "stranger" (.removeMember "b") ==
      Except.error VoteError.notResponsabile &&
    r45After == r45Before

/-! ## R57-07 — the semantic no-expiry premise covers a preserving non-admin
admission (T5713) -/

/-- A non-admin admission by a responsabile: the state really changes
(membership grows) while the target question's ballots, the franchise, and
the proposer's standing are preserved — and the semantic premise holds. -/
def nonAdminAdmission : VoteEvent :=
  .admitMember "observer" "observer@vote.test" []

#guard
  let after := applyVoteEvent legacyThreshold r45Before "a" nonAdminAdmission
  after != r45Before &&
    franchise after == franchise r45Before &&
    lookupQuestion "q" after == lookupQuestion "q" r45Before

example : PreservesQuestionSemantics legacyThreshold r45Before "a"
    nonAdminAdmission "q" := by decide

-- The premise discriminates: a franchise-changing admission, a
-- target-ballot cast, and a standing-changing role change do not satisfy it.
example : ¬ PreservesQuestionSemantics legacyThreshold r45Before "a"
    (.admitMember "x" "x@vote.test" [.adminRole .publicAdmin]) "q" := by decide

example : ¬ PreservesQuestionSemantics legacyThreshold r45Before "b"
    (.cast "q" .assent) "q" := by decide

example : ¬ PreservesQuestionSemantics legacyThreshold r45Before "a"
    (.setRoles "b" []) "q" := by decide

end KelGroups.Vote
