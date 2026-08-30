import KelGroups.Vote.Fold
import KelGroups.Vote.Validate
import KelGroups.Vote.Invariants

/-!
# Required vote machine — executed witnesses

Every *question* witness here is produced by executing the production
fold `foldVote` from the empty payload over a signed question-event
trace, under an explicit canonical `GroupView`. Each trace is checked
admissible by `validateVoteEvent` at every step (R-68, R-69).

The franchise is never seeded by vote-local membership events: the
constructors that could have done so have left the sum (T6222) and had
nowhere to write in any case. Fixtures below are literal `GroupView` values.

The two franchise-change observation witnesses (R-53 and INV-54-FRANCHISE)
evaluate the same payload under an explicit pre-change view and an
explicit post-change view. They demonstrate that a verdict is sensitive
to a canonical franchise change. They do **not** claim those views are
production-reachable in S62-A: a base membership/role transition that
could produce the post view is rejected on the new integrated path
until S62-B/S62-C (T6224 `checkV3BaseReachable`).

The two V-2 witnesses below are consequences of an **unruled** policy
choice (R-48): the operator has deliberately left open whether a tie
passes under `legacyThreshold` and whether `zeroThreshold` passes
everything instantly. They are delivered as executed facts about those
named instances only — never as machine-wide truths — and freezing
either as the product answer is a rejection reason.

Deliberate divergence from legacy, recorded here: legacy seeds a
collection with the proposer; these traces open questions with empty
tallies, so a zero-threshold pass really is a pass with no ballot cast
at all.
-/

namespace KelGroups.Vote

/-! ## Canonical-view fixtures -/

private def adminMember (key : Key) : Key × Member :=
  (key, { key, email := key ++ "@vote.test",
          roles := [Role.adminRole Admin.publicAdmin] })

private def observerMember (key : Key) : Key × Member :=
  (key, { key, email := key ++ "@vote.test", roles := [] })

/-- Canonical view of the listed keys as public-admin responsabili. -/
def viewOf (keys : List Key) : GroupView :=
  { members := keys.map adminMember }

/-- Canonical view mixing admin and non-admin members. -/
def viewOfMixed (admins observers : List Key) : GroupView :=
  { members := admins.map adminMember ++ observers.map observerMember }

def oneAdminView : GroupView := viewOf ["a"]
def threeAdminView : GroupView := viewOf ["a", "b", "c"]
def fourAdminView : GroupView := viewOf ["a", "b", "c", "d"]
def fiveAdminView : GroupView := viewOf ["a", "b", "c", "d", "e"]

/-- Post-departure view for R-53: `a` is gone, four admins remain.
Not claimed production-reachable in S62-A. -/
def fourAdminAfterALeftView : GroupView := viewOf ["b", "c", "d", "e"]

/-- Post-role-change view for INV-54-FRANCHISE: `a` remains a member
without an admin role. Not claimed production-reachable in S62-A. -/
def aLostAdminView : GroupView := viewOfMixed ["b", "c", "d"] ["a"]

/-! ## Trace helpers -/

private def witnessTraceValidFrom (θ : Threshold) (view : GroupView)
    (gs : VoteState) : List (Key × VoteEvent) → Bool
  | [] => true
  | (signer, event) :: rest =>
      (validateVoteEvent θ view gs signer event == Except.ok ()) &&
        witnessTraceValidFrom θ view
          (applyVoteEvent θ view gs signer event) rest

private def witnessTraceValid (θ : Threshold) (view : GroupView)
    (events : List (Key × VoteEvent)) : Bool :=
  witnessTraceValidFrom θ view emptyVoteState events

/-- `openQuestion` issued by `who`. -/
def vOpen (who : Key) (questionId : QuestionId) : Key × VoteEvent :=
  (who, .openQuestion questionId .collective)

/-- `cast` issued by `who`. -/
def vCast (who : Key) (questionId : QuestionId) (ballot : Ballot) :
    Key × VoteEvent :=
  (who, .cast questionId ballot)

/-- The one closure record of a state whose question `questionId` has
closed, if the log holds exactly one record for it. -/
private def soleClosure (gs : VoteState) (questionId : QuestionId) :
    Option ClosureRecord :=
  match gs.closed with
  | [record] => if record.questionId == questionId then some record else none
  | _ => none

/-! ## R-48a — under the legacy policy, a tie passes (unruled consequence) -/

/-- Four responsabili, two assents, no dissent. `legacyThreshold 4 = 2`,
so the second assent reaches the threshold: the question closes positive
with no dissent recorded. Executed through `foldVote`. -/
def tieEvents : List (Key × VoteEvent) :=
  [vOpen "a" "q",
    vCast "a" "q" .assent,
    vCast "b" "q" .assent]

def tiePassesUnderLegacyThreshold : VoteState :=
  foldVote legacyThreshold fourAdminView tieEvents

#guard
  witnessTraceValid legacyThreshold fourAdminView tieEvents &&
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

/-- `zeroThreshold` maps every responsabile count to `0`, so a question
that opens with empty tallies already meets the threshold: it opens and
closes positive in the same `openQuestion` event, with no ballot cast
at all. -/
def zeroEvents : List (Key × VoteEvent) :=
  [vOpen "a" "q"]

def zeroThresholdPassesWithNoBallot : VoteState :=
  foldVote zeroThreshold oneAdminView zeroEvents

#guard
  witnessTraceValid zeroThreshold oneAdminView zeroEvents &&
    lookupQuestion "q" zeroThresholdPassesWithNoBallot == none &&
    (match soleClosure zeroThresholdPassesWithNoBallot "q" with
      | some record =>
          record.verdict == Verdict.positive &&
            record.question.assents == [] && record.question.dissents == []
      | none => false)

/-! ## R-53 — a question can pass because a responsabile left

This slice-local witness evaluates the same open question under an
explicit five-admin view and an explicit four-admin view, isolating the
verdict's sensitivity to the franchise from the transition that changes
it. The production-reachable base transition that actually produces such
a view is `Reactivegas.checkV3BaseReachable` (T6224); the post view here
is a fixture, and says nothing on its own about reachability.
-/

/-- Five responsabili, two assents: under `legacyThreshold 5 = 3` the
question is open. -/
def departureEvents : List (Key × VoteEvent) :=
  [vOpen "a" "q",
    vCast "a" "q" .assent,
    vCast "b" "q" .assent]

def departureOpen : VoteState :=
  foldVote legacyThreshold fiveAdminView departureEvents

/-- Same payload, recomputed against the four-admin post view. `a` is
absent, so `legacyThreshold 4 = 2` carries the stale tally past the
threshold and the closure cause is the franchise route: the deciding
side records a key the current franchise no longer counts. -/
def departureCarriesStaleAssents : VoteState :=
  sweepClosures legacyThreshold fourAdminAfterALeftView departureOpen

#guard
  witnessTraceValid legacyThreshold fiveAdminView departureEvents &&
    lookupQuestion "q" departureOpen != none &&
    isResponsabile "a" fiveAdminView &&
    !isResponsabile "a" fourAdminAfterALeftView &&
    lookupQuestion "q" departureCarriesStaleAssents == none &&
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
after the switch they are counted once, on the assent side, and the
dissent tally is back to zero. Legacy left the other list untouched and
double-counted switchers; this machine cannot. -/
def switchEvents : List (Key × VoteEvent) :=
  [vOpen "a" "q",
    vCast "b" "q" .dissent,
    vCast "b" "q" .assent]

def switchLeavesOneList : VoteState :=
  foldVote legacyThreshold threeAdminView switchEvents

#guard
  witnessTraceValid legacyThreshold threeAdminView switchEvents &&
    (match lookupQuestion "q" switchLeavesOneList with
      | some question => question.assents == ["b"] && question.dissents == []
      | none => false)

/-- Two dissents of three responsabili reach the same threshold: the
question closes negative. The dissent side is first-class; assent is
not the only path to a verdict. -/
def dissentEvents : List (Key × VoteEvent) :=
  [vOpen "a" "q",
    vCast "a" "q" .dissent,
    vCast "b" "q" .dissent]

def dissentReachesThresholdClosesNegative : VoteState :=
  foldVote legacyThreshold threeAdminView dissentEvents

#guard
  witnessTraceValid legacyThreshold threeAdminView dissentEvents &&
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

/-- Re-issuing `openQuestion` for an id that has already closed is a
no-op: the closure log keeps exactly its one record and no open
question revives. Every question ever opened is in exactly one of the
two places. -/
def noReviveEvents : List (Key × VoteEvent) :=
  tieEvents ++ [vOpen "a" "q"]

def decidedQuestionStaysDecided : VoteState :=
  foldVote legacyThreshold fourAdminView noReviveEvents

#guard
  witnessTraceValid legacyThreshold fourAdminView noReviveEvents &&
    lookupQuestion "q" decidedQuestionStaysDecided == none &&
    decidedQuestionStaysDecided.closed.length == 1 &&
    (match soleClosure decidedQuestionStaysDecided "q" with
      | some record => record.verdict == Verdict.positive
      | none => false)

/-! ## Admissibility point tests -/

/-- A state with one responsabile and one open collective question,
produced by the production fold under an explicit one-admin view. -/
def votePointState : VoteState :=
  foldVote legacyThreshold oneAdminView [vOpen "a" "q"]

-- A cast by a non-member is rejected with the franchise error (R-44).
#guard
  validateVoteEvent legacyThreshold oneAdminView votePointState
    "stranger" (.cast "q" .assent) ==
    Except.error VoteError.notResponsabile

-- A cast by a member without an admin role is rejected with the same
-- franchise error (R-44): the franchise tracks admin roles, not
-- membership. The mixed view is a fixture, not a mid-fold mutation.
#guard
  validateVoteEvent legacyThreshold (viewOfMixed ["a"] ["b"])
    votePointState "b" (.cast "q" .assent) ==
      Except.error VoteError.notResponsabile

-- A question opening by a non-responsabile is rejected (R-45: no path
-- by which a non-responsabile influences the machine).
#guard
  validateVoteEvent legacyThreshold oneAdminView votePointState "stranger"
    (.openQuestion "r" .collective) == Except.error VoteError.notResponsabile

-- R-45 on the production fold: a non-responsabile opening is a complete
-- no-op, including under `zeroThreshold` where a successful empty-tally
-- open would close positive. Auditor instrument nonresponsabile-open.lean
-- (sha256 1f7aa80a) against 757dac98 is the complementary mutant.
#guard
  let gs := foldVote zeroThreshold oneAdminView
    [("stranger", .openQuestion "q" .collective)]
  lookupQuestion "q" gs == none && gs.closed == []

-- A cast on an unknown question id is rejected with the lookup error.
#guard
  validateVoteEvent legacyThreshold oneAdminView votePointState "a"
    (.cast "missing" .assent) == Except.error VoteError.questionNotFound

-- An admissible cast is accepted.
#guard
  validateVoteEvent legacyThreshold oneAdminView votePointState "a"
    (.cast "q" .assent) == Except.ok ()

/-- INV-54-FRANCHISE: after losing standing, a recast cannot switch
position. Four responsabili so that one assent stays open after the
caster drops admin (`legacyThreshold 4 = 2`). The post view is an
explicit fixture: S62-A has no production route that writes it. -/
def lostStandingEvents : List (Key × VoteEvent) :=
  [vOpen "a" "q",
    vCast "a" "q" .assent]

def lostStandingOpen : VoteState :=
  foldVote legacyThreshold fourAdminView lostStandingEvents

#guard
  witnessTraceValid legacyThreshold fourAdminView lostStandingEvents &&
    (match lookupQuestion "q"
        (applyVoteEvent legacyThreshold aLostAdminView lostStandingOpen
          "a" (.cast "q" .dissent)) with
      | some question => question.assents == ["a"] && question.dissents == []
      | none => false)

-- Renunciation's proposer-only restriction is Slice B (R-58); in this
-- slice an existing-question renounce by a responsabile validates and
-- folds to no effect. A non-responsabile renounce is inside the
-- universal rejection class (see the R-45 section below).
#guard
  validateVoteEvent legacyThreshold oneAdminView votePointState "a"
    (.renounce "q") == Except.ok ()

/-! ## R57-04 universal class — question events rejected and inert after
bootstrap (T5712)

The retired member/role events are not merely outside this class: they have
left the sum (T6222), so the three question constructors below are the whole
of it.
-/

/-- The three current question constructors, signed by a non-responsabile,
from a production-reachable bootstrapped payload (one responsabile, one
open question). -/
def strangerRejectedQuestionEvents : List VoteEvent :=
  [.openQuestion "r" .collective,
    .cast "q" .assent,
    .renounce "q"]

#guard
  strangerRejectedQuestionEvents.all (fun event =>
    validateVoteEvent legacyThreshold oneAdminView votePointState
        "stranger" event == Except.error VoteError.notResponsabile &&
      applyVoteEvent legacyThreshold oneAdminView votePointState
        "stranger" event == votePointState)

/-! ## R57-05 — the retained R-45 oracle on the production fold -/

/-- Three responsabili, question `q` open on one assent. A stranger's cast is
refused with `notResponsabile` and leaves the entire payload unchanged: no
tally, no threshold drop, no closure, no verdict. The franchise is read from
the canonical view, which no vote event can write, and there is no membership
constructor left for a stranger to reach for either. -/
def r45PreEvents : List (Key × VoteEvent) :=
  [vOpen "a" "q", vCast "a" "q" .assent]

def r45Before : VoteState :=
  foldVote legacyThreshold threeAdminView r45PreEvents

def r45After : VoteState :=
  applyVoteEvent legacyThreshold threeAdminView r45Before
    "stranger" (.cast "q" .assent)

#guard
  franchiseSize threeAdminView == 3 &&
    isResponsabile "stranger" threeAdminView == false &&
    validateVoteEvent legacyThreshold threeAdminView r45Before
      "stranger" (.cast "q" .assent) ==
        Except.error VoteError.notResponsabile &&
    r45After == r45Before

/-! ## R57-07 — the semantic no-expiry premise

The franchise and proposer-standing conjuncts this premise carried while the
vote payload owned membership are gone: a vote event cannot move the franchise,
and there is no member/role event left to move it with. The observation is now
"the target question keeps its exact value". A question event that touches
another question satisfies the premise; a target-ballot cast fails it.
-/

example : PreservesQuestionSemantics legacyThreshold threeAdminView
    r45Before "a" (.openQuestion "other" .collective) "q" := by decide

example : PreservesQuestionSemantics legacyThreshold threeAdminView
    r45Before "a" (.renounce "q") "q" := by decide

example : ¬ PreservesQuestionSemantics legacyThreshold threeAdminView
    r45Before "b" (.cast "q" .assent) "q" := by decide

example : PreservesQuestionSemantics legacyThreshold threeAdminView
    r45Before "stranger" (.cast "q" .dissent) "q" := by decide

end KelGroups.Vote
