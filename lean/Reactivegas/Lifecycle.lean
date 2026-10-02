import Reactivegas.Invariants

/-!
# V-5 lifecycle and S-12 refusals (#81) — executed witnesses and inversions

Operator rulings V-5 (a proposer who renounces, or who leaves the group, closes
their question **negatively**, the closure kept with its cause) and S-12 (a
renounce by anyone but the proposer, and a ballot on a permission question by
anyone but its designee, are refused and change nothing).

Every row of the #81 acceptance table is one Bool oracle over a `Machine`: the
checked vote step, the vote-machine departure closure, and the integrated
production root. `production` is the shipped machine; each can-fail mutant is a
`Machine` that differs from production in exactly one named part, and the
*same* oracle is evaluated on both. Both facts are theorems.

Two production paths are exercised by every row:

* the vote machine — `KelGroups.Vote.applyVoteEventChecked` from a payload the
  production fold `foldVote` produced (`foldVoteWith_production` binds the
  checked-step fold used below to `foldVote`), and
  `KelGroups.Vote.closeProposerQuestions` for departure;
* the integrated root `Reactivegas.apply` — `appFold`/`voteApply` for vote
  events, and the committed base transition that runs `baseHook` for departure,
  from aggregates the production `foldIntegrated` produced.

Expected closure records are computed from the fixture's own pre-state by the
production lookup (`lookupQuestion`) and sweep (`sweepStep`); the literals
`.negative`, `.renounced` and `.proposerDeparted` are the rulings themselves.
-/

set_option maxHeartbeats 8000000

namespace Reactivegas.Lifecycle

open KelGroups.Vote (VoteState VoteEvent VoteError Threshold Question
  QuestionId ClosureRecord ClosureCause Verdict lookupQuestion sweepClosures
  sweepStep verdictOf validateVoteEvent effectedState applyVoteEventChecked
  foldVote closeProposerQuestions)

/-! ## The machine under judgement -/

/-- The checked vote step's shape (`applyVoteEventChecked`). -/
abbrev VoteStep :=
  Threshold → KelGroups.GroupView → VoteState → KelGroups.Key → VoteEvent →
    Except VoteError VoteState

/-- The integrated production root's shape at the witnesses' threshold. -/
abbrev Root :=
  KelGroups.GroupState State → KelGroups.Key →
    KelGroups.IntegratedEvent Proposal AppEvent →
      Except ProductionError (KelGroups.IntegratedResult State)

/-- One transition system under judgement: the checked vote step, the vote
machine's departure closure, and the integrated root. -/
structure Machine where
  vote : VoteStep
  depart : KelGroups.Key → VoteState → VoteState
  root : Root

/-- The witnesses' threshold: legacy `maggioranza`, as in S62-B. -/
def θ : Threshold := s62bThreshold

/-- The shipped machine. -/
def production : Machine :=
  { vote := applyVoteEventChecked
    depart := closeProposerQuestions
    root := Reactivegas.apply θ probeAuth }

/-! ## Folds over a machine's own step -/

/-- Fold signed vote events through a checked step from the empty payload; a
refused event leaves the payload as it was. -/
def foldVoteWith (step : VoteStep) (threshold : Threshold)
    (view : KelGroups.GroupView) (events : List (KelGroups.Key × VoteEvent)) :
    VoteState :=
  events.foldl
    (fun gs signed =>
      match step threshold view gs signed.1 signed.2 with
      | .ok next => next
      | .error _ => gs)
    KelGroups.Vote.emptyVoteState

/-- The checked-step fold of the production step *is* the production fold
`foldVote`, so every fold leg below is a `foldVote` witness for `production`. -/
theorem foldVoteWith_production (threshold : Threshold)
    (view : KelGroups.GroupView) (events : List (KelGroups.Key × VoteEvent)) :
    foldVoteWith applyVoteEventChecked threshold view events
      = foldVote threshold view events := by
  have hstep :
      (fun (gs : VoteState) (signed : KelGroups.Key × VoteEvent) =>
        match applyVoteEventChecked threshold view gs signed.1 signed.2 with
        | .ok next => next
        | .error _ => gs)
        = (fun current signed =>
            KelGroups.Vote.applyVoteEvent threshold view current signed.1
              signed.2) := by
    funext gs signed
    unfold applyVoteEventChecked KelGroups.Vote.applyVoteEvent
    rcases validateVoteEvent threshold view gs signed.1 signed.2 with _ | ⟨⟩ <;> rfl
  simp only [foldVoteWith, foldVote, hstep]

/-- A vote event as the integrated app event carrying it. -/
def appOf : VoteEvent → AppEvent
  | .openQuestion questionId kind => .openQuestion questionId kind
  | .cast questionId ballot => .cast questionId ballot
  | .renounce questionId => .renounce questionId

def asIntegrated (trace : List (KelGroups.Key × VoteEvent)) :
    List (KelGroups.Key × KelGroups.IntegratedEvent Proposal AppEvent) :=
  trace.map (fun signed => (signed.1, .app (appOf signed.2)))

/-! ## Fixture: renounce (three responsabili, one plain member)

`legacyThreshold 3 = 2`. `qc` closes negative by tally during the setup, so the
closure log is non-empty before any V-5 closure; `q1` (two ballots) and `q2`
(a permission question addressed to `c`) are `a`'s; `q3` is `b`'s. -/

def v5RenounceGroup : KelGroups.GroupState State :=
  s62bGroup
    [ s62bMember "a" [s62bAdmin], s62bMember "b" [s62bAdmin]
    , s62bMember "c" [s62bAdmin], s62bMember "m" [] ]
    State.empty

def v5RenounceView : KelGroups.GroupView := KelGroups.groupView v5RenounceGroup

def v5RenounceSetup : List (KelGroups.Key × VoteEvent) :=
  [ ("b", .openQuestion "qc" .collective), ("b", .cast "qc" .dissent)
  , ("c", .cast "qc" .dissent)
  , ("a", .openQuestion "q1" .collective), ("b", .cast "q1" .assent)
  , ("c", .cast "q1" .dissent)
  , ("a", .openQuestion "q2" (.permission "c"))
  , ("b", .openQuestion "q3" .collective), ("a", .cast "q3" .assent) ]

/-- Vote-machine pre-state: the production fold over the setup. -/
def v5RenounceVotes : VoteState := foldVote θ v5RenounceView v5RenounceSetup

/-- Integrated pre-state: the production integrated fold over the same setup. -/
def v5RenouncePre : KelGroups.GroupState State :=
  KelGroups.foldIntegrated (integration θ probeAuth) v5RenounceGroup
    (asIntegrated v5RenounceSetup)

/-! ## Fixture: departure (five responsabili)

`legacyThreshold 5 = 3`, `legacyThreshold 4 = 2`. `d` proposed `qd1` (assents
`d`, `a`) and `qd2` (permission, designee `b`, no ballot); `a` proposed `qx`
(assents `a`, `d`); `b` proposed `qy` (one dissent). `qc` closes by tally in the
setup. After `d` leaves, `qd1` and `qx` both cross the four-responsabile
threshold on a stale tally, `qd2` and `qy` do not. -/

def v5DepartGroup : KelGroups.GroupState State :=
  s62bGroup
    [ s62bMember "a" [s62bAdmin], s62bMember "b" [s62bAdmin]
    , s62bMember "c" [s62bAdmin], s62bMember "d" [s62bAdmin]
    , s62bMember "e" [s62bAdmin] ]
    State.empty

def v5DepartView : KelGroups.GroupView := KelGroups.groupView v5DepartGroup

/-- The post-departure canonical view: the same relation without `d`. -/
def v5PostView : KelGroups.GroupView :=
  { members := KelGroups.assocErase "d" v5DepartGroup.members }

def v5DepartSetup : List (KelGroups.Key × VoteEvent) :=
  [ ("e", .openQuestion "qc" .collective), ("b", .cast "qc" .dissent)
  , ("c", .cast "qc" .dissent), ("e", .cast "qc" .dissent)
  , ("d", .openQuestion "qd1" .collective), ("d", .cast "qd1" .assent)
  , ("a", .cast "qd1" .assent)
  , ("d", .openQuestion "qd2" (.permission "b"))
  , ("a", .openQuestion "qx" .collective), ("a", .cast "qx" .assent)
  , ("d", .cast "qx" .assent)
  , ("b", .openQuestion "qy" .collective), ("c", .cast "qy" .dissent) ]

def v5DepartVotes : VoteState := foldVote θ v5DepartView v5DepartSetup

def v5DepartPre : KelGroups.GroupState State :=
  KelGroups.foldIntegrated (integration θ probeAuth) v5DepartGroup
    (asIntegrated v5DepartSetup)

def departD : KelGroups.IntegratedEvent Proposal AppEvent :=
  .propose (Proposal.departure "d")

def approveD : KelGroups.IntegratedEvent Proposal AppEvent :=
  .approve (proposalDigest (Proposal.departure "d"))

/-! ## Expected values, from the fixture's own pre-state -/

/-- A V-5 closure record of `questionId` as it stood in `gs` (D-1). -/
def v5Record (gs : VoteState) (questionId : QuestionId) (cause : ClosureCause) :
    Option ClosureRecord :=
  (lookupQuestion questionId gs).map (fun question =>
    { questionId, question, verdict := Verdict.negative, cause })

/-- The record a closure log holds for `questionId`, if any. -/
def recordOf (gs : VoteState) (questionId : QuestionId) : Option ClosureRecord :=
  gs.closed.find? (fun record => record.questionId == questionId)

/-- `questionId` was open in `pre` and is open with the same value in `post`. -/
def keeps (pre post : VoteState) (questionId : QuestionId) : Bool :=
  (lookupQuestion questionId pre).isSome
    && lookupQuestion questionId post == lookupQuestion questionId pre

/-- The departing member's questions, in pre-state open order (D-2). -/
def leaverIds : List QuestionId :=
  (v5DepartVotes.openQuestions.map Prod.fst).filter
    (fun questionId => questionId == "qd1" || questionId == "qd2")

def departRecords : List ClosureRecord :=
  leaverIds.filterMap (fun questionId =>
    v5Record v5DepartVotes questionId ClosureCause.proposerDeparted)

/-- The unrelated question's closure under the post franchise, computed by the
production sweep step. -/
def qxRecord : Option ClosureRecord :=
  (lookupQuestion "qx" v5DepartVotes).bind (fun question =>
    sweepStep θ v5PostView ("qx", question))

/-! ## Outcomes of each machine on the fixtures -/

/-- `a` renounces their own `q1`, on the vote machine. -/
def renounceVote (m : Machine) : Option VoteState :=
  (m.vote θ v5RenounceView v5RenounceVotes "a" (.renounce "q1")).toOption

/-- The same renounce through the integrated root: a successful app event that
moves nothing but the vote payload. -/
def renounceRoot (m : Machine) : Option VoteState :=
  match m.root v5RenouncePre "a" (.app (.renounce "q1")) with
  | .ok result =>
      if result.change == none
          && result.state.members == v5RenouncePre.members
          && result.state.appFold
              == { v5RenouncePre.appFold with votes := result.state.appFold.votes }
      then some result.state.appFold.votes
      else none
  | .error _ => none

/-- Two renounces by the proposer, then an attempt to re-open the first id,
folded through the machine's own step (`foldVote` for production). -/
def renounceFold (m : Machine) : VoteState :=
  foldVoteWith m.vote θ v5RenounceView
    (v5RenounceSetup ++
      [("a", .renounce "q1"), ("a", .renounce "q2"),
       ("a", .openQuestion "q1" .collective)])

/-- `d`'s departure on the vote machine: the departure closure alone. -/
def departVote (m : Machine) : VoteState := m.depart "d" v5DepartVotes

/-- The vote-machine composition the hook owes a departure: the closure, then
the sweep over the post view. -/
def departVoteSwept (m : Machine) : VoteState :=
  sweepClosures θ v5PostView (departVote m)

/-- The integrated departure: `a` proposes (not an assent), `b` and `c`
approve (two of the three required at five responsabili), `e` approves and
the base change is enacted. Returns the vote payload just before the enacting
transition and the enacting result. -/
def departRoot (m : Machine) :
    Option (VoteState × KelGroups.IntegratedResult State) :=
  match m.root v5DepartPre "a" departD with
  | .error _ => none
  | .ok proposed =>
      match m.root proposed.state "b" approveD with
      | .error _ => none
      | .ok first =>
          match m.root first.state "c" approveD with
          | .error _ => none
          | .ok approved =>
              if first.change == none && approved.change == none then
                match m.root approved.state "e" approveD with
                | .error _ => none
                | .ok enacted => some (approved.state.appFold.votes, enacted)
              else none

/-- The enacting transition's vote payload, when it really is `d`'s departure
committed against the post view. -/
def departRootVotes (m : Machine) : Option VoteState :=
  match departRoot m with
  | some (_, enacted) =>
      if enacted.change == some (KelGroups.BaseChange.memberRemoved "d")
          && KelGroups.groupView enacted.state == v5PostView
      then some enacted.state.appFold.votes
      else none
  | none => none

/-! ## Row oracles — one per acceptance row, each taking the machine -/

private def renounceL1 : Option VoteState → Bool
  | some post =>
      (lookupQuestion "q1" v5RenounceVotes).isSome
        && lookupQuestion "q1" post == none
  | none => false

/-- **L-1**: a `renounce` by the question's proposer removes it from the open
set — on the checked step, on the fold (both renounced ids), and through the
integrated root. -/
def checkL1 (m : Machine) : Bool :=
  renounceL1 (renounceVote m) && renounceL1 (renounceRoot m)
    && lookupQuestion "q1" (renounceFold m) == none
    && lookupQuestion "q2" (renounceFold m) == none

private def departL2 (post : VoteState) : Bool :=
  leaverIds.length == 2
    && leaverIds.all (fun questionId =>
        (lookupQuestion questionId v5DepartVotes).isSome
          && lookupQuestion questionId post == none
          && (recordOf post questionId).isSome)

/-- L-2's vote-machine leg: the departure closure closes the leaver's
questions. -/
def checkL2Vote (m : Machine) : Bool := departL2 (departVote m)

/-- L-2's integrated leg: the closures are in the very transition that commits
`memberRemoved`, and the payload one step earlier still holds them open. -/
def checkL2Root (m : Machine) : Bool :=
  match departRoot m, departRootVotes m with
  | some (before, _), some post => before == v5DepartVotes && departL2 post
  | _, _ => false

/-- **L-2**: removing the proposer from the group closes every open question
they proposed, inside the very transition that commits `memberRemoved` — the
payload one step earlier still holds them open. -/
def checkL2 (m : Machine) : Bool := checkL2Vote m && checkL2Root m

private def verdictIs (post : VoteState) (questionId : QuestionId) : Bool :=
  (recordOf post questionId).map (·.verdict) == some Verdict.negative

/-- **L-3**: both V-5 closures carry `verdict = .negative`. -/
def checkL3 (m : Machine) : Bool :=
  (match renounceVote m, renounceRoot m with
   | some byVote, some byRoot => verdictIs byVote "q1" && verdictIs byRoot "q1"
   | _, _ => false)
    && verdictIs (renounceFold m) "q1" && verdictIs (renounceFold m) "q2"
    && leaverIds.length == 2
    && leaverIds.all (verdictIs (departVote m))
    && (match departRootVotes m with
        | some post => leaverIds.all (verdictIs post)
        | none => false)

private def causeIs (cause : ClosureCause) (post : VoteState)
    (questionId : QuestionId) : Bool :=
  (recordOf post questionId).map (·.cause) == some cause

/-- **L-4**: the causes are exactly `.renounced` (renounce) and
`.proposerDeparted` (departure). -/
def checkL4 (m : Machine) : Bool :=
  (match renounceVote m, renounceRoot m with
   | some byVote, some byRoot =>
       causeIs .renounced byVote "q1" && causeIs .renounced byRoot "q1"
   | _, _ => false)
    && causeIs .renounced (renounceFold m) "q1"
    && causeIs .renounced (renounceFold m) "q2"
    && leaverIds.length == 2
    && leaverIds.all (causeIs .proposerDeparted (departVote m))
    && (match departRootVotes m with
        | some post => leaverIds.all (causeIs .proposerDeparted post)
        | none => false)

private def renounceL5 (pre : VoteState) : Option VoteState → Bool
  | some post =>
      (v5Record pre "q1" .renounced).isSome
        && post.closed == pre.closed ++ (v5Record pre "q1" .renounced).toList
  | none => false

/-- **L-5**: each V-5 closure is *appended* to `VoteState.closed` with the
question as it stood — the earlier log is kept, nothing is discarded, and a
closed id is never revived by re-opening it (D-6). -/
def checkL5 (m : Machine) : Bool :=
  renounceL5 v5RenounceVotes (renounceVote m)
    && renounceL5 v5RenouncePre.appFold.votes (renounceRoot m)
    && (renounceFold m).closed
        == v5RenounceVotes.closed
          ++ (v5Record v5RenounceVotes "q1" .renounced).toList
          ++ (v5Record v5RenounceVotes "q2" .renounced).toList
    && lookupQuestion "q1" (renounceFold m) == none
    && departRecords.length == 2
    && (departVote m).closed == v5DepartVotes.closed ++ departRecords
    && (match departRootVotes m with
        | some post =>
            post.closed.take (v5DepartVotes.closed.length + 2)
              == v5DepartVotes.closed ++ departRecords
        | none => false)

private def renounceL6 (pre : VoteState) : Option VoteState → Bool
  | some post =>
      (recordOf post "q1").isSome
        && keeps pre post "q2"
        && (post.closed.drop pre.closed.length).all
            (fun record => record.questionId == "q1")
  | none => false

private def departL6 (post : VoteState) : Bool :=
  leaverIds.length == 2
    && leaverIds.all (fun questionId => (recordOf post questionId).isSome)
    && (post.closed.drop v5DepartVotes.closed.length).all (fun record =>
    record.cause != .proposerDeparted || record.question.proposer == "d")

/-- **L-6**: the V-5 rule closes no question but the renouncing / departing
proposer's own: a renounce closes exactly the named question (D-4), and a
departure closure touches only the leaver's questions. Each leg first requires
that the V-5 closure fired, so an absent rule cannot satisfy it vacuously. -/
def checkL6 (m : Machine) : Bool :=
  renounceL6 v5RenounceVotes (renounceVote m)
    && renounceL6 v5RenouncePre.appFold.votes (renounceRoot m)
    && keeps v5DepartVotes (departVote m) "qx"
    && keeps v5DepartVotes (departVote m) "qy"
    && departL6 (departVote m)
    && (match departRootVotes m with
        | some post => keeps v5DepartVotes post "qy" && departL6 post
        | none => false)

/-- L-6a's vote-machine leg: the closure-then-sweep composition over the post
view, with the fixture conjuncts showing the fixture discriminates (a sweep
alone would close `qd1` positive on the same stale tally). -/
def checkL6aVote (m : Machine) : Bool :=
  (match lookupQuestion "qd1" v5DepartVotes with
   | some question => verdictOf θ v5PostView question == Verdict.positive
   | none => false)
    && (match qxRecord with
        | some record =>
            record.verdict == Verdict.positive
              && record.cause == ClosureCause.franchiseChange
        | none => false)
    && departRecords.length == 2
    && (departVoteSwept m).closed
        == v5DepartVotes.closed ++ departRecords ++ qxRecord.toList
    && keeps v5DepartVotes (departVoteSwept m) "qy"

/-- L-6a's integrated leg: the one enacting transition carries both closures. -/
def checkL6aRoot (m : Machine) : Bool :=
  match departRootVotes m with
  | some post =>
      departRecords.length == 2
        && post.closed == v5DepartVotes.closed ++ departRecords ++ qxRecord.toList
        && keeps v5DepartVotes post "qy"
        && post.openQuestions.length == 1
  | none => false

/-- **L-6a**: in one departure transition the leaver's questions close
`.negative`/`.proposerDeparted` *and* the unrelated `qx`, crossing its threshold
under the post franchise, closes with `.franchiseChange`; all retained, in D-3
order. -/
def checkL6a (m : Machine) : Bool := checkL6aVote m && checkL6aRoot m

private def renounceL6b (pre : VoteState) : Option VoteState → Bool
  | some post =>
      keeps pre post "q3"
        && (recordOf post "q3").isNone
        && post.openQuestions.length + 1 == pre.openQuestions.length
  | none => false

/-- **L-6b**: a renounce under an unchanged franchise leaves unrelated
questions unaffected. -/
def checkL6b (m : Machine) : Bool :=
  renounceL6b v5RenounceVotes (renounceVote m)
    && renounceL6b v5RenouncePre.appFold.votes (renounceRoot m)
    && keeps v5RenounceVotes (renounceFold m) "q3"

/-- The integrated refusal shape (INV81-REFUSAL-INERT): an error, never a
successful identity. -/
private def rootRefuses (m : Machine) (signer : KelGroups.Key) (event : AppEvent) :
    Bool :=
  match m.root v5RenouncePre signer (.app event) with
  | .error (.integrated (.app StepError.rejected)) => true
  | _ => false

private def voteRefuses (m : Machine) (signer : KelGroups.Key) (event : VoteEvent)
    (error : VoteError) : Bool :=
  match m.vote θ v5RenounceView v5RenounceVotes signer event with
  | .error got => got == error
  | .ok _ => false

private def foldInert (m : Machine) (signer : KelGroups.Key) (event : VoteEvent) :
    Bool :=
  foldVoteWith m.vote θ v5RenounceView (v5RenounceSetup ++ [(signer, event)])
    == v5RenounceVotes

/-- **R-1**: a `renounce` by a responsabile who is not the proposer is refused
with `notProposer`, reaches neither effect nor sweep, and the integrated path
reports an error. The proposer's own renounce of the same question is
admitted (no over-refusal). -/
def checkR1 (m : Machine) : Bool :=
  voteRefuses m "b" (.renounce "q1") .notProposer
    && foldInert m "b" (.renounce "q1")
    && rootRefuses m "b" (.renounce "q1")
    && voteRefuses m "c" (.renounce "q2") .notProposer
    && (renounceVote m).isSome

/-- **R-2**: a `cast` on a permission question by a responsabile who is not its
designee is refused with `notDesignee` and changes nothing; the designee's own
ballot and a non-designee ballot on a collective question are admitted. -/
def checkR2 (m : Machine) : Bool :=
  voteRefuses m "b" (.cast "q2" .assent) .notDesignee
    && voteRefuses m "a" (.cast "q2" .dissent) .notDesignee
    && foldInert m "b" (.cast "q2" .assent)
    && rootRefuses m "b" (.cast "q2" .assent)
    && (match m.vote θ v5RenounceView v5RenounceVotes "c" (.cast "q2" .assent) with
        | .ok post => (recordOf post "q2").map (·.verdict) == some Verdict.positive
        | .error _ => false)
    && (m.vote θ v5RenounceView v5RenounceVotes "b" (.cast "q3" .dissent)).toOption.isSome

/-- **INV81-ORDER**: first-error identity — `notResponsabile`, then
`questionNotFound`, then `notProposer` / `notDesignee`. -/
def checkOrder (m : Machine) : Bool :=
  voteRefuses m "m" (.renounce "q1") .notResponsabile
    && voteRefuses m "x" (.renounce "q1") .notResponsabile
    && voteRefuses m "m" (.renounce "zz") .notResponsabile
    && voteRefuses m "b" (.renounce "zz") .questionNotFound
    && voteRefuses m "m" (.cast "q2" .assent) .notResponsabile
    && voteRefuses m "b" (.cast "zz" .assent) .questionNotFound
    && voteRefuses m "b" (.renounce "q1") .notProposer
    && voteRefuses m "b" (.cast "q2" .assent) .notDesignee
    && rootRefuses m "m" (.renounce "q1")
    && rootRefuses m "m" (.cast "q2" .assent)

/-- **INV81-V3**: the post-base sweep still runs on a departure — the S62-B
franchise-only closure, where the leaver proposed nothing, still closes.
With three responsabili the proposer's signature is not an assent: `dora`'s
approval pends and `eve`'s second assent enacts. -/
def checkV3 (m : Machine) : Bool :=
  let approveEve : KelGroups.IntegratedEvent Proposal AppEvent :=
    .approve (proposalDigest (Proposal.departure "eve"))
  match m.root v3Group "alice" removeEve with
  | .error _ => false
  | .ok proposed =>
      match m.root proposed.state "dora" approveEve with
      | .error _ => false
      | .ok approved =>
          approved.change == none
            && (match m.root approved.state "eve" approveEve with
                | .error _ => false
                | .ok enacted =>
                    enacted.change == some (KelGroups.BaseChange.memberRemoved "eve")
                      && enacted.state.appFold.votes.openQuestions == []
                      && recordOf enacted.state.appFold.votes "q"
                          == sweepStep θ (KelGroups.groupView enacted.state)
                              ("q", v3Question)
                      && (recordOf enacted.state.appFold.votes "q").isSome)

/-! ## Fixture and path agreement (production) -/

/-- The fixtures are what the oracles assume, and the two paths agree on them:
the integrated fold reaches the same vote payload as `foldVote`, members and
economy untouched; proposers are the openers; `qc` closed by tally. -/
def checkFixtures : Bool :=
  v5RenouncePre.appFold.votes == v5RenounceVotes
    && v5RenouncePre.members == v5RenounceGroup.members
    && v5DepartPre.appFold.votes == v5DepartVotes
    && v5DepartPre.members == v5DepartGroup.members
    && (recordOf v5RenounceVotes "qc").map (·.cause) == some ClosureCause.tally
    && (recordOf v5DepartVotes "qc").map (·.cause) == some ClosureCause.tally
    && ["q1", "q2"].all (fun questionId =>
        (lookupQuestion questionId v5RenounceVotes).map (·.proposer) == some "a")
    && (lookupQuestion "q3" v5RenounceVotes).map (·.proposer) == some "b"
    && ["qd1", "qd2"].all (fun questionId =>
        (lookupQuestion questionId v5DepartVotes).map (·.proposer) == some "d")
    && (lookupQuestion "qx" v5DepartVotes).map (·.proposer) == some "a"
    && (lookupQuestion "qy" v5DepartVotes).map (·.proposer) == some "b"
    && v5DepartVotes.openQuestions.length == 4

/-- The two production paths agree on the closure: the integrated renounce's
payload is the checked step's, and the integrated departure's payload is the
vote machine's closure-then-sweep. -/
def checkPathsAgree : Bool :=
  (match renounceVote production, renounceRoot production with
   | some byVote, some byRoot => byVote == byRoot
   | _, _ => false)
    && departRootVotes production == some (departVoteSwept production)

/-- INV81-REFUSAL-INERT on the production integrated fold: folding the refused
events leaves the whole aggregate unchanged. -/
def checkRefusalFoldInert : Bool :=
  [ ("b", AppEvent.renounce "q1"), ("b", AppEvent.cast "q2" .assent)
  , ("m", AppEvent.renounce "q1") ].all (fun signed =>
    KelGroups.foldIntegrated (integration θ probeAuth) v5RenouncePre
        [(signed.1, .app signed.2)]
      == v5RenouncePre)

/-! ## Mutation-only definitions (not production helpers)

Each mutant machine differs from `production` in one named part. The builders
below rebuild the production shape around a substituted part; `checkAssembly`
shows that with the production parts plugged in they *are* production on every
fixture step, so a mutant's rejection is caused by its substitution alone. -/

/-- Mutation-only: close every open entry `select` picks, with the given
verdict and cause, appending records iff `record`. -/
def closeWith (select : QuestionId × Question → Bool) (verdict : Verdict)
    (cause : ClosureCause) (record : Bool) (gs : VoteState) : VoteState :=
  { openQuestions := gs.openQuestions.filter (fun entry => !select entry)
    closed := gs.closed ++
      (if record then
        (gs.openQuestions.filter select).map (fun entry =>
          { questionId := entry.1, question := entry.2, verdict, cause })
      else []) }

/-- Mutation-only: a checked vote step with substituted validation and renounce
effect; every other effect and the sweep are production's. -/
def voteStepWith
    (validate : Threshold → KelGroups.GroupView → VoteState → KelGroups.Key →
      VoteEvent → Except VoteError Unit)
    (renounce : VoteState → KelGroups.Key → QuestionId → VoteState) : VoteStep :=
  fun threshold view gs signer event =>
    match validate threshold view gs signer event with
    | .error err => .error err
    | .ok () =>
        .ok (sweepClosures threshold view
          (match event with
           | .renounce questionId => renounce gs signer questionId
           | other => effectedState gs signer other))

/-- Production's renounce effect, in the builder's shape. -/
def productionRenounce (gs : VoteState) (signer : KelGroups.Key)
    (questionId : QuestionId) : VoteState :=
  effectedState gs signer (.renounce questionId)

abbrev VotesAfter :=
  KelGroups.BaseChange → KelGroups.GroupView → VoteState → VoteState

/-- Mutation-only: the hook's vote half with a substituted departure closure. -/
def votesAfterWith (depart : KelGroups.Key → VoteState → VoteState) : VotesAfter :=
  fun change post votes =>
    sweepClosures θ post
      (match change with
       | .memberAdmitted _ => votes
       | .memberRemoved key => depart key votes
       | .rolesChanged _ => votes)

/-- Mutation-only: an integrated root with a substituted vote step and hook vote
half; economic events, economic cleanup and the boundary are production's. -/
def rootWith (vote : VoteStep) (after : VotesAfter) : Root :=
  fun gs signer event =>
    let integ : KelGroups.Integration State AppEvent Proposal StepError :=
      { integration θ probeAuth with
        appFold := fun signer pre post s e =>
          let viaVote : VoteEvent → Except StepError State := fun ev =>
            match vote θ pre s.votes signer ev with
            | .error _ => .error StepError.rejected
            | .ok votes => .ok { s with votes }
          match e with
          | .openQuestion questionId kind => viaVote (.openQuestion questionId kind)
          | .cast questionId ballot => viaVote (.cast questionId ballot)
          | .renounce questionId => viaVote (.renounce questionId)
          | other => appFold θ probeAuth signer pre post s other
        baseHook := fun change pre post s =>
          match economicCleanup change pre post s with
          | none => .error StepError.rejected
          | some cleaned => .ok { cleaned with votes := after change post s.votes } }
    if productionWellFormed gs then
      match KelGroups.applyIntegratedEvent integ gs signer event with
      | .ok result =>
          if productionWellFormed result.state then .ok result
          else .error ProductionError.comuneReserved
      | .error err => .error (ProductionError.integrated err)
    else .error ProductionError.comuneReserved

def voteMutant (vote : VoteStep) : Machine :=
  { vote, depart := closeProposerQuestions
    root := rootWith vote (votesAfterWith closeProposerQuestions) }

def departMutant (depart : KelGroups.Key → VoteState → VoteState) : Machine :=
  { vote := applyVoteEventChecked, depart
    root := rootWith applyVoteEventChecked (votesAfterWith depart) }

def hookMutant (after : VotesAfter) : Machine :=
  { vote := applyVoteEventChecked, depart := closeProposerQuestions
    root := rootWith applyVoteEventChecked after }

/-! ### Renounce mutants -/

/-- L-1 mutant: renounce leaves the question open (the superseded no-op). -/
def renounceNoop (gs : VoteState) (_ : KelGroups.Key) (_ : QuestionId) :
    VoteState := gs

/-- L-3 mutant: renounce closes `.positive`. -/
def renouncePositive (gs : VoteState) (_ : KelGroups.Key) (questionId : QuestionId) :
    VoteState :=
  closeWith (·.1 == questionId) .positive .renounced true gs

/-- L-4 mutant: renounce records `.tally`. -/
def renounceTally (gs : VoteState) (_ : KelGroups.Key) (questionId : QuestionId) :
    VoteState :=
  closeWith (·.1 == questionId) .negative .tally true gs

/-- L-5 mutant: renounce removes the question and discards the record. -/
def renounceDiscard (gs : VoteState) (_ : KelGroups.Key) (questionId : QuestionId) :
    VoteState :=
  closeWith (·.1 == questionId) .negative .renounced false gs

/-- L-6 mutant: renounce closes every open question. -/
def renounceAll (gs : VoteState) (_ : KelGroups.Key) (_ : QuestionId) :
    VoteState :=
  closeWith (fun _ => true) .negative .renounced true gs

/-- L-6b mutant: a bare renounce also closes the questions other members
proposed. -/
def renounceAlsoUnrelated (gs : VoteState) (signer : KelGroups.Key)
    (questionId : QuestionId) : VoteState :=
  closeWith (fun entry => entry.1 == questionId || entry.2.proposer != signer)
    .negative .renounced true gs

/-- The production-shaped renounce closure, built by the mutation builder. -/
def renounceShaped (gs : VoteState) (_ : KelGroups.Key) (questionId : QuestionId) :
    VoteState :=
  closeWith (·.1 == questionId) .negative .renounced true gs

/-! ### Departure mutants -/

/-- L-2 mutant: departure closes nothing (the superseded hook). -/
def departNone (_ : KelGroups.Key) (gs : VoteState) : VoteState := gs

def departPositive (key : KelGroups.Key) (gs : VoteState) : VoteState :=
  closeWith (·.2.proposer == key) .positive .proposerDeparted true gs

def departTally (key : KelGroups.Key) (gs : VoteState) : VoteState :=
  closeWith (·.2.proposer == key) .negative .tally true gs

def departDiscard (key : KelGroups.Key) (gs : VoteState) : VoteState :=
  closeWith (·.2.proposer == key) .negative .proposerDeparted false gs

def departAll (_ : KelGroups.Key) (gs : VoteState) : VoteState :=
  closeWith (fun _ => true) .negative .proposerDeparted true gs

/-- The production-shaped departure closure, built by the mutation builder. -/
def departShaped (key : KelGroups.Key) (gs : VoteState) : VoteState :=
  closeWith (·.2.proposer == key) .negative .proposerDeparted true gs

/-! ### Hook-order mutants (L-6a, INV81-V3) -/

/-- Sweep first, then the departure closure: the leaver's question that the
stale tally carries is closed by the sweep with the wrong cause. -/
def votesAfterSweepFirst : VotesAfter :=
  fun change post votes =>
    match change with
    | .memberRemoved key => closeProposerQuestions key (sweepClosures θ post votes)
    | .memberAdmitted _ => sweepClosures θ post votes
    | .rolesChanged _ => sweepClosures θ post votes

/-- The sweep narrowed away on departure: only the V-5 closure runs. -/
def votesAfterNoSweepOnDeparture : VotesAfter :=
  fun change post votes =>
    match change with
    | .memberRemoved key => closeProposerQuestions key votes
    | .memberAdmitted _ => sweepClosures θ post votes
    | .rolesChanged _ => sweepClosures θ post votes

/-- Both causes collapsed to one: every record a base change appends is
re-labelled `.proposerDeparted`. -/
def votesAfterCollapsed : VotesAfter :=
  fun change post votes =>
    let after := votesAfterWith closeProposerQuestions change post votes
    { after with
      closed := votes.closed ++
        (after.closed.drop votes.closed.length).map
          (fun record => { record with cause := .proposerDeparted }) }

/-- L-2 "not at all" / L-6a "suppresses the `proposerDeparted` closure", in
the hook only: the departure transition sweeps but runs no V-5 closure. The
vote machine's `depart` stays production's. -/
def votesAfterNoClosure : VotesAfter := votesAfterWith departNone

/-- L-2 "closes on a later step": the vote half of every *later* app event
first closes, as V-5 departures, the open questions of proposers who are no
longer members; the departure transition itself runs no closure. -/
def voteStepClosingAbsentProposers : VoteStep :=
  fun threshold view gs signer event =>
    let absent := (gs.openQuestions.map (·.2.proposer)).eraseDups.filter
      (fun key => !KelGroups.GroupView.isMember key view)
    applyVoteEventChecked threshold view
      (absent.foldl (fun acc key => closeProposerQuestions key acc) gs) signer event

/-! ### Refusal mutants -/

/-- R-1 mutant: any responsabile may renounce an existing question. -/
def validateAnyRenouncer (threshold : Threshold) (view : KelGroups.GroupView)
    (gs : VoteState) (signer : KelGroups.Key) (event : VoteEvent) :
    Except VoteError Unit :=
  match event with
  | .renounce questionId =>
      if !(KelGroups.Vote.isResponsabile signer view) then .error .notResponsabile
      else
        match lookupQuestion questionId gs with
        | some _ => .ok ()
        | none => .error .questionNotFound
  | other => validateVoteEvent threshold view gs signer other

/-- R-2 mutant: any responsabile's ballot on a permission question is recorded. -/
def validateAnyCaster (threshold : Threshold) (view : KelGroups.GroupView)
    (gs : VoteState) (signer : KelGroups.Key) (event : VoteEvent) :
    Except VoteError Unit :=
  match event with
  | .cast questionId _ =>
      if !(KelGroups.Vote.isResponsabile signer view) then .error .notResponsabile
      else
        match lookupQuestion questionId gs with
        | some _ => .ok ()
        | none => .error .questionNotFound
  | other => validateVoteEvent threshold view gs signer other

/-- INV81-ORDER mutant: the proposer / designee identity is judged before
standing. -/
def validateIdentityFirst (threshold : Threshold) (view : KelGroups.GroupView)
    (gs : VoteState) (signer : KelGroups.Key) (event : VoteEvent) :
    Except VoteError Unit :=
  match event with
  | .renounce questionId =>
      match lookupQuestion questionId gs with
      | some question =>
          if signer != question.proposer then .error .notProposer
          else validateVoteEvent threshold view gs signer event
      | none => validateVoteEvent threshold view gs signer event
  | .cast questionId _ =>
      match lookupQuestion questionId gs with
      | some { kind := .permission designee, .. } =>
          if signer != designee then .error .notDesignee
          else validateVoteEvent threshold view gs signer event
      | _ => validateVoteEvent threshold view gs signer event
  | other => validateVoteEvent threshold view gs signer other

/-! ### The mutant machines -/

def mutRenounceNoop : Machine := voteMutant (voteStepWith validateVoteEvent renounceNoop)
def mutRenouncePositive : Machine :=
  voteMutant (voteStepWith validateVoteEvent renouncePositive)
def mutRenounceTally : Machine := voteMutant (voteStepWith validateVoteEvent renounceTally)
def mutRenounceDiscard : Machine :=
  voteMutant (voteStepWith validateVoteEvent renounceDiscard)
def mutRenounceAll : Machine := voteMutant (voteStepWith validateVoteEvent renounceAll)
def mutRenounceAlsoUnrelated : Machine :=
  voteMutant (voteStepWith validateVoteEvent renounceAlsoUnrelated)
def mutDepartNone : Machine := departMutant departNone
def mutDepartPositive : Machine := departMutant departPositive
def mutDepartTally : Machine := departMutant departTally
def mutDepartDiscard : Machine := departMutant departDiscard
def mutDepartAll : Machine := departMutant departAll
def mutSweepFirst : Machine := hookMutant votesAfterSweepFirst
def mutNoSweepOnDeparture : Machine := hookMutant votesAfterNoSweepOnDeparture
def mutCollapsedCauses : Machine := hookMutant votesAfterCollapsed
def mutHookNoClosure : Machine := hookMutant votesAfterNoClosure
/-- The deferred-closure machine: production vote step and departure closure,
an integrated root whose closure arrives one transition late. -/
def mutDeferredClosure : Machine :=
  { vote := applyVoteEventChecked, depart := closeProposerQuestions
    root := rootWith voteStepClosingAbsentProposers votesAfterNoClosure }

/-- Non-vacuity of the deferred mutant: right after the departure `qd2` is
still open, and the next app event (`a` opens `qz`) closes it
`.negative`/`.proposerDeparted` — the closure is late, not absent. -/
def checkDeferredClosesLater : Bool :=
  match departRoot mutDeferredClosure with
  | some (_, enacted) =>
      (lookupQuestion "qd2" enacted.state.appFold.votes).isSome
        && (match mutDeferredClosure.root enacted.state "a"
              (.app (.openQuestion "qz" .collective)) with
            | .ok later =>
                lookupQuestion "qd2" later.state.appFold.votes == none
                  && (recordOf later.state.appFold.votes "qd2").map
                      (fun record => (record.verdict, record.cause))
                    == some (Verdict.negative, ClosureCause.proposerDeparted)
            | .error _ => false)
  | none => false
def mutAnyRenouncer : Machine :=
  voteMutant (voteStepWith validateAnyRenouncer productionRenounce)
def mutAnyCaster : Machine :=
  voteMutant (voteStepWith validateAnyCaster productionRenounce)
def mutIdentityFirst : Machine :=
  voteMutant (voteStepWith validateIdentityFirst productionRenounce)

/-! ### Assembly control -/

private def sameOutcome
    (x y : Except ProductionError (KelGroups.IntegratedResult State)) : Bool :=
  match x, y with
  | .ok a, .ok b => a == b
  | .error e, .error f => e == f
  | _, _ => false

private def sameVote (x y : Except VoteError VoteState) : Bool :=
  match x, y with
  | .ok a, .ok b => a == b
  | .error e, .error f => e == f
  | _, _ => false

/-- With production's parts plugged in, the mutation builders reproduce
production on every fixture step the oracles take, and the production-shaped
closures built by `closeWith` equal the shipped ones. -/
def checkAssembly : Bool :=
  let assembled := rootWith applyVoteEventChecked (votesAfterWith closeProposerQuestions)
  let step := voteStepWith validateVoteEvent productionRenounce
  let roots :
      List (KelGroups.GroupState State × KelGroups.Key ×
        KelGroups.IntegratedEvent Proposal AppEvent) :=
    [ (v5RenouncePre, "a", .app (.renounce "q1"))
    , (v5RenouncePre, "b", .app (.renounce "q1"))
    , (v5RenouncePre, "b", .app (.cast "q2" .assent))
    , (v5RenouncePre, "m", .app (.renounce "q1"))
    , (v5RenouncePre, "c", .app (.cast "q2" .assent))
    , (v5DepartPre, "a", departD) ]
  let votes : List (KelGroups.Key × VoteEvent) :=
    [ ("a", .renounce "q1"), ("b", .renounce "q1"), ("b", .cast "q2" .assent)
    , ("c", .cast "q2" .assent), ("m", .renounce "q1"), ("b", .renounce "zz") ]
  roots.all (fun r => sameOutcome (assembled r.1 r.2.1 r.2.2)
      (Reactivegas.apply θ probeAuth r.1 r.2.1 r.2.2))
    && votes.all (fun v => sameVote (step θ v5RenounceView v5RenounceVotes v.1 v.2)
      (applyVoteEventChecked θ v5RenounceView v5RenounceVotes v.1 v.2))
    && renounceShaped v5RenounceVotes "a" "q1" == productionRenounce v5RenounceVotes "a" "q1"
    && departShaped "d" v5DepartVotes == closeProposerQuestions "d" v5DepartVotes
    && departRoot (hookMutant (votesAfterWith closeProposerQuestions)) == departRoot production

/-! ## The rows, decided -/

theorem fixtures_hold : checkFixtures = true := by decide
theorem paths_agree : checkPathsAgree = true := by decide
theorem refusal_fold_inert : checkRefusalFoldInert = true := by decide
theorem assembly_faithful : checkAssembly = true := by decide

theorem l1_holds : checkL1 production = true := by decide
theorem l2_holds : checkL2 production = true := by decide
theorem l3_holds : checkL3 production = true := by decide
theorem l4_holds : checkL4 production = true := by decide
theorem l5_holds : checkL5 production = true := by decide
theorem l6_holds : checkL6 production = true := by decide
theorem l6a_holds : checkL6a production = true := by decide
theorem l6b_holds : checkL6b production = true := by decide
theorem r1_holds : checkR1 production = true := by decide
theorem r2_holds : checkR2 production = true := by decide
theorem order_holds : checkOrder production = true := by decide
theorem v3_holds : checkV3 production = true := by decide

/-! ## Every mutant, rejected by its row's own oracle (INV81-CANFAIL) -/

theorem l1_renounce_noop_caught : checkL1 mutRenounceNoop = false := by decide
theorem l2_depart_none_caught : checkL2 mutDepartNone = false := by decide
theorem l3_renounce_positive_caught : checkL3 mutRenouncePositive = false := by decide
theorem l3_depart_positive_caught : checkL3 mutDepartPositive = false := by decide
theorem l4_renounce_tally_caught : checkL4 mutRenounceTally = false := by decide
theorem l4_depart_tally_caught : checkL4 mutDepartTally = false := by decide
theorem l5_renounce_discard_caught : checkL5 mutRenounceDiscard = false := by decide
theorem l5_depart_discard_caught : checkL5 mutDepartDiscard = false := by decide
theorem l6_renounce_all_caught : checkL6 mutRenounceAll = false := by decide
theorem l6_depart_all_caught : checkL6 mutDepartAll = false := by decide
theorem l6a_sweep_first_caught : checkL6a mutSweepFirst = false := by decide
theorem l6a_no_sweep_caught : checkL6a mutNoSweepOnDeparture = false := by decide
theorem l6a_collapsed_caught : checkL6a mutCollapsedCauses = false := by decide
theorem l6a_suppress_departed_caught : checkL6a mutHookNoClosure = false := by decide
theorem l2_hook_no_closure_caught : checkL2 mutHookNoClosure = false := by decide
theorem l2_later_step_caught : checkL2 mutDeferredClosure = false := by decide
theorem deferred_mutant_closes_later : checkDeferredClosesLater = true := by decide

/-! ### The departure rows' atomicity is judged by the integrated leg alone

For each hook-only mutant the vote-machine leg of the row oracle still holds:
only the integrated transition can see that the closure is missing, late, out
of order or mislabelled, and it rejects. -/

theorem l2_root_leg_alone_no_closure :
    checkL2Vote mutHookNoClosure = true ∧ checkL2Root mutHookNoClosure = false := by
  decide
theorem l2_root_leg_alone_later_step :
    checkL2Vote mutDeferredClosure = true ∧ checkL2Root mutDeferredClosure = false := by
  decide
theorem l6a_root_leg_alone_suppress_departed :
    checkL6aVote mutHookNoClosure = true ∧ checkL6aRoot mutHookNoClosure = false := by
  decide
theorem l6a_root_leg_alone_suppress_franchise :
    checkL6aVote mutNoSweepOnDeparture = true
      ∧ checkL6aRoot mutNoSweepOnDeparture = false := by
  decide
theorem l6a_root_leg_alone_sweep_first :
    checkL6aVote mutSweepFirst = true ∧ checkL6aRoot mutSweepFirst = false := by
  decide
theorem l6a_root_leg_alone_collapsed :
    checkL6aVote mutCollapsedCauses = true ∧ checkL6aRoot mutCollapsedCauses = false := by
  decide
theorem l6b_also_unrelated_caught : checkL6b mutRenounceAlsoUnrelated = false := by decide
theorem r1_any_renouncer_caught : checkR1 mutAnyRenouncer = false := by decide
theorem r2_any_caster_caught : checkR2 mutAnyCaster = false := by decide
theorem order_identity_first_caught : checkOrder mutIdentityFirst = false := by decide
theorem v3_no_sweep_caught : checkV3 mutNoSweepOnDeparture = false := by decide

end Reactivegas.Lifecycle
