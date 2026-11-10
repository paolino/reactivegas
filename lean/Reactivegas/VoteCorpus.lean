import Lean
import Reactivegas.Invariants

/-!
# Integrated vote corpus (`reactivegas#75`)

The third frozen corpus: signed vote events driven through the production root
`Reactivegas.apply`, over `GroupState State`, so one replayable file carries the
vote machine, the base transitions that move its franchise, and the economic
payload that closures authorize.

Everything is call, not restatement: steps are produced by `Reactivegas.apply`,
the JSON instances are the integrated corpus' own (`Invariants`), and the
journey of `VOTE_TRACES_V1` is lifted onto the production root with the same
question names where they carry over.

## Refusal identity

At the production root every refused vote event is `StepError.rejected`
(`voteApply`), so the identity of the refusal is not observable there. Each step
therefore records `voteError`: the `VoteError` that
`KelGroups.Vote.applyVoteEventChecked` returns on the arguments `voteApply`
receives for that step (canonical view before the step, the payload's votes,
the signer, the vote event). A refusal that never reaches the vote machine (a
signer who is not a member, an `openBound` that cannot be bound) records
`none`. No `VoteError` or `StepError` changes.

## Coverage over discovered extents

Constructor lists of `VoteEvent`, `VoteError`, `ClosureCause` and `Verdict` are
read from the inductive declarations in the Lean environment by `ctorNames%`
when this module elaborates, never written by hand; adding a constructor to any
of them makes `voteCorpusCoversDiscovered` fail until the corpus exhibits it.

This module imports no replay-context code and no exporter.
-/

namespace Reactivegas

deriving instance Lean.ToJson, Lean.FromJson for KelGroups.Vote.VoteError

/-! ## Steps -/

/-- One stored step: the signed event, whether the root accepted it, the
complete `GroupState State` after it, the base change it committed, and the
`VoteError` the vote machine returned when it refused a vote event. -/
structure VoteTraceStep where
  signer : KelGroups.Key
  event : KelGroups.IntegratedEvent Proposal AppEvent
  accepted : Bool
  state : KelGroups.GroupState State
  change : Option KelGroups.BaseChange
  voteError : Option KelGroups.Vote.VoteError
deriving Repr, DecidableEq, BEq, Lean.ToJson, Lean.FromJson

/-- The vote event `voteApply` hands to the vote machine for an app event, if
the app event is vote-routed. `openBound` reaches the machine as an
`openQuestion` only when its target can be bound; the bindings it adds never
touch the votes. -/
def voteEventOfApp (s : State) : AppEvent → Option KelGroups.Vote.VoteEvent
  | .openQuestion qid kind => some (.openQuestion qid kind)
  | .cast qid ballot => some (.cast qid ballot)
  | .renounce qid => some (.renounce qid)
  | .openBound qid kind target =>
      if bindable s qid target then some (.openQuestion qid kind) else none
  | _ => none

/-- The vote machine's refusal for this step, on the exact arguments
`voteApply` receives: only an `.app` event of a current member reaches it. -/
def voteErrorOf (θ : KelGroups.Vote.Threshold) (gs : KelGroups.GroupState State)
    (signer : KelGroups.Key) :
    KelGroups.IntegratedEvent Proposal AppEvent → Option KelGroups.Vote.VoteError
  | .app event =>
      if KelGroups.GroupView.isMember signer (KelGroups.groupView gs) then
        match voteEventOfApp gs.appFold event with
        | some voteEvent =>
            match KelGroups.Vote.applyVoteEventChecked θ (KelGroups.groupView gs)
                gs.appFold.votes signer voteEvent with
            | .error err => some err
            | .ok _ => none
        | none => none
      else none
  | _ => none

def snapshotVoteStep (θ : KelGroups.Vote.Threshold) (gs : KelGroups.GroupState State)
    (signer : KelGroups.Key) (event : KelGroups.IntegratedEvent Proposal AppEvent) :
    VoteTraceStep :=
  match Reactivegas.apply θ probeAuth gs signer event with
  | .ok r =>
      { signer, event, accepted := true, state := r.state, change := r.change
        voteError := none }
  | .error _ =>
      { signer, event, accepted := false, state := gs, change := none
        voteError := voteErrorOf θ gs signer event }

def nextVoteState (θ : KelGroups.Vote.Threshold) (gs : KelGroups.GroupState State)
    (signer : KelGroups.Key) (event : KelGroups.IntegratedEvent Proposal AppEvent) :
    KelGroups.GroupState State :=
  match Reactivegas.apply θ probeAuth gs signer event with
  | .ok r => r.state
  | .error _ => gs

def emitVoteSteps (θ : KelGroups.Vote.Threshold) (gs : KelGroups.GroupState State) :
    List (KelGroups.Key × KelGroups.IntegratedEvent Proposal AppEvent) →
      List VoteTraceStep
  | [] => []
  | signed :: rest =>
      snapshotVoteStep θ gs signed.1 signed.2 ::
        emitVoteSteps θ (nextVoteState θ gs signed.1 signed.2) rest

/-! ## The corpus

Three responsabili (`anna`, `bruno`, `elena`; threshold two under
`legacyThreshold`), one member without a role (`dora`) and a conto/cassa
payload. A fourth responsabile (`carlo`) is admitted directly part-way, so the
franchise size the threshold is applied to grows from three to four after the
first step. Every step is a signed event of `Reactivegas.apply`:

* the `VOTE_TRACES_V1` journey: empty-tally opening, refusals by
  `notResponsabile` / `questionNotFound` / `notDesignee` / `notProposer`, a
  below-threshold assent, a mixed tally, a position switch closing positive, an
  idempotent re-cast, a dissent closing negative, a permission question decided
  by its designee, and the proposer's renounce closing negative;
* a signer who is not a member, and an `openBound` that cannot be bound, as
  refusals that never reach the vote machine;
* bound questions: the closure of a question opened with `openBound` mints an
  authorization that `grantPermission` spends (positive) or `denyPermission`
  spends with a refund (negative);
* base transitions through `propose` / `approve`: the departure of a proposer
  closes their open question negative (`proposerDeparted`), and the departure
  of a voter shrinks the franchise so a question closes by `franchiseChange`
  while a collection with an accepted pledge is open.
-/

def voteCorpusInitial : KelGroups.GroupState State :=
  s62bGroup
    [ s62bMember "anna" [s62bAdmin], s62bMember "bruno" [s62bAdmin]
    , s62bMember "elena" [s62bAdmin], s62bMember "dora" [] ]
    { State.empty with
      conti := [("dora", 100), ("bruno", 60), (comuneId, 0)]
      casse := [("anna", 0)] }

def voteCorpusEvents :
    List (KelGroups.Key × KelGroups.IntegratedEvent Proposal AppEvent) :=
  let app := fun (e : AppEvent) => KelGroups.IntegratedEvent.app e
  [ ("anna", app (.openQuestion "q:olio" .collective))
  , ("dora", app (.cast "q:olio" .assent))
  , ("anna", app (.cast "q:nessuna" .assent))
  , ("anna", app (.cast "q:olio" .assent))
  , ("bruno", app (.cast "q:olio" .dissent))
  , ("bruno", app (.cast "q:olio" .assent))
  , ("anna", app (.openQuestion "q:sconto" .collective))
  , ("elena", app (.cast "q:sconto" .dissent))
  , ("elena", app (.cast "q:sconto" .dissent))
  , ("bruno", app (.cast "q:sconto" .dissent))
  , ("anna", app (.openQuestion "q:incarico" (.permission "bruno")))
  , ("elena", app (.cast "q:incarico" .assent))
  , ("bruno", app (.cast "q:incarico" .assent))
  , ("anna", app (.openQuestion "q:aperta" .collective))
  , ("bruno", app (.openQuestion "q:altra" .collective))
  , ("bruno", app (.cast "q:altra" .assent))
  , ("dora", app (.renounce "q:aperta"))
  , ("elena", app (.renounce "q:nessuna"))
  , ("elena", app (.renounce "q:aperta"))
  , ("anna", app (.renounce "q:aperta"))
  , ("zed", app (.cast "q:altra" .assent))
  , ("anna", app (.openPurchase 1))
  , ("anna", app (.pledge "dora" 1 30))
  , ("anna", app (.acceptPledge "dora" 1))
  , ("anna", app (.openBound "q:fantasma" .collective (.permission 9)))
  , ("anna", app (.openBound "q:acquisto" .collective (.permission 1)))
  , ("bruno", app (.cast "q:acquisto" .assent))
  , ("elena", app (.cast "q:acquisto" .assent))
  , ("anna", app (.grantPermission 1))
  , ("anna", app (.closePurchase 1))
  , ("anna", app (.openPurchase 2))
  , ("anna", app (.pledge "dora" 2 20))
  , ("anna", app (.acceptPledge "dora" 2))
  , ("bruno", app (.openBound "q:rifiuto" .collective (.permission 2)))
  , ("elena", app (.cast "q:rifiuto" .dissent))
  , ("anna", app (.cast "q:rifiuto" .dissent))
  , ("anna", app (.denyPermission 2))
  , ("anna", app (.openPurchase 3))
  , ("anna", app (.pledge "dora" 3 10))
  , ("anna", app (.acceptPledge "dora" 3))
  , ("anna", .direct (.admitMember "carlo" "carlo@s62b" [s62bAdmin]))
  , ("elena", app (.openBound "q:franchise" .collective (.permission 3)))
  , ("carlo", app (.cast "q:franchise" .assent))
  , ("anna", .propose (Proposal.departure "bruno"))
  , ("elena", .approve (proposalDigest (Proposal.departure "bruno")))
  , ("carlo", .approve (proposalDigest (Proposal.departure "bruno")))
  , ("anna", .propose (Proposal.departure "carlo"))
  , ("dora", .approve (proposalDigest (Proposal.departure "carlo")))
  , ("elena", .approve (proposalDigest (Proposal.departure "carlo")))
  , ("carlo", .approve (proposalDigest (Proposal.departure "carlo")))
  , ("anna", app (.openQuestion "q:resto" .collective))
  , ("anna", app (.grantPermission 3))
  ]

/-- The corpus under a threshold policy. Generation binds `legacyThreshold`;
a replayer binds whatever its context table says. -/
def emitVoteCorpus (θ : KelGroups.Vote.Threshold) : List VoteTraceStep :=
  emitVoteSteps θ voteCorpusInitial voteCorpusEvents

/-- Replay stored steps from `gs` under `θ`: the index of the first step whose
recomputation differs from the stored one, or `none` when all agree. -/
def firstVoteMismatch (θ : KelGroups.Vote.Threshold) :
    Nat → KelGroups.GroupState State → List VoteTraceStep → Option Nat
  | _, _, [] => none
  | i, gs, st :: rest =>
      if snapshotVoteStep θ gs st.signer st.event == st then
        firstVoteMismatch θ (i + 1) (nextVoteState θ gs st.signer st.event) rest
      else some i

/-! ## Observables

Each is evaluated over stored steps and the stored initial state, never over the
generator, so the file's own bytes are what the check sees. -/

/-- Each step with the state it was applied to. -/
def withPre (initial : KelGroups.GroupState State) :
    List VoteTraceStep → List (KelGroups.GroupState State × VoteTraceStep)
  | [] => []
  | st :: rest => (initial, st) :: withPre st.state rest

def closedOf (gs : KelGroups.GroupState State) : List KelGroups.Vote.ClosureRecord :=
  gs.appFold.votes.closed

/-- Closure records a step appended (`closed` is append-only). -/
def newClosures (pre post : KelGroups.GroupState State) :
    List KelGroups.Vote.ClosureRecord :=
  (closedOf post).drop (closedOf pre).length

def isVoteRouted : AppEvent → Bool
  | .openQuestion _ _ | .cast _ _ | .renounce _ | .openBound _ _ _ => true
  | _ => false

def appOf : KelGroups.IntegratedEvent Proposal AppEvent → Option AppEvent
  | .app e => some e
  | _ => none

/-- V1: a signed vote event the root accepted through its `.app` route. -/
def voteRouteObservable (steps : List VoteTraceStep) : Bool :=
  steps.any (fun st =>
    st.accepted && match appOf st.event with
      | some e => isVoteRouted e
      | none => false)

/-- A payload is economically active when a balance is nonzero or an open
collection holds an accepted pledge. -/
def economicallyActive (s : State) : Bool :=
  s.conti.any (fun kv => kv.2 != 0) || s.casse.any (fun kv => kv.2 != 0)
    || s.collections.any (fun c => !c.accepted.isEmpty)

def spendsAuth : AppEvent → Bool
  | .grantPermission _ | .denyPermission _ | .backdonate _ => true
  | _ => false

/-- Question ids whose authorization a step minted. -/
def mintedIds (pre post : KelGroups.GroupState State) : List KelGroups.Vote.QuestionId :=
  (post.appFold.live.filter (fun a =>
      !(pre.appFold.live.any (fun b => b.questionId == a.questionId)))).map (·.questionId)

/-- Question ids whose authorization a step consumed. -/
def spentIds (pre post : KelGroups.GroupState State) : List KelGroups.Vote.QuestionId :=
  (pre.appFold.live.filter (fun a =>
      !(post.appFold.live.any (fun b => b.questionId == a.questionId)))).map (·.questionId)

/-- A closure that minted an authorization, and a later accepted app-decided
event that spent exactly that authorization. -/
def mintedAndSpent (pairs : List (KelGroups.GroupState State × VoteTraceStep)) : Nat :=
  (pairs.zipIdx.filter (fun ((pre, st), i) =>
      st.accepted
        && (mintedIds pre st.state).any (fun q =>
          (newClosures pre st.state).any (fun r => r.questionId == q)
            && (pairs.drop (i + 1)).any (fun (pre', st') =>
              st'.accepted
                && (match appOf st'.event with | some e => spendsAuth e | none => false)
                && (spentIds pre' st'.state).contains q)))).length

/-- A negative closure followed by an accepted `denyPermission` that dissolved
its collection and refunded the pledges. -/
def refundOnNegative (pairs : List (KelGroups.GroupState State × VoteTraceStep)) : Nat :=
  (pairs.filter (fun (pre, st) =>
      st.accepted
        && (match appOf st.event with
            | some (.denyPermission c) =>
                pre.appFold.collections.any (fun col =>
                  col.id == c && !(col.accepted ++ col.pending).isEmpty)
                  && !(st.state.appFold.collections.any (fun col => col.id == c))
                  && decide (sumBal pre.appFold.conti < sumBal st.state.appFold.conti)
                  && (spentIds pre st.state).any (fun q =>
                    (closedOf st.state).any (fun r =>
                      r.questionId == q && r.verdict == .negative))
            | _ => false))).length

/-- V2: a closure record and an economic effect of that closure in one file:
an authorization minted by a closure and spent later, and a refund on a negative
closure. -/
def closureWithEconomics (steps : List VoteTraceStep)
    (initial : KelGroups.GroupState State) : Bool :=
  let pairs := withPre initial steps
  decide (0 < mintedAndSpent pairs) && decide (0 < refundOnNegative pairs)

/-- V3: a `franchiseChange` closure at a step whose pre-state payload was
economically active. -/
def franchiseClosureWhileActive (steps : List VoteTraceStep)
    (initial : KelGroups.GroupState State) : Bool :=
  (withPre initial steps).any (fun (pre, st) =>
    st.accepted
      && (newClosures pre st.state).any (fun r => r.cause == .franchiseChange)
      && economicallyActive pre.appFold)

/-- Lifecycle rows (V-5): the proposer's renounce closes negative `renounced`,
and a proposer's departure closes their question negative `proposerDeparted`,
with both records in the final stored state. -/
def lifecycleRows (steps : List VoteTraceStep) (initial : KelGroups.GroupState State) :
    Bool :=
  let pairs := withPre initial steps
  let final := (steps.getLast?.map (·.state)).getD initial
  pairs.any (fun (pre, st) =>
      st.accepted
        && (match appOf st.event with
            | some (.renounce q) =>
                (newClosures pre st.state).any (fun r =>
                  r.questionId == q && r.verdict == .negative && r.cause == .renounced
                    && r.question.proposer == st.signer
                    && (closedOf final).contains r)
            | _ => false))
    && pairs.any (fun (pre, st) =>
      st.accepted
        && (match st.change with
            | some (.memberRemoved k) =>
                (newClosures pre st.state).any (fun r =>
                  r.verdict == .negative && r.cause == .proposerDeparted
                    && r.question.proposer == k && (closedOf final).contains r)
            | _ => false))

/-- Items of the lifted `VOTE_TRACES_V1` journey, each with the evidence that it
appears as steps through the production root. -/
def journeyItems (steps : List VoteTraceStep) (initial : KelGroups.GroupState State) :
    List (String × Bool) :=
  let pairs := withPre initial steps
  let votesOf := fun (gs : KelGroups.GroupState State) => gs.appFold.votes
  let refusedWith := fun (e : KelGroups.Vote.VoteError) (isEvent : AppEvent → Bool) =>
    steps.any (fun st =>
      !st.accepted && st.voteError == some e
        && (match appOf st.event with | some a => isEvent a | none => false))
  let isCast := fun (a : AppEvent) => match a with | .cast _ _ => true | _ => false
  let isRenounce := fun (a : AppEvent) => match a with | .renounce _ => true | _ => false
  [ ("collective question opened with empty tallies",
      pairs.any (fun (_, st) =>
        st.accepted
          && (match appOf st.event with
              | some (.openQuestion q .collective) =>
                  (KelGroups.Vote.lookupQuestion q (votesOf st.state)).any
                    (fun qu => qu.assents.isEmpty && qu.dissents.isEmpty)
              | _ => false)))
  , ("cast by a non-responsabile refused notResponsabile",
      refusedWith .notResponsabile isCast)
  , ("cast on an unknown question refused questionNotFound",
      refusedWith .questionNotFound isCast)
  , ("assent below threshold leaves the question open",
      pairs.any (fun (pre, st) =>
        st.accepted
          && (match appOf st.event with
              | some (.cast q .assent) =>
                  (KelGroups.Vote.lookupQuestion q (votesOf pre)).isSome
                    && (KelGroups.Vote.lookupQuestion q (votesOf st.state)).any
                      (fun qu => qu.assents.contains st.signer)
              | _ => false)))
  , ("one assent and one dissent, still open",
      pairs.any (fun (_, st) =>
        (votesOf st.state).openQuestions.any (fun kv =>
          kv.2.assents.length == 1 && kv.2.dissents.length == 1
            && kv.2.kind == .collective)))
  , ("ballot switch dissent to assent reaches threshold: positive",
      pairs.any (fun (pre, st) =>
        st.accepted
          && (match appOf st.event with
              | some (.cast q .assent) =>
                  (KelGroups.Vote.lookupQuestion q (votesOf pre)).any
                    (fun qu => qu.dissents.contains st.signer)
                    && (newClosures pre st.state).any (fun r =>
                      r.questionId == q && r.verdict == .positive && r.cause == .tally)
              | _ => false)))
  , ("dissent reaches threshold: negative",
      pairs.any (fun (pre, st) =>
        st.accepted
          && (match appOf st.event with
              | some (.cast q .dissent) =>
                  (newClosures pre st.state).any (fun r =>
                    r.questionId == q && r.verdict == .negative && r.cause == .tally
                      && r.question.kind == .collective)
              | _ => false)))
  , ("idempotent re-cast leaves the votes unchanged",
      pairs.any (fun (pre, st) =>
        st.accepted
          && (match appOf st.event with
              | some (.cast q b) =>
                  (KelGroups.Vote.lookupQuestion q (votesOf pre)).any (fun qu =>
                    (match b with
                     | .assent => qu.assents.contains st.signer
                     | .dissent => qu.dissents.contains st.signer))
                    && votesOf pre == votesOf st.state
              | _ => false)))
  , ("permission question: non-designee refused notDesignee",
      refusedWith .notDesignee isCast)
  , ("permission question: designee decides, positive",
      pairs.any (fun (pre, st) =>
        st.accepted
          && (match appOf st.event with
              | some (.cast q .assent) =>
                  (newClosures pre st.state).any (fun r =>
                    r.questionId == q && r.verdict == .positive
                      && r.question.kind == .permission st.signer)
              | _ => false)))
  , ("a question stays open to the end",
      !((votesOf ((steps.getLast?.map (·.state)).getD initial)).openQuestions.isEmpty))
  , ("renounce by a non-responsabile refused notResponsabile",
      refusedWith .notResponsabile isRenounce)
  , ("renounce of an unknown question refused questionNotFound",
      refusedWith .questionNotFound isRenounce)
  , ("renounce by a responsabile who is not the proposer refused notProposer",
      refusedWith .notProposer isRenounce)
  , ("the proposer's renounce closes negative while another question stays open",
      pairs.any (fun (pre, st) =>
        st.accepted
          && (match appOf st.event with
              | some (.renounce q) =>
                  (newClosures pre st.state).any (fun r =>
                    r.questionId == q && r.verdict == .negative && r.cause == .renounced)
                    && !((votesOf st.state).openQuestions.isEmpty)
              | _ => false))) ]

/-- Every journey item present. -/
def journeyLifted (steps : List VoteTraceStep) (initial : KelGroups.GroupState State) :
    Bool :=
  (journeyItems steps initial).all (·.2)

/-- Cross-check that does not use `voteErrorOf`: a refused `openQuestion`, `cast`
or `renounce` of a current member always reaches the vote machine, so it carries
a `voteError`; an accepted step carries none. -/
def refusalIdentityCrossCheck (steps : List VoteTraceStep)
    (initial : KelGroups.GroupState State) : Bool :=
  (withPre initial steps).all (fun (pre, st) =>
    (st.accepted == st.voteError.isNone || !(KelGroups.GroupView.isMember st.signer (KelGroups.groupView pre))
      || !(match appOf st.event with
          | some (.openQuestion _ _) | some (.cast _ _) | some (.renounce _) => true
          | _ => false))
      && (!st.accepted || st.voteError.isNone))

/-- Every step's `voteError`, recomputed from the stored predecessor state under
`θ`, and the invariants of the field: accepted steps carry none; a refused
vote-routed step of a member that reaches the machine carries its refusal. -/
def voteErrorIdentityObserved (θ : KelGroups.Vote.Threshold)
    (steps : List VoteTraceStep) (initial : KelGroups.GroupState State) : Bool :=
  (withPre initial steps).all (fun (pre, st) =>
    (st.voteError == voteErrorOf θ pre st.signer st.event)
      && (!st.accepted || st.voteError.isNone)
      && (st.voteError.isNone || (!st.accepted
            && (match appOf st.event with
                | some e => isVoteRouted e
                | none => false))))
  && refusalIdentityCrossCheck steps initial

/-! ## Coverage over discovered extents -/

open Lean Elab Term in
/-- The constructor names of an inductive type, read from the environment when
the term elaborates. -/
elab "ctorNames% " id:ident : term => do
  let n ← realizeGlobalConstNoOverloadWithInfo id
  match (← getEnv).find? n with
  | some (.inductInfo ii) => return toExpr (ii.ctors.map (fun c => c.getString!))
  | _ => throwError "{n} is not an inductive type"

/-- The extents the corpus must cover, discovered from the declarations. -/
def discoveredExtents : List (String × List String) :=
  [ ("VoteEvent", ctorNames% KelGroups.Vote.VoteEvent)
  , ("VoteError", ctorNames% KelGroups.Vote.VoteError)
  , ("ClosureCause", ctorNames% KelGroups.Vote.ClosureCause)
  , ("Verdict", ctorNames% KelGroups.Vote.Verdict)
  , ("AppEvent.openBound",
      (ctorNames% AppEvent).filter (· == "openBound")) ]

/-- The constructor name a derived `ToJson` writes: a bare string for a
constructor without fields, the single key otherwise. -/
def ctorNameOfJson : Lean.Json → Option String
  | .str s => some s
  | .obj kvs => (kvs.toList.head?).map (·.1)
  | _ => none

def jsonCtorName {α : Type} [Lean.ToJson α] (a : α) : List String :=
  (ctorNameOfJson (Lean.toJson a)).toList

/-- The constructors of each extent the steps exhibit. `Verdict.open` appears as
the verdict of a question still open in a stored state, evaluated under
`legacyThreshold` and the stored state's own canonical view. -/
def seenNames (steps : List VoteTraceStep) (ty : String) : List String :=
  match ty with
  | "VoteEvent" | "AppEvent.openBound" =>
      steps.flatMap (fun st => match appOf st.event with
        | some e => jsonCtorName e
        | none => [])
  | "VoteError" => steps.flatMap (fun st => (st.voteError.map jsonCtorName).getD [])
  | "ClosureCause" =>
      steps.flatMap (fun st => (closedOf st.state).flatMap (fun r => jsonCtorName r.cause))
  | "Verdict" =>
      steps.flatMap (fun st =>
        (closedOf st.state).flatMap (fun r => jsonCtorName r.verdict)
          ++ st.state.appFold.votes.openQuestions.flatMap (fun kv =>
            jsonCtorName (KelGroups.Vote.verdictOf KelGroups.Vote.legacyThreshold
              (KelGroups.groupView st.state) kv.2)))
  | _ => []

/-- Coverage of the given extents by the steps: the error names every missing
constructor. -/
def coverageAgainst (extents : List (String × List String))
    (steps : List VoteTraceStep) : Except String Unit :=
  let missing := extents.filterMap (fun (ty, names) =>
    let seen := seenNames steps ty
    let absent := names.filter (fun n => !seen.contains n)
    if absent.isEmpty then none else some s!"{ty}: {absent}")
  if extents.isEmpty || extents.any (fun e => e.2.isEmpty) then
    .error "coverage: an extent is empty"
  else if missing.isEmpty then .ok ()
  else .error s!"coverage: constructors missing from the corpus: {missing}"

/-- Coverage over the extents discovered from the environment. -/
def voteCorpusCoversDiscovered (steps : List VoteTraceStep) : Except String Unit :=
  coverageAgainst discoveredExtents steps

/-! ## Executed at build time

The corpus meets every observable, and the discovery really reads inductive
declarations (a fixture type), so a coverage check that ignored its extent
argument or a discovery that returned nothing fails the build. -/

private inductive CtorFixture where
  | one
  | two (n : Nat)
  | three

private def isOkE {ε α : Type} : Except ε α → Bool
  | .ok _ => true
  | .error _ => false

#guard (ctorNames% CtorFixture) == ["one", "two", "three"]
#guard (ctorNames% AppEvent).contains "openBound"
#guard voteRouteObservable (emitVoteCorpus KelGroups.Vote.legacyThreshold)
#guard closureWithEconomics (emitVoteCorpus KelGroups.Vote.legacyThreshold) voteCorpusInitial
#guard franchiseClosureWhileActive (emitVoteCorpus KelGroups.Vote.legacyThreshold) voteCorpusInitial
#guard journeyLifted (emitVoteCorpus KelGroups.Vote.legacyThreshold) voteCorpusInitial
#guard lifecycleRows (emitVoteCorpus KelGroups.Vote.legacyThreshold) voteCorpusInitial
#guard voteErrorIdentityObserved KelGroups.Vote.legacyThreshold
  (emitVoteCorpus KelGroups.Vote.legacyThreshold) voteCorpusInitial
#guard isOkE (voteCorpusCoversDiscovered (emitVoteCorpus KelGroups.Vote.legacyThreshold))
#guard !(isOkE (coverageAgainst (discoveredExtents ++ [("ClosureCause", ["unseen"])])
  (emitVoteCorpus KelGroups.Vote.legacyThreshold)))

/-! Each observable fails on a corpus that lacks what it names: a prefix that
stops before the evidence, or a stored refusal identity erased. -/

private def liveCorpus : List VoteTraceStep := emitVoteCorpus KelGroups.Vote.legacyThreshold

#guard !voteRouteObservable []
#guard !closureWithEconomics (liveCorpus.take 36) voteCorpusInitial
#guard !closureWithEconomics (liveCorpus.take 28) voteCorpusInitial
#guard !franchiseClosureWhileActive (liveCorpus.take 49) voteCorpusInitial
#guard !lifecycleRows (liveCorpus.take 19) voteCorpusInitial
#guard !lifecycleRows (liveCorpus.take 45) voteCorpusInitial
#guard !journeyLifted (liveCorpus.take 5) voteCorpusInitial
#guard !voteErrorIdentityObserved KelGroups.Vote.legacyThreshold
  (liveCorpus.map (fun st => { st with voteError := none })) voteCorpusInitial
#guard refusalIdentityCrossCheck liveCorpus voteCorpusInitial
#guard !refusalIdentityCrossCheck (liveCorpus.map (fun st => { st with voteError := none })) voteCorpusInitial
#guard !voteErrorIdentityObserved KelGroups.Vote.legacyThreshold
  (liveCorpus.map (fun st =>
    if st.accepted then { st with voteError := some .notResponsabile } else st))
  voteCorpusInitial


/-- The decoded file: stored initial state and stored steps. -/
def decodeVoteFile (bytes : String) :
    Except String (KelGroups.GroupState State × List VoteTraceStep) := do
  let j ← Lean.Json.parse bytes
  let initial ← j.getObjValAs? (KelGroups.GroupState State) "initial"
  let steps ← j.getObjValAs? (List VoteTraceStep) "steps"
  return (initial, steps)

end Reactivegas
