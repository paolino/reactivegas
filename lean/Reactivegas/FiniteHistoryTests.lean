import Reactivegas.FiniteHistory

/-!
# Finite-history controls (`reactivegas#66`, slice S5-C)

Positive controls and the named negative controls of the two validators of
`Reactivegas.FiniteHistory`. Every history validated here is produced by
`Reactivegas.apply` or `emitTrace` at elaboration time; nothing a control
compares against is typed in. Each planted defect is applied to a history that
the same file shows is validated, and is refused for its own reason at the step
it was planted in.
-/

namespace Reactivegas

/-! ## Genesis controls -/

def fhAdmin (key : KelGroups.Key) : KelGroups.Member :=
  { key, email := key ++ "@finite-history",
    roles := [KelGroups.Role.adminRole KelGroups.Admin.publicAdmin] }

def fhMember (key : KelGroups.Key) : KelGroups.Member :=
  { key, email := key ++ "@finite-history", roles := [] }

def fhFounders : List (KelGroups.Key × KelGroups.Member) :=
  [ ("alice", fhAdmin "alice"), ("bob", fhMember "bob") ]

/-- The founding aggregate: `boot`'s own output. -/
def fhGenesis : KelGroups.GroupState State :=
  match Reactivegas.boot fhFounders State.empty with
  | some gs => gs
  | none => { members := [], pendingProposals := [], pendingBase := [], appFold := State.empty }

#guard (Reactivegas.boot fhFounders State.empty).isSome
#guard genesisRefusal fhGenesis = none

/-- Control: `comuneId` among the members. -/
def fhComuneMember : KelGroups.GroupState State :=
  { fhGenesis with members := (comuneId, fhAdmin comuneId) :: fhGenesis.members }

/-- Control: a non-empty `live`. -/
def fhLive : KelGroups.GroupState State :=
  { fhGenesis with appFold := { fhGenesis.appFold with
      live := [{ questionId := "forged", target := .permission 1, verdict := .positive }] } }

/-- Control: a non-empty `bindings`. -/
def fhBindings : KelGroups.GroupState State :=
  { fhGenesis with appFold := { fhGenesis.appFold with
      bindings := [("forged", .backdonation 5)] } }

/-- Control: a non-empty closed-vote record. -/
def fhClosed : KelGroups.GroupState State :=
  { fhGenesis with appFold := { fhGenesis.appFold with
      votes := { fhGenesis.appFold.votes with
        closed := [{ questionId := "forged"
                     question := { kind := .collective, proposer := "alice"
                                   assents := [], dissents := [] }
                     verdict := .positive, cause := .tally }] } } }

/-- Control: a pending proposal. -/
def fhPendingProposal : KelGroups.GroupState State :=
  { fhGenesis with pendingProposals :=
      [("depart:bob", { proposal := .removeMember "bob", proposer := "alice", approvals := [] })] }

/-- Control: a pending base mutation. -/
def fhPendingBase : KelGroups.GroupState State :=
  { fhGenesis with pendingBase :=
      [("depart:bob", { mutation := .removeMember "bob", proposer := "alice", approvals := [] })] }

#guard genesisRefusal fhComuneMember = some .comuneReserved
#guard genesisRefusal fhLive = some .payloadNotClean
#guard genesisRefusal fhBindings = some .payloadNotClean
#guard genesisRefusal fhClosed = some .payloadNotClean
#guard genesisRefusal fhPendingProposal = some .notBootOutput
#guard genesisRefusal fhPendingBase = some .notBootOutput

/-! ## Integrated history controls -/

/-- The history a run of `events` leaves, recorded from `Reactivegas.apply`
itself: the stored state of a refused step is its predecessor. -/
def fhRecord (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth) :
    KelGroups.GroupState State →
      List (KelGroups.Key × KelGroups.IntegratedEvent Proposal AppEvent) →
      List HistoryStep
  | _, [] => []
  | gs, (signer, event) :: rest =>
      match Reactivegas.apply θ auth gs signer event with
      | .ok r =>
          { signer, event, accepted := true, state := r.state, change := r.change }
            :: fhRecord θ auth r.state rest
      | .error _ =>
          { signer, event, accepted := false, state := gs, change := none }
            :: fhRecord θ auth gs rest

def fhEvents : List (KelGroups.Key × KelGroups.IntegratedEvent Proposal AppEvent) :=
  let app := fun (e : AppEvent) => KelGroups.IntegratedEvent.app e
  [ ("alice", app (.openPurchase 1))
  , ("alice", app (.openPurchase 1))
  , ("alice", app (.deposit "bob" 40))
  , ("bob", app (.pledge "bob" 1 30))
  , ("alice", app (.grantPermission 9))
  , ("alice", app (.acceptPledge "bob" 1))
  , ("alice", .propose (Proposal.departure "bob")) ]

def fhHistory : History :=
  { initial := fhGenesis
    steps := fhRecord KelGroups.Vote.legacyThreshold probeAuth fhGenesis fhEvents }

/-- Replace the element at index `i`. -/
def setAt {α : Type} (i : Nat) (f : α → α) : List α → List α
  | [] => []
  | x :: xs => if i = 0 then f x :: xs else x :: setAt (i - 1) f xs

/-- Plant `f` in step `i` of the recorded history. -/
def fhPlant (i : Nat) (f : HistoryStep → HistoryStep) : History :=
  { fhHistory with steps := setAt i f fhHistory.steps }

def fhValidated : Option HistoryRefusal :=
  historyRefusal KelGroups.Vote.legacyThreshold probeAuth fhHistory

def fhRefusalOf (h : History) : Option HistoryRefusal :=
  historyRefusal KelGroups.Vote.legacyThreshold probeAuth h

-- The fixture has accepted and refused steps, several of each.
#guard fhHistory.steps.length = 7
#guard (fhHistory.steps.filter (·.accepted)).length ≥ 3
#guard (fhHistory.steps.filter (fun st => !st.accepted)).length ≥ 2
-- First accepted and refused step, read from the recorded history.
#guard (fhHistory.steps.findIdx? (·.accepted)) = some 0
#guard (fhHistory.steps.findIdx? (fun st => !st.accepted)) = some 1

-- Positive control: the recorded history is validated.
#guard fhValidated = none

-- Planted post-state on an accepted step (the third accepted one).
#guard fhRefusalOf (fhPlant 3 (fun st =>
    { st with state := { st.state with members := st.state.members.tail } }))
  = some (.postStateDiffers 3)

-- Flipped `accepted`, both directions.
#guard fhRefusalOf (fhPlant 2 (fun st => { st with accepted := false }))
  = some (.appliedButStoredRefused 2)
#guard fhRefusalOf (fhPlant 4 (fun st => { st with accepted := true }))
  = some (.refusedButStoredAccepted 4)

-- Planted base change on an accepted step; planted state and base change on a
-- refused step.
#guard fhRefusalOf (fhPlant 5 (fun st => { st with change := some (.memberRemoved "bob") }))
  = some (.changeDiffers 5)
#guard fhRefusalOf (fhPlant 1 (fun st =>
    { st with state := { st.state with members := st.state.members.tail } }))
  = some (.refusedStepMoved 1)
#guard fhRefusalOf (fhPlant 4 (fun st => { st with change := some (.memberRemoved "bob") }))
  = some (.refusedStepChange 4)

-- A non-genesis initial aggregate is refused before any step is recomputed.
#guard fhRefusalOf { fhHistory with initial := fhPendingProposal } = some .notBootOutput
#guard fhRefusalOf { fhHistory with initial := fhLive } = some .payloadNotClean
#guard fhRefusalOf { fhHistory with initial := fhComuneMember } = some .comuneReserved

/-! ## Fixed-view controls -/

def fhTrace : Trace := seedCorpus.getD 3 { schema := "", version := 0, initial := State.empty, steps := [] }

def fhTraceRefusalOf (t : Trace) : Option TraceRefusal :=
  traceRefusal seedView seedAuth t

#guard fhTrace.steps.length = 7
#guard (seedCorpus.map (fun t => traceRefusal seedView seedAuth t)) = [none, none, none, none, none]
#guard (fhTrace.steps.filter (fun st => match st.result with | .refused _ => true | .applied _ => false)).length ≥ 1

/-- The index of a refused step of the trace. -/
def fhRefusedIdx : Option Nat :=
  fhTrace.steps.findIdx? (fun st => match st.result with | .refused _ => true | .applied _ => false)

def fhTracePlant (i : Nat) (f : TraceStep → TraceStep) : Trace :=
  { fhTrace with steps := setAt i f fhTrace.steps }

-- Non-empty initial state: `Trace.initial` may be arbitrary, `Reach.boot` may not.
#guard fhTraceRefusalOf
    { fhTrace with initial := { State.empty with conti := [("1", 5)] } } = some .initialNotEmpty
-- A view containing `comuneId`.
#guard traceRefusal { seedView with members := (comuneId, fhAdmin comuneId) :: seedView.members }
    seedAuth fhTrace = some .comuneInView
-- Planted continuity, post-state, flipped result, claim.
#guard fhTraceRefusalOf (fhTracePlant 2 (fun st => { st with input := State.empty }))
  = some (.inputDiffers 2)
#guard fhTraceRefusalOf (fhTracePlant 3 (fun st =>
    { st with result := .applied { State.empty with conti := [("1", 5)] } }))
  = some (.postStateDiffers 3)
#guard fhTraceRefusalOf (fhTracePlant 0 (fun st =>
    { st with result := .refused (guardClaim (guardOf st.event)) }))
  = some (.storedRefusedButApplied 0)
#guard fhRefusedIdx.map (fun i => fhTraceRefusalOf (fhTracePlant i (fun st =>
    { st with result := .applied st.input }))) = fhRefusedIdx.map (fun i => some (.storedAppliedButRefused i))
#guard fhRefusedIdx.map (fun i => fhTraceRefusalOf (fhTracePlant i (fun st =>
    { st with result := .refused (guardClaim (if guardOf st.event = .donate then .deposit else .donate)) })))
  = fhRefusedIdx.map (fun i => some (.claimDiffers i))

end Reactivegas
