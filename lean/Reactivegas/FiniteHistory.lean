import Reactivegas.Trace

/-!
# Finite-history correspondence (`reactivegas#66`, slice S5-C)

Ruling RG-S4-REACH-20260906: a finite replay and `Reach` are not the same
object unconditionally. Nothing here decides whether a state is reachable. Every
definition validates **one supplied history**; validating a history of a state
never stands for deciding that no other history of it exists, or that a state
without a validated history is unreachable.

Two validators, over two histories, with the genesis and view premises checked
by the validator rather than assumed by a theorem:

* the integrated validator `historyRefusal` validates a supplied history of
  `Reactivegas.apply` steps: its initial aggregate must be exactly the output of
  `Reactivegas.boot` on that aggregate's members and payload;
* the fixed-view validator `traceRefusal` validates a supplied `Trace` of
  `stepEvent` steps under one view: its initial state must be `State.empty` and
  its view must not contain `comuneId`.

An integrated history can change the view, so no bridge from the first to
`Reach` is claimed.

This module is imported by no module of the corpus exporter's import closure.
-/

namespace Reactivegas

/-! ## Integrated histories -/

/-- One stored step of a supplied integrated history: the signed event, whether
it was accepted, the complete aggregate stored after it and the base change
stored with it. -/
structure HistoryStep where
  signer : KelGroups.Key
  event : KelGroups.IntegratedEvent Proposal AppEvent
  accepted : Bool
  state : KelGroups.GroupState State
  change : Option KelGroups.BaseChange
deriving DecidableEq, Repr

/-- A supplied integrated history: an initial aggregate and signed steps. -/
structure History where
  initial : KelGroups.GroupState State
  steps : List HistoryStep
deriving DecidableEq, Repr

/-- The first reason the validator refuses a supplied integrated history. -/
inductive HistoryRefusal where
  | comuneReserved
  | payloadNotClean
  | notBootOutput
  | appliedButStoredRefused (step : Nat)
  | refusedButStoredAccepted (step : Nat)
  | postStateDiffers (step : Nat)
  | changeDiffers (step : Nat)
  | refusedStepMoved (step : Nat)
  | refusedStepChange (step : Nat)
deriving DecidableEq, Repr

/-- Why the supplied initial aggregate is not exactly `boot`'s output on its own
members and payload; `none` when it is. Validation of a supplied aggregate, not
a decision about any other. -/
def genesisRefusal (_gs : KelGroups.GroupState State) : Option HistoryRefusal :=
  none

/-- Why one stored step disagrees with `Reactivegas.apply` from the stored state
before it; `none` when it agrees. -/
def stepRefusal (_θ : KelGroups.Vote.Threshold) (_auth : BackdonateAuth) (_i : Nat)
    (_gs : KelGroups.GroupState State) (_st : HistoryStep) : Option HistoryRefusal :=
  none

/-- The first step, in order, that disagrees with `Reactivegas.apply` when each
step is recomputed from the state the previous step stored. -/
def stepsRefusal (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth) :
    Nat → KelGroups.GroupState State → List HistoryStep → Option HistoryRefusal
  | _, _, [] => none
  | i, gs, st :: rest =>
      match stepRefusal θ auth i gs st with
      | some r => some r
      | none => stepsRefusal θ auth (i + 1) st.state rest

/-- Validation of a supplied integrated history: the genesis check, then every
step recomputed. `none` means the supplied history was validated; it says
nothing about any state that has no supplied history. -/
def historyRefusal (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth)
    (h : History) : Option HistoryRefusal :=
  match genesisRefusal h.initial with
  | some r => some r
  | none => stepsRefusal θ auth 0 h.initial h.steps

/-- The stored aggregates of a supplied history: the initial one, then each
step's stored post-state. -/
def storedStates (h : History) : List (KelGroups.GroupState State) :=
  h.initial :: h.steps.map (·.state)

/-- Every step with the stored aggregate before it. -/
def stepPairs : KelGroups.GroupState State → List HistoryStep →
    List (KelGroups.GroupState State × HistoryStep)
  | _, [] => []
  | gs, st :: rest => (gs, st) :: stepPairs st.state rest

/-- The aggregate a history ends on, given the aggregate it starts from. -/
def finalFrom : KelGroups.GroupState State → List HistoryStep →
    KelGroups.GroupState State
  | gs, [] => gs
  | _, st :: rest => finalFrom st.state rest

/-- The always-refuse authorization the replay sidecar names refuses every
backdonate. -/
theorem probeAuth_refuses (s : State) (w : Int) : probeAuth s w = false := by
  sorry

/-- Genesis is checked, not assumed: the validator's genesis check passes
exactly when the supplied aggregate is `boot`'s output on its own members and
payload. -/
theorem genesisRefusal_none_iff (gs : KelGroups.GroupState State) :
    genesisRefusal gs = none ↔ Reactivegas.boot gs.members gs.appFold = some gs := by
  sorry

/-- A validated history starts from `boot`'s output. -/
theorem validated_genesis (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth)
    (h : History) (hv : historyRefusal θ auth h = none) :
    Reactivegas.boot h.initial.members h.initial.appFold = some h.initial := by
  sorry

/-- For every threshold, authorization, initial aggregate and steps: every
aggregate a validated history stores, the initial one included, is a production
history. -/
theorem validated_is_production (θ : KelGroups.Vote.Threshold)
    (auth : BackdonateAuth) (h : History) (hv : historyRefusal θ auth h = none) :
    ∀ gs ∈ storedStates h, ProductionHistory θ auth gs := by
  sorry

/-- In a validated history a step stored as refused keeps the stored state of
its predecessor, stores no base change, and `apply` returns an error on that
predecessor, signer and event; a step stored as accepted stores exactly the
state and base change `apply` returns. -/
theorem validated_step_outcome (θ : KelGroups.Vote.Threshold)
    (auth : BackdonateAuth) (h : History) (hv : historyRefusal θ auth h = none) :
    ∀ p ∈ stepPairs h.initial h.steps,
      (p.2.accepted = false →
        p.2.state = p.1 ∧ p.2.change = none ∧
          ∃ e, Reactivegas.apply θ auth p.1 p.2.signer p.2.event = .error e) ∧
      (p.2.accepted = true →
        ∃ r, Reactivegas.apply θ auth p.1 p.2.signer p.2.event = .ok r ∧
          p.2.state = r.state ∧ p.2.change = r.change) := by
  sorry

/-- Every aggregate that is a production history is the final stored state of
some supplied history the validator accepts, under the same threshold and
authorization. -/
theorem production_has_validated_witness (θ : KelGroups.Vote.Threshold)
    (auth : BackdonateAuth) {gs : KelGroups.GroupState State}
    (hp : ProductionHistory θ auth gs) :
    ∃ h : History, historyRefusal θ auth h = none ∧ finalFrom h.initial h.steps = gs := by
  sorry

/-! ## Fixed-view histories -/

/-- The first reason the validator refuses a supplied fixed-view trace. -/
inductive TraceRefusal where
  | comuneInView
  | initialNotEmpty
  | inputDiffers (step : Nat)
  | storedAppliedButRefused (step : Nat)
  | storedRefusedButApplied (step : Nat)
  | postStateDiffers (step : Nat)
  | claimDiffers (step : Nat)
deriving DecidableEq, Repr

/-- The state after a stored step: the stored post-state of an applied step, the
unchanged predecessor of a refused one. -/
def traceNext (cur : State) (st : TraceStep) : State :=
  match st.result with
  | .applied s => s
  | .refused _ => cur

/-- Why one stored step disagrees with `stepDetailed` under the view from the
state before it; `none` when it agrees. -/
def traceStepRefusal (_view : KelGroups.GroupView) (_auth : BackdonateAuth) (_i : Nat)
    (_cur : State) (_st : TraceStep) : Option TraceRefusal :=
  none

/-- The first step, in order, that disagrees with `stepDetailed` when each step
is recomputed from the state the previous step left. -/
def traceStepsRefusal (view : KelGroups.GroupView) (auth : BackdonateAuth) :
    Nat → State → List TraceStep → Option TraceRefusal
  | _, _, [] => none
  | i, cur, st :: rest =>
      match traceStepRefusal view auth i cur st with
      | some r => some r
      | none => traceStepsRefusal view auth (i + 1) (traceNext cur st) rest

/-- Validation of a supplied fixed-view trace: the view must not contain
`comuneId`, the initial state must be `State.empty`, then every step is
recomputed. `Trace.initial` may be arbitrary, `Reach.boot` may not. -/
def traceRefusal (view : KelGroups.GroupView) (auth : BackdonateAuth)
    (t : Trace) : Option TraceRefusal :=
  traceStepsRefusal view auth 0 t.initial t.steps

/-- The states a supplied trace passes through: the initial one, then the state
after each step. -/
def traceStates : State → List TraceStep → List State
  | cur, [] => [cur]
  | cur, st :: rest => cur :: traceStates (traceNext cur st) rest

/-- Every step with the state before it. -/
def tracePairs : State → List TraceStep → List (State × TraceStep)
  | _, [] => []
  | cur, st :: rest => (cur, st) :: tracePairs (traceNext cur st) rest

/-- The state a trace ends on, given the state it starts from. -/
def traceFinal : State → List TraceStep → State
  | cur, [] => cur
  | cur, st :: rest => traceFinal (traceNext cur st) rest

/-- For every view and authorization: a validated trace passes only through
states satisfying `Reach view auth`, and every step is the `stepEvent` outcome
from the state before it. -/
theorem fixedView_validated_reaches (view : KelGroups.GroupView)
    (auth : BackdonateAuth) (t : Trace) (hv : traceRefusal view auth t = none) :
    (∀ s ∈ traceStates t.initial t.steps, Reach view auth s) ∧
    (∀ p ∈ tracePairs t.initial t.steps, p.2.input = p.1 ∧
      match p.2.result with
      | .applied s' => stepEvent view p.1 p.2.event auth = some s'
      | .refused _ => stepEvent view p.1 p.2.event auth = none) := by
  sorry

/-- Every state satisfying `Reach view auth` is the final state of some supplied
trace the validator accepts under that view and authorization. -/
theorem reach_has_fixedView_witness (view : KelGroups.GroupView)
    (auth : BackdonateAuth) {s : State} (hr : Reach view auth s) :
    ∃ t : Trace, traceRefusal view auth t = none ∧ traceFinal t.initial t.steps = s := by
  sorry

/-- A trace whose initial state is not `State.empty` is refused. -/
theorem traceRefusal_initial_not_empty (view : KelGroups.GroupView)
    (auth : BackdonateAuth) (t : Trace) (h : t.initial ≠ State.empty) :
    traceRefusal view auth t ≠ none := by
  sorry

/-- A trace under a view that contains `comuneId` is refused. -/
theorem traceRefusal_comune_in_view (view : KelGroups.GroupView)
    (auth : BackdonateAuth) (t : Trace)
    (h : KelGroups.GroupView.isMember comuneId view = true) :
    traceRefusal view auth t ≠ none := by
  sorry

end Reactivegas
