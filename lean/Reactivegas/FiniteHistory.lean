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
def genesisRefusal (gs : KelGroups.GroupState State) : Option HistoryRefusal :=
  match Reactivegas.boot gs.members gs.appFold with
  | none => if productionWellFormed gs then some .payloadNotClean else some .comuneReserved
  | some g => if g = gs then none else some .notBootOutput

/-- Why one stored step disagrees with `Reactivegas.apply` from the stored state
before it; `none` when it agrees. -/
def stepRefusal (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth) (i : Nat)
    (gs : KelGroups.GroupState State) (st : HistoryStep) : Option HistoryRefusal :=
  match Reactivegas.apply θ auth gs st.signer st.event with
  | .ok r =>
      if st.accepted = false then some (.appliedButStoredRefused i)
      else if st.state ≠ r.state then some (.postStateDiffers i)
      else if st.change ≠ r.change then some (.changeDiffers i)
      else none
  | .error _ =>
      if st.accepted = true then some (.refusedButStoredAccepted i)
      else if st.state ≠ gs then some (.refusedStepMoved i)
      else if st.change ≠ none then some (.refusedStepChange i)
      else none

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
theorem probeAuth_refuses (s : State) (w : Int) : probeAuth s w = false := rfl

/-- Genesis is checked, not assumed: the validator's genesis check passes
exactly when the supplied aggregate is `boot`'s output on its own members and
payload. -/
theorem genesisRefusal_none_iff (gs : KelGroups.GroupState State) :
    genesisRefusal gs = none ↔ Reactivegas.boot gs.members gs.appFold = some gs := by
  unfold genesisRefusal
  cases hb : Reactivegas.boot gs.members gs.appFold with
  | none =>
      simp only [reduceCtorEq, iff_false]
      split <;> simp
  | some g =>
      by_cases hg : g = gs <;> simp [hg]

/-- One step the validator does not refuse is, from the stored state before it,
a refusal of `apply` stored as refused with the state kept and no base change,
or the exact state and base change `apply` returns. -/
theorem stepRefusal_none_outcome (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth)
    (i : Nat) (gs : KelGroups.GroupState State) (st : HistoryStep)
    (h : stepRefusal θ auth i gs st = none) :
    (st.accepted = false →
      st.state = gs ∧ st.change = none ∧
        ∃ e, Reactivegas.apply θ auth gs st.signer st.event = .error e) ∧
    (st.accepted = true →
      ∃ r, Reactivegas.apply θ auth gs st.signer st.event = .ok r ∧
        st.state = r.state ∧ st.change = r.change) := by
  unfold stepRefusal at h
  cases happ : Reactivegas.apply θ auth gs st.signer st.event with
  | error e =>
      rw [happ] at h
      cases hacc : st.accepted <;> by_cases h1 : st.state = gs <;>
        by_cases h2 : st.change = none <;> simp_all
  | ok r =>
      rw [happ] at h
      cases hacc : st.accepted <;> by_cases h1 : st.state = r.state <;>
        by_cases h2 : st.change = r.change <;> simp_all

/-- A step recorded from `apply` itself is not refused. -/
theorem stepRefusal_of_apply_ok (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth)
    (i : Nat) (gs : KelGroups.GroupState State) (signer : KelGroups.Key)
    (event : KelGroups.IntegratedEvent Proposal AppEvent)
    (r : KelGroups.IntegratedResult State)
    (happ : Reactivegas.apply θ auth gs signer event = .ok r) :
    stepRefusal θ auth i gs
      { signer, event, accepted := true, state := r.state, change := r.change } = none := by
  simp [stepRefusal, happ]

theorem historyRefusal_none (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth)
    (h : History) (hv : historyRefusal θ auth h = none) :
    genesisRefusal h.initial = none ∧ stepsRefusal θ auth 0 h.initial h.steps = none := by
  unfold historyRefusal at hv
  cases hg : genesisRefusal h.initial with
  | some r => rw [hg] at hv; simp at hv
  | none => rw [hg] at hv; exact ⟨rfl, hv⟩

/-- A validated history starts from `boot`'s output. -/
theorem validated_genesis (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth)
    (h : History) (hv : historyRefusal θ auth h = none) :
    Reactivegas.boot h.initial.members h.initial.appFold = some h.initial :=
  (genesisRefusal_none_iff h.initial).mp (historyRefusal_none θ auth h hv).1

theorem stepsRefusal_production (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth) :
    ∀ (steps : List HistoryStep) (i : Nat) (gs : KelGroups.GroupState State),
      ProductionHistory θ auth gs → stepsRefusal θ auth i gs steps = none →
      ∀ s ∈ steps.map (·.state), ProductionHistory θ auth s := by
  intro steps
  induction steps with
  | nil => intro i gs _ _ s hs; simp at hs
  | cons st rest ih =>
    intro i gs hp hr s hs
    unfold stepsRefusal at hr
    cases hst : stepRefusal θ auth i gs st with
    | some r => rw [hst] at hr; simp at hr
    | none =>
      rw [hst] at hr
      have hout := stepRefusal_none_outcome θ auth i gs st hst
      have hnext : ProductionHistory θ auth st.state := by
        cases hacc : st.accepted
        · obtain ⟨hs', _, _⟩ := hout.1 hacc
          rw [hs']; exact hp
        · obtain ⟨r, happ, hsr, _⟩ := hout.2 hacc
          rw [hsr]; exact ProductionHistory.apply hp st.signer st.event happ
      simp only [List.map_cons, List.mem_cons] at hs
      rcases hs with rfl | hs
      · exact hnext
      · exact ih (i + 1) st.state hnext hr s hs

/-- For every threshold, authorization, initial aggregate and steps: every
aggregate a validated history stores, the initial one included, is a production
history. -/
theorem validated_is_production (θ : KelGroups.Vote.Threshold)
    (auth : BackdonateAuth) (h : History) (hv : historyRefusal θ auth h = none) :
    ∀ gs ∈ storedStates h, ProductionHistory θ auth gs := by
  intro gs hgs
  have hboot : ProductionHistory θ auth h.initial :=
    ProductionHistory.boot h.initial.members h.initial.appFold (validated_genesis θ auth h hv)
  simp only [storedStates, List.mem_cons] at hgs
  rcases hgs with rfl | hgs
  · exact hboot
  · exact stepsRefusal_production θ auth h.steps 0 h.initial hboot
      (historyRefusal_none θ auth h hv).2 gs hgs

theorem stepsRefusal_outcome (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth) :
    ∀ (steps : List HistoryStep) (i : Nat) (gs : KelGroups.GroupState State),
      stepsRefusal θ auth i gs steps = none →
      ∀ p ∈ stepPairs gs steps,
        (p.2.accepted = false →
          p.2.state = p.1 ∧ p.2.change = none ∧
            ∃ e, Reactivegas.apply θ auth p.1 p.2.signer p.2.event = .error e) ∧
        (p.2.accepted = true →
          ∃ r, Reactivegas.apply θ auth p.1 p.2.signer p.2.event = .ok r ∧
            p.2.state = r.state ∧ p.2.change = r.change) := by
  intro steps
  induction steps with
  | nil => intro i gs _ p hp; simp [stepPairs] at hp
  | cons st rest ih =>
    intro i gs hr p hp
    unfold stepsRefusal at hr
    cases hst : stepRefusal θ auth i gs st with
    | some r => rw [hst] at hr; simp at hr
    | none =>
      rw [hst] at hr
      simp only [stepPairs, List.mem_cons] at hp
      rcases hp with rfl | hp
      · exact stepRefusal_none_outcome θ auth i gs st hst
      · exact ih (i + 1) st.state hr p hp

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
          p.2.state = r.state ∧ p.2.change = r.change) :=
  stepsRefusal_outcome θ auth h.steps 0 h.initial (historyRefusal_none θ auth h hv).2

theorem finalFrom_append_singleton :
    ∀ (xs : List HistoryStep) (gs : KelGroups.GroupState State) (y : HistoryStep),
      finalFrom gs (xs ++ [y]) = y.state := by
  intro xs
  induction xs with
  | nil => intro gs y; simp [finalFrom]
  | cons x xs ih => intro gs y; simp [finalFrom, ih]

theorem stepsRefusal_append (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth) :
    ∀ (xs ys : List HistoryStep) (i : Nat) (gs : KelGroups.GroupState State),
      stepsRefusal θ auth i gs xs = none →
      stepsRefusal θ auth (i + xs.length) (finalFrom gs xs) ys = none →
      stepsRefusal θ auth i gs (xs ++ ys) = none := by
  intro xs
  induction xs with
  | nil => intro ys i gs _ h2; simpa [finalFrom] using h2
  | cons x xs ih =>
    intro ys i gs h1 h2
    unfold stepsRefusal at h1 ⊢
    cases hx : stepRefusal θ auth i gs x with
    | some r => rw [hx] at h1; simp at h1
    | none =>
      rw [hx] at h1
      simp only [List.cons_append]
      rw [hx]
      refine ih ys (i + 1) x.state h1 ?_
      have : i + (x :: xs).length = i + 1 + xs.length := by simp; omega
      rw [this] at h2
      simpa [finalFrom] using h2

/-- Every aggregate that is a production history is the final stored state of
some supplied history the validator accepts, under the same threshold and
authorization. -/
theorem production_has_validated_witness (θ : KelGroups.Vote.Threshold)
    (auth : BackdonateAuth) {gs : KelGroups.GroupState State}
    (hp : ProductionHistory θ auth gs) :
    ∃ h : History, historyRefusal θ auth h = none ∧ finalFrom h.initial h.steps = gs := by
  induction hp with
  | boot members payload hboot =>
    rename_i gs
    have hshape : gs.members = members ∧ gs.appFold = payload := by
      simp only [Reactivegas.boot] at hboot
      split at hboot
      · injection hboot with hg; subst hg; exact ⟨rfl, rfl⟩
      · simp at hboot
    have hb : Reactivegas.boot gs.members gs.appFold = some gs := by
      rw [hshape.1, hshape.2]; exact hboot
    refine ⟨{ initial := gs, steps := [] }, ?_, rfl⟩
    simp [historyRefusal, (genesisRefusal_none_iff gs).mpr hb, stepsRefusal]
  | apply prior signer event happ ih =>
    rename_i gs0 result
    obtain ⟨h, hv, hfinal⟩ := ih
    obtain ⟨hg, hs⟩ := historyRefusal_none θ auth h hv
    refine ⟨{ h with steps := h.steps ++
      [{ signer, event, accepted := true, state := result.state, change := result.change }] },
      ?_, ?_⟩
    · unfold historyRefusal
      simp only [hg]
      refine stepsRefusal_append θ auth h.steps _ 0 h.initial hs ?_
      rw [hfinal]
      simp [stepsRefusal, stepRefusal_of_apply_ok θ auth _ gs0 signer event result happ]
    · exact finalFrom_append_singleton _ _ _

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
def traceStepRefusal (view : KelGroups.GroupView) (auth : BackdonateAuth) (i : Nat)
    (cur : State) (st : TraceStep) : Option TraceRefusal :=
  if st.input ≠ cur then some (.inputDiffers i)
  else
    match st.result, stepDetailed view cur st.event auth with
    | .applied stored, .applied actual =>
        if stored = actual then none else some (.postStateDiffers i)
    | .refused claim, .refused g =>
        if claim = guardClaim g then none else some (.claimDiffers i)
    | .applied _, .refused _ => some (.storedAppliedButRefused i)
    | .refused _, .applied _ => some (.storedRefusedButApplied i)

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
  if KelGroups.GroupView.isMember comuneId view then some .comuneInView
  else if t.initial ≠ State.empty then some .initialNotEmpty
  else traceStepsRefusal view auth 0 t.initial t.steps

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

theorem traceStepRefusal_none_outcome (view : KelGroups.GroupView) (auth : BackdonateAuth)
    (i : Nat) (cur : State) (st : TraceStep)
    (h : traceStepRefusal view auth i cur st = none) :
    st.input = cur ∧
    (match st.result with
      | .applied s' => stepEvent view cur st.event auth = some s'
      | .refused _ => stepEvent view cur st.event auth = none) := by
  unfold traceStepRefusal at h
  by_cases hin : st.input = cur
  · refine ⟨hin, ?_⟩
    simp only [hin, ne_eq, not_true_eq_false, if_false] at h
    unfold stepDetailed at h
    cases hse : stepEvent view cur st.event auth with
    | none =>
        rw [hse] at h
        cases hr : st.result <;> simp_all
    | some s' =>
        rw [hse] at h
        cases hr : st.result with
        | applied stored => simp_all
        | refused c => simp_all
  · simp [hin] at h

theorem traceStepsRefusal_reaches (view : KelGroups.GroupView) (auth : BackdonateAuth) :
    ∀ (steps : List TraceStep) (i : Nat) (cur : State),
      Reach view auth cur → traceStepsRefusal view auth i cur steps = none →
      ∀ s ∈ traceStates cur steps, Reach view auth s := by
  intro steps
  induction steps with
  | nil => intro i cur hr _ s hs; simp [traceStates] at hs; subst hs; exact hr
  | cons st rest ih =>
    intro i cur hr hv s hs
    unfold traceStepsRefusal at hv
    cases hst : traceStepRefusal view auth i cur st with
    | some r => rw [hst] at hv; simp at hv
    | none =>
      rw [hst] at hv
      have hout := traceStepRefusal_none_outcome view auth i cur st hst
      have hnext : Reach view auth (traceNext cur st) := by
        unfold traceNext
        cases hr' : st.result with
        | applied s' =>
            rw [hr'] at hout
            exact Reach.trans hr hout.2
        | refused _ => exact hr
      simp only [traceStates, List.mem_cons] at hs
      rcases hs with rfl | hs
      · exact hr
      · exact ih (i + 1) (traceNext cur st) hnext hv s hs

theorem traceStepsRefusal_outcome (view : KelGroups.GroupView) (auth : BackdonateAuth) :
    ∀ (steps : List TraceStep) (i : Nat) (cur : State),
      traceStepsRefusal view auth i cur steps = none →
      ∀ p ∈ tracePairs cur steps, p.2.input = p.1 ∧
        match p.2.result with
        | .applied s' => stepEvent view p.1 p.2.event auth = some s'
        | .refused _ => stepEvent view p.1 p.2.event auth = none := by
  intro steps
  induction steps with
  | nil => intro i cur _ p hp; simp [tracePairs] at hp
  | cons st rest ih =>
    intro i cur hv p hp
    unfold traceStepsRefusal at hv
    cases hst : traceStepRefusal view auth i cur st with
    | some r => rw [hst] at hv; simp at hv
    | none =>
      rw [hst] at hv
      simp only [tracePairs, List.mem_cons] at hp
      rcases hp with rfl | hp
      · exact traceStepRefusal_none_outcome view auth i cur st hst
      · exact ih (i + 1) (traceNext cur st) hv p hp

theorem traceRefusal_none (view : KelGroups.GroupView) (auth : BackdonateAuth) (t : Trace)
    (h : traceRefusal view auth t = none) :
    KelGroups.GroupView.isMember comuneId view = false ∧ t.initial = State.empty ∧
      traceStepsRefusal view auth 0 t.initial t.steps = none := by
  unfold traceRefusal at h
  by_cases hm : KelGroups.GroupView.isMember comuneId view = true
  · simp [hm] at h
  · by_cases hi : t.initial = State.empty
    · rw [if_neg hm, if_neg (fun hne => hne hi)] at h
      exact ⟨by simpa using hm, hi, h⟩
    · simp [hm, hi] at h

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
  obtain ⟨hm, hi, hs⟩ := traceRefusal_none view auth t hv
  have hboot : Reach view auth t.initial := by
    rw [hi]
    exact Reach.boot (by simp [comune_not_a_member, hm])
  exact ⟨traceStepsRefusal_reaches view auth t.steps 0 t.initial hboot hs,
    traceStepsRefusal_outcome view auth t.steps 0 t.initial hs⟩

theorem traceFinal_append_singleton :
    ∀ (xs : List TraceStep) (cur : State) (y : TraceStep),
      traceFinal cur (xs ++ [y]) = traceNext (traceFinal cur xs) y := by
  intro xs
  induction xs with
  | nil => intro cur y; simp [traceFinal]
  | cons x xs ih => intro cur y; simp [traceFinal, ih]

theorem traceStepsRefusal_append (view : KelGroups.GroupView) (auth : BackdonateAuth) :
    ∀ (xs ys : List TraceStep) (i : Nat) (cur : State),
      traceStepsRefusal view auth i cur xs = none →
      traceStepsRefusal view auth (i + xs.length) (traceFinal cur xs) ys = none →
      traceStepsRefusal view auth i cur (xs ++ ys) = none := by
  intro xs
  induction xs with
  | nil => intro ys i cur _ h2; simpa [traceFinal] using h2
  | cons x xs ih =>
    intro ys i cur h1 h2
    unfold traceStepsRefusal at h1 ⊢
    cases hx : traceStepRefusal view auth i cur x with
    | some r => rw [hx] at h1; simp at h1
    | none =>
      rw [hx] at h1
      simp only [List.cons_append]
      rw [hx]
      refine ih ys (i + 1) (traceNext cur x) h1 ?_
      have : i + (x :: xs).length = i + 1 + xs.length := by simp; omega
      rw [this] at h2
      simpa [traceFinal] using h2

/-- Every state satisfying `Reach view auth` is the final state of some supplied
trace the validator accepts under that view and authorization. -/
theorem reach_has_fixedView_witness (view : KelGroups.GroupView)
    (auth : BackdonateAuth) {s : State} (hr : Reach view auth s) :
    ∃ t : Trace, traceRefusal view auth t = none ∧ traceFinal t.initial t.steps = s := by
  induction hr with
  | boot h =>
    have hm : KelGroups.GroupView.isMember comuneId view = false := by
      simpa [comune_not_a_member] using h
    refine ⟨⟨"reactivegas.trace", 1, State.empty, []⟩, ?_, rfl⟩
    simp [traceRefusal, hm, traceStepsRefusal]
  | @trans s e s' _ hstep ih =>
    obtain ⟨t, hv, hfinal⟩ := ih
    obtain ⟨hm, hi, hs⟩ := traceRefusal_none view auth t hv
    refine ⟨{ t with steps := t.steps ++ [{ input := s, event := e, result := .applied s' }] }, ?_, ?_⟩
    · unfold traceRefusal
      simp only [hm, hi, ne_eq, not_true_eq_false, if_false, Bool.false_eq_true]
      rw [hi] at hs
      refine traceStepsRefusal_append view auth t.steps _ 0 State.empty hs ?_
      rw [← hi, hfinal]
      simp [traceStepsRefusal, traceStepRefusal, stepDetailed, hstep]
    · simp [traceFinal_append_singleton, traceNext]

/-- A trace whose initial state is not `State.empty` is refused. -/
theorem traceRefusal_initial_not_empty (view : KelGroups.GroupView)
    (auth : BackdonateAuth) (t : Trace) (h : t.initial ≠ State.empty) :
    traceRefusal view auth t ≠ none := by
  intro hv
  exact h (traceRefusal_none view auth t hv).2.1

/-- A trace under a view that contains `comuneId` is refused. -/
theorem traceRefusal_comune_in_view (view : KelGroups.GroupView)
    (auth : BackdonateAuth) (t : Trace)
    (h : KelGroups.GroupView.isMember comuneId view = true) :
    traceRefusal view auth t ≠ none := by
  intro hv
  have := (traceRefusal_none view auth t hv).1
  rw [h] at this
  exact Bool.noConfusion this

end Reactivegas
