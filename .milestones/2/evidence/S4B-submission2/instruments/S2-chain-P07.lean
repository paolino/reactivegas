-- S2-chain-P07.lean — P07 close/permission chain against REAL mutant Step.
-- FROZEN INSTRUMENT (submission 2; P07neg + P07pos — SAME bytes run twice.
-- Supersedes the hermetic miniature, retained unexecuted-supplementary).
-- NEG run: LEAN_PATH resolves a shadow mutant Step.olean first (permission
-- atom `col.permitted` becomes `true`; exact audited bytes compiled from the
-- real file against the current clean dep oleans, so the shadow world is
-- consistent). POS run: LEAN_PATH resolves the clean build olean. Direct
-- `lean` (NOT `lake env lean`) with explicit LEAN_PATH: the measured receipt
-- shows project paths FIRST, toolchain second, inherited `LEAN_PATH` LAST —
-- so under `lake env` the clean project olean would always win over an
-- inherited shadow entry. Determinism requires the explicit shadow-first
-- order, and each run's outcome
-- authenticates which olean loaded (neg must fail at `step_close_inv`; pos
-- must go green). Imports REAL Types/State/Step (mutant Step in neg, clean in
-- pos). Everything else below is copied verbatim: pure inversion helpers
-- (mutant-independent) and the chain statements/proofs plus the P07
-- correspondence, byte-identical to production. No TraceTests import anywhere:
-- trace decide-assertions can neither fire nor mask here (their flips are O4
-- mandatory-path evidence, reported separately, never conflated).
-- Expect NEG: exit 1 with the failure AT `step_close_inv` (the permission-atom
-- mutant breaks the `col.permitted` conjunct its proof derives);
-- `close_guard_inv` still proves (pure Bool decomposition);
-- `close_permission_to_close` elaborates only via the broken link;
-- `permissionToClose_corr` still proves (inline-expression truth table).
-- Expect POS: exit 0, everything elaborates (same bytes, clean Step).
import KelGroups.Types
import Reactivegas.State
import Reactivegas.Step

variable {view : KelGroups.GroupView}
variable {auth : BackdonateAuth}

theorem option_bind_inv {α β : Type} {o : Option α} {f : α → Option β} {b : β}
    (h : o.bind f = some b) : ∃ x, o = some x ∧ f x = some b := by
  cases o with
  | none => exact Option.noConfusion h
  | some x => exact ⟨x, rfl, h⟩

theorem demand_eq_true_of_some {b : Bool} (h : demand b = some ()) : b = true := by
  unfold demand at h
  split at h
  · next hb => exact hb
  · exact Option.noConfusion h

private theorem bool_and_left {b₁ b₂ : Bool} (h : (b₁ && b₂) = true) : b₁ = true := by
  cases hb : b₁ with
  | true => rfl
  | false => rw [hb] at h; exact Bool.noConfusion h

private theorem bool_and_right {b₁ b₂ : Bool} (h : (b₁ && b₂) = true) : b₂ = true := by
  cases hb : b₂ with
  | true => rfl
  | false => cases b₁ <;> rw [hb] at h <;> exact Bool.noConfusion h

private theorem eq_nil_of_isEmpty {α : Type} {l : List α} (h : l.isEmpty = true) :
    l = [] := by
  cases l with
  | nil => rfl
  | cons a t => exact Bool.noConfusion h

def permissionToClose (col : Collection) : Prop :=
  col.permitted ∧ col.pending = []

theorem close_guard_inv {a : KelGroups.Key} {col : Collection}
    (h : (isResponsabile view a && col.referente == a && col.permitted &&
      col.pending.isEmpty) = true) :
    isResponsabile view a = true ∧ col.referente = a ∧ col.permitted ∧ col.pending = [] :=
  ⟨bool_and_left (bool_and_left (bool_and_left h)),
    beq_iff_eq.mp (bool_and_right (bool_and_left (bool_and_left h))),
    bool_and_right (bool_and_left h),
    eq_nil_of_isEmpty (bool_and_right h)⟩

theorem step_close_inv {s s' : State} {a : KelGroups.Key} {c : CollId}
    (hstep : stepEvent view s (.closePurchase a c) auth = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧
      (isResponsabile view a && col.referente == a && col.permitted &&
        col.pending.isEmpty) = true ∧
      s' = { s with
        casse := bump s.casse col.referente (-(sumPledges col.accepted)),
        collections := rest } := by
  obtain ⟨w1, hw1, hw2⟩ := option_bind_inv hstep
  obtain ⟨col, rest⟩ := w1
  obtain ⟨_, hdem, hx⟩ := option_bind_inv hw2
  refine ⟨col, rest, hw1, ?_, ?_⟩
  · exact bool_and_left (demand_eq_true_of_some hdem)
  · simp only [pure, Option.some.injEq] at hx
    exact hx.symm

theorem close_permission_to_close {s s' : State} {a : KelGroups.Key} {c : CollId}
    (h : stepEvent view s (.closePurchase a c) auth = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧ permissionToClose col := by
  obtain ⟨col, rest, hpull, hg, _⟩ := step_close_inv h
  obtain ⟨_, _, hperm, hempty⟩ := close_guard_inv hg
  exact ⟨col, rest, hpull, hperm, hempty⟩

/-- P07 correspondence, UNCHANGED (contrast): relates an inline field
expression and never reads `step`; valid under every guard implementation. -/
theorem permissionToClose_corr (col : Collection) :
    permissionToClose col ↔ ((col.permitted && col.pending.isEmpty) = true) := by
  obtain ⟨id, ref, perm, acc, pend⟩ := col
  cases perm <;> cases pend <;> simp [permissionToClose]
