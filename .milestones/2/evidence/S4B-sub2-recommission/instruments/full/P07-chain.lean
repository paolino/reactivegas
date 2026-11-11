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

theorem permissionToClose_corr (col : Collection) :
    permissionToClose col ↔ ((col.permitted && col.pending.isEmpty) = true) := by
  obtain ⟨id, ref, perm, acc, pend⟩ := col
  cases perm <;> cases pend <;> simp [permissionToClose]

def witnessAdmin : KelGroups.Member := ⟨"a", "a@audit", [.adminRole .publicAdmin]⟩
def witnessView : KelGroups.GroupView := ⟨[("a", witnessAdmin)]⟩
def selected (perm : Bool) : Collection := ⟨7, "a", perm, [⟨"u", 23⟩], []⟩
def unrelated : Collection := ⟨9, "other", false, [], [⟨"v", 5⟩]⟩
def witnessState (perm : Bool) : State := { State.empty with conti := [("u", 11)], casse := [("a", 40)], collections := [unrelated, selected perm] }
def closeResult (perm : Bool) := stepEvent witnessView (witnessState perm) (.closePurchase "a" 7) (fun _ _ => false)
#eval IO.println s!"P07-WITNESS selectedBinding={decide (pullCollection 7 (witnessState true).collections = some (selected true, [unrelated]))} authorized={ (closeResult true).isSome} forbidden={ (closeResult false).isSome} preservesOther={decide ((closeResult true).map (fun s => s.collections) = some [unrelated])} amount={decide ((closeResult true).map (fun s => bal s.casse "a") = some 17)}"
