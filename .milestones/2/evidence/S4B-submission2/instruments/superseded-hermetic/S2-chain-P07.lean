-- S2-chain-P07.lean — isolated P07 close/permission chain (hermetic miniature).
-- FROZEN INSTRUMENT (submission 2, OT1). Single-file elaboration; core-only
-- imports (no project modules are imported, so no oleans beyond the toolchain
-- are needed and no earlier-gate masking is possible).
--
-- Environment: minimal local copies with REAL logic and shapes, EXCEPT the
-- closePurchase permission atom, which carries the AUDITED mutant
-- (`col.permitted` becomes `true`; see S2-mut-close-perm.diff). Miniature
-- scope, documented: Event/AppEvent/step/stepEvent cover closePurchase only
-- (the tested statements quantify over closePurchase inputs exclusively);
-- State omits the vote payload (unobserved by this chain); all other copied
-- definitions keep production bodies. Target statements and proofs below are
-- byte-identical to production `close_guard_inv` (Invariants.lean:178),
-- `step_close_inv` (:305) and `close_permission_to_close` (:647); the P07
-- correspondence is included UNCHANGED as contrast (it still proves: it
-- relates an inline field expression and never reads `step`).
-- Expect: exit 1 with the failure AT `step_close_inv` (the permission-atom
-- mutant breaks the `col.permitted` conjunct its proof derives);
-- `close_guard_inv` still proves (pure Bool decomposition);
-- `close_permission_to_close` elaborates only via the broken link.
-- A first failure in this named chain establishes its sensitivity.

namespace KelGroups

abbrev Key := String

inductive Admin where
  | publicAdmin
  | privateAdmin
deriving DecidableEq, BEq, Repr

inductive Role where
  | adminRole (admin : Admin)
  | appRole (name : String)
deriving DecidableEq, BEq, Repr

structure Member where
  key : Key
  email : String
  roles : List Role
deriving DecidableEq, BEq, Repr

def isAdminRole : Role → Bool
  | .adminRole _ => true
  | .appRole _ => false

def hasAdmin (roles : List Role) : Bool := roles.any isAdminRole

def assocLookup {κ ν : Type} [BEq κ] (key : κ) : List (κ × ν) → Option ν
  | [] => none
  | (candidate, value) :: rest =>
      if candidate == key then some value else assocLookup key rest

structure GroupView where
  members : List (Key × Member)
deriving DecidableEq, BEq, Repr

namespace GroupView

def lookupMember (key : Key) (view : GroupView) : Option Member :=
  assocLookup key view.members

def isAdmin (key : Key) (view : GroupView) : Bool :=
  match lookupMember key view with
  | some member => hasAdmin member.roles
  | none => false

end GroupView

end KelGroups

abbrev CollId := Nat

structure Pledge where
  user : KelGroups.Key
  amount : Int
deriving DecidableEq, BEq, Repr

structure Collection where
  id : CollId
  referente : KelGroups.Key
  permitted : Bool
  accepted : List Pledge
  pending : List Pledge
deriving DecidableEq, BEq, Repr

structure State where
  conti : List (KelGroups.Key × Int)
  casse : List (KelGroups.Key × Int)
  collections : List Collection
deriving DecidableEq, BEq, Repr

abbrev comuneId : KelGroups.Key := "comune"

def bal (m : List (KelGroups.Key × Int)) (u : KelGroups.Key) : Int :=
  match m with
  | [] => 0
  | (k, v) :: t => if k = u then v else bal t u

def comuneBal (s : State) : Int := bal s.conti comuneId

def stalled (s : State) : Prop := comuneBal s < 0

instance stalledDecidable (s : State) : Decidable (stalled s) :=
  Int.decLt (comuneBal s) 0

def bump (m : List (KelGroups.Key × Int)) (u : KelGroups.Key) (d : Int) : List (KelGroups.Key × Int) :=
  match m with
  | [] => [(u, d)]
  | (k, v) :: t => if k = u then (k, v + d) :: t else (k, v) :: bump t u d

def sumPledges : List Pledge → Int
  | [] => 0
  | p :: t => p.amount + sumPledges t

def pullCollection (c : CollId) : List Collection → Option (Collection × List Collection)
  | [] => none
  | x :: t =>
    if x.id = c then some (x, t)
    else match pullCollection c t with
      | some (y, rest) => some (y, x :: rest)
      | none => none

def demand (b : Bool) : Option Unit := if b then some () else none

def isResponsabile (view : KelGroups.GroupView) (u : KelGroups.Key) : Bool :=
  KelGroups.GroupView.isAdmin u view

abbrev BackdonateAuth := State → Int → Bool

inductive AppEvent where
  | closePurchase (c : CollId)

inductive Event where
  | closePurchase (a : KelGroups.Key) (c : CollId)

/-- The closePurchase arm below carries the AUDITED permission-atom mutant
(`col.permitted` becomes `true`); every other token keeps its production body.
Production arm: Step.lean:126-132. -/
def step (view : KelGroups.GroupView) (s : State) (signer : KelGroups.Key)
    (e : AppEvent) (auth : BackdonateAuth) : Option State :=
  match e with
  | .closePurchase c => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile view signer && col.referente == signer
      && true && col.pending.isEmpty && !(decide (stalled s)))
    pure { s with
      casse := bump s.casse col.referente (-(sumPledges col.accepted)),
      collections := rest }

def stepEvent (view : KelGroups.GroupView) (s : State) (e : Event)
    (auth : BackdonateAuth) : Option State :=
  let go (signer : KelGroups.Key) (app : AppEvent) : Option State :=
    step view s signer app auth
  match e with
  | .closePurchase a c => go a (.closePurchase c)

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
