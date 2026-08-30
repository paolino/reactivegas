import Reactivegas.Predicates

variable {view : KelGroups.GroupView}

/-!
# Invariants and preservation theorems

The flagship is `conservation_preserved` (L6): every successful event
preserves `Σ casse − Σ conti − Σ open escrow = 0`. Each documented law
(L1–L8) gets at least one machine-checked theorem; L7 proves solvency:
the guards reject every overdrawing debit, so `insolvent` is
unreachable from boot (`reach_solvent`, `not_insolvent_of_reach`).
-/

/-! ### Proof helpers -/

/-- Inversion through one `do`-bind of the step function. -/
theorem option_bind_inv {α β : Type} {o : Option α} {f : α → Option β} {b : β}
    (h : o.bind f = some b) : ∃ x, o = some x ∧ f x = some b := by
  cases o with
  | none => exact Option.noConfusion h
  | some x => exact ⟨x, rfl, h⟩

/-- A passing demand forces its Boolean condition. -/
theorem demand_eq_true_of_some {b : Bool} (h : demand b = some ()) : b = true := by
  unfold demand at h
  split at h
  · next hb => exact hb
  · exact Option.noConfusion h

/-- A failing demand has a non-true condition. -/
theorem demand_none_of_ne_true {b : Bool} (h : ¬(b = true)) : demand b = none := by
  unfold demand
  split
  · next hb => exact absurd hb h
  · rfl

/-- From `(!b) = true` conclude `b = false`. -/
private theorem bool_not_true {b : Bool} (h : (!b) = true) : b = false := by
  cases b with
  | false => rfl
  | true => exact Bool.noConfusion h

/-- Left conjunct of a passing Boolean conjunction. -/
private theorem bool_and_left {b₁ b₂ : Bool} (h : (b₁ && b₂) = true) : b₁ = true := by
  cases hb : b₁ with
  | true => rfl
  | false => rw [hb] at h; exact Bool.noConfusion h

/-- Right conjunct of a passing Boolean conjunction. -/
private theorem bool_and_right {b₁ b₂ : Bool} (h : (b₁ && b₂) = true) : b₂ = true := by
  cases hb : b₂ with
  | true => rfl
  | false => cases b₁ <;> rw [hb] at h <;> exact Bool.noConfusion h

/-- An empty-flagged list is the empty list. -/
private theorem eq_nil_of_isEmpty {α : Type} {l : List α} (h : l.isEmpty = true) :
    l = [] := by
  cases l with
  | nil => rfl
  | cons a t => exact Bool.noConfusion h

/-- Collections left behind by stripping `u` never name `u` as referente. -/
theorem stripCollections_referente_ne (u : KelGroups.Key) (cols : List Collection) :
    ∀ c ∈ (stripCollections u cols).1, c.referente ≠ u := by
  induction cols with
  | nil => intro c hc; cases hc
  | cons x t ih =>
    simp only [stripCollections]
    split
    · next _ => dsimp only; intro c hc; exact ih c hc
    · next hx =>
      dsimp only
      intro c hc
      rcases List.mem_cons.mp hc with hc' | hc'
      · subst hc'; exact hx
      · exact ih c hc'

private theorem pullCollection_mem_lemma {c : CollId} :
    ∀ (cols : List Collection) (x : Collection) (rest : List Collection),
      pullCollection c cols = some (x, rest) → x ∈ cols := by
  intro cols
  induction cols with
  | nil => intro x rest h; exact Option.noConfusion h
  | cons z t ih =>
    intro x rest h
    rw [pullCollection] at h
    split at h
    · next hz =>
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      exact List.mem_cons_self ..
    · next hz =>
      cases hx : pullCollection c t with
      | none => rw [hx] at h; exact Option.noConfusion h
      | some w =>
        obtain ⟨y', rest'⟩ := w
        rw [hx] at h
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨hy, hr⟩ := h
        subst hy
        subst hr
        exact List.mem_cons_of_mem _ (ih y' rest' hx)

/-- The pulled collection was a member of the original list. -/
theorem pullCollection_mem {c : CollId} {cols : List Collection} {x : Collection}
    {rest : List Collection} (h : pullCollection c cols = some (x, rest)) :
    x ∈ cols :=
  pullCollection_mem_lemma cols x rest h

/-- `pullCollection` is deterministic: same input, same output pair. -/
private theorem pullCollection_det {c : CollId} {cols : List Collection}
    {p₁ p₂ : Collection × List Collection}
    (h1 : pullCollection c cols = some p₁) (h2 : pullCollection c cols = some p₂) :
    p₁ = p₂ := by
  rw [h1] at h2
  simpa using h2

/-- Pointwise user absence from a failed membership scan. -/
theorem user_absent_of_any_false {u : KelGroups.Key} {l : List Pledge}
    (h : l.any (fun p => p.user == u) = false) : ∀ p ∈ l, p.user ≠ u := by
  intro p hp
  intro hpu
  have ht : l.any (fun p => p.user == u) = true :=
    List.any_eq_true.mpr ⟨p, hp, by simp [hpu]⟩
  rw [h] at ht
  exact Bool.noConfusion ht

/-- Folding `bump _ _ w` over a user list raises the total by `w` per
user. -/
private theorem sumBal_foldl_bump :
    ∀ (l : List KelGroups.Key) (m : List (KelGroups.Key × Int)) (w : Int),
      sumBal (l.foldl (fun acc u => bump acc u w) m) =
        sumBal m + w * (l.length : Int) := by
  intro l
  induction l with
  | nil =>
    intro m w
    simp [sumBal]
  | cons x t ih =>
    intro m w
    simp [List.foldl]
    rw [ih, bump_sum]
    rw [Int.mul_add, Int.mul_one]
    omega

/-! ### Guard inversions -/

/-- Decompose the pledge guard into its conjuncts. -/
theorem pledge_guard_inv {s : State} {a u : KelGroups.Key} {col : Collection} {v : Int}
    (h : (isResponsabile view a && KelGroups.GroupView.isMember u view &&
      !(col.accepted.any (fun p => p.user == u)) &&
      !(col.pending.any (fun p => p.user == u)) &&
      decide (0 < v) && decide (bal s.conti u ≥ v)) = true) :
    isResponsabile view a = true ∧ KelGroups.GroupView.isMember u view = true ∧
      col.accepted.any (fun p => p.user == u) = false ∧
      col.pending.any (fun p => p.user == u) = false ∧
      0 < v ∧ bal s.conti u ≥ v := by
  have hv2 : decide (bal s.conti u ≥ v) = true := bool_and_right h
  have hv1 : decide (0 < v) = true := bool_and_right (bool_and_left h)
  have hn2 := bool_and_right (bool_and_left (bool_and_left h))
  have hn1 := bool_and_right (bool_and_left (bool_and_left (bool_and_left h)))
  have hAB := bool_and_left (bool_and_left (bool_and_left (bool_and_left h)))
  exact ⟨bool_and_left hAB, bool_and_right hAB, bool_not_true hn1, bool_not_true hn2,
    decide_eq_true_iff.mp hv1, decide_eq_true_iff.mp hv2⟩

/-- Decompose the accept/refuse/correct guard. -/
theorem auth_referente_guard_inv {a : KelGroups.Key} {col : Collection}
    (h : (isResponsabile view a && col.referente == a) = true) :
    isResponsabile view a = true ∧ col.referente = a :=
  ⟨bool_and_left h, beq_iff_eq.mp (bool_and_right h)⟩

/-- Decompose the positive-closure guard. -/
theorem close_guard_inv {a : KelGroups.Key} {col : Collection}
    (h : (isResponsabile view a && col.referente == a && col.permitted &&
      col.pending.isEmpty) = true) :
    isResponsabile view a = true ∧ col.referente = a ∧ col.permitted ∧ col.pending = [] :=
  ⟨bool_and_left (bool_and_left (bool_and_left h)),
    beq_iff_eq.mp (bool_and_right (bool_and_left (bool_and_left h))),
    bool_and_right (bool_and_left h),
    eq_nil_of_isEmpty (bool_and_right h)⟩

/-- Decompose the failure-closure guard. -/
theorem fail_guard_inv {a : KelGroups.Key} {col : Collection}
    (h : (isResponsabile view a && col.referente == a && col.pending.isEmpty) = true) :
    isResponsabile view a = true ∧ col.referente = a ∧ col.pending = [] :=
  ⟨bool_and_left (bool_and_left h),
    beq_iff_eq.mp (bool_and_right (bool_and_left h)),
    eq_nil_of_isEmpty (bool_and_right h)⟩

/-! ### Event inversions -/

theorem step_grant_inv {s s' : State} {a : KelGroups.Key} {c : CollId}
    (hstep : stepEvent view s (.grantPermission a c) = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧
      isResponsabile view a = true ∧
      s' = { s with collections := { col with permitted := true } :: rest } := by
  obtain ⟨w1, hw1, hw2⟩ := option_bind_inv hstep
  obtain ⟨col, rest⟩ := w1
  obtain ⟨_, hdem, hx⟩ := option_bind_inv hw2
  refine ⟨col, rest, hw1, ?_, ?_⟩
  · exact demand_eq_true_of_some hdem
  · simp only [pure, Option.some.injEq] at hx
    exact hx.symm

theorem step_deny_inv {s s' : State} {a : KelGroups.Key} {c : CollId}
    (hstep : stepEvent view s (.denyPermission a c) = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧
      isResponsabile view a = true ∧
      s' = { s with
        conti := refundAll s.conti (col.accepted ++ col.pending),
        collections := rest } := by
  obtain ⟨w1, hw1, hw2⟩ := option_bind_inv hstep
  obtain ⟨col, rest⟩ := w1
  obtain ⟨_, hdem, hx⟩ := option_bind_inv hw2
  refine ⟨col, rest, hw1, ?_, ?_⟩
  · exact demand_eq_true_of_some hdem
  · simp only [pure, Option.some.injEq] at hx
    exact hx.symm

theorem step_pledge_inv {s s' : State} {a u : KelGroups.Key} {c : CollId} {v : Int}
    (hstep : stepEvent view s (.pledge a u c v) = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧
      (isResponsabile view a && KelGroups.GroupView.isMember u view &&
          !(col.accepted.any (fun p => p.user == u)) &&
          !(col.pending.any (fun p => p.user == u)) &&
          decide (0 < v) && decide (bal s.conti u ≥ v)) =
        true ∧
      s' = { s with
        conti := bump s.conti u (-v),
        collections := { col with pending := ⟨u, v⟩ :: col.pending } :: rest } := by
  obtain ⟨w1, hw1, hw2⟩ := option_bind_inv hstep
  obtain ⟨col, rest⟩ := w1
  obtain ⟨_, hdem, hx⟩ := option_bind_inv hw2
  refine ⟨col, rest, hw1, ?_, ?_⟩
  · exact bool_and_left (demand_eq_true_of_some hdem)
  · simp only [pure, Option.some.injEq] at hx
    exact hx.symm

theorem step_accept_inv {s s' : State} {a u : KelGroups.Key} {c : CollId}
    (hstep : stepEvent view s (.acceptPledge a u c) = some s') :
    ∃ col rest v pend',
      pullCollection c s.collections = some (col, rest) ∧
      splitUser u col.pending = some (v, pend') ∧
      (isResponsabile view a && col.referente == a) = true ∧
      s' = { s with collections :=
        { col with pending := pend', accepted := ⟨u, v⟩ :: col.accepted } :: rest } := by
  obtain ⟨w1, hw1, hw2⟩ := option_bind_inv hstep
  obtain ⟨col, rest⟩ := w1
  obtain ⟨w2, hw3, hw4⟩ := option_bind_inv hw2
  obtain ⟨v, pend'⟩ := w2
  obtain ⟨_, hdem, hx⟩ := option_bind_inv hw4
  refine ⟨col, rest, v, pend', hw1, hw3, ?_, ?_⟩
  · exact bool_and_left (demand_eq_true_of_some hdem)
  · simp only [pure, Option.some.injEq] at hx
    exact hx.symm

theorem step_refuse_inv {s s' : State} {a u : KelGroups.Key} {c : CollId}
    (hstep : stepEvent view s (.refusePledge a u c) = some s') :
    ∃ col rest v pend',
      pullCollection c s.collections = some (col, rest) ∧
      splitUser u col.pending = some (v, pend') ∧
      (isResponsabile view a && col.referente == a) = true ∧
      s' = { s with
        conti := bump s.conti u v,
        collections := { col with pending := pend' } :: rest } := by
  obtain ⟨w1, hw1, hw2⟩ := option_bind_inv hstep
  obtain ⟨col, rest⟩ := w1
  obtain ⟨w2, hw3, hw4⟩ := option_bind_inv hw2
  obtain ⟨v, pend'⟩ := w2
  obtain ⟨_, hdem, hx⟩ := option_bind_inv hw4
  refine ⟨col, rest, v, pend', hw1, hw3, ?_, ?_⟩
  · exact demand_eq_true_of_some hdem
  · simp only [pure, Option.some.injEq] at hx
    exact hx.symm

theorem step_correct_inv {s s' : State} {a u : KelGroups.Key} {c : CollId} {v' : Int}
    (hstep : stepEvent view s (.correctPledge a u c v') = some s') :
    ∃ col rest v acc',
      pullCollection c s.collections = some (col, rest) ∧
      splitUser u col.accepted = some (v, acc') ∧
      (isResponsabile view a && col.referente == a &&
          decide (0 ≤ v') && decide (bal s.conti u + (v - v') ≥ 0)) =
        true ∧
      s' = { s with
        conti := bump s.conti u (v - v'),
        collections := { col with accepted := ⟨u, v'⟩ :: acc' } :: rest } := by
  obtain ⟨w1, hw1, hw2⟩ := option_bind_inv hstep
  obtain ⟨col, rest⟩ := w1
  obtain ⟨w2, hw3, hw4⟩ := option_bind_inv hw2
  obtain ⟨v, acc'⟩ := w2
  obtain ⟨_, hdem, hx⟩ := option_bind_inv hw4
  refine ⟨col, rest, v, acc', hw1, hw3, ?_, ?_⟩
  · exact demand_eq_true_of_some hdem
  · simp only [pure, Option.some.injEq] at hx
    exact hx.symm

theorem step_close_inv {s s' : State} {a : KelGroups.Key} {c : CollId}
    (hstep : stepEvent view s (.closePurchase a c) = some s') :
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

theorem step_fail_inv {s s' : State} {a : KelGroups.Key} {c : CollId}
    (hstep : stepEvent view s (.failPurchase a c) = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧
      (isResponsabile view a && col.referente == a && col.pending.isEmpty) = true ∧
      s' = { s with
        conti := refundAll s.conti (col.accepted ++ col.pending),
        collections := rest } := by
  obtain ⟨w1, hw1, hw2⟩ := option_bind_inv hstep
  obtain ⟨col, rest⟩ := w1
  obtain ⟨_, hdem, hx⟩ := option_bind_inv hw2
  refine ⟨col, rest, hw1, ?_, ?_⟩
  · exact demand_eq_true_of_some hdem
  · simp only [pure, Option.some.injEq] at hx
    exact hx.symm

/-! ### L6 flagship: conservation preserved by every event -/

theorem conservation_preserved {s s' : State} {e : Event}
    (hcon : conservation s) (hstep : stepEvent view s e = some s') : conservation s' := by
  cases e with
  | addUser a u | electResponsabile a u
  | removeResponsabile a u | removeMember a u =>
    exact Option.noConfusion hstep
  | openPurchase a c =>
    simp only [stepEvent, step] at hstep
    split at hstep
    · simp only [Option.some.injEq] at hstep
      subst hstep
      simp only [conservation] at hcon ⊢
      rw [escrowSum_cons]
      simp only [escrowOf, sumPledges]
      omega
    · exact Option.noConfusion hstep
  | grantPermission a c =>
    obtain ⟨col, rest, hpull, _, hs'⟩ := step_grant_inv hstep
    subst hs'
    simp only [conservation] at hcon ⊢
    rw [escrowSum_cons]
    have hps := pullCollection_sum hpull
    simp only [escrowOf] at hps ⊢
    omega
  | denyPermission a c =>
    obtain ⟨col, rest, hpull, _, hs'⟩ := step_deny_inv hstep
    subst hs'
    simp only [conservation] at hcon ⊢
    rw [refundAll_sum, sumPledges_append]
    have hps := pullCollection_sum hpull
    simp only [escrowOf] at hps
    omega
  | deposit a u v =>
    simp only [stepEvent, step] at hstep
    split at hstep
    · simp only [Option.some.injEq] at hstep
      subst hstep
      simp only [conservation] at hcon ⊢
      rw [bump_sum, bump_sum]
      omega
    · exact Option.noConfusion hstep
  | withdraw a u v =>
    simp only [stepEvent, step] at hstep
    split at hstep
    · simp only [Option.some.injEq] at hstep
      subst hstep
      simp only [conservation] at hcon ⊢
      rw [bump_sum, bump_sum]
      omega
    · exact Option.noConfusion hstep
  | transferCassa a f v =>
    simp only [stepEvent, step] at hstep
    split at hstep
    · simp only [Option.some.injEq] at hstep
      subst hstep
      simp only [conservation] at hcon ⊢
      rw [bump_sum, bump_sum]
      omega
    · exact Option.noConfusion hstep
  | pledge a u c v =>
    obtain ⟨col, rest, hpull, _, hs'⟩ := step_pledge_inv hstep
    subst hs'
    simp only [conservation] at hcon ⊢
    rw [bump_sum, escrowSum_cons]
    have hps := pullCollection_sum hpull
    simp only [escrowOf, sumPledges] at hps ⊢
    omega
  | acceptPledge a u c =>
    obtain ⟨col, rest, v, pend', hpull, hspl, _, hs'⟩ := step_accept_inv hstep
    subst hs'
    simp only [conservation] at hcon ⊢
    rw [escrowSum_cons]
    have hps := pullCollection_sum hpull
    have hsm := splitUser_sum hspl
    simp only [escrowOf, sumPledges] at hps ⊢
    omega
  | refusePledge a u c =>
    obtain ⟨col, rest, v, pend', hpull, hspl, _, hs'⟩ := step_refuse_inv hstep
    subst hs'
    simp only [conservation] at hcon ⊢
    rw [bump_sum, escrowSum_cons]
    have hps := pullCollection_sum hpull
    have hsm := splitUser_sum hspl
    simp only [escrowOf] at hps ⊢
    omega
  | correctPledge a u c v' =>
    obtain ⟨col, rest, v, acc', hpull, hspl, _, hs'⟩ := step_correct_inv hstep
    subst hs'
    simp only [conservation] at hcon ⊢
    rw [bump_sum, escrowSum_cons]
    have hps := pullCollection_sum hpull
    have hsm := splitUser_sum hspl
    simp only [escrowOf, sumPledges] at hps ⊢
    omega
  | closePurchase a c =>
    obtain ⟨col, rest, hpull, hg, hs'⟩ := step_close_inv hstep
    obtain ⟨_, _, _, hempty⟩ := close_guard_inv hg
    subst hs'
    simp only [conservation] at hcon ⊢
    rw [bump_sum]
    have hps := pullCollection_sum hpull
    simp only [escrowOf, sumPledges, hempty] at hps
    omega
  | failPurchase a c =>
    obtain ⟨col, rest, hpull, _, hs'⟩ := step_fail_inv hstep
    subst hs'
    simp only [conservation] at hcon ⊢
    rw [refundAll_sum, sumPledges_append]
    have hps := pullCollection_sum hpull
    simp only [escrowOf] at hps
    omega
  | donate a v =>
    simp only [stepEvent, step] at hstep
    split at hstep
    · simp only [Option.some.injEq] at hstep
      subst hstep
      simp only [conservation] at hcon ⊢
      rw [bump_sum, bump_sum]
      omega
    · exact Option.noConfusion hstep
  | backdonate a w =>
    simp only [stepEvent, step] at hstep
    split at hstep
    · simp only [Option.some.injEq] at hstep
      subst hstep
      simp only [conservation] at hcon ⊢
      rw [sumBal_foldl_bump, bump_sum]
      rw [Int.mul_comm w]
      omega
    · exact Option.noConfusion hstep

/-! ### AUTH -/

/-- **AUTH**: every successful declaration is authored by a responsabile. -/
theorem step_authorized {s s' : State} {e : Event} (h : stepEvent view s e = some s') :
    authorizedStep view s e s' := by
  cases e with
  | addUser a u | electResponsabile a u
  | removeResponsabile a u | removeMember a u =>
    exact Option.noConfusion h
  | openPurchase a c =>
    simp only [stepEvent, step] at h
    show isResponsabile view a
    split at h
    · next g => exact bool_and_left g
    · exact Option.noConfusion h
  | grantPermission a c =>
    obtain ⟨_, _, _, hr, _⟩ := step_grant_inv h
    exact hr
  | denyPermission a c =>
    obtain ⟨_, _, _, hr, _⟩ := step_deny_inv h
    exact hr
  | deposit a u v =>
    simp only [stepEvent, step] at h
    show isResponsabile view a
    split at h
    · next g => exact bool_and_left (bool_and_left (bool_and_left g))
    · exact Option.noConfusion h
  | withdraw a u v =>
    simp only [stepEvent, step] at h
    show isResponsabile view a
    split at h
    · next g => exact bool_and_left (bool_and_left (bool_and_left (bool_and_left g)))
    · exact Option.noConfusion h
  | transferCassa a f v =>
    simp only [stepEvent, step] at h
    show isResponsabile view a
    split at h
    · next g =>
        exact bool_and_left (bool_and_left (bool_and_left g))
    · exact Option.noConfusion h
  | pledge a u c v =>
    obtain ⟨_, _, _, hg, _⟩ := step_pledge_inv h
    exact (pledge_guard_inv hg).1
  | acceptPledge a u c =>
    obtain ⟨_, _, _, _, _, _, hg, _⟩ := step_accept_inv h
    exact (auth_referente_guard_inv hg).1
  | refusePledge a u c =>
    obtain ⟨_, _, _, _, _, _, hg, _⟩ := step_refuse_inv h
    exact (auth_referente_guard_inv hg).1
  | correctPledge a u c v =>
    obtain ⟨_, _, _, _, _, _, hg, _⟩ := step_correct_inv h
    exact bool_and_left
      (bool_and_left (bool_and_left hg))
  | closePurchase a c =>
    obtain ⟨_, _, _, hg, _⟩ := step_close_inv h
    exact (close_guard_inv hg).1
  | failPurchase a c =>
    obtain ⟨_, _, _, hg, _⟩ := step_fail_inv h
    exact (fail_guard_inv hg).1
  | donate a v =>
    simp only [stepEvent, step] at h
    show isResponsabile view a
    split at h
    · next g => exact bool_and_left g
    · exact Option.noConfusion h
  | backdonate a w =>
    simp only [stepEvent, step] at h
    show isResponsabile view a
    split at h
    · next g => exact bool_and_left (bool_and_left (bool_and_left g))
    · exact Option.noConfusion h

/-! ### L1 governance enacts -/

/-- Removing responsabile `u` cancels their open questions: no collection
left in the state names `u` as referente. -/
theorem governance_enacts_remove {s s' : State} {a u : KelGroups.Key}
    (h : stepEvent view s (.removeResponsabile a u) = some s') :
    governanceEnacts u s' :=
  Option.noConfusion h

/-! ### L2 closure permission -/

/-- A positive closure only happens on a collection that had group assent
(`permitted`) and zero pending pledges. -/
theorem close_permission_to_close {s s' : State} {a : KelGroups.Key} {c : CollId}
    (h : stepEvent view s (.closePurchase a c) = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧ permissionToClose col := by
  obtain ⟨col, rest, hpull, hg, _⟩ := step_close_inv h
  obtain ⟨_, _, hperm, hempty⟩ := close_guard_inv hg
  exact ⟨col, rest, hpull, hperm, hempty⟩

/-! ### L3 escrow at pledge -/

/-- A successful pledge debits the pledger immediately and holds exactly
the pledged amount in the collection's escrow. -/
theorem pledge_escrow_debit {s s' : State} {a u : KelGroups.Key} {c : CollId} {v : Int}
    (h : stepEvent view s (.pledge a u c v) = some s') :
    bal s'.conti u = bal s.conti u - v ∧
      ∃ col ∈ s'.collections, col.id = c ∧ escrowHeld col u v := by
  obtain ⟨col, rest, hpull, _, hs'⟩ := step_pledge_inv h
  have hid : col.id = c := pullCollection_id hpull
  subst hs'
  refine ⟨?_, { col with pending := ⟨u, v⟩ :: col.pending },
    List.mem_cons_self .., hid, ?_⟩
  · show bal (bump s.conti u (-v)) u = bal s.conti u - v
    have hb := bal_bump s.conti u (-v)
    omega
  · refine ⟨col.pending, ?_⟩
    show splitUser u (⟨u, v⟩ :: col.pending) = some (v, col.pending)
    simp [splitUser]

/-! ### L4 closure spends the referente's cassa -/

/-- Positive closure moves the collected total out of the referente's
cash box. -/
theorem close_spends_referente {s s' : State} {a : KelGroups.Key} {c : CollId}
    (h : stepEvent view s (.closePurchase a c) = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧
      bal s'.casse col.referente
        = bal s.casse col.referente - sumPledges col.accepted := by
  obtain ⟨col, rest, hpull, _, hs'⟩ := step_close_inv h
  subst hs'
  refine ⟨col, rest, hpull, ?_⟩
  show bal (bump s.casse col.referente (-(sumPledges col.accepted))) col.referente
    = bal s.casse col.referente - sumPledges col.accepted
  have hb := bal_bump s.casse col.referente (-(sumPledges col.accepted))
  omega

/-! ### L5 double entry -/

/-- Deposits move the user's conto and the acting cashier's cassa
together. -/
theorem deposit_double_entry {s s' : State} {a u : KelGroups.Key} {v : Int}
    (h : stepEvent view s (.deposit a u v) = some s') : doubleEntry s s' a u v := by
  simp only [stepEvent, step] at h
  split at h
  · simp only [Option.some.injEq] at h
    subst h
    exact ⟨bal_bump .., bal_bump ..⟩
  · exact Option.noConfusion h

/-- Withdrawals are symmetric to deposits. -/
theorem withdraw_double_entry {s s' : State} {a u : KelGroups.Key} {v : Int}
    (h : stepEvent view s (.withdraw a u v) = some s') : doubleEntry s s' a u (-v) := by
  simp only [stepEvent, step] at h
  split at h
  · simp only [Option.some.injEq] at h
    subst h
    exact ⟨bal_bump .., bal_bump ..⟩
  · exact Option.noConfusion h

/-! ### L7 solvency: overdrafts are rejected, insolvency unreachable -/

/-- `bump` leaves every other key's balance untouched. -/
private theorem bal_bump_ne_lemma {u : KelGroups.Key} {d : Int} :
    ∀ (m : List (KelGroups.Key × Int)) (k : KelGroups.Key), k ≠ u →
      bal (bump m u d) k = bal m k := by
  intro m
  induction m with
  | nil =>
    intro k hk
    show bal [(u, d)] k = bal [] k
    simp only [bal, if_neg (Ne.symm hk)]
  | cons kv t ih =>
    obtain ⟨k', v⟩ := kv
    intro k hk
    rw [bump]
    split
    · next h =>
      have hkk' : k' ≠ k := fun hc => hk (hc.symm.trans h)
      show bal ((k', v + d) :: t) k = bal ((k', v) :: t) k
      rw [bal_cons, bal_cons, if_neg hkk', if_neg hkk']
    · next h =>
      show bal ((k', v) :: bump t u d) k = bal ((k', v) :: t) k
      rw [bal_cons, bal_cons]
      split
      · rfl
      · exact ih k hk

/-- `bump` leaves every other key's balance untouched. -/
theorem bal_bump_ne {m : List (KelGroups.Key × Int)} {u : KelGroups.Key} {d : Int} {k : KelGroups.Key}
    (hk : k ≠ u) : bal (bump m u d) k = bal m k :=
  bal_bump_ne_lemma m k hk

/-- Folding a non-negative `bump` never lowers a key's balance. -/
private theorem bal_foldl_bump_ge :
    ∀ (l : List KelGroups.Key) (m : List (KelGroups.Key × Int)) (w : Int) (k : KelGroups.Key),
      0 ≤ w →
        bal (l.foldl (fun acc u => bump acc u w) m) k ≥ bal m k := by
  intro l
  induction l with
  | nil =>
    intro m w k hw
    simp
  | cons x t ih =>
    intro m w k hw
    simp [List.foldl]
    have htail := ih (bump m x w) w k hw
    by_cases hx : k = x
    · rw [hx] at htail ⊢
      have hb := bal_bump m x w
      omega
    · have hb : bal (bump m x w) k = bal m k := bal_bump_ne hx
      omega

/-- A successful split names the pledge it removed. -/
private theorem splitUser_amount_lemma {u : KelGroups.Key} :
    ∀ (l : List Pledge) (v : Int) (r : List Pledge),
      splitUser u l = some (v, r) → ∃ p ∈ l, p.user = u ∧ p.amount = v := by
  intro l
  induction l with
  | nil => intro v r h; exact Option.noConfusion h
  | cons p t ih =>
    intro v r h
    rw [splitUser] at h
    split at h
    · next hp =>
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      exact ⟨p, List.mem_cons_self .., hp, rfl⟩
    · next hp =>
      cases hx : splitUser u t with
      | none => rw [hx] at h; exact Option.noConfusion h
      | some w =>
        obtain ⟨wv, wr⟩ := w
        rw [hx] at h
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨hv, hr⟩ := h
        obtain ⟨q, hq, hqu, hqa⟩ := ih wv wr hx
        exact ⟨q, List.mem_cons_of_mem _ hq, hqu, hv ▸ hqa⟩

theorem splitUser_amount {u : KelGroups.Key} {l : List Pledge} {v : Int} {r : List Pledge}
    (h : splitUser u l = some (v, r)) : ∃ p ∈ l, p.user = u ∧ p.amount = v :=
  splitUser_amount_lemma l v r h

/-- Refunding only non-negative amounts never lowers a balance. -/
private theorem refundAll_bal_ge_lemma {l : List Pledge} :
    ∀ (m : List (KelGroups.Key × Int)) (w : KelGroups.Key),
      (∀ p ∈ l, 0 ≤ p.amount) → bal (refundAll m l) w ≥ bal m w := by
  induction l with
  | nil => intro m w _; show bal m w ≥ bal m w; omega
  | cons p t ih =>
    intro m w hamt
    have hamt' : ∀ q ∈ t, 0 ≤ q.amount := fun q hq => hamt q (List.mem_cons_of_mem _ hq)
    have ha := hamt p (List.mem_cons_self ..)
    have h2 : bal (List.foldl (fun acc q => bump acc q.user q.amount)
          (bump m p.user p.amount) t) w
        ≥ bal (bump m p.user p.amount) w :=
      ih (bump m p.user p.amount) w hamt'
    show bal (List.foldl (fun acc q => bump acc q.user q.amount)
          (bump m p.user p.amount) t) w ≥ bal m w
    by_cases hc : w = p.user
    · rw [hc] at h2 ⊢
      have hb := bal_bump m p.user p.amount
      omega
    · have hb : bal (bump m p.user p.amount) w = bal m w := bal_bump_ne hc
      omega

theorem refundAll_bal_ge {m : List (KelGroups.Key × Int)} {l : List Pledge} {w : KelGroups.Key}
    (hamt : ∀ p ∈ l, 0 ≤ p.amount) : bal (refundAll m l) w ≥ bal m w :=
  refundAll_bal_ge_lemma m w hamt

/-- Collections left behind by stripping `r` were in the original list. -/
private theorem stripCollections_sublist_lemma (r : KelGroups.Key) :
    ∀ (cols : List Collection) (y : Collection), y ∈ (stripCollections r cols).1 →
      y ∈ cols := by
  intro cols
  induction cols with
  | nil => intro y hy; cases hy
  | cons c t ih =>
    intro y hy
    simp only [stripCollections] at hy
    split at hy
    · next _ =>
      dsimp only at hy
      exact List.mem_cons_of_mem _ (ih y hy)
    · next _ =>
      dsimp only at hy
      rcases List.mem_cons.mp hy with hc | hc
      · exact List.mem_cons.mpr (Or.inl hc)
      · exact List.mem_cons_of_mem _ (ih y hc)

theorem stripCollections_sublist (r : KelGroups.Key) (cols : List Collection)
    {y : Collection} (hy : y ∈ (stripCollections r cols).1) : y ∈ cols :=
  stripCollections_sublist_lemma r cols y hy

/-- Every refunded pledge comes from some collection of the original list. -/
private theorem stripCollections_amount_lemma (r : KelGroups.Key) :
    ∀ (cols : List Collection) (p : Pledge),
      p ∈ (stripCollections r cols).2 →
        ∃ c ∈ cols, p ∈ c.accepted ++ c.pending := by
  intro cols
  induction cols with
  | nil => intro p hp; cases hp
  | cons c t ih =>
    intro p hp
    simp only [stripCollections] at hp
    split at hp
    · next _ =>
      dsimp only at hp
      rcases List.mem_append.mp hp with hm | hm
      · rcases List.mem_append.mp hm with hm1 | hm2
        · exact ⟨c, List.mem_cons_self .., List.mem_append.mpr (Or.inl hm1)⟩
        · exact ⟨c, List.mem_cons_self .., List.mem_append.mpr (Or.inr hm2)⟩
      · obtain ⟨c', hc', hp'⟩ := ih p hm
        exact ⟨c', List.mem_cons_of_mem _ hc', hp'⟩
    · next _ =>
      dsimp only at hp
      obtain ⟨c', hc', hp'⟩ := ih p hp
      exact ⟨c', List.mem_cons_of_mem _ hc', hp'⟩

/-- Empty payload is solvent: accounts start empty and there is no escrow. -/
theorem solvent_init : solvent view State.empty :=
  ⟨fun _ _ => by simp [State.empty, bal], by
    intro col hc
    cases hc⟩

/-- No event admits `comuneId` into `users`. -/
private theorem comune_not_a_member_step {s s' : State} {e : Event}
    (h : comune_not_a_member view) (_hstep : stepEvent view s e = some s') :
    comune_not_a_member view := h

/-- Non-comune credits and pledged amounts are preserved by a successful
step. Stronger than member-scoped `solvent`: a dormant non-member conto
cannot go negative, so `addUser` cannot expose hidden debt. -/
private theorem credit_pledges_step {s s' : State} {e : Event}
    (hcred : ∀ u : KelGroups.Key, u ≠ comuneId → bal s.conti u ≥ 0)
    (hamt : ∀ col ∈ s.collections, ∀ p ∈ col.accepted ++ col.pending, 0 ≤ p.amount)
    (hstep : stepEvent view s e = some s') :
    (∀ u : KelGroups.Key, u ≠ comuneId → bal s'.conti u ≥ 0) ∧
      (∀ col ∈ s'.collections, ∀ p ∈ col.accepted ++ col.pending, 0 ≤ p.amount) := by
  cases e with
  | addUser a u | electResponsabile a u
  | removeResponsabile a u | removeMember a u =>
    exact Option.noConfusion hstep
  | openPurchase a c =>
    simp only [stepEvent, step] at hstep
    split at hstep
    · simp only [Option.some.injEq] at hstep
      subst hstep
      refine ⟨hcred, ?_⟩
      intro c0 hc0 p hp
      rcases List.mem_cons.mp hc0 with hc0 | hc0
      · subst hc0
        simp at hp
      · exact hamt c0 hc0 p hp
    · exact Option.noConfusion hstep
  | grantPermission a c =>
    obtain ⟨col, rest, hpull, _, hs'⟩ := step_grant_inv hstep
    subst hs'
    refine ⟨hcred, ?_⟩
    intro c0 hc0 p hp
    rcases List.mem_cons.mp hc0 with hc0 | hc0
    · subst hc0
      dsimp only at hp
      exact hamt col (pullCollection_mem hpull) p hp
    · exact hamt c0 (pullCollection_sublist hpull c0 hc0) p hp
  | denyPermission a c =>
    obtain ⟨col, rest, hpull, _, hs'⟩ := step_deny_inv hstep
    subst hs'
    refine ⟨?_, ?_⟩
    · intro w hwne
      show bal (refundAll s.conti (col.accepted ++ col.pending)) w ≥ 0
      have h1 : bal (refundAll s.conti (col.accepted ++ col.pending)) w
          ≥ bal s.conti w :=
        refundAll_bal_ge (fun p hp => hamt col (pullCollection_mem hpull) p hp)
      have h2 := hcred w hwne
      omega
    · intro c0 hc0 p hp
      exact hamt c0 (pullCollection_sublist hpull c0 hc0) p hp
  | deposit a u v =>
    simp only [stepEvent, step] at hstep
    split at hstep
    · next g =>
      simp only [Option.some.injEq] at hstep
      subst hstep
      have hv : 0 ≤ v := decide_eq_true_iff.mp (bool_and_right g)
      refine ⟨fun w hwne => ?_, hamt⟩
      by_cases hc : w = u
      · rw [hc]
        show bal (bump s.conti u v) u ≥ 0
        have hb := bal_bump s.conti u v
        have h0 := hcred u (hc ▸ hwne)
        omega
      · show bal (bump s.conti u v) w ≥ 0
        rw [bal_bump_ne hc]
        exact hcred w hwne
    · exact Option.noConfusion hstep
  | withdraw a u v =>
    simp only [stepEvent, step] at hstep
    split at hstep
    · next g =>
      simp only [Option.some.injEq] at hstep
      subst hstep
      have hv : bal s.conti u ≥ v :=
        decide_eq_true_iff.mp (bool_and_right (bool_and_left g))
      refine ⟨fun w hwne => ?_, hamt⟩
      by_cases hc : w = u
      · rw [hc]
        show bal (bump s.conti u (-v)) u ≥ 0
        have hb := bal_bump s.conti u (-v)
        omega
      · show bal (bump s.conti u (-v)) w ≥ 0
        rw [bal_bump_ne hc]
        exact hcred w hwne
    · exact Option.noConfusion hstep
  | transferCassa a f v =>
    simp only [stepEvent, step] at hstep
    split at hstep
    · simp only [Option.some.injEq] at hstep
      subst hstep
      exact ⟨hcred, hamt⟩
    · exact Option.noConfusion hstep
  | pledge a u c v =>
    obtain ⟨col, rest, hpull, hg, hs'⟩ := step_pledge_inv hstep
    obtain ⟨_, _, _, _, hvpos, hfunds⟩ := pledge_guard_inv hg
    subst hs'
    refine ⟨fun w hwne => ?_, ?_⟩
    · by_cases hc : w = u
      · rw [hc]
        show bal (bump s.conti u (-v)) u ≥ 0
        have hb := bal_bump s.conti u (-v)
        omega
      · show bal (bump s.conti u (-v)) w ≥ 0
        rw [bal_bump_ne hc]
        exact hcred w hwne
    · intro c0 hc0 p hp
      rcases List.mem_cons.mp hc0 with hc0 | hc0
      · subst hc0
        dsimp only at hp
        rcases List.mem_append.mp hp with hm | hm
        · exact hamt col (pullCollection_mem hpull) p
            (List.mem_append.mpr (Or.inl hm))
        · rcases List.mem_cons.mp hm with heq | hinP
          · subst heq
            show 0 ≤ v
            omega
          · exact hamt col (pullCollection_mem hpull) p
              (List.mem_append.mpr (Or.inr hinP))
      · exact hamt c0 (pullCollection_sublist hpull c0 hc0) p hp
  | acceptPledge a u c =>
    obtain ⟨col, rest, v, pend', hpull, hspl, _, hs'⟩ := step_accept_inv hstep
    obtain ⟨q, hq, -, hqa⟩ := splitUser_amount hspl
    have hv : 0 ≤ v := by
      have h0 := hamt col (pullCollection_mem hpull) q
        (List.mem_append.mpr (Or.inr hq))
      omega
    subst hs'
    refine ⟨hcred, ?_⟩
    intro c0 hc0 p hp
    rcases List.mem_cons.mp hc0 with hc0 | hc0
    · subst hc0
      dsimp only at hp
      rcases List.mem_append.mp hp with hm | hm
      · rcases List.mem_cons.mp hm with heq | hinA
        · subst heq
          exact hv
        · exact hamt col (pullCollection_mem hpull) p
            (List.mem_append.mpr (Or.inl hinA))
      · exact hamt col (pullCollection_mem hpull) p
          (List.mem_append.mpr (Or.inr (splitUser_sublist hspl p hm)))
    · exact hamt c0 (pullCollection_sublist hpull c0 hc0) p hp
  | refusePledge a u c =>
    obtain ⟨col, rest, v, pend', hpull, hspl, _, hs'⟩ := step_refuse_inv hstep
    obtain ⟨q, hq, -, hqa⟩ := splitUser_amount hspl
    have hv : 0 ≤ v := by
      have h0 := hamt col (pullCollection_mem hpull) q
        (List.mem_append.mpr (Or.inr hq))
      omega
    subst hs'
    refine ⟨fun w hwne => ?_, ?_⟩
    · by_cases hc : w = u
      · rw [hc]
        show bal (bump s.conti u v) u ≥ 0
        have hb := bal_bump s.conti u v
        have h0 := hcred u (hc ▸ hwne)
        omega
      · show bal (bump s.conti u v) w ≥ 0
        rw [bal_bump_ne hc]
        exact hcred w hwne
    · intro c0 hc0 p hp
      rcases List.mem_cons.mp hc0 with hc0 | hc0
      · subst hc0
        dsimp only at hp
        rcases List.mem_append.mp hp with hm | hm
        · exact hamt col (pullCollection_mem hpull) p
            (List.mem_append.mpr (Or.inl hm))
        · exact hamt col (pullCollection_mem hpull) p
            (List.mem_append.mpr (Or.inr (splitUser_sublist hspl p hm)))
      · exact hamt c0 (pullCollection_sublist hpull c0 hc0) p hp
  | correctPledge a u c v' =>
    obtain ⟨col, rest, v, acc', hpull, hspl, hg, hs'⟩ := step_correct_inv hstep
    have hv' : 0 ≤ v' :=
      decide_eq_true_iff.mp (bool_and_right (bool_and_left hg))
    have hfunds : bal s.conti u + (v - v') ≥ 0 :=
      decide_eq_true_iff.mp (bool_and_right hg)
    subst hs'
    refine ⟨fun w hwne => ?_, ?_⟩
    · by_cases hc : w = u
      · rw [hc]
        show bal (bump s.conti u (v - v')) u ≥ 0
        have hb := bal_bump s.conti u (v - v')
        omega
      · show bal (bump s.conti u (v - v')) w ≥ 0
        rw [bal_bump_ne hc]
        exact hcred w hwne
    · intro c0 hc0 p hp
      rcases List.mem_cons.mp hc0 with hc0 | hc0
      · subst hc0
        dsimp only at hp
        rcases List.mem_append.mp hp with hm | hm
        · rcases List.mem_cons.mp hm with heq | hinA
          · subst heq
            show 0 ≤ v'
            omega
          · exact hamt col (pullCollection_mem hpull) p
              (List.mem_append.mpr (Or.inl (splitUser_sublist hspl p hinA)))
        · exact hamt col (pullCollection_mem hpull) p
            (List.mem_append.mpr (Or.inr hm))
      · exact hamt c0 (pullCollection_sublist hpull c0 hc0) p hp
  | closePurchase a c =>
    obtain ⟨col, rest, hpull, _, hs'⟩ := step_close_inv hstep
    subst hs'
    refine ⟨hcred, ?_⟩
    intro c0 hc0 p hp
    exact hamt c0 (pullCollection_sublist hpull c0 hc0) p hp
  | failPurchase a c =>
    obtain ⟨col, rest, hpull, _, hs'⟩ := step_fail_inv hstep
    subst hs'
    refine ⟨?_, ?_⟩
    · intro w hwne
      show bal (refundAll s.conti (col.accepted ++ col.pending)) w ≥ 0
      have h1 : bal (refundAll s.conti (col.accepted ++ col.pending)) w
          ≥ bal s.conti w :=
        refundAll_bal_ge (fun p hp => hamt col (pullCollection_mem hpull) p hp)
      have h2 := hcred w hwne
      omega
    · intro c0 hc0 p hp
      exact hamt c0 (pullCollection_sublist hpull c0 hc0) p hp
  | donate a v =>
    simp only [stepEvent, step] at hstep
    split at hstep
    · simp only [Option.some.injEq] at hstep
      subst hstep
      refine ⟨?_, hamt⟩
      intro w hwne
      rw [bal_bump_ne hwne]
      exact hcred w hwne
    · exact Option.noConfusion hstep
  | backdonate a w =>
    simp only [stepEvent, step] at hstep
    split at hstep
    · next g =>
      simp only [Option.some.injEq] at hstep
      subst hstep
      have hwpos : 0 < w :=
        decide_eq_true_iff.mp
          (bool_and_right (bool_and_left (bool_and_left g)))
      refine ⟨?_, hamt⟩
      intro k hkne
      show bal
          ((memberKeys view).foldl (fun acc u => bump acc u w)
            (bump s.conti comuneId
              (-(((memberKeys view).length : Int) * w)))) k ≥ 0
      have hge :=
        bal_foldl_bump_ge (memberKeys view)
          (bump s.conti comuneId (-(((memberKeys view).length : Int) * w))) w k
          (Int.le_of_lt hwpos)
      have h0 : bal (bump s.conti comuneId
            (-(((memberKeys view).length : Int) * w))) k
          = bal s.conti k := bal_bump_ne hkne
      have hcredk := hcred k hkne
      omega
    · exact Option.noConfusion hstep

/-- The comune account is never a member of any state reachable from
boot: the guarded boot excludes `comuneId` and every event preserves
the exclusion (`addUser` refuses it and no other event inserts it). -/
theorem comune_not_a_member_of_reach {s : State} (hr : Reach view s) :
    comune_not_a_member view := by
  induction hr with
  | boot h => exact h
  | trans _ _ ih => exact ih

/-- Non-comune credits and pledged amounts on every reachable state. -/
private theorem credit_pledges_of_reach {s : State} (hr : Reach view s) :
    (∀ u : KelGroups.Key, u ≠ comuneId → bal s.conti u ≥ 0) ∧
      (∀ col ∈ s.collections, ∀ p ∈ col.accepted ++ col.pending, 0 ≤ p.amount) := by
  induction hr with
  | boot h =>
    refine ⟨?_, ?_⟩
    · intro u _
      simp [State.empty, bal]
    · intro col hc p hp
      cases hc
  | trans _ hstep ih =>
    exact credit_pledges_step ih.1 ih.2 hstep

/-- **L7 flagship**: every successful event preserves solvency — all
balances stay non-negative and all pledged amounts stay non-negative,
so refunds can never push an account below zero. -/
theorem solvent_preserved {s s' : State} {e : Event}
    (hr : Reach view s)
    (hsolv : solvent view s) (hstep : stepEvent view s e = some s') : solvent view s' := by
  have := hsolv
  have ⟨hcred, hamt⟩ := credit_pledges_step
    (credit_pledges_of_reach hr).1 (credit_pledges_of_reach hr).2 hstep
  have hcom : comune_not_a_member view :=
    comune_not_a_member_step (comune_not_a_member_of_reach hr) hstep
  refine ⟨?_, hamt⟩
  intro u hu
  exact hcred u (fun heq => hcom (heq ▸ hu))

/-- Solvency holds on every state reachable from boot. -/
theorem reach_solvent {s : State} (hr : Reach view s) : solvent view s := by
  induction hr with
  | boot h => exact solvent_init
  | trans hr hstep ih => exact solvent_preserved hr ih hstep

/-- Insolvency is impossible: no reachable state has a negative member
account. Group (comune) insolvency remains reachable by design. -/
theorem not_insolvent_of_reach {s : State} (hr : Reach view s) : ¬ insolvent view s := by
  intro ⟨u, hmem, hneg⟩
  have hs := (reach_solvent hr).1 u hmem
  omega

/-! ### L8 one pledge per user per collection -/

/-- Core 3×3 case table: uniqueness is preserved when one fresh pledge of
an absent user is consed onto the pending list. Stated over plain lists
to keep every membership syntactically aligned. -/
private theorem unique_mem_cons_inv {acc pend : List Pledge} {u : KelGroups.Key} {v : Int}
    {p q : Pledge}
    (hp : p ∈ acc ++ (⟨u, v⟩ :: pend)) (hq : q ∈ acc ++ (⟨u, v⟩ :: pend))
    (hus : p.user = q.user)
    (hu : ∀ w ∈ acc ++ pend, w.user ≠ u)
    (hun : ∀ a ∈ acc ++ pend, ∀ b ∈ acc ++ pend, a.user = b.user → a = b) :
    p = q := by
  rcases List.mem_append.mp hp with hpa | hpp
  · rcases List.mem_append.mp hq with hqa | hqp
    · exact hun p (List.mem_append.mpr (Or.inl hpa)) q
        (List.mem_append.mpr (Or.inl hqa)) hus
    · rcases List.mem_cons.mp hqp with heqq | hinP'
      · subst heqq
        exact absurd (hus.trans rfl) (hu p (List.mem_append.mpr (Or.inl hpa)))
      · exact hun p (List.mem_append.mpr (Or.inl hpa)) q
          (List.mem_append.mpr (Or.inr hinP')) hus
  · rcases List.mem_cons.mp hpp with heqp | hinP
    · subst heqp
      rcases List.mem_append.mp hq with hqa | hqp
      · exact absurd hus.symm (hu q (List.mem_append.mpr (Or.inl hqa)))
      · rcases List.mem_cons.mp hqp with heqq | hinP'
        · subst heqq; rfl
        · exact absurd hus.symm (hu q (List.mem_append.mpr (Or.inr hinP')))
    · rcases List.mem_append.mp hq with hqa | hqp
      · exact hun p (List.mem_append.mpr (Or.inr hinP)) q
          (List.mem_append.mpr (Or.inl hqa)) hus
      · rcases List.mem_cons.mp hqp with heqq | hinP'
        · subst heqq
          exact absurd (hus.trans rfl) (hu p (List.mem_append.mpr (Or.inr hinP)))
        · exact hun p (List.mem_append.mpr (Or.inr hinP)) q
            (List.mem_append.mpr (Or.inr hinP')) hus

/-- Uniqueness survives consing one new pledge of an absent user onto the
pending list. -/
theorem uniquePledges_pend_cons {col : Collection} {u : KelGroups.Key} {v : Int}
    (hu : ∀ p ∈ col.accepted ++ col.pending, p.user ≠ u)
    (hun : uniquePledges col) :
    uniquePledges { col with pending := ⟨u, v⟩ :: col.pending } := by
  unfold uniquePledges at hun
  intro p hp q hq hus
  have hp0 := hp
  have hq0 := hq
  dsimp only at hp0 hq0
  exact unique_mem_cons_inv hp0 hq0 hus hu hun

/-- A second pledge by the same user in the same collection is rejected:
the guard scans both pledge lists for the pledger. -/
theorem pledge_rejected_when_member {s : State} {a u : KelGroups.Key} {c : CollId}
    {v : Int} {col : Collection} {rest : List Collection}
    (hpull : pullCollection c s.collections = some (col, rest))
    (hdup : ∃ q, q ∈ col.accepted ++ col.pending ∧ q.user = u) :
    stepEvent view s (.pledge a u c v) = none := by
  by_cases hnone : stepEvent view s (.pledge a u c v) = none
  · exact hnone
  · cases hstep : stepEvent view s (.pledge a u c v) with
    | none => exact absurd hstep hnone
    | some s' =>
      obtain ⟨col₀, rest₀, hpull₀, hg, _⟩ := step_pledge_inv hstep
      obtain ⟨hdet, -⟩ : col₀ = col ∧ rest₀ = rest := by
        have hpair := pullCollection_det hpull₀ hpull
        simpa using hpair
      obtain ⟨_, _, hn1, hn2, _, _⟩ := pledge_guard_inv hg
      rw [hdet] at hn1 hn2
      obtain ⟨q, hq, hqu⟩ := hdup
      rcases List.mem_append.mp hq with hm | hm
      · have ht : col.accepted.any (fun p => p.user == u) = true :=
          List.any_eq_true.mpr ⟨q, hm, by simp [hqu]⟩
        rw [ht] at hn1
        exact Bool.noConfusion hn1
      · have ht : col.pending.any (fun p => p.user == u) = true :=
          List.any_eq_true.mpr ⟨q, hm, by simp [hqu]⟩
        rw [ht] at hn2
        exact Bool.noConfusion hn2

/-- Pledging preserves L8 across the whole state. -/
theorem pledge_preserves_allUnique {s s' : State} {a u : KelGroups.Key} {c : CollId}
    {v : Int} (hun : allUniquePledges s)
    (h : stepEvent view s (.pledge a u c v) = some s') : allUniquePledges s' := by
  obtain ⟨col, rest, hpull, hg, hs'⟩ := step_pledge_inv h
  obtain ⟨_, _, hna1, hna2, _, _⟩ := pledge_guard_inv hg
  subst hs'
  have ha1 := user_absent_of_any_false hna1
  have ha2 := user_absent_of_any_false hna2
  have habs : ∀ p ∈ col.accepted ++ col.pending, p.user ≠ u := by
    intro p hp
    rcases List.mem_append.mp hp with hm | hm
    · exact ha1 p hm
    · exact ha2 p hm
  intro c0 hc0
  rcases List.mem_cons.mp hc0 with hc0 | hc0
  · subst hc0
    exact uniquePledges_pend_cons habs (hun col (pullCollection_mem hpull))
  · exact hun c0 (pullCollection_sublist hpull c0 hc0)
