import Reactivegas.Predicates

/-!
# Invariants and preservation theorems

The flagship is `conservation_preserved` (L6): every successful event
preserves `Σ casse − Σ conti − Σ open escrow = 0`. Each documented law
(L1–L8) gets at least one machine-checked theorem; L7 shows that soft
insolvency is *reachable* and therefore must stay reportable rather
than rejected.
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
theorem stripCollections_referente_ne (u : UserId) (cols : List Collection) :
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
theorem user_absent_of_any_false {u : UserId} {l : List Pledge}
    (h : l.any (fun p => p.user == u) = false) : ∀ p ∈ l, p.user ≠ u := by
  intro p hp
  intro hpu
  have ht : l.any (fun p => p.user == u) = true :=
    List.any_eq_true.mpr ⟨p, hp, by simp [hpu]⟩
  rw [h] at ht
  exact Bool.noConfusion ht

/-! ### Guard inversions -/

/-- Decompose the pledge guard into its conjuncts. -/
theorem pledge_guard_inv {s : State} {a u : UserId} {col : Collection}
    (h : (isResponsabile s a && s.users.contains u &&
      !(col.accepted.any (fun p => p.user == u)) &&
      !(col.pending.any (fun p => p.user == u))) = true) :
    isResponsabile s a = true ∧ s.users.contains u = true ∧
      col.accepted.any (fun p => p.user == u) = false ∧
      col.pending.any (fun p => p.user == u) = false := by
  have hn2 := bool_and_right h
  have hn1 := bool_and_right (bool_and_left h)
  have hAB := bool_and_left (bool_and_left h)
  exact ⟨bool_and_left hAB, bool_and_right hAB, bool_not_true hn1, bool_not_true hn2⟩

/-- Decompose the accept/refuse/correct guard. -/
theorem auth_referente_guard_inv {s : State} {a : UserId} {col : Collection}
    (h : (isResponsabile s a && col.referente == a) = true) :
    isResponsabile s a = true ∧ col.referente = a :=
  ⟨bool_and_left h, beq_iff_eq.mp (bool_and_right h)⟩

/-- Decompose the positive-closure guard. -/
theorem close_guard_inv {s : State} {a : UserId} {col : Collection}
    (h : (isResponsabile s a && col.referente == a && col.permitted &&
      col.pending.isEmpty) = true) :
    isResponsabile s a = true ∧ col.referente = a ∧ col.permitted ∧ col.pending = [] :=
  ⟨bool_and_left (bool_and_left (bool_and_left h)),
    beq_iff_eq.mp (bool_and_right (bool_and_left (bool_and_left h))),
    bool_and_right (bool_and_left h),
    eq_nil_of_isEmpty (bool_and_right h)⟩

/-- Decompose the failure-closure guard. -/
theorem fail_guard_inv {s : State} {a : UserId} {col : Collection}
    (h : (isResponsabile s a && col.referente == a && col.pending.isEmpty) = true) :
    isResponsabile s a = true ∧ col.referente = a ∧ col.pending = [] :=
  ⟨bool_and_left (bool_and_left h),
    beq_iff_eq.mp (bool_and_right (bool_and_left h)),
    eq_nil_of_isEmpty (bool_and_right h)⟩

/-! ### Event inversions -/

theorem step_grant_inv {s s' : State} {a : UserId} {c : CollId}
    (hstep : step s (.grantPermission a c) = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧
      isResponsabile s a = true ∧
      s' = { s with collections := { col with permitted := true } :: rest } := by
  obtain ⟨w1, hw1, hw2⟩ := option_bind_inv hstep
  obtain ⟨col, rest⟩ := w1
  obtain ⟨_, hdem, hx⟩ := option_bind_inv hw2
  refine ⟨col, rest, hw1, ?_, ?_⟩
  · exact demand_eq_true_of_some hdem
  · simp only [pure, Option.some.injEq] at hx
    exact hx.symm

theorem step_deny_inv {s s' : State} {a : UserId} {c : CollId}
    (hstep : step s (.denyPermission a c) = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧
      isResponsabile s a = true ∧
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

theorem step_pledge_inv {s s' : State} {a u : UserId} {c : CollId} {v : Int}
    (hstep : step s (.pledge a u c v) = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧
      (isResponsabile s a && s.users.contains u &&
          !(col.accepted.any (fun p => p.user == u)) &&
          !(col.pending.any (fun p => p.user == u))) =
        true ∧
      s' = { s with
        conti := bump s.conti u (-v),
        collections := { col with pending := ⟨u, v⟩ :: col.pending } :: rest } := by
  obtain ⟨w1, hw1, hw2⟩ := option_bind_inv hstep
  obtain ⟨col, rest⟩ := w1
  obtain ⟨_, hdem, hx⟩ := option_bind_inv hw2
  refine ⟨col, rest, hw1, ?_, ?_⟩
  · exact demand_eq_true_of_some hdem
  · simp only [pure, Option.some.injEq] at hx
    exact hx.symm

theorem step_accept_inv {s s' : State} {a u : UserId} {c : CollId}
    (hstep : step s (.acceptPledge a u c) = some s') :
    ∃ col rest v pend',
      pullCollection c s.collections = some (col, rest) ∧
      splitUser u col.pending = some (v, pend') ∧
      (isResponsabile s a && col.referente == a) = true ∧
      s' = { s with collections :=
        { col with pending := pend', accepted := ⟨u, v⟩ :: col.accepted } :: rest } := by
  obtain ⟨w1, hw1, hw2⟩ := option_bind_inv hstep
  obtain ⟨col, rest⟩ := w1
  obtain ⟨w2, hw3, hw4⟩ := option_bind_inv hw2
  obtain ⟨v, pend'⟩ := w2
  obtain ⟨_, hdem, hx⟩ := option_bind_inv hw4
  refine ⟨col, rest, v, pend', hw1, hw3, ?_, ?_⟩
  · exact demand_eq_true_of_some hdem
  · simp only [pure, Option.some.injEq] at hx
    exact hx.symm

theorem step_refuse_inv {s s' : State} {a u : UserId} {c : CollId}
    (hstep : step s (.refusePledge a u c) = some s') :
    ∃ col rest v pend',
      pullCollection c s.collections = some (col, rest) ∧
      splitUser u col.pending = some (v, pend') ∧
      (isResponsabile s a && col.referente == a) = true ∧
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

theorem step_correct_inv {s s' : State} {a u : UserId} {c : CollId} {v' : Int}
    (hstep : step s (.correctPledge a u c v') = some s') :
    ∃ col rest v acc',
      pullCollection c s.collections = some (col, rest) ∧
      splitUser u col.accepted = some (v, acc') ∧
      (isResponsabile s a && col.referente == a) = true ∧
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

theorem step_close_inv {s s' : State} {a : UserId} {c : CollId}
    (hstep : step s (.closePurchase a c) = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧
      (isResponsabile s a && col.referente == a && col.permitted &&
        col.pending.isEmpty) = true ∧
      s' = { s with
        casse := bump s.casse col.referente (-(sumPledges col.accepted)),
        collections := rest } := by
  obtain ⟨w1, hw1, hw2⟩ := option_bind_inv hstep
  obtain ⟨col, rest⟩ := w1
  obtain ⟨_, hdem, hx⟩ := option_bind_inv hw2
  refine ⟨col, rest, hw1, ?_, ?_⟩
  · exact demand_eq_true_of_some hdem
  · simp only [pure, Option.some.injEq] at hx
    exact hx.symm

theorem step_fail_inv {s s' : State} {a : UserId} {c : CollId}
    (hstep : step s (.failPurchase a c) = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧
      (isResponsabile s a && col.referente == a && col.pending.isEmpty) = true ∧
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
    (hcon : conservation s) (hstep : step s e = some s') : conservation s' := by
  cases e with
  | addUser a u =>
    simp only [step] at hstep
    split at hstep
    · simp only [Option.some.injEq] at hstep
      subst hstep
      simpa only [conservation] using hcon
    · exact Option.noConfusion hstep
  | electResponsabile a u =>
    simp only [step] at hstep
    split at hstep
    · simp only [Option.some.injEq] at hstep
      subst hstep
      simpa only [conservation] using hcon
    · exact Option.noConfusion hstep
  | removeResponsabile a u =>
    simp only [step] at hstep
    split at hstep
    · simp only [Option.some.injEq] at hstep
      subst hstep
      simp only [conservation] at hcon ⊢
      rw [refundAll_sum]
      have hst := stripCollections_sum u s.collections
      omega
    · exact Option.noConfusion hstep
  | openPurchase a c =>
    simp only [step] at hstep
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
    simp only [step] at hstep
    split at hstep
    · simp only [Option.some.injEq] at hstep
      subst hstep
      simp only [conservation] at hcon ⊢
      rw [bump_sum, bump_sum]
      omega
    · exact Option.noConfusion hstep
  | withdraw a u v =>
    simp only [step] at hstep
    split at hstep
    · simp only [Option.some.injEq] at hstep
      subst hstep
      simp only [conservation] at hcon ⊢
      rw [bump_sum, bump_sum]
      omega
    · exact Option.noConfusion hstep
  | transferCassa a f v =>
    simp only [step] at hstep
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

/-! ### AUTH -/

/-- **AUTH**: every successful declaration is authored by a responsabile. -/
theorem step_authorized {s s' : State} {e : Event} (h : step s e = some s') :
    authorizedStep s e s' := by
  cases e with
  | addUser a u =>
    simp only [step] at h
    show isResponsabile s a
    split at h
    · next g => exact bool_and_left g
    · exact Option.noConfusion h
  | electResponsabile a u =>
    simp only [step] at h
    show isResponsabile s a
    split at h
    · next g => exact bool_and_left (bool_and_left g)
    · exact Option.noConfusion h
  | removeResponsabile a u =>
    simp only [step] at h
    show isResponsabile s a
    split at h
    · next g => exact bool_and_left g
    · exact Option.noConfusion h
  | openPurchase a c =>
    simp only [step] at h
    show isResponsabile s a
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
    simp only [step] at h
    show isResponsabile s a
    split at h
    · next g => exact bool_and_left (bool_and_left g)
    · exact Option.noConfusion h
  | withdraw a u v =>
    simp only [step] at h
    show isResponsabile s a
    split at h
    · next g => exact bool_and_left (bool_and_left g)
    · exact Option.noConfusion h
  | transferCassa a f v =>
    simp only [step] at h
    show isResponsabile s a
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
    exact (auth_referente_guard_inv hg).1
  | closePurchase a c =>
    obtain ⟨_, _, _, hg, _⟩ := step_close_inv h
    exact (close_guard_inv hg).1
  | failPurchase a c =>
    obtain ⟨_, _, _, hg, _⟩ := step_fail_inv h
    exact (fail_guard_inv hg).1

/-! ### L1 governance enacts -/

/-- Removing responsabile `u` cancels their open questions: no collection
left in the state names `u` as referente. -/
theorem governance_enacts_remove {s s' : State} {a u : UserId}
    (h : step s (.removeResponsabile a u) = some s') :
    governanceEnacts u s' := by
  simp only [step] at h
  split at h
  · simp only [Option.some.injEq] at h
    subst h
    intro c hc
    exact stripCollections_referente_ne u s.collections c hc
  · exact Option.noConfusion h

/-! ### L2 closure permission -/

/-- A positive closure only happens on a collection that had group assent
(`permitted`) and zero pending pledges. -/
theorem close_permission_to_close {s s' : State} {a : UserId} {c : CollId}
    (h : step s (.closePurchase a c) = some s') :
    ∃ col rest,
      pullCollection c s.collections = some (col, rest) ∧ permissionToClose col := by
  obtain ⟨col, rest, hpull, hg, _⟩ := step_close_inv h
  obtain ⟨_, _, hperm, hempty⟩ := close_guard_inv hg
  exact ⟨col, rest, hpull, hperm, hempty⟩

/-! ### L3 escrow at pledge -/

/-- A successful pledge debits the pledger immediately and holds exactly
the pledged amount in the collection's escrow. -/
theorem pledge_escrow_debit {s s' : State} {a u : UserId} {c : CollId} {v : Int}
    (h : step s (.pledge a u c v) = some s') :
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
theorem close_spends_referente {s s' : State} {a : UserId} {c : CollId}
    (h : step s (.closePurchase a c) = some s') :
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
theorem deposit_double_entry {s s' : State} {a u : UserId} {v : Int}
    (h : step s (.deposit a u v) = some s') : doubleEntry s s' a u v := by
  simp only [step] at h
  split at h
  · simp only [Option.some.injEq] at h
    subst h
    exact ⟨bal_bump .., bal_bump ..⟩
  · exact Option.noConfusion h

/-- Withdrawals are symmetric to deposits. -/
theorem withdraw_double_entry {s s' : State} {a u : UserId} {v : Int}
    (h : step s (.withdraw a u v) = some s') : doubleEntry s s' a u (-v) := by
  simp only [step] at h
  split at h
  · simp only [Option.some.injEq] at h
    subst h
    exact ⟨bal_bump .., bal_bump ..⟩
  · exact Option.noConfusion h

/-! ### L7 insolvency is reachable, hence reportable not rejected -/

/-- The state after booting with responsabile `0`, adding user `1`,
depositing 10 for them and withdrawing 50: user 1 sits at −40 while the
books still balance. -/
def badState : State :=
  { users := [0, 1], responsabili := [0], conti := [(1, -40)], casse := [(0, -40)], collections := [] }

theorem insolvency_reachable : Reach badState := by
  have h1 : step (State.init 0) (.addUser 0 1)
      = some { users := [0, 1], responsabili := [0], conti := [], casse := [], collections := [] } :=
    rfl
  have h2 : step { users := [0, 1], responsabili := [0], conti := [], casse := [], collections := [] }
        (.deposit 0 1 10)
      = some { users := [0, 1], responsabili := [0], conti := [(1, 10)], casse := [(0, 10)], collections := [] } :=
    rfl
  have h3 : step { users := [0, 1], responsabili := [0], conti := [(1, 10)], casse := [(0, 10)], collections := [] }
        (.withdraw 0 1 50)
      = some badState :=
    rfl
  exact Reach.trans (Reach.trans (Reach.trans (Reach.boot 0) h1) h2) h3

theorem insolvency_example : insolvent badState :=
  ⟨1, List.Mem.tail _ (List.Mem.head _), by decide⟩

/-! ### L8 one pledge per user per collection -/

/-- Core 3×3 case table: uniqueness is preserved when one fresh pledge of
an absent user is consed onto the pending list. Stated over plain lists
to keep every membership syntactically aligned. -/
private theorem unique_mem_cons_inv {acc pend : List Pledge} {u : UserId} {v : Int}
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
theorem uniquePledges_pend_cons {col : Collection} {u : UserId} {v : Int}
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
theorem pledge_rejected_when_member {s : State} {a u : UserId} {c : CollId}
    {v : Int} {col : Collection} {rest : List Collection}
    (hpull : pullCollection c s.collections = some (col, rest))
    (hdup : ∃ q, q ∈ col.accepted ++ col.pending ∧ q.user = u) :
    step s (.pledge a u c v) = none := by
  by_cases hnone : step s (.pledge a u c v) = none
  · exact hnone
  · cases hstep : step s (.pledge a u c v) with
    | none => exact absurd hstep hnone
    | some s' =>
      obtain ⟨col₀, rest₀, hpull₀, hg, _⟩ := step_pledge_inv hstep
      obtain ⟨hdet, -⟩ : col₀ = col ∧ rest₀ = rest := by
        have hpair := pullCollection_det hpull₀ hpull
        simpa using hpair
      obtain ⟨_, _, hn1, hn2⟩ := pledge_guard_inv hg
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
theorem pledge_preserves_allUnique {s s' : State} {a u : UserId} {c : CollId}
    {v : Int} (hun : allUniquePledges s)
    (h : step s (.pledge a u c v) = some s') : allUniquePledges s' := by
  obtain ⟨col, rest, hpull, hg, hs'⟩ := step_pledge_inv h
  obtain ⟨_, _, hna1, hna2⟩ := pledge_guard_inv hg
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
