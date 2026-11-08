import Reactivegas.CompositionRoot

/-!
# No negative authorization is stranded (issue #81, L-7)

The V-5 closures (a proposer's renounce, a proposer's departure) are negative
closures like any other, so they mint a negative authorization when their
question was bound to a collection's permission, and a responsabile's
`denyPermission` spends it, refunding every pledge to its owner. This module
states and proves that for the production root `Reactivegas.apply`, on top of
the provenance machinery of `Reactivegas.CompositionRoot`; it changes no
production definition.

Status (R-35): enforced: PROVED-IN-MODEL.
-/

namespace Reactivegas.Composition

open Reactivegas

/-! ### No stranding (issue #81, L-7)

Every negative closure of a question bound to collection `c` mints a negative
authorization for `c` (`syncAuth`), and nothing but spending it — a
`denyPermission c` — or the departure of `c` takes it away. Spending it refunds
every pledge of `c` to its owner. The statements below are about the production
root; none changes a production definition.

The binding is not kept after the closure, so the statement is about a step and
its continuation: `closesNegativeBound` names the closing step, `runHolding` the
steps after it that keep `c` and do not spend. -/

/-- The bindings the vote step sees when the root handles `event` from payload
`s`: an `openBound` adds its own binding before the vote step runs. -/
def bindingsAt (s : State) : KelGroups.IntegratedEvent Proposal AppEvent →
    List (KelGroups.Vote.QuestionId × EconomicTarget)
  | .app (.openBound q _ target) => (q, target) :: s.bindings
  | _ => s.bindings

/-- A root step from payload `pre` to payload `post` closed question `qid`
negatively while it was bound to the permission of collection `c`, and `c` is
still present afterwards. The question id is new to the closure log. -/
def closesNegativeBound (pre post : State)
    (event : KelGroups.IntegratedEvent Proposal AppEvent) (c : CollId)
    (qid : KelGroups.Vote.QuestionId) : Bool :=
  decide (∃ r ∈ post.votes.closed, r.questionId = qid ∧ r.verdict = .negative)
    && decide (∀ p ∈ pre.votes.closed, p.questionId ≠ qid)
    && decide (List.lookup qid (bindingsAt pre event) = some (.permission c))
    && targetPresent post (.permission c)

/-- Replay `steps` through the production root while each succeeds, none is a
`denyPermission c` (the spend), and collection `c` is still present afterwards;
`none` at the first step that fails any of the three. -/
def runHolding (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth) (c : CollId) :
    KelGroups.GroupState State →
      List (KelGroups.Key × KelGroups.IntegratedEvent Proposal AppEvent) →
        Option (KelGroups.GroupState State)
  | gs, [] => some gs
  | gs, (signer, e) :: rest =>
      match apply θ auth gs signer e with
      | .ok r =>
          if e = .app (.denyPermission c) then none
          else if targetPresent r.state.appFold (.permission c) then
            runHolding θ auth c r.state rest
          else none
      | .error _ => none

/-- What one owner is owed back from a collection: the sum of their accepted and
pending pledges. -/
def pledgedBy (col : Collection) (u : KelGroups.Key) : Int :=
  sumPledges ((col.accepted ++ col.pending).filter (fun p => p.user == u))

/-! #### Minting -/

theorem targetPresent_pruneAuth (s : State) (t : EconomicTarget) :
    targetPresent (pruneAuth s) t = targetPresent s t := by
  cases t <;> rfl

/-- A closure record new to the log, whose question was bound, puts its
authorization among the unspent ones. -/
theorem syncAuth_mints {s : State} {votes : KelGroups.Vote.VoteState}
    {r : KelGroups.Vote.ClosureRecord} {t : EconomicTarget}
    (hr : r ∈ votes.closed)
    (hfresh : ∀ p ∈ s.votes.closed, p.questionId ≠ r.questionId)
    (hb : List.lookup r.questionId s.bindings = some t) :
    ({ questionId := r.questionId, target := t, verdict := r.verdict } : LiveAuth)
      ∈ (syncAuth s votes).live := by
  simp only [syncAuth, List.mem_append]
  right
  refine List.mem_filterMap.mpr ⟨r, List.mem_filter.mpr ⟨hr, ?_⟩, ?_⟩
  · have hno : (s.votes.closed.any fun p => p.questionId == r.questionId) = false := by
      rw [Bool.eq_false_iff]
      intro h
      obtain ⟨p, hp, hpq⟩ := List.any_eq_true.mp h
      exact hfresh p hp (beq_iff_eq.mp hpq)
    simp [hno]
  · simp [hb]

theorem pruneSync_mints {s : State} {votes : KelGroups.Vote.VoteState}
    {r : KelGroups.Vote.ClosureRecord} {t : EconomicTarget}
    (hr : r ∈ votes.closed)
    (hfresh : ∀ p ∈ s.votes.closed, p.questionId ≠ r.questionId)
    (hb : List.lookup r.questionId s.bindings = some t)
    (hp : targetPresent (pruneAuth (syncAuth s votes)) t = true) :
    ({ questionId := r.questionId, target := t, verdict := r.verdict } : LiveAuth)
      ∈ (pruneAuth (syncAuth s votes)).live := by
  rw [targetPresent_pruneAuth] at hp
  exact List.mem_filter.mpr ⟨syncAuth_mints hr hfresh hb, hp⟩

theorem pruneSync_keeps {s : State} {votes : KelGroups.Vote.VoteState} {a : LiveAuth}
    (ha : a ∈ s.live)
    (hp : targetPresent (pruneAuth (syncAuth s votes)) a.target = true) :
    a ∈ (pruneAuth (syncAuth s votes)).live := by
  rw [targetPresent_pruneAuth] at hp
  exact List.mem_filter.mpr ⟨List.mem_append_left _ ha, hp⟩

theorem voteApply_mints {θ : KelGroups.Vote.Threshold} {view : KelGroups.GroupView}
    {s s' : State} {signer : KelGroups.Key} {ev : KelGroups.Vote.VoteEvent}
    {r : KelGroups.Vote.ClosureRecord} {t : EconomicTarget}
    (h : voteApply θ view s signer ev = .ok s') (hr : r ∈ s'.votes.closed)
    (hfresh : ∀ p ∈ s.votes.closed, p.questionId ≠ r.questionId)
    (hb : List.lookup r.questionId s.bindings = some t) :
    ({ questionId := r.questionId, target := t, verdict := r.verdict } : LiveAuth)
      ∈ s'.live := by
  unfold voteApply at h
  split at h
  · exact Except.noConfusion h
  · next votes hvotes =>
    simp only [Except.ok.injEq] at h
    subst h
    exact syncAuth_mints hr hfresh hb

theorem voteApply_keeps_live {θ : KelGroups.Vote.Threshold} {view : KelGroups.GroupView}
    {s s' : State} {signer : KelGroups.Key} {ev : KelGroups.Vote.VoteEvent} {a : LiveAuth}
    (h : voteApply θ view s signer ev = .ok s') (ha : a ∈ s.live) : a ∈ s'.live := by
  unfold voteApply at h
  split at h
  · exact Except.noConfusion h
  · simp only [Except.ok.injEq] at h
    subst h
    exact List.mem_append_left _ ha

/-- An app event that is not a vote step leaves the vote payload as it was. -/
theorem appFoldCore_nonvote_votes {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {signer : KelGroups.Key} {pre post : KelGroups.GroupView} {s s' : State}
    {e : AppEvent} (h : appFoldCore θ auth signer pre post s e = .ok s')
    (hq : ∀ qid kind, e ≠ .openQuestion qid kind) (hc : ∀ qid b, e ≠ .cast qid b)
    (hr : ∀ qid, e ≠ .renounce qid) (hb : ∀ qid kind t, e ≠ .openBound qid kind t) :
    s'.votes = s.votes := by
  cases e with
  | openQuestion qid kind => exact absurd rfl (hq qid kind)
  | cast qid b => exact absurd rfl (hc qid b)
  | renounce qid => exact absurd rfl (hr qid)
  | openBound qid kind t => exact absurd rfl (hb qid kind t)
  | grantPermission c =>
    simp only [appFoldCore] at h
    obtain ⟨a, rest, hpull, heff⟩ := spendThen_ok h
    exact (step_auth_frame heff).2.1
  | backdonate w =>
    simp only [appFoldCore] at h
    obtain ⟨a, rest, hpull, heff⟩ := spendThen_ok h
    exact (step_auth_frame heff).2.1
  | denyPermission c =>
    simp only [appFoldCore] at h
    split at h
    · obtain ⟨a, rest, col, cs, hpull, _, hs⟩ := denyByClosure_spec (ofOption_ok h)
      subst hs
      rfl
    · exact Except.noConfusion h
  | openPurchase _ | deposit _ _ | withdraw _ _ | transferCassa _ _ | donate _
  | pledge _ _ _ | acceptPledge _ _ | refusePledge _ _ | correctPledge _ _ _
  | closePurchase _ | failPurchase _ =>
    simp only [appFoldCore] at h
    exact (step_auth_frame (ofOption_ok h)).2.1

/-- The app fold mints the authorization of a bound question it closes. -/
theorem appFoldCore_mints {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {signer : KelGroups.Key} {pre post : KelGroups.GroupView} {s s' : State}
    {e : AppEvent} {r : KelGroups.Vote.ClosureRecord} {t : EconomicTarget}
    (h : appFoldCore θ auth signer pre post s e = .ok s')
    (hr : r ∈ s'.votes.closed)
    (hfresh : ∀ p ∈ s.votes.closed, p.questionId ≠ r.questionId)
    (hb : List.lookup r.questionId (bindingsAt s (.app e)) = some t) :
    ({ questionId := r.questionId, target := t, verdict := r.verdict } : LiveAuth)
      ∈ s'.live := by
  by_cases hv : (∃ qid kind, e = .openQuestion qid kind) ∨ (∃ qid b, e = .cast qid b)
      ∨ (∃ qid, e = .renounce qid) ∨ (∃ qid kind t, e = .openBound qid kind t)
  · rcases hv with ⟨qid, kind, rfl⟩ | ⟨qid, b, rfl⟩ | ⟨qid, rfl⟩ | ⟨qid, kind, t', rfl⟩
    · simp only [appFoldCore] at h
      exact voteApply_mints h hr hfresh hb
    · simp only [appFoldCore] at h
      exact voteApply_mints h hr hfresh hb
    · simp only [appFoldCore] at h
      exact voteApply_mints h hr hfresh hb
    · simp only [appFoldCore] at h
      split at h
      · exact voteApply_mints h hr hfresh hb
      · exact Except.noConfusion h
  · simp only [not_or, not_exists] at hv
    obtain ⟨hq, hc, hrn, hbd⟩ := hv
    have hsame := appFoldCore_nonvote_votes h
      (fun qid kind hh => hq qid kind hh) (fun qid b hh => hc qid b hh)
      (fun qid hh => hrn qid hh) (fun qid kind t' hh => hbd qid kind t' hh)
    rw [hsame] at hr
    exact absurd rfl (hfresh r hr)

theorem economicCleanup_bindings {change : KelGroups.BaseChange}
    {pre post : KelGroups.GroupView} {s s' : State}
    (h : economicCleanup change pre post s = some s') : s'.bindings = s.bindings := by
  cases change <;> simp only [economicCleanup] at h
  case memberAdmitted =>
    obtain rfl := Option.some.inj h
    rfl
  case memberRemoved =>
    obtain ⟨_, _, h⟩ := option_bind_inv h
    obtain rfl := Option.some.inj h
    split <;> rfl
  case rolesChanged =>
    split at h
    · obtain ⟨_, _, h⟩ := option_bind_inv h
      obtain rfl := Option.some.inj h
      rfl
    · obtain rfl := Option.some.inj h
      rfl

theorem baseHook_mints {θ : KelGroups.Vote.Threshold} {change : KelGroups.BaseChange}
    {pre post : KelGroups.GroupView} {s s' : State}
    {r : KelGroups.Vote.ClosureRecord} {t : EconomicTarget}
    (h : baseHook θ change pre post s = .ok s') (hr : r ∈ s'.votes.closed)
    (hfresh : ∀ p ∈ s.votes.closed, p.questionId ≠ r.questionId)
    (hb : List.lookup r.questionId s.bindings = some t)
    (hp : targetPresent s' t = true) :
    ({ questionId := r.questionId, target := t, verdict := r.verdict } : LiveAuth)
      ∈ s'.live := by
  unfold baseHook at h
  split at h
  · exact Except.noConfusion h
  · next cleaned hclean =>
    simp only [Except.ok.injEq] at h
    subst h
    obtain ⟨_, hv⟩ := economicCleanup_frame hclean
    have hbd := economicCleanup_bindings hclean
    exact pruneSync_mints hr (by rw [hv]; exact hfresh) (by rw [hbd]; exact hb) hp

theorem baseHook_keeps_live {θ : KelGroups.Vote.Threshold} {change : KelGroups.BaseChange}
    {pre post : KelGroups.GroupView} {s s' : State} {a : LiveAuth}
    (h : baseHook θ change pre post s = .ok s') (ha : a ∈ s.live)
    (hp : targetPresent s' a.target = true) : a ∈ s'.live := by
  unfold baseHook at h
  split at h
  · exact Except.noConfusion h
  · next cleaned hclean =>
    simp only [Except.ok.injEq] at h
    subst h
    obtain ⟨hl, _⟩ := economicCleanup_frame hclean
    exact pruneSync_keeps (by rw [hl]; exact ha) hp

/-- The integrated boundary on an event that is not an app event changes the
payload only by a committed base change's sealed hook. -/
theorem applyIntegrated_nonapp_cases {AppState AppEvent BaseProposal AppError : Type}
    (I : KelGroups.Integration AppState AppEvent BaseProposal AppError)
    (gs : KelGroups.GroupState AppState) (signer : KelGroups.Key)
    (event : KelGroups.IntegratedEvent BaseProposal AppEvent)
    (r : KelGroups.IntegratedResult AppState)
    (hne : ∀ e, event ≠ .app e)
    (h : KelGroups.applyIntegratedEvent I gs signer event = .ok r) :
    r.state.appFold = gs.appFold
      ∨ (∃ change pre post, I.baseHook change pre post gs.appFold = .ok r.state.appFold) := by
  cases hc : r.change with
  | some change =>
    exact Or.inr ⟨change, _, _,
      KelGroups.base_change_runs_hook I gs signer event r change h hc⟩
  | none =>
    cases event with
    | app e => exact absurd rfl (hne e)
    | direct command =>
      cases command with
      | admitMember key email roles =>
        simp only [KelGroups.applyIntegratedEvent] at h
        split at h
        · exact Except.noConfusion h
        · rw [(KelGroups.commitBaseChange_ok h).1] at hc
          exact Option.noConfusion hc
    | propose proposal =>
      simp only [KelGroups.applyIntegratedEvent] at h
      split at h
      · exact Except.noConfusion h
      · exact Or.inl (by rw [tryEnactBase_unchanged h hc])
    | approve proposalId =>
      simp only [KelGroups.applyIntegratedEvent] at h
      split at h
      · exact Except.noConfusion h
      · split at h
        · exact Except.noConfusion h
        · exact Or.inl (by rw [tryEnactBase_unchanged h hc])

theorem apply_nonapp_cases {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {gs : KelGroups.GroupState State} {signer : KelGroups.Key}
    {event : KelGroups.IntegratedEvent Proposal AppEvent}
    {res : KelGroups.IntegratedResult State}
    (hne : ∀ e, event ≠ .app e) (h : apply θ auth gs signer event = .ok res) :
    res.state.appFold = gs.appFold
      ∨ (∃ change pre post, baseHook θ change pre post gs.appFold = .ok res.state.appFold) := by
  unfold apply at h
  split at h
  · split at h
    · next inner hinner =>
      split at h
      · simp only [Except.ok.injEq] at h
        subst h
        exact applyIntegrated_nonapp_cases _ gs signer event inner hne hinner
      · exact Except.noConfusion h
    · exact Except.noConfusion h
  · exact Except.noConfusion h

/-- **Minting.** A root step that closes a question negatively while it was bound
to the permission of collection `c`, with `c` still present, leaves a negative
authorization for `c` carrying that question's id among the unspent ones. -/
theorem apply_mints_negative {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {gs : KelGroups.GroupState State} {signer : KelGroups.Key}
    {event : KelGroups.IntegratedEvent Proposal AppEvent}
    {res : KelGroups.IntegratedResult State} {c : CollId}
    {qid : KelGroups.Vote.QuestionId}
    (happly : apply θ auth gs signer event = .ok res)
    (h : closesNegativeBound gs.appFold res.state.appFold event c qid = true) :
    ∃ a ∈ res.state.appFold.live,
      a.questionId = qid ∧ a.target = .permission c ∧ a.verdict = .negative := by
  simp only [closesNegativeBound, Bool.and_eq_true, decide_eq_true_eq] at h
  obtain ⟨⟨⟨⟨r, hr, hrq, hrv⟩, hfresh⟩, hbind⟩, hpres⟩ := h
  subst hrq
  have hfresh' : ∀ p ∈ gs.appFold.votes.closed, p.questionId ≠ r.questionId := hfresh
  by_cases happ : ∃ e, event = .app e
  · obtain ⟨e, rfl⟩ := happ
    obtain ⟨s', hcore, hres⟩ := apply_app_ok happly
    rw [hres] at hr hpres
    have hmint := appFoldCore_mints hcore hr hfresh' hbind
    refine ⟨{ questionId := r.questionId, target := .permission c, verdict := r.verdict },
      ?_, rfl, rfl, hrv⟩
    rw [hres]
    rw [targetPresent_pruneAuth] at hpres
    exact List.mem_filter.mpr ⟨hmint, hpres⟩
  · have hne : ∀ e, event ≠ .app e := fun e he => happ ⟨e, he⟩
    rcases apply_nonapp_cases hne happly with heq | ⟨change, pre, post, hhook⟩
    · rw [heq] at hr
      exact absurd rfl (hfresh' r hr)
    · have hb' : List.lookup r.questionId gs.appFold.bindings = some (.permission c) := by
        cases event with
        | app e => exact absurd rfl (hne e)
        | direct _ => exact hbind
        | propose _ => exact hbind
        | approve _ => exact hbind
      have hmint := baseHook_mints hhook hr hfresh' hb' hpres
      exact ⟨_, hmint, rfl, rfl, hrv⟩

/-! #### Persistence until spent -/

theorem mem_rest_of_pullLive {target : EconomicTarget} {verdict : KelGroups.Vote.Verdict}
    {l : List LiveAuth} {hit : LiveAuth} {rest : List LiveAuth} {a : LiveAuth}
    (hp : pullLive target verdict l = some (hit, rest)) (ha : a ∈ l) (hne : a ≠ hit) :
    a ∈ rest := by
  obtain ⟨_, _, hperm⟩ := pullLive_spec hp
  rcases List.mem_cons.mp (hperm.mem_iff.mp ha) with h | h
  · exact absurd h hne
  · exact h

theorem appFoldCore_keeps_negative {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {signer : KelGroups.Key} {pre post : KelGroups.GroupView} {s s' : State}
    {e : AppEvent} {c : CollId} {a : LiveAuth}
    (h : appFoldCore θ auth signer pre post s e = .ok s')
    (ha : a ∈ s.live) (hv : a.verdict = .negative) (ht : a.target = .permission c)
    (hd : e ≠ .denyPermission c) : a ∈ s'.live := by
  cases e with
  | openQuestion qid kind =>
    simp only [appFoldCore] at h
    exact voteApply_keeps_live h ha
  | cast qid b =>
    simp only [appFoldCore] at h
    exact voteApply_keeps_live h ha
  | renounce qid =>
    simp only [appFoldCore] at h
    exact voteApply_keeps_live h ha
  | openBound qid kind t =>
    simp only [appFoldCore] at h
    split at h
    · exact voteApply_keeps_live h ha
    · exact Except.noConfusion h
  | grantPermission c' =>
    simp only [appFoldCore] at h
    obtain ⟨hit, rest, hpull, heff⟩ := spendThen_ok h
    have hne : a ≠ hit := by
      intro heq
      have := (pullLive_spec hpull).2.1
      rw [← heq, hv] at this
      exact absurd this (by decide)
    have hrest := mem_rest_of_pullLive hpull ha hne
    rw [(step_auth_frame heff).1]
    exact hrest
  | backdonate w =>
    simp only [appFoldCore] at h
    obtain ⟨hit, rest, hpull, heff⟩ := spendThen_ok h
    have hne : a ≠ hit := by
      intro heq
      have := (pullLive_spec hpull).2.1
      rw [← heq, hv] at this
      exact absurd this (by decide)
    have hrest := mem_rest_of_pullLive hpull ha hne
    rw [(step_auth_frame heff).1]
    exact hrest
  | denyPermission c' =>
    simp only [appFoldCore] at h
    split at h
    · obtain ⟨hit, rest, col, cs, hpull, _, hs⟩ := denyByClosure_spec (ofOption_ok h)
      subst hs
      have hne : a ≠ hit := by
        intro heq
        have h1 := (pullLive_spec hpull).1
        rw [← heq, ht] at h1
        exact hd (by cases h1; rfl)
      exact mem_rest_of_pullLive hpull ha hne
    · exact Except.noConfusion h
  | openPurchase _ | deposit _ _ | withdraw _ _ | transferCassa _ _ | donate _
  | pledge _ _ _ | acceptPledge _ _ | refusePledge _ _ | correctPledge _ _ _
  | closePurchase _ | failPurchase _ =>
    simp only [appFoldCore] at h
    rw [(step_auth_frame (ofOption_ok h)).1]
    exact ha

/-- **Persistence.** A negative authorization for collection `c` survives every
root step that keeps `c` and is not a `denyPermission c`. -/
theorem apply_keeps_negative {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {gs : KelGroups.GroupState State} {signer : KelGroups.Key}
    {event : KelGroups.IntegratedEvent Proposal AppEvent}
    {res : KelGroups.IntegratedResult State} {c : CollId} {a : LiveAuth}
    (happly : apply θ auth gs signer event = .ok res)
    (ha : a ∈ gs.appFold.live) (hv : a.verdict = .negative)
    (ht : a.target = .permission c) (hd : event ≠ .app (.denyPermission c))
    (hpres : targetPresent res.state.appFold (.permission c) = true) :
    a ∈ res.state.appFold.live := by
  by_cases happ : ∃ e, event = .app e
  · obtain ⟨e, rfl⟩ := happ
    obtain ⟨s', hcore, hres⟩ := apply_app_ok happly
    rw [hres] at hpres ⊢
    have hkeep := appFoldCore_keeps_negative hcore ha hv ht
      (fun hh => hd (by rw [hh]))
    refine List.mem_filter.mpr ⟨hkeep, ?_⟩
    rw [targetPresent_pruneAuth] at hpres
    show targetPresent s' a.target = true
    rw [ht]
    exact hpres
  · have hne : ∀ e, event ≠ .app e := fun e he => happ ⟨e, he⟩
    rcases apply_nonapp_cases hne happly with heq | ⟨change, pre, post, hhook⟩
    · rw [heq]
      exact ha
    · exact baseHook_keeps_live hhook ha (by rw [ht]; exact hpres)

/-- **The continuation holds.** After a closing step, replaying steps that keep
`c` and never spend leaves the authorization unspent, `c` present, and the
aggregate a production history. -/
theorem runHolding_keeps_negative {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {c : CollId} {a : LiveAuth} (hv : a.verdict = .negative)
    (ht : a.target = .permission c) :
    ∀ (steps : List (KelGroups.Key × KelGroups.IntegratedEvent Proposal AppEvent))
      (gs gsN : KelGroups.GroupState State),
      ProductionHistory θ auth gs → a ∈ gs.appFold.live →
      targetPresent gs.appFold (.permission c) = true →
      runHolding θ auth c gs steps = some gsN →
        ProductionHistory θ auth gsN ∧ a ∈ gsN.appFold.live
          ∧ targetPresent gsN.appFold (.permission c) = true
  | [], gs, gsN, hist, ha, hp, hrun => by
    simp only [runHolding, Option.some.injEq] at hrun
    subst hrun
    exact ⟨hist, ha, hp⟩
  | (signer, e) :: rest, gs, gsN, hist, ha, hp, hrun => by
    simp only [runHolding] at hrun
    split at hrun
    · next r happly =>
      split at hrun
      · exact Option.noConfusion hrun
      · next hne =>
        split at hrun
        · next hpres =>
          exact runHolding_keeps_negative hv ht rest r.state gsN
            (.apply hist signer e happly)
            (apply_keeps_negative happly ha hv ht hne hpres) hpres hrun
        · exact Option.noConfusion hrun
    · exact Option.noConfusion hrun

/-- **No stranding (L-7c).** In a production history, a root step that closes a
question negatively while it was bound to the permission of collection `c`, with
`c` present, leaves a negative authorization for `c`; every continuation that
keeps `c` and does not spend it is again a production history in which `c` is
present and the authorization is unspent. The unspent authorization is what
`denyPermission c` spends; see `spend_refunds`. -/
theorem noStranding_of_history {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {gs : KelGroups.GroupState State} (hist : ProductionHistory θ auth gs)
    {signer : KelGroups.Key} {event : KelGroups.IntegratedEvent Proposal AppEvent}
    {res : KelGroups.IntegratedResult State} {c : CollId}
    {qid : KelGroups.Vote.QuestionId}
    (happly : apply θ auth gs signer event = .ok res)
    (hcloses : closesNegativeBound gs.appFold res.state.appFold event c qid = true)
    {steps : List (KelGroups.Key × KelGroups.IntegratedEvent Proposal AppEvent)}
    {gsN : KelGroups.GroupState State}
    (hrun : runHolding θ auth c res.state steps = some gsN) :
    ProductionHistory θ auth gsN ∧ targetPresent gsN.appFold (.permission c) = true
      ∧ ∃ a ∈ gsN.appFold.live,
          a.questionId = qid ∧ a.target = .permission c ∧ a.verdict = .negative := by
  obtain ⟨a, ha, hq, ht, hv⟩ := apply_mints_negative happly hcloses
  have hpres : targetPresent res.state.appFold (.permission c) = true := by
    simp only [closesNegativeBound, Bool.and_eq_true] at hcloses
    exact hcloses.2
  obtain ⟨hh, ha', hp'⟩ := runHolding_keeps_negative hv ht steps res.state gsN
    (.apply hist signer event happly) ha hpres hrun
  exact ⟨hh, hp', a, ha', hq, ht, hv⟩

/-- The decidable form of the conclusion of `noStranding_of_history`: the payload
holds an unspent negative authorization for the permission of collection `c`
that carries question `qid`'s id. -/
def holdsNegativeAuth (s : State) (c : CollId) (qid : KelGroups.Vote.QuestionId) : Bool :=
  s.live.any (fun a => decide (a.questionId = qid ∧ a.target = .permission c
    ∧ a.verdict = .negative))

theorem holdsNegativeAuth_iff {s : State} {c : CollId} {qid : KelGroups.Vote.QuestionId} :
    holdsNegativeAuth s c qid = true ↔
      ∃ a ∈ s.live, a.questionId = qid ∧ a.target = .permission c ∧ a.verdict = .negative := by
  simp [holdsNegativeAuth, List.any_eq_true]

/-! #### Spending refunds every pledge to its owner -/

theorem bal_refundAll (l : List Pledge) :
    ∀ (m : List (KelGroups.Key × Int)) (u : KelGroups.Key),
      bal (refundAll m l) u = bal m u + sumPledges (l.filter (fun p => p.user == u)) := by
  induction l with
  | nil => intro m u; simp [refundAll, sumPledges]
  | cons p t ih =>
    intro m u
    have h1 : refundAll m (p :: t) = refundAll (bump m p.user p.amount) t := rfl
    rw [h1, ih]
    by_cases hu : p.user = u
    · subst hu
      rw [bal_bump]
      simp [sumPledges] <;> omega
    · rw [bal_bump_ne (Ne.symm hu)]
      simp [hu]

/-- **Spending refunds.** The negative continuation hands each pledger of `c`
exactly the sum of their pledges, and conserves money: what leaves escrow
arrives in the accounts. -/
theorem denyByClosure_refunds {s s' : State} {c : CollId}
    (h : denyByClosure s c = some s') :
    ∃ col, pullCollection c s.collections = some (col, s'.collections)
      ∧ (∀ u, bal s'.conti u = bal s.conti u + pledgedBy col u)
      ∧ sumBal s'.conti + escrowSum s'.collections
        = sumBal s.conti + escrowSum s.collections := by
  obtain ⟨a, rest, col, cs, _, hcol, hs⟩ := denyByClosure_spec h
  subst hs
  refine ⟨col, hcol, fun u => bal_refundAll (col.accepted ++ col.pending) s.conti u, ?_⟩
  have hsum := pullCollection_sum hcol
  show sumBal (refundAll s.conti (col.accepted ++ col.pending)) + escrowSum cs
    = sumBal s.conti + escrowSum s.collections
  rw [refundAll_sum, sumPledges_append, hsum]
  simp only [escrowOf]
  omega

/-- A root success on `denyPermission c` refunds exactly, whoever the signer. -/
theorem apply_deny_refunds {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {gs : KelGroups.GroupState State} {signer : KelGroups.Key} {c : CollId}
    {r : KelGroups.IntegratedResult State}
    (h : apply θ auth gs signer (.app (.denyPermission c)) = .ok r) :
    ∃ col cs, pullCollection c gs.appFold.collections = some (col, cs)
      ∧ r.state.appFold.collections = cs
      ∧ ∀ u, bal r.state.appFold.conti u = bal gs.appFold.conti u + pledgedBy col u := by
  obtain ⟨s', hcore, hr⟩ := apply_app_ok h
  simp only [appFoldCore] at hcore
  split at hcore
  · obtain ⟨col, hcol, hbal, _⟩ := denyByClosure_refunds (ofOption_ok hcore)
    refine ⟨col, s'.collections, hcol, ?_, ?_⟩
    · rw [hr]; rfl
    · intro u
      rw [hr]
      exact hbal u
  · exact Except.noConfusion hcore

theorem pullLive_of_mem {target : EconomicTarget} {verdict : KelGroups.Vote.Verdict} :
    ∀ {l : List LiveAuth} {a : LiveAuth}, a ∈ l → a.target = target → a.verdict = verdict →
      ∃ hit rest, pullLive target verdict l = some (hit, rest)
  | [], _, h, _, _ => by cases h
  | x :: xs, a, h, ht, hv => by
    unfold pullLive
    by_cases hx : x.target = target ∧ x.verdict = verdict
    · rw [if_pos hx]
      exact ⟨x, xs, rfl⟩
    · rw [if_neg hx]
      rcases List.mem_cons.mp h with rfl | hxs
      · exact absurd ⟨ht, hv⟩ hx
      · obtain ⟨hit, rest, hh⟩ := pullLive_of_mem hxs ht hv
        rw [hh]
        exact ⟨hit, x :: rest, rfl⟩

theorem pullCollection_of_present {c : CollId} :
    ∀ {cols : List Collection}, cols.any (fun col => col.id == c) = true →
      ∃ col rest, pullCollection c cols = some (col, rest)
  | [], h => by simp at h
  | x :: xs, h => by
    unfold pullCollection
    by_cases hx : x.id = c
    · rw [if_pos hx]
      exact ⟨x, xs, rfl⟩
    · rw [if_neg hx]
      have hxs : xs.any (fun col => col.id == c) = true := by
        simp only [List.any_cons, Bool.or_eq_true] at h
        rcases h with h | h
        · exact absurd (beq_iff_eq.mp h) hx
        · exact h
      obtain ⟨col, rest, hh⟩ := pullCollection_of_present hxs
      rw [hh]
      exact ⟨col, x :: rest, rfl⟩

/-- **Spending an unspent negative authorization for a present collection
succeeds and refunds every pledger exactly.** -/
theorem spend_refunds {s : State} {c : CollId} {a : LiveAuth}
    (ha : a ∈ s.live) (hv : a.verdict = .negative) (ht : a.target = .permission c)
    (hp : targetPresent s (.permission c) = true) :
    ∃ s' col, denyByClosure s c = some s'
      ∧ pullCollection c s.collections = some (col, s'.collections)
      ∧ (∀ u, bal s'.conti u = bal s.conti u + pledgedBy col u)
      ∧ sumBal s'.conti + escrowSum s'.collections
        = sumBal s.conti + escrowSum s.collections := by
  obtain ⟨hit, rest, hpull⟩ := pullLive_of_mem ha ht hv
  obtain ⟨col, cs, hcol⟩ := pullCollection_of_present hp
  have hden : denyByClosure s c = some { s with
      live := rest, conti := refundAll s.conti (col.accepted ++ col.pending),
      collections := cs } := by
    simp [denyByClosure, hpull, hcol]
  obtain ⟨col', hcol', hbal, hsum⟩ := denyByClosure_refunds hden
  exact ⟨_, col', hden, hcol', hbal, hsum⟩

#print axioms apply_mints_negative
#print axioms apply_keeps_negative
#print axioms noStranding_of_history
#print axioms holdsNegativeAuth_iff
#print axioms spend_refunds
#print axioms apply_deny_refunds

end Reactivegas.Composition
