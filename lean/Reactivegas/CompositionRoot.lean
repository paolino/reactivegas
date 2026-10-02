import Reactivegas.Step
import Reactivegas.Invariants

/-!
# What the production root requires of the app-decided events (issue #76)

`Reactivegas.Composition.route` says which producer decides an event; this
module states what the production root `Reactivegas.apply` actually requires of
the vote producer for the three app-decided events. Each success of
`grantPermission`, `denyPermission` and `backdonate` spent an unspent
authorization for its exact target under its exact verdict, so without one the
root refuses; and every unspent authorization of a production history is
backed by a closure record its own vote fold appended. Spending is proved over
arbitrary aggregates; provenance over `ProductionHistory`, because an
aggregate built any other way — carrying a planted authorization, say — is not
a production history.

Status (R-35): enforced: PROVED-IN-MODEL, as for the classifier.
-/

namespace Reactivegas.Composition

open Reactivegas

/-- One spend: the authorization removed is the one the event names. -/
theorem pullLive_spec {target : EconomicTarget} {verdict : KelGroups.Vote.Verdict} :
    ∀ {l : List LiveAuth} {a : LiveAuth} {rest : List LiveAuth},
      pullLive target verdict l = some (a, rest) →
        a.target = target ∧ a.verdict = verdict ∧ l.Perm (a :: rest)
  | [], _, _, h => by simp [pullLive] at h
  | x :: xs, a, rest, h => by
    unfold pullLive at h
    split at h
    · next hx =>
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      exact ⟨hx.1, hx.2, List.Perm.refl _⟩
    · split at h
      · next hit rest' hrec =>
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        obtain ⟨h1, h2, hp⟩ := pullLive_spec hrec
        exact ⟨h1, h2, (hp.cons x).trans (List.Perm.swap hit x rest')⟩
      · exact Option.noConfusion h

/-- The economic core never writes the authorization or vote payload. -/
theorem step_auth_frame {view : KelGroups.GroupView} {s s' : State}
    {signer : KelGroups.Key} {e : AppEvent} {auth : BackdonateAuth}
    (h : step view s signer e auth = some s') :
    s'.live = s.live ∧ s'.votes = s.votes ∧ s'.bindings = s.bindings := by
  cases e <;> simp only [step] at h
  case openQuestion | cast | renounce | openBound => exact Option.noConfusion h
  case openPurchase | deposit | withdraw | transferCassa | donate | backdonate =>
    split at h
    · obtain rfl := Option.some.inj h
      exact ⟨rfl, rfl, rfl⟩
    · exact Option.noConfusion h
  case grantPermission | denyPermission | pledge | closePurchase | failPurchase =>
    obtain ⟨⟨_, _⟩, _, h⟩ := option_bind_inv h
    obtain ⟨_, _, h⟩ := option_bind_inv h
    obtain rfl := Option.some.inj h
    exact ⟨rfl, rfl, rfl⟩
  case acceptPledge | refusePledge | correctPledge =>
    obtain ⟨⟨_, _⟩, _, h⟩ := option_bind_inv h
    obtain ⟨⟨_, _⟩, _, h⟩ := option_bind_inv h
    obtain ⟨_, _, h⟩ := option_bind_inv h
    obtain rfl := Option.some.inj h
    exact ⟨rfl, rfl, rfl⟩

/-- A root success on an app event is the pruned result of the app fold. -/
theorem apply_app_ok {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {gs : KelGroups.GroupState State} {signer : KelGroups.Key} {e : AppEvent}
    {r : KelGroups.IntegratedResult State}
    (h : apply θ auth gs signer (.app e) = .ok r) :
    ∃ s', appFoldCore θ auth signer (KelGroups.groupView gs)
        (KelGroups.groupView gs) gs.appFold e = .ok s'
      ∧ r.state.appFold = pruneAuth s' := by
  unfold apply at h
  split at h
  · split at h
    · next inner hinner =>
      split at h
      · simp only [Except.ok.injEq] at h
        subst h
        simp only [KelGroups.applyIntegratedEvent] at hinner
        split at hinner
        · simp only [integration, appFold] at hinner
          split at hinner
          · next appState hfold =>
            simp only [Except.ok.injEq] at hinner
            subst hinner
            split at hfold
            · next s' hcore =>
              simp only [Except.ok.injEq] at hfold
              subst hfold
              exact ⟨s', hcore, rfl⟩
            · exact Except.noConfusion hfold
          · exact Except.noConfusion hinner
        · exact Except.noConfusion hinner
      · exact Except.noConfusion h
    · exact Except.noConfusion h
  · exact Except.noConfusion h

theorem pruneAuth_live_sublist (s : State) : (pruneAuth s).live.Sublist s.live :=
  List.filter_sublist

theorem ofOption_ok {o : Option State} {s : State} (h : ofOption o = .ok s) :
    o = some s := by
  cases o with
  | none => exact Except.noConfusion h
  | some x =>
    simp only [ofOption, Except.ok.injEq] at h
    rw [h]

/-- A spend-then-effect success removed one authorization for exactly that
target and verdict, and the effect ran on the remainder. -/
theorem spendThen_ok {target : EconomicTarget} {verdict : KelGroups.Vote.Verdict}
    {s s' : State} {effect : State → Option State}
    (h : spendThen target verdict s effect = .ok s') :
    ∃ a rest, pullLive target verdict s.live = some (a, rest)
      ∧ effect { s with live := rest } = some s' := by
  unfold spendThen at h
  split at h
  · exact Except.noConfusion h
  · next a rest hpull =>
    split at h
    · next s'' heff =>
      simp only [Except.ok.injEq] at h
      subst h
      exact ⟨a, rest, hpull, heff⟩
    · exact Except.noConfusion h

/-- **Permission, positive.** A root success on `grantPermission c` spent an
unspent positive authorization bound to collection `c`; what remains unspent
is drawn from the rest. With none, the root refuses. -/
theorem apply_grant_spends {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {gs : KelGroups.GroupState State} {signer : KelGroups.Key} {c : CollId}
    {r : KelGroups.IntegratedResult State}
    (h : apply θ auth gs signer (.app (.grantPermission c)) = .ok r) :
    ∃ a rest, pullLive (.permission c) .positive gs.appFold.live = some (a, rest)
      ∧ r.state.appFold.live.Sublist rest := by
  obtain ⟨s', hcore, hr⟩ := apply_app_ok h
  simp only [appFoldCore] at hcore
  obtain ⟨a, rest, hpull, heff⟩ := spendThen_ok hcore
  refine ⟨a, rest, hpull, ?_⟩
  rw [hr]
  have hframe := (step_auth_frame heff).1
  refine (pruneAuth_live_sublist s').trans ?_
  rw [hframe]
  exact List.Sublist.refl _

/-- **Backdonation.** A root success on `backdonate w` spent an unspent
positive authorization bound to the share `w` itself. -/
theorem apply_backdonate_spends {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {gs : KelGroups.GroupState State} {signer : KelGroups.Key} {w : Int}
    {r : KelGroups.IntegratedResult State}
    (h : apply θ auth gs signer (.app (.backdonate w)) = .ok r) :
    ∃ a rest, pullLive (.backdonation w) .positive gs.appFold.live = some (a, rest)
      ∧ r.state.appFold.live.Sublist rest := by
  obtain ⟨s', hcore, hr⟩ := apply_app_ok h
  simp only [appFoldCore] at hcore
  obtain ⟨a, rest, hpull, heff⟩ := spendThen_ok hcore
  refine ⟨a, rest, hpull, ?_⟩
  rw [hr]
  have hframe := (step_auth_frame heff).1
  refine (pruneAuth_live_sublist s').trans ?_
  rw [hframe]
  exact List.Sublist.refl _

/-- The negative continuation spends a negative authorization for `c` and
refunds every accepted and pending pledge of `c`. -/
theorem denyByClosure_spec {s s' : State} {c : CollId}
    (h : denyByClosure s c = some s') :
    ∃ a rest col cs,
      pullLive (.permission c) .negative s.live = some (a, rest)
        ∧ pullCollection c s.collections = some (col, cs)
        ∧ s' = { s with
            live := rest
            conti := refundAll s.conti (col.accepted ++ col.pending)
            collections := cs } := by
  unfold denyByClosure at h
  split at h
  · exact Option.noConfusion h
  · next a rest hpull =>
    split at h
    · exact Option.noConfusion h
    · next col cs hcol =>
      simp only [Option.some.injEq] at h
      exact ⟨a, rest, col, cs, hpull, hcol, h.symm⟩

/-- **Permission, negative.** A root success on `denyPermission c` spent an
unspent negative authorization bound to collection `c`. -/
theorem apply_deny_spends {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {gs : KelGroups.GroupState State} {signer : KelGroups.Key} {c : CollId}
    {r : KelGroups.IntegratedResult State}
    (h : apply θ auth gs signer (.app (.denyPermission c)) = .ok r) :
    ∃ a rest, pullLive (.permission c) .negative gs.appFold.live = some (a, rest)
      ∧ r.state.appFold.live.Sublist rest := by
  obtain ⟨s', hcore, hr⟩ := apply_app_ok h
  simp only [appFoldCore] at hcore
  split at hcore
  · obtain ⟨a, rest, col, cs, hpull, _, hs⟩ := denyByClosure_spec (ofOption_ok hcore)
    refine ⟨a, rest, hpull, ?_⟩
    rw [hr, hs]
    exact pruneAuth_live_sublist _
  · exact Except.noConfusion hcore

/-! ### Provenance over production histories -/

/-- Every unspent authorization is backed by a closure record with its id and
verdict in the payload's own closure log. -/
def liveBacked (s : State) : Bool :=
  s.live.all (fun a => s.votes.closed.any
    (fun r => decide (r.questionId = a.questionId ∧ r.verdict = a.verdict)))

theorem liveBacked_iff (s : State) :
    liveBacked s = true ↔
      ∀ a ∈ s.live, ∃ r ∈ s.votes.closed,
        r.questionId = a.questionId ∧ r.verdict = a.verdict := by
  simp [liveBacked, List.all_eq_true, List.any_eq_true]

theorem tryEnactBase_unchanged {AppState AppEvent BaseProposal AppError : Type}
    {I : KelGroups.Integration AppState AppEvent BaseProposal AppError}
    {gs : KelGroups.GroupState AppState} {pid : KelGroups.ProposalId}
    {r : KelGroups.IntegratedResult AppState}
    (h : KelGroups.tryEnactBase I gs pid = .ok r) (hc : r.change = none) :
    r.state.appFold = gs.appFold := by
  unfold KelGroups.tryEnactBase at h
  split at h
  · simp only [Except.ok.injEq] at h
    subst h
    rfl
  · split at h
    · rw [(KelGroups.commitBaseChange_ok h).1] at hc
      exact Option.noConfusion hc
    · simp only [Except.ok.injEq] at h
      subst h
      rfl

/-! The `authStep_*` lemmas below share one conclusion, written out in each
statement: a payload step from `s` to `t` that keeps every closure record of
`s`, and whose unspent authorizations are those of `s` or are backed by a
record of `t` with the same question id and verdict. -/

theorem authStep_refl (s : State) : ((∀ r ∈ s.votes.closed, r ∈ s.votes.closed)
      ∧ ∀ a ∈ s.live, a ∈ s.live ∨
        ∃ r ∈ s.votes.closed, r.questionId = a.questionId ∧ r.verdict = a.verdict) :=
  ⟨fun _ h => h, fun _ h => Or.inl h⟩

theorem authStep_trans {s t u : State} (h1 : ((∀ r ∈ s.votes.closed, r ∈ t.votes.closed)
      ∧ ∀ a ∈ t.live, a ∈ s.live ∨
        ∃ r ∈ t.votes.closed, r.questionId = a.questionId ∧ r.verdict = a.verdict))
    (h2 : ((∀ r ∈ t.votes.closed, r ∈ u.votes.closed)
      ∧ ∀ a ∈ u.live, a ∈ t.live ∨
        ∃ r ∈ u.votes.closed, r.questionId = a.questionId ∧ r.verdict = a.verdict)) :
    ((∀ r ∈ s.votes.closed, r ∈ u.votes.closed)
      ∧ ∀ a ∈ u.live, a ∈ s.live ∨
        ∃ r ∈ u.votes.closed, r.questionId = a.questionId ∧ r.verdict = a.verdict) := by
  refine ⟨fun r hr => h2.1 r (h1.1 r hr), fun a ha => ?_⟩
  rcases h2.2 a ha with hat | hback
  · rcases h1.2 a hat with has | ⟨r, hr, hq, hv⟩
    · exact Or.inl has
    · exact Or.inr ⟨r, h2.1 r hr, hq, hv⟩
  · exact Or.inr hback

theorem liveBacked_of_authStep {s t : State}
    (h : ((∀ r ∈ s.votes.closed, r ∈ t.votes.closed)
      ∧ ∀ a ∈ t.live, a ∈ s.live ∨
        ∃ r ∈ t.votes.closed, r.questionId = a.questionId ∧ r.verdict = a.verdict))
    (hs : liveBacked s = true) : liveBacked t = true := by
  rw [liveBacked_iff] at hs ⊢
  intro a ha
  rcases h.2 a ha with has | hback
  · obtain ⟨r, hr, hq, hv⟩ := hs a has
    exact ⟨r, h.1 r hr, hq, hv⟩
  · exact hback

/-- Same closure log, unspent authorizations drawn from the old ones. -/
theorem authStep_of_frame {s t : State} (hv : t.votes = s.votes)
    (hl : ∀ a ∈ t.live, a ∈ s.live) : ((∀ r ∈ s.votes.closed, r ∈ t.votes.closed)
      ∧ ∀ a ∈ t.live, a ∈ s.live ∨
        ∃ r ∈ t.votes.closed, r.questionId = a.questionId ∧ r.verdict = a.verdict) :=
  ⟨fun r hr => hv ▸ hr, fun a ha => Or.inl (hl a ha)⟩

theorem authStep_prune (s : State) : ((∀ r ∈ s.votes.closed, r ∈ (pruneAuth s).votes.closed)
      ∧ ∀ a ∈ (pruneAuth s).live, a ∈ s.live ∨
        ∃ r ∈ (pruneAuth s).votes.closed, r.questionId = a.questionId ∧ r.verdict = a.verdict) :=
  authStep_of_frame rfl (fun _ ha => (List.mem_filter.mp ha).1)

theorem authStep_step {view : KelGroups.GroupView} {s s' : State}
    {signer : KelGroups.Key} {e : AppEvent} {auth : BackdonateAuth}
    (h : step view s signer e auth = some s') : ((∀ r ∈ s.votes.closed, r ∈ (s').votes.closed)
      ∧ ∀ a ∈ (s').live, a ∈ s.live ∨
        ∃ r ∈ (s').votes.closed, r.questionId = a.questionId ∧ r.verdict = a.verdict) := by
  obtain ⟨hl, hv, _⟩ := step_auth_frame h
  exact authStep_of_frame hv (fun a ha => hl ▸ ha)

/-- Minting backs every new authorization by a record of the vote step's own
log, provided that log kept every old record. -/
theorem authStep_syncAuth {s : State} {votes : KelGroups.Vote.VoteState}
    (hkeep : ∀ r ∈ s.votes.closed, r ∈ votes.closed) :
    ((∀ r ∈ s.votes.closed, r ∈ (syncAuth s votes).votes.closed)
      ∧ ∀ a ∈ (syncAuth s votes).live, a ∈ s.live ∨
        ∃ r ∈ (syncAuth s votes).votes.closed, r.questionId = a.questionId ∧ r.verdict = a.verdict) := by
  refine ⟨hkeep, fun a ha => ?_⟩
  simp only [syncAuth, List.mem_append] at ha
  rcases ha with hold | hnew
  · exact Or.inl hold
  · right
    obtain ⟨r, hr, hmap⟩ := List.mem_filterMap.mp hnew
    obtain ⟨target, _, hrec⟩ := Option.map_eq_some_iff.mp hmap
    subst hrec
    exact ⟨r, (List.mem_filter.mp hr).1, rfl, rfl⟩

theorem effectedState_keeps_closed (gs : KelGroups.Vote.VoteState)
    (signer : KelGroups.Key) (ev : KelGroups.Vote.VoteEvent) :
    ∀ r ∈ gs.closed, r ∈ (KelGroups.Vote.effectedState gs signer ev).closed := by
  intro r hr
  cases ev with
  | openQuestion qid kind =>
    simp only [KelGroups.Vote.effectedState]
    split <;> exact hr
  | cast qid ballot =>
    simp only [KelGroups.Vote.effectedState]
    split <;> exact hr
  | renounce qid =>
    simp only [KelGroups.Vote.effectedState]
    split
    · exact List.mem_append_left _ hr
    · exact hr

theorem sweepClosures_keeps_closed (θ : KelGroups.Vote.Threshold)
    (view : KelGroups.GroupView) (gs : KelGroups.Vote.VoteState) :
    ∀ r ∈ gs.closed, r ∈ (KelGroups.Vote.sweepClosures θ view gs).closed :=
  fun _ hr => List.mem_append_left _ hr

theorem closeProposerQuestions_keeps_closed (key : KelGroups.Key)
    (gs : KelGroups.Vote.VoteState) :
    ∀ r ∈ gs.closed, r ∈ (KelGroups.Vote.closeProposerQuestions key gs).closed :=
  fun _ hr => List.mem_append_left _ hr

theorem authStep_voteApply {θ : KelGroups.Vote.Threshold} {view : KelGroups.GroupView}
    {s s' : State} {signer : KelGroups.Key} {ev : KelGroups.Vote.VoteEvent}
    (h : voteApply θ view s signer ev = .ok s') : ((∀ r ∈ s.votes.closed, r ∈ (s').votes.closed)
      ∧ ∀ a ∈ (s').live, a ∈ s.live ∨
        ∃ r ∈ (s').votes.closed, r.questionId = a.questionId ∧ r.verdict = a.verdict) := by
  unfold voteApply at h
  split at h
  · exact Except.noConfusion h
  · next votes hvotes =>
    simp only [Except.ok.injEq] at h
    subst h
    apply authStep_syncAuth
    unfold KelGroups.Vote.applyVoteEventChecked at hvotes
    split at hvotes
    · exact Except.noConfusion hvotes
    · simp only [Except.ok.injEq] at hvotes
      subst hvotes
      exact fun r hr => sweepClosures_keeps_closed _ _ _ r
        (effectedState_keeps_closed _ _ _ r hr)

theorem authStep_appFoldCore {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {signer : KelGroups.Key} {pre post : KelGroups.GroupView} {s s' : State}
    {e : AppEvent} (h : appFoldCore θ auth signer pre post s e = .ok s') :
    ((∀ r ∈ s.votes.closed, r ∈ (s').votes.closed)
      ∧ ∀ a ∈ (s').live, a ∈ s.live ∨
        ∃ r ∈ (s').votes.closed, r.questionId = a.questionId ∧ r.verdict = a.verdict) := by
  cases e with
  | openQuestion qid kind => exact authStep_voteApply h
  | cast qid ballot => exact authStep_voteApply h
  | renounce qid => exact authStep_voteApply h
  | openBound qid kind target =>
    simp only [appFoldCore] at h
    split at h
    · refine authStep_trans ?_ (authStep_voteApply h)
      exact authStep_of_frame rfl (fun _ ha => ha)
    · exact Except.noConfusion h
  | grantPermission c =>
    simp only [appFoldCore] at h
    obtain ⟨a, rest, hpull, heff⟩ := spendThen_ok h
    refine authStep_trans ?_ (authStep_step heff)
    exact authStep_of_frame rfl (fun x hx => (pullLive_spec hpull).2.2.mem_iff.mpr
      (List.mem_cons_of_mem _ hx))
  | backdonate w =>
    simp only [appFoldCore] at h
    obtain ⟨a, rest, hpull, heff⟩ := spendThen_ok h
    refine authStep_trans ?_ (authStep_step heff)
    exact authStep_of_frame rfl (fun x hx => (pullLive_spec hpull).2.2.mem_iff.mpr
      (List.mem_cons_of_mem _ hx))
  | denyPermission c =>
    simp only [appFoldCore] at h
    split at h
    · obtain ⟨a, rest, col, cs, hpull, _, hs⟩ := denyByClosure_spec (ofOption_ok h)
      subst hs
      exact authStep_of_frame rfl (fun x hx => ((pullLive_spec hpull).2.2.mem_iff.mpr
        (List.mem_cons_of_mem _ hx)))
    · exact Except.noConfusion h
  | openPurchase _ | deposit _ _ | withdraw _ _ | transferCassa _ _ | donate _
  | pledge _ _ _ | acceptPledge _ _ | refusePledge _ _ | correctPledge _ _ _
  | closePurchase _ | failPurchase _ =>
    simp only [appFoldCore] at h
    exact authStep_step (ofOption_ok h)

theorem economicCleanup_frame {change : KelGroups.BaseChange}
    {pre post : KelGroups.GroupView} {s s' : State}
    (h : economicCleanup change pre post s = some s') :
    s'.live = s.live ∧ s'.votes = s.votes := by
  cases change <;> simp only [economicCleanup] at h
  case memberAdmitted =>
    obtain rfl := Option.some.inj h
    exact ⟨rfl, rfl⟩
  case memberRemoved =>
    obtain ⟨_, _, h⟩ := option_bind_inv h
    obtain rfl := Option.some.inj h
    split <;> exact ⟨rfl, rfl⟩
  case rolesChanged =>
    split at h
    · obtain ⟨_, _, h⟩ := option_bind_inv h
      obtain rfl := Option.some.inj h
      exact ⟨rfl, rfl⟩
    · obtain rfl := Option.some.inj h
      exact ⟨rfl, rfl⟩

theorem authStep_baseHook {θ : KelGroups.Vote.Threshold} {change : KelGroups.BaseChange}
    {pre post : KelGroups.GroupView} {s s' : State}
    (h : baseHook θ change pre post s = .ok s') : ((∀ r ∈ s.votes.closed, r ∈ (s').votes.closed)
      ∧ ∀ a ∈ (s').live, a ∈ s.live ∨
        ∃ r ∈ (s').votes.closed, r.questionId = a.questionId ∧ r.verdict = a.verdict) := by
  unfold baseHook at h
  split at h
  · exact Except.noConfusion h
  · next cleaned hclean =>
    simp only [Except.ok.injEq] at h
    subst h
    obtain ⟨hl, hv⟩ := economicCleanup_frame hclean
    refine authStep_trans (authStep_of_frame hv (fun a ha => hl ▸ ha))
      (authStep_trans (authStep_syncAuth ?_) (authStep_prune _))
    intro r hr
    rw [hv] at hr
    apply sweepClosures_keeps_closed
    cases change with
    | memberAdmitted _ => exact hr
    | memberRemoved key => exact closeProposerQuestions_keeps_closed key _ r hr
    | rolesChanged _ => exact hr

/-- The integrated boundary changes the app payload only through the app fold
or, on a committed base change, the sealed hook. -/
theorem applyIntegrated_payload_cases {AppState AppEvent BaseProposal AppError : Type}
    (I : KelGroups.Integration AppState AppEvent BaseProposal AppError)
    (gs : KelGroups.GroupState AppState) (signer : KelGroups.Key)
    (event : KelGroups.IntegratedEvent BaseProposal AppEvent)
    (r : KelGroups.IntegratedResult AppState)
    (h : KelGroups.applyIntegratedEvent I gs signer event = .ok r) :
    r.state.appFold = gs.appFold
      ∨ (∃ e, I.appFold signer (KelGroups.groupView gs) (KelGroups.groupView gs)
          gs.appFold e = .ok r.state.appFold)
      ∨ (∃ change pre post, I.baseHook change pre post gs.appFold = .ok r.state.appFold) := by
  cases hc : r.change with
  | some change =>
    exact Or.inr (Or.inr ⟨change, _, _,
      KelGroups.base_change_runs_hook I gs signer event r change h hc⟩)
  | none =>
    cases event with
    | app e =>
      simp only [KelGroups.applyIntegratedEvent] at h
      split at h
      · split at h
        · next appState hfold =>
          simp only [Except.ok.injEq] at h
          subst h
          exact Or.inr (Or.inl ⟨e, hfold⟩)
        · exact Except.noConfusion h
      · exact Except.noConfusion h
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

/-- The production root changes the app payload only through the app fold or,
on a committed base change, the sealed hook. -/
theorem apply_payload_cases {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {gs : KelGroups.GroupState State} {signer : KelGroups.Key}
    {event : KelGroups.IntegratedEvent Proposal AppEvent}
    {res : KelGroups.IntegratedResult State}
    (h : apply θ auth gs signer event = .ok res) :
    res.state.appFold = gs.appFold
      ∨ (∃ e, appFold θ auth signer (KelGroups.groupView gs) (KelGroups.groupView gs)
          gs.appFold e = .ok res.state.appFold)
      ∨ (∃ change pre post, baseHook θ change pre post gs.appFold = .ok res.state.appFold) := by
  unfold apply at h
  split at h
  · split at h
    · next inner hinner =>
      split at h
      · simp only [Except.ok.injEq] at h
        subst h
        exact applyIntegrated_payload_cases _ gs signer event inner hinner
      · exact Except.noConfusion h
    · exact Except.noConfusion h
  · exact Except.noConfusion h

theorem authStep_apply {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {gs : KelGroups.GroupState State} {signer : KelGroups.Key}
    {event : KelGroups.IntegratedEvent Proposal AppEvent}
    {res : KelGroups.IntegratedResult State}
    (h : apply θ auth gs signer event = .ok res) :
    ((∀ r ∈ gs.appFold.votes.closed, r ∈ res.state.appFold.votes.closed)
      ∧ ∀ a ∈ res.state.appFold.live, a ∈ gs.appFold.live ∨
        ∃ r ∈ res.state.appFold.votes.closed, r.questionId = a.questionId ∧ r.verdict = a.verdict) := by
  rcases apply_payload_cases h with heq | ⟨e, hfold⟩ | ⟨change, pre, post, hhook⟩
  · rw [heq]
    exact authStep_refl _
  · simp only [appFold] at hfold
    split at hfold
    · next s' hcore =>
      simp only [Except.ok.injEq] at hfold
      rw [← hfold]
      exact authStep_trans (authStep_appFoldCore hcore) (authStep_prune s')
    · exact Except.noConfusion hfold
  · exact authStep_baseHook hhook

/-- **Provenance.** In every production history — a guarded boot followed by
successful calls of the production root — each unspent authorization is backed
by a closure record, with its question id and verdict, that the history's own
vote fold appended: the boot payload holds no closure record, binding or
authorization, and only the vote step and the sealed hook append records. -/
theorem liveBacked_of_history {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {gs : KelGroups.GroupState State} (h : ProductionHistory θ auth gs) :
    liveBacked gs.appFold = true := by
  induction h with
  | boot members payload hboot =>
    simp only [Reactivegas.boot] at hboot
    split at hboot
    · next hclean =>
      simp only [Option.some.injEq] at hboot
      subst hboot
      simp only [Bool.and_eq_true, cleanOrigin, List.isEmpty_iff] at hclean
      simp [liveBacked, hclean.2.2]
    · exact Option.noConfusion hboot
  | apply _ signer event happly ih =>
    exact liveBacked_of_authStep (authStep_apply happly) ih



/-! ### Authority follows its collection -/

/-- After `pruneAuth` every unspent authorization and every binding names a
target that is there. -/
theorem pruneAuth_present (s : State) :
    (∀ a ∈ (pruneAuth s).live, targetPresent (pruneAuth s) a.target = true)
      ∧ ∀ b ∈ (pruneAuth s).bindings, targetPresent (pruneAuth s) b.2 = true := by
  have hsame : targetPresent (pruneAuth s) = targetPresent s := rfl
  rw [hsame]
  exact ⟨fun _ ha => (List.mem_filter.mp ha).2, fun _ hb => (List.mem_filter.mp hb).2⟩

theorem appFold_pruned {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth}
    {signer : KelGroups.Key} {pre post : KelGroups.GroupView} {s s' : State}
    {e : AppEvent} (h : appFold θ auth signer pre post s e = .ok s') :
    ∃ x, s' = pruneAuth x := by
  simp only [appFold] at h
  split at h
  · next x _ =>
    simp only [Except.ok.injEq] at h
    exact ⟨x, h.symm⟩
  · exact Except.noConfusion h

theorem baseHook_pruned {θ : KelGroups.Vote.Threshold} {change : KelGroups.BaseChange}
    {pre post : KelGroups.GroupView} {s s' : State}
    (h : baseHook θ change pre post s = .ok s') : ∃ x, s' = pruneAuth x := by
  unfold baseHook at h
  split at h
  · exact Except.noConfusion h
  · simp only [Except.ok.injEq] at h
    exact ⟨_, h.symm⟩

/-- **A collection takes its authority with it.** In every production history,
every unspent authorization and every binding of a permission names a
collection that is present. Every route that removes a collection —
`closePurchase`, `failPurchase`, `denyPermission`, and the admin wind-up of a
departure or role loss — ends in `pruneAuth`, so a collection id reused later
names a new collection that inherits nothing. -/
theorem authTargetsPresent_of_history {θ : KelGroups.Vote.Threshold}
    {auth : BackdonateAuth} {gs : KelGroups.GroupState State}
    (h : ProductionHistory θ auth gs) :
    (∀ a ∈ gs.appFold.live, targetPresent gs.appFold a.target = true)
      ∧ ∀ b ∈ gs.appFold.bindings, targetPresent gs.appFold b.2 = true := by
  induction h with
  | boot members payload hboot =>
    simp only [Reactivegas.boot] at hboot
    split at hboot
    · next hclean =>
      simp only [Option.some.injEq] at hboot
      subst hboot
      simp only [Bool.and_eq_true, cleanOrigin, List.isEmpty_iff] at hclean
      simp [hclean.2.1.2, hclean.2.2]
    · exact Option.noConfusion hboot
  | apply _ signer event happly ih =>
    rcases apply_payload_cases happly with heq | ⟨e, hfold⟩ | ⟨change, pre, post, hhook⟩
    · rw [heq]
      exact ih
    · obtain ⟨x, hx⟩ := appFold_pruned hfold
      rw [hx]
      exact pruneAuth_present x
    · obtain ⟨x, hx⟩ := baseHook_pruned hhook
      rw [hx]
      exact pruneAuth_present x

/-! ### The provenance antecedent is reachable -/

/-- Extend a production history by running signed events through the root;
`none` on the first refusal. -/
def extendHistory {θ : KelGroups.Vote.Threshold} {auth : BackdonateAuth} :
    (gs : KelGroups.GroupState State) → ProductionHistory θ auth gs →
      List (KelGroups.Key × KelGroups.IntegratedEvent Proposal AppEvent) →
        Option (Σ' g, ProductionHistory θ auth g)
  | gs, h, [] => some ⟨gs, h⟩
  | gs, h, (signer, e) :: rest =>
      match happly : apply θ auth gs signer e with
      | .ok r => extendHistory r.state (.apply h signer e happly) rest
      | .error _ => none

/-- A real history: boot, open a purchase, open a question bound to it, and
close it positively. Its root holds one unspent authorization. -/
def boundHistory :
    Option (Σ' g, ProductionHistory KelGroups.Vote.legacyThreshold probeAuth g) :=
  let members : List (KelGroups.Key × KelGroups.Member) :=
    [ ("alice", { key := "alice", email := "alice@example",
                  roles := [KelGroups.Role.adminRole KelGroups.Admin.publicAdmin] })
    , ("bob", { key := "bob", email := "bob@example", roles := [] }) ]
  match hboot : boot members State.empty with
  | some gs =>
      extendHistory gs (.boot members State.empty hboot)
        [ ("alice", .app (.openPurchase 1))
        , ("alice", .app (.openBound "q" .collective (.permission 1)))
        , ("alice", .app (.cast "q" .assent)) ]
  | none => none

/-- `liveBacked_of_history` is not vacuous: a reachable history holds an
unspent authorization, and it is backed. -/
theorem boundHistory_holds_backed_authorization :
    boundHistory.map (fun p => (p.1.appFold.live.length, liveBacked p.1.appFold))
      = some (1, true) := by
  decide

#print axioms apply_grant_spends
#print axioms apply_deny_spends
#print axioms apply_backdonate_spends
#print axioms denyByClosure_spec
#print axioms liveBacked_of_history
#print axioms authTargetsPresent_of_history
#print axioms boundHistory_holds_backed_authorization

end Reactivegas.Composition
