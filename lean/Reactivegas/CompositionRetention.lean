import Reactivegas.CompositionRoot
import Reactivegas.FiniteHistoryTests
import Reactivegas.CorpusHistory

/-!
# No open question is dropped without a record (issue #66, S5)

The vote payload closes a question by removing it from the open set and
appending a closure record, as one operation (R-61): a purchase-approval
question holds members' money in escrow, and a question erased without a
verdict strands it. `closed` being append-only is proved already
(`effectedState_keeps_closed`, `authStep_apply`); this module states the other
half for the production root `Reactivegas.apply`. Every question id open in the
vote payload before a successful event is, after it, still open or named by a
closure record. It covers every route that writes the vote payload: the vote
events, the bound open, the app-decided and economic events (which leave the
payload as it was), and the sealed base hook with the proposer-departure
closure. It changes no production definition and none of the theorems it builds
on.

Status (R-35): enforced: PROVED-IN-MODEL.
-/

namespace Reactivegas.Composition

open Reactivegas

/-- A payload step from `s` to `t` that keeps every closure record and, for every
question open in `s`, leaves it open in `t` or names it by a closure record of
`t`. Sequential composition needs the kept records: a question closed in the
middle must stay named afterwards. A notation, not a definition: it names no
predicate. -/
local macro "RetainedStep " s:term:max t:term:max : term =>
  `((∀ r ∈ ($s).closed, r ∈ ($t).closed) ∧
    ∀ q x, (q, x) ∈ ($s).openQuestions →
      (∃ y, (q, y) ∈ ($t).openQuestions) ∨ ∃ r ∈ ($t).closed, r.questionId = q)


/-! ### The association-list steps -/

theorem mem_assocErase_of_ne {κ ν : Type} [BEq κ] [LawfulBEq κ] (j k : κ) (v : ν)
    (hne : k ≠ j) :
    ∀ l : List (κ × ν), (k, v) ∈ l → (k, v) ∈ KelGroups.assocErase j l
  | [], h => by simp at h
  | (c, w) :: rest, h => by
    unfold KelGroups.assocErase
    split
    · next hc =>
      have hcj : c = j := beq_iff_eq.mp hc
      rcases List.mem_cons.mp h with h1 | h1
      · exact absurd ((Prod.mk.inj h1).1.trans hcj) hne
      · exact h1
    · next hc =>
      rcases List.mem_cons.mp h with h1 | h1
      · exact List.mem_cons.mpr (Or.inl h1)
      · exact List.mem_cons.mpr (Or.inr (mem_assocErase_of_ne j k v hne rest h1))

/-- Inserting under `j` keeps a key open: its own, with the new value, and
every other, with the old one. -/
theorem key_of_assocInsert (j : KelGroups.Vote.QuestionId)
    (w : KelGroups.Vote.Question)
    (l : List (KelGroups.Vote.QuestionId × KelGroups.Vote.Question)) (k : KelGroups.Vote.QuestionId)
    (x : KelGroups.Vote.Question) (h : (k, x) ∈ l) :
    ∃ y, (k, y) ∈ KelGroups.assocInsert j w l := by
  by_cases hk : k = j
  · subst hk
    exact ⟨w, List.mem_cons.mpr (Or.inl rfl)⟩
  · exact ⟨x, List.mem_cons.mpr (Or.inr (mem_assocErase_of_ne j k x hk l h))⟩

/-! ### The production root

The proof is one theorem on purpose: each route's step is argued inside it, so
a production definition that drops a question breaks this theorem's own
declaration, not only a helper that the theorem rests on. -/

/-- **No open question is dropped without a record.** For every successful
event at the production root, every question id open in the vote payload
before is, in the payload after, still open or named by a closure record. -/
theorem apply_retains_open_questions {θ : KelGroups.Vote.Threshold}
    {auth : BackdonateAuth} {gs : KelGroups.GroupState State} {signer : KelGroups.Key}
    {event : KelGroups.IntegratedEvent Proposal AppEvent}
    {res : KelGroups.IntegratedResult State}
    (h : apply θ auth gs signer event = .ok res) :
    ∀ q x, (q, x) ∈ gs.appFold.votes.openQuestions →
      (∃ y, (q, y) ∈ res.state.appFold.votes.openQuestions)
        ∨ ∃ r ∈ res.state.appFold.votes.closed, r.questionId = q := by
  have refl : ∀ v : KelGroups.Vote.VoteState, RetainedStep v v :=
    fun _ => ⟨fun _ h => h, fun _ x h => Or.inl ⟨x, h⟩⟩
  have ofEq : ∀ a b : KelGroups.Vote.VoteState, b = a → RetainedStep a b := by
    intro a b hab
    subst hab
    exact refl b
  have trans : ∀ a b c : KelGroups.Vote.VoteState,
      RetainedStep a b → RetainedStep b c → RetainedStep a c := by
    intro a b c h1 h2
    refine ⟨fun r hr => h2.1 r (h1.1 r hr), fun q x hq => ?_⟩
    rcases h1.2 q x hq with ⟨y, hy⟩ | ⟨r, hr, hrq⟩
    · exact h2.2 q y hy
    · exact Or.inr ⟨r, h2.1 r hr, hrq⟩
  -- the vote events' own effect
  have heff : ∀ (gs : KelGroups.Vote.VoteState) (signer : KelGroups.Key)
      (ev : KelGroups.Vote.VoteEvent),
      RetainedStep gs (KelGroups.Vote.effectedState gs signer ev) := by
    intro gs signer ev
    cases ev with
    | openQuestion qid kind =>
      simp only [KelGroups.Vote.effectedState]
      split
      · exact ⟨fun _ hr => hr, fun q x hq => Or.inl (key_of_assocInsert qid _ _ q x hq)⟩
      · exact refl gs
    | cast qid ballot =>
      simp only [KelGroups.Vote.effectedState]
      split
      · exact ⟨fun _ hr => hr, fun q x hq => Or.inl (key_of_assocInsert qid _ _ q x hq)⟩
      · exact refl gs
    | renounce qid =>
      simp only [KelGroups.Vote.effectedState]
      split
      · refine ⟨fun r hr => List.mem_append_left _ hr, fun q x hq => ?_⟩
        by_cases hk : q = qid
        · right
          exact ⟨_, List.mem_append_right _ (List.mem_singleton.mpr rfl), hk.symm⟩
        · left
          exact ⟨x, mem_assocErase_of_ne qid q x hk _ hq⟩
      · exact refl gs
  -- the sweep closes a decided question with a record under its id
  have hsweep : ∀ (view : KelGroups.GroupView) (gs : KelGroups.Vote.VoteState),
      RetainedStep gs (KelGroups.Vote.sweepClosures θ view gs) := by
    intro view gs
    refine ⟨sweepClosures_keeps_closed θ view gs, fun q x hq => ?_⟩
    by_cases hv : KelGroups.Vote.verdictOf θ view x = .open
    · left
      exact ⟨x, List.mem_filter.mpr ⟨hq, decide_eq_true hv⟩⟩
    · right
      have hsome : ∃ rec, KelGroups.Vote.sweepStep θ view (q, x) = some rec
          ∧ rec.questionId = q := by
        unfold KelGroups.Vote.sweepStep
        split
        · rename_i hvd
          exact absurd hvd hv
        · exact ⟨_, rfl, rfl⟩
      obtain ⟨rec, hrec, hq'⟩ := hsome
      exact ⟨rec, List.mem_append_right _ (List.mem_filterMap.mpr ⟨(q, x), hq, hrec⟩), hq'⟩
  -- a departing proposer's questions close negatively, each with a record
  have hdepart : ∀ (key : KelGroups.Key) (gs : KelGroups.Vote.VoteState),
      RetainedStep gs (KelGroups.Vote.closeProposerQuestions key gs) := by
    intro key gs
    refine ⟨closeProposerQuestions_keeps_closed key gs, fun q x hq => ?_⟩
    cases hb : (x.proposer == key)
    · left
      exact ⟨x, List.mem_filter.mpr ⟨hq, by simp [bne, hb]⟩⟩
    · right
      exact ⟨{ questionId := q, question := x, verdict := .negative,
               cause := .proposerDeparted },
        List.mem_append_right _
          (List.mem_map.mpr ⟨(q, x), List.mem_filter.mpr ⟨hq, hb⟩, rfl⟩), rfl⟩
  -- the checked vote step: effect, then sweep
  have hvote : ∀ (view : KelGroups.GroupView) (s s' : State) (signer : KelGroups.Key)
      (ev : KelGroups.Vote.VoteEvent),
      voteApply θ view s signer ev = .ok s' → RetainedStep s.votes s'.votes := by
    intro view s s' signer ev hv
    unfold voteApply at hv
    split at hv
    · exact Except.noConfusion hv
    · next votes hvotes =>
      simp only [Except.ok.injEq] at hv
      subst hv
      unfold KelGroups.Vote.applyVoteEventChecked at hvotes
      split at hvotes
      · exact Except.noConfusion hvotes
      · simp only [Except.ok.injEq] at hvotes
        subst hvotes
        exact trans _ _ _ (heff _ _ _) (hsweep _ _)
  -- the app fold: vote events, the bound open, and the events that leave the payload alone
  have hcore : ∀ (signer : KelGroups.Key) (pre post : KelGroups.GroupView)
      (s s' : State) (e : AppEvent),
      appFoldCore θ auth signer pre post s e = .ok s' → RetainedStep s.votes s'.votes := by
    intro signer pre post s s' e hc
    cases e with
    | openQuestion qid kind => exact hvote _ _ _ _ _ hc
    | cast qid ballot => exact hvote _ _ _ _ _ hc
    | renounce qid => exact hvote _ _ _ _ _ hc
    | openBound qid kind target =>
      simp only [appFoldCore] at hc
      split at hc
      · have h2 := hvote _ _ _ _ _ hc
        exact h2
      · exact Except.noConfusion hc
    | grantPermission c =>
      simp only [appFoldCore] at hc
      obtain ⟨a, rest, hpull, heff'⟩ := spendThen_ok hc
      exact ofEq _ _ (step_auth_frame heff').2.1
    | backdonate w =>
      simp only [appFoldCore] at hc
      obtain ⟨a, rest, hpull, heff'⟩ := spendThen_ok hc
      exact ofEq _ _ (step_auth_frame heff').2.1
    | denyPermission c =>
      simp only [appFoldCore] at hc
      split at hc
      · obtain ⟨a, rest, col, cs, hpull, _, hs⟩ := denyByClosure_spec (ofOption_ok hc)
        subst hs
        exact refl _
      · exact Except.noConfusion hc
    | openPurchase _ | deposit _ _ | withdraw _ _ | transferCassa _ _ | donate _
    | pledge _ _ _ | acceptPledge _ _ | refusePledge _ _ | correctPledge _ _ _
    | closePurchase _ | failPurchase _ =>
      simp only [appFoldCore] at hc
      exact ofEq _ _ (step_auth_frame (ofOption_ok hc)).2.1
  -- the sealed base hook: cleanup keeps the payload, a departure closes, the sweep follows
  have hhook : ∀ (change : KelGroups.BaseChange) (pre post : KelGroups.GroupView)
      (s s' : State),
      baseHook θ change pre post s = .ok s' → RetainedStep s.votes s'.votes := by
    intro change pre post s s' hh
    unfold baseHook at hh
    split at hh
    · exact Except.noConfusion hh
    · next cleaned hclean =>
      simp only [Except.ok.injEq] at hh
      subst hh
      obtain ⟨_, hv⟩ := economicCleanup_frame hclean
      refine trans _ cleaned.votes _ (ofEq _ _ hv) ?_
      refine trans _ _ _ ?_ (hsweep post _)
      cases change with
      | memberAdmitted _ => exact ofEq _ _ hv.symm
      | memberRemoved key =>
        rw [hv]
        exact hdepart key _
      | rolesChanged _ => exact ofEq _ _ hv.symm
  rcases apply_payload_cases h with heq | ⟨e, hfold⟩ | ⟨change, pre, post, hhook'⟩
  · rw [heq]
    exact (refl _).2
  · simp only [appFold] at hfold
    split at hfold
    · next s' hcore' =>
      simp only [Except.ok.injEq] at hfold
      rw [← hfold]
      exact (hcore _ _ _ _ _ _ hcore').2
    · exact Except.noConfusion hfold
  · exact (hhook _ _ _ _ _ hhook').2

/-! ### The antecedent is reachable, on both arms -/

def retentionMembers : List (KelGroups.Key × KelGroups.Member) :=
  [ ("alice", { key := "alice", email := "alice@example",
                roles := [KelGroups.Role.adminRole KelGroups.Admin.publicAdmin] })
  , ("bob", { key := "bob", email := "bob@example", roles := [] }) ]

/-- A production history from a guarded boot followed by the given events. -/
def retentionHistory (events : List (KelGroups.Key × KelGroups.IntegratedEvent Proposal AppEvent)) :
    Option (Σ' g, ProductionHistory KelGroups.Vote.legacyThreshold probeAuth g) :=
  match hboot : boot retentionMembers State.empty with
  | some gs => extendHistory gs (.boot retentionMembers State.empty hboot) events
  | none => none

/-- What the payload holds after a history: the open ids and the closed ids. -/
def retentionView (p : Σ' g, ProductionHistory KelGroups.Vote.legacyThreshold probeAuth g) :
    List KelGroups.Vote.QuestionId × List KelGroups.Vote.QuestionId :=
  (p.1.appFold.votes.openQuestions.map Prod.fst,
    p.1.appFold.votes.closed.map (fun r => r.questionId))

/-- `apply_retains_open_questions` is not vacuous: a real production history
holds an open question, and a renounce then moves it from the open set to a
record carrying its id. -/
theorem retention_arms_reachable :
    (retentionHistory [("alice", .app (.openQuestion "q" .collective))]).map retentionView
        = some (["q"], [])
      ∧ (retentionHistory [("alice", .app (.openQuestion "q" .collective)),
            ("alice", .app (.renounce "q"))]).map retentionView = some ([], ["q"]) := by
  decide

#print axioms apply_retains_open_questions
#print axioms retention_arms_reachable

end Reactivegas.Composition
