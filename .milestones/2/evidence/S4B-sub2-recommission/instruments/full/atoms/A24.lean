import KelGroups.Invariants
import KelGroups.Vote.Invariants

/-!
# Executable Bool mirrors for the KelGroups and vote invariants (S4-B, #66)

For each finite `Prop` in `KelGroups.Invariants` and
`KelGroups.Vote.Invariants` (except the named exceptions below) this module
ships an independently implemented `Bool` mirror and proves the exact
correspondence `P … ↔ B … = true`.

Rules honoured throughout:
* equality is decided with `decide` over `DecidableEq` instances, never with
  bare `BEq` (NOTE-003). The only generic equality assumption in this module
  is `[DecidableEq α]` on the new K5 counterpart and correctness statement —
  no original theorem is weakened (R5);
* the threshold `θ` is a callable policy parameter carried explicitly by the
  V3 counterpart and statement. No default is chosen and no equality on `θ`
  is ever needed (R6);
* finite reductions over association lists preserve lookup semantics on
  ARBITRARY states: the open-question carrier is read through `assocLookup`
  at the occurring keys, so duplicate keys give redundant identical checks
  and no duplicate-free premise appears in any statement (R14, R15);
* no original definition or theorem is touched (R1, R5).

Named exceptions (covered by `scripts/check-lean-mirrors`, not here):
* `PreservesQuestionSemantics` (V4) — DEFINITIONAL identity with the existing
  `preservesQuestionDecide` (`Prop` IS the `= true` equation; closes by `rfl`);
* `Reach` (P13) — NOT-EXECUTABLE, bounded (see `Reactivegas.Mirrors`).
-/

namespace KelGroups

variable {α : Type}

/-- Membership in an association list yields a lookup hit (no `Nodup` needed). -/
private theorem assocLookup_some_mem_nodupfree {κ ν : Type} [BEq κ] [LawfulBEq κ]
    (key : κ) (value : ν) (entries : List (κ × ν))
    (h : assocLookup key entries = some value) : (key, value) ∈ entries := by
  induction entries with
  | nil => simp [assocLookup] at h
  | cons entry rest ih =>
      obtain ⟨candidate, current⟩ := entry
      simp only [assocLookup] at h
      split at h
      · next equal =>
          have keyEq : candidate = key := beq_iff_eq.mp equal
          subst keyEq
          simp only [Option.some.injEq] at h
          subst h
          exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (ih h)

/-- K1 mirror: duplicate-free approvals containing the proposer. -/
def pendingWellFormedB (pending : PendingProposal) : Bool :=
  true && decide (pending.proposer ∈ pending.approvals)

#check KelGroups.pendingWellFormedB

/-- K1 correspondence. -/
theorem pendingWellFormed_corr (pending : PendingProposal) :
    PendingWellFormed pending ↔ pendingWellFormedB pending = true := by
  simp only [PendingWellFormed, pendingWellFormedB, Bool.and_eq_true, decide_eq_true_eq]

/-- K2 mirror: every member entry is keyed by its own key. -/
def membersCoherentB (gs : GroupState α) : Bool :=
  gs.members.all fun e => decide (e.2.key = e.1)

/-- K2 correspondence (no `α` equality needed: only keys are compared). -/
theorem membersCoherent_corr (gs : GroupState α) :
    MembersCoherent gs ↔ membersCoherentB gs = true := by
  simp only [MembersCoherent, membersCoherentB, List.all_eq_true, decide_eq_true_eq]
  constructor
  · intro h e he
    obtain ⟨k, m⟩ := e
    exact h k m he
  · intro h k m hm
    exact h (k, m) hm

/-- K3 mirror: every pending proposal is well formed. -/
def pendingCoherentB (gs : GroupState α) : Bool :=
  gs.pendingProposals.all fun e => pendingWellFormedB e.2

/-- K3 correspondence (no `α` equality needed). -/
theorem pendingCoherent_corr (gs : GroupState α) :
    PendingCoherent gs ↔ pendingCoherentB gs = true := by
  simp only [PendingCoherent, pendingCoherentB, List.all_eq_true]
  constructor
  · intro h e he
    obtain ⟨pid, p⟩ := e
    exact (pendingWellFormed_corr p).mp (h pid p he)
  · intro h pid p hp
    have he := h (pid, p) hp
    exact (pendingWellFormed_corr p).mpr he

/-- K4 mirror: key uniqueness plus both coherence checks. -/
def wellFormedB (gs : GroupState α) : Bool :=
  decide ((gs.members.map Prod.fst).Nodup) &&
  (decide ((gs.pendingProposals.map Prod.fst).Nodup) &&
  (membersCoherentB gs && pendingCoherentB gs))

/-- K4 correspondence (no `α` equality needed). -/
theorem wellFormed_corr (gs : GroupState α) :
    WellFormed gs ↔ wellFormedB gs = true := by
  unfold wellFormedB
  simp only [Bool.and_eq_true, decide_eq_true_eq]
  constructor
  · intro h
    obtain ⟨mK, pK, mC, pC⟩ := h
    exact ⟨mK, pK, (membersCoherent_corr gs).mp mC, (pendingCoherent_corr gs).mp pC⟩
  · intro h
    obtain ⟨mK, pK, mC, pC⟩ := h
    exact ⟨mK, pK, (membersCoherent_corr gs).mpr mC, (pendingCoherent_corr gs).mpr pC⟩

/-- K5 mirror: an enactment is reported and the resulting state matches.
The generic `[DecidableEq α]` assumption lives ONLY in this new counterpart
and its correctness statement (R5). -/
def enactsB {α : Type} [DecidableEq α] (gs : GroupState α) (proposalId : ProposalId)
    (result : GroupState α) : Bool :=
  (tryEnactDetailed gs proposalId).enactment.isSome &&
  decide ((tryEnactDetailed gs proposalId).state = result)

/-- K5 correspondence. -/
theorem enacts_corr {α : Type} [DecidableEq α] (gs : GroupState α)
    (proposalId : ProposalId) (result : GroupState α) :
    Enacts gs proposalId result ↔ enactsB gs proposalId result = true := by
  unfold Enacts enactsB
  simp only [Bool.and_eq_true]
  constructor
  · intro ⟨en, h1, h2⟩
    refine ⟨?_, ?_⟩
    · simp [h1]
    · rw [decide_eq_true_eq]
      exact h2.symm
  · intro ⟨hs, hd⟩
    have hst : (tryEnactDetailed gs proposalId).state = result := of_decide_eq_true hd
    cases he : (tryEnactDetailed gs proposalId).enactment with
    | none => simp [he] at hs
    | some en => exact ⟨en, rfl, hst.symm⟩

namespace Vote

/-- V1 mirror: duplicate-free disjoint tallies. -/
def questionCleanB (q : Question) : Bool :=
  decide (q.assents.Nodup) && (decide (q.dissents.Nodup) &&
    q.assents.all (fun k => decide (k ∉ q.dissents)))

/-- V1 correspondence (only keys are compared). -/
theorem questionClean_corr (q : Question) :
    QuestionClean q ↔ questionCleanB q = true := by
  simp only [QuestionClean, questionCleanB, Bool.and_eq_true, List.all_eq_true,
    decide_eq_true_eq]

/-- The open-question cleanliness obligation, finitised through `assocLookup`
at the occurring keys: for a fixed key the lookup returns at most one
question, so the check is exact on arbitrary open-question lists, including
ones with duplicate keys. -/
private theorem openCleanIff (gs : VoteState) :
    (∀ qid q, assocLookup qid gs.openQuestions = some q → QuestionClean q) ↔
      ∀ qid ∈ gs.openQuestions.map Prod.fst,
        (match assocLookup qid gs.openQuestions with
        | some q => questionCleanB q
        | none => true) = true := by
  constructor
  · intro h qid hmem
    cases hm : assocLookup qid gs.openQuestions with
    | none => rfl
    | some q => exact (questionClean_corr q).mp (h qid q hm)
  · intro h qid q hq
    have hmem : qid ∈ gs.openQuestions.map Prod.fst := by
      obtain hm := assocLookup_some_mem_nodupfree qid q gs.openQuestions hq
      exact List.mem_map.mpr ⟨(qid, q), hm, rfl⟩
    have he := h qid hmem
    simp only [hq] at he
    exact (questionClean_corr q).mpr he

/-- V2 mirror: key uniqueness, open/closed disjointness, per-question
cleanliness through lookup, closed cleanliness, no open verdict in `closed`.
`view` is phantom here (as in `SweepReady` itself): the franchise enters only
at V3 through the threshold. -/
def sweepReadyB (_view : GroupView) (gs : VoteState) : Bool :=
  decide ((gs.openQuestions.map Prod.fst).Nodup) &&
  (decide ((gs.closed.map (·.questionId)).Nodup) &&
  ((gs.openQuestions.map Prod.fst).all
    (fun qid => decide (qid ∉ gs.closed.map (·.questionId))) &&
  ((gs.openQuestions.map Prod.fst).all (fun qid =>
    match assocLookup qid gs.openQuestions with
    | some q => questionCleanB q
    | none => true) &&
  (gs.closed.all (fun c => questionCleanB c.question) &&
  gs.closed.all (fun c => decide (c.verdict ≠ Verdict.open))))))

/-- V2 correspondence (no threshold, no `α`: only keys and verdicts). -/
theorem sweepReady_corr (view : GroupView) (gs : VoteState) :
    SweepReady view gs ↔ sweepReadyB view gs = true := by
  unfold sweepReadyB
  simp only [Bool.and_eq_true, List.all_eq_true, decide_eq_true_eq]
  constructor
  · intro h
    obtain ⟨oN, cN, dj, oC, cC, cO⟩ := h
    refine ⟨oN, cN, dj, (openCleanIff gs).mp oC, ?_, ?_⟩
    · intro c hc
      exact (questionClean_corr c.question).mp (cC c hc)
    · intro c hc
      exact cO c hc
  · intro h
    obtain ⟨oN, cN, dj, oC, cC, cO⟩ := h
    refine ⟨oN, cN, dj, (openCleanIff gs).mpr oC, ?_, ?_⟩
    · intro c hc
      exact (questionClean_corr c.question).mpr (cC c hc)
    · intro c hc
      exact cO c hc

/-- The no-stale-open obligation, finitised like `openCleanIff`: the threshold
`θ` is applied as a callable policy, never compared (R6). -/
private theorem opensOpenIff (θ : Threshold) (view : GroupView) (gs : VoteState) :
    (∀ qid q, assocLookup qid gs.openQuestions = some q →
      verdictOf θ view q = Verdict.open) ↔
      ∀ qid ∈ gs.openQuestions.map Prod.fst,
        (match assocLookup qid gs.openQuestions with
        | some q => decide (verdictOf θ view q = Verdict.open)
        | none => true) = true := by
  constructor
  · intro h qid hmem
    cases hm : assocLookup qid gs.openQuestions with
    | none => rfl
    | some q =>
        show decide (verdictOf θ view q = Verdict.open) = true
        rw [decide_eq_true_eq]
        exact h qid q hm
  · intro h qid q hq
    have hmem : qid ∈ gs.openQuestions.map Prod.fst := by
      obtain hm := assocLookup_some_mem_nodupfree qid q gs.openQuestions hq
      exact List.mem_map.mpr ⟨(qid, q), hm, rfl⟩
    have he := h qid hmem
    simp only [hq] at he
    exact of_decide_eq_true he

/-- V3 mirror: the sweep shape plus the per-question open verdict. -/
def voteWellFormedB (θ : Threshold) (view : GroupView) (gs : VoteState) : Bool :=
  sweepReadyB view gs &&
  (gs.openQuestions.map Prod.fst).all (fun qid =>
    match assocLookup qid gs.openQuestions with
    | some q => decide (verdictOf θ view q = Verdict.open)
    | none => true)

/-- V3 correspondence. -/
theorem voteWellFormed_corr (θ : Threshold) (view : GroupView) (gs : VoteState) :
    VoteWellFormed θ view gs ↔ voteWellFormedB θ view gs = true := by
  unfold voteWellFormedB
  simp only [Bool.and_eq_true, List.all_eq_true]
  constructor
  · intro h
    exact ⟨(sweepReady_corr view gs).mp h.toSweepReady,
      (opensOpenIff θ view gs).mp h.opensOpen⟩
  · intro ⟨hs, ho⟩
    exact ⟨(sweepReady_corr view gs).mpr hs, (opensOpenIff θ view gs).mpr ho⟩

end Vote

end KelGroups
