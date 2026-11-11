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

/-- K1 mirror: duplicate-free approvals, and the proposer absent from them
whenever more than one admin is counted. -/
def pendingWellFormedB (admins : Nat) (pending : PendingProposal) : Bool :=
  decide (pending.approvals.Nodup) &&
    (!decide (1 < admins) || decide (pending.proposer ∉ pending.approvals))

/-- K1 correspondence. -/
theorem pendingWellFormed_corr (admins : Nat) (pending : PendingProposal) :
    PendingWellFormed admins pending ↔ pendingWellFormedB admins pending = true := by
  unfold PendingWellFormed pendingWellFormedB
  by_cases h : 1 < admins <;> simp [h]

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

/-- K3 mirror: every pending proposal is well formed at the current admin
count. -/
def pendingCoherentB (gs : GroupState α) : Bool :=
  gs.pendingProposals.all fun e => pendingWellFormedB (adminCount gs) e.2

/-- K3 correspondence (no `α` equality needed). -/
theorem pendingCoherent_corr (gs : GroupState α) :
    PendingCoherent gs ↔ pendingCoherentB gs = true := by
  simp only [PendingCoherent, pendingCoherentB, List.all_eq_true]
  constructor
  · intro h e he
    obtain ⟨pid, p⟩ := e
    exact (pendingWellFormed_corr _ p).mp (h pid p he)
  · intro h pid p hp
    have he := h (pid, p) hp
    exact (pendingWellFormed_corr _ p).mpr he

/-- Integrated-store twin of K1. -/
def pendingBaseWellFormedB (admins : Nat) (pending : PendingBase) : Bool :=
  decide (pending.approvals.Nodup) &&
    (!decide (1 < admins) || decide (pending.proposer ∉ pending.approvals))

/-- Integrated K1 correspondence. -/
theorem pendingBaseWellFormed_corr (admins : Nat) (pending : PendingBase) :
    PendingBaseWellFormed admins pending ↔
      pendingBaseWellFormedB admins pending = true := by
  unfold PendingBaseWellFormed pendingBaseWellFormedB
  by_cases h : 1 < admins <;> simp [h]

/-- Integrated-store twin of K3. -/
def basePendingCoherentB (gs : GroupState α) : Bool :=
  gs.pendingBase.all fun e => pendingBaseWellFormedB (adminCount gs) e.2

/-- Integrated K3 correspondence. -/
theorem basePendingCoherent_corr (gs : GroupState α) :
    BasePendingCoherent gs ↔ basePendingCoherentB gs = true := by
  simp only [BasePendingCoherent, basePendingCoherentB, List.all_eq_true]
  constructor
  · intro h e he
    exact (pendingBaseWellFormed_corr _ e.2).mp (h e.1 e.2 he)
  · intro h pid p hp
    exact (pendingBaseWellFormed_corr _ p).mpr (h (pid, p) hp)

/-- K4 mirror: key uniqueness plus the three coherence checks. -/
def wellFormedB (gs : GroupState α) : Bool :=
  decide ((gs.members.map Prod.fst).Nodup) &&
  (decide ((gs.pendingProposals.map Prod.fst).Nodup) &&
  (membersCoherentB gs && (pendingCoherentB gs && basePendingCoherentB gs)))

/-- K4 correspondence (no `α` equality needed). -/
theorem wellFormed_corr (gs : GroupState α) :
    WellFormed gs ↔ wellFormedB gs = true := by
  unfold wellFormedB
  simp only [Bool.and_eq_true, decide_eq_true_eq]
  constructor
  · intro h
    obtain ⟨mK, pK, mC, pC, bC⟩ := h
    exact ⟨mK, pK, (membersCoherent_corr gs).mp mC, (pendingCoherent_corr gs).mp pC,
      (basePendingCoherent_corr gs).mp bC⟩
  · intro h
    obtain ⟨mK, pK, mC, pC, bC⟩ := h
    exact ⟨mK, pK, (membersCoherent_corr gs).mpr mC, (pendingCoherent_corr gs).mpr pC,
      (basePendingCoherent_corr gs).mpr bC⟩

/-- Count-free strong mirror: duplicate-free approvals without the proposer. -/
def pendingStrongB (pending : PendingProposal) : Bool :=
  decide (pending.approvals.Nodup) && decide (pending.proposer ∉ pending.approvals)

/-- Strong correspondence. -/
theorem pendingStrong_corr (pending : PendingProposal) :
    PendingStrong pending ↔ pendingStrongB pending = true := by
  simp only [PendingStrong, pendingStrongB, Bool.and_eq_true, decide_eq_true_eq]

/-- Every historical pending entry is strong. -/
def strongCoherentB (gs : GroupState α) : Bool :=
  gs.pendingProposals.all fun e => pendingStrongB e.2

/-- Strong-coherence correspondence. -/
theorem strongCoherent_corr (gs : GroupState α) :
    StrongCoherent gs ↔ strongCoherentB gs = true := by
  simp only [StrongCoherent, strongCoherentB, List.all_eq_true]
  constructor
  · intro h e he
    exact (pendingStrong_corr e.2).mp (h e.1 e.2 he)
  · intro h pid p hp
    exact (pendingStrong_corr p).mpr (h (pid, p) hp)

/-- Integrated-store twin of the strong mirror. -/
def pendingBaseStrongB (pending : PendingBase) : Bool :=
  decide (pending.approvals.Nodup) && decide (pending.proposer ∉ pending.approvals)

/-- Integrated strong correspondence. -/
theorem pendingBaseStrong_corr (pending : PendingBase) :
    PendingBaseStrong pending ↔ pendingBaseStrongB pending = true := by
  simp only [PendingBaseStrong, pendingBaseStrongB, Bool.and_eq_true, decide_eq_true_eq]

/-- Every integrated pending entry is strong. -/
def strongBaseCoherentB (gs : GroupState α) : Bool :=
  gs.pendingBase.all fun e => pendingBaseStrongB e.2

/-- Integrated strong-coherence correspondence. -/
theorem strongBaseCoherent_corr (gs : GroupState α) :
    StrongBaseCoherent gs ↔ strongBaseCoherentB gs = true := by
  simp only [StrongBaseCoherent, strongBaseCoherentB, List.all_eq_true]
  constructor
  · intro h e he
    exact (pendingBaseStrong_corr e.2).mp (h e.1 e.2 he)
  · intro h pid p hp
    exact (pendingBaseStrong_corr p).mpr (h (pid, p) hp)

/-- Raw-fold structural mirror: key uniqueness on members and both pending
stores, member-key coherence, duplicate-free approvals in both stores. -/
def rawStructuralB (gs : GroupState α) : Bool :=
  decide ((gs.members.map Prod.fst).Nodup) &&
  (decide ((gs.pendingProposals.map Prod.fst).Nodup) &&
  (membersCoherentB gs &&
  (gs.pendingProposals.all (fun e => decide (e.2.approvals.Nodup)) &&
  (decide ((gs.pendingBase.map Prod.fst).Nodup) &&
  gs.pendingBase.all (fun e => decide (e.2.approvals.Nodup))))))

/-- Raw-structural correspondence. -/
theorem rawStructural_corr (gs : GroupState α) :
    RawStructural gs ↔ rawStructuralB gs = true := by
  unfold RawStructural rawStructuralB
  simp only [Bool.and_eq_true, decide_eq_true_eq, List.all_eq_true]
  constructor
  · intro ⟨mK, pK, mC, pN, bK, bN⟩
    exact ⟨mK, pK, (membersCoherent_corr gs).mp mC, fun e he => pN e.1 e.2 he, bK,
      fun e he => bN e.1 e.2 he⟩
  · intro ⟨mK, pK, mC, pN, bK, bN⟩
    exact ⟨mK, pK, (membersCoherent_corr gs).mpr mC, fun pid p hp => pN (pid, p) hp, bK,
      fun pid p hp => bN (pid, p) hp⟩

/-- Admissibility mirror: every event of the trace passes the boundary
validator on the state the raw fold has reached. The validator's verdict is
read with `Except.isOk`, never compared; like `TraceAdmissible` it is a right
fold over a state continuation, so no recursive auxiliary is generated. -/
def traceAdmissibleB (digest : Proposal → ProposalId) (appFoldFn : AppFold α)
    (validKey : Key → Bool) (config : GroupConfig α) (gs : GroupState α)
    (trace : List (Key × GroupEvent α)) : Bool :=
  trace.foldr
    (fun step admissibleFrom current =>
      (validateEvent validKey config current step.1 step.2).isOk &&
        admissibleFrom (applyEvent digest appFoldFn current step.1 step.2))
    (fun _ => true) gs

/-- Admissibility correspondence, by induction on the trace. -/
theorem traceAdmissible_corr (digest : Proposal → ProposalId) (appFoldFn : AppFold α)
    (validKey : Key → Bool) (config : GroupConfig α) (gs : GroupState α)
    (trace : List (Key × GroupEvent α)) :
    TraceAdmissible digest appFoldFn validKey config gs trace ↔
      traceAdmissibleB digest appFoldFn validKey config gs trace = true := by
  induction trace generalizing gs with
  | nil => simp [TraceAdmissible, traceAdmissibleB]
  | cons step rest ih =>
      obtain ⟨signer, event⟩ := step
      show (validateEvent validKey config gs signer event = .ok () ∧
          TraceAdmissible digest appFoldFn validKey config
            (applyEvent digest appFoldFn gs signer event) rest) ↔
        ((validateEvent validKey config gs signer event).isOk &&
          traceAdmissibleB digest appFoldFn validKey config
            (applyEvent digest appFoldFn gs signer event) rest) = true
      cases validateEvent validKey config gs signer event with
      | error e => simp [Except.isOk, Except.toBool]
      | ok u =>
          cases u
          simp only [Except.isOk, Except.toBool, true_and, Bool.true_and]
          exact ih _

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
