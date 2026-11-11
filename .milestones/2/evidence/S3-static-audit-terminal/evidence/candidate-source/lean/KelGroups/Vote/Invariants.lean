import KelGroups.Vote.Fold

/-!
# Required vote machine — invariants over the production fold

Every theorem here is about the production fold `foldVote` (R-68): no
property is discharged against a hand-written record literal or a state a
test-only constructor built. Reachable means: the state `foldVote` produces
from some event list.

The well-formedness carrier `VoteWellFormed` is the conjunction the named
theorems stand on; it holds at the empty state and is preserved by every
`applyVoteEvent` step. The franchise invariant is a property of the whole
trace (a tally key may have lost standing after casting, V-3), so it is
stated with the cast-time prefix as its witness rather than as a final-state
predicate.

The no-expiry premise `PreservesQuestionSemantics` observes the target
question's ballots, the franchise, and the proposer's standing through the
production step itself; it replaces the constructor whitelist and covers the
preserving non-admin member admission (R57-07).

Axiom sets of the contractual theorems are printed at the bottom; the frozen
gate reads the printed lines.
-/

namespace KelGroups.Vote

/-! ## Proof-side vocabulary -/

/-- VC-1 (R-57): both tallies are duplicate-free and mutually disjoint. -/
def QuestionClean (q : Question) : Prop :=
  q.assents.Nodup ∧ q.dissents.Nodup ∧ ∀ k, k ∈ q.assents → k ∈ q.dissents → False

/-- Every key recorded in any tally of the state, open or closed questions
alike. The franchise theorem is stated over this. -/
def tallyKeysOfQuestion (q : Question) : List Key := q.assents ++ q.dissents

def tallyKeysOfState (gs : VoteState) : List Key :=
  (gs.openQuestions.map (fun entry => tallyKeysOfQuestion entry.2)).flatten ++
    (gs.closed.map (fun record => tallyKeysOfQuestion record.question)).flatten

/-- The state-side shape the sweep needs and every branch preserves: the
representation invariants of the carrier minus the no-stale-open property,
which is exactly what the sweep alone re-establishes. -/
structure SweepReady (view : GroupView) (gs : VoteState) : Prop where
  openNodup : (gs.openQuestions.map Prod.fst).Nodup
  closedNodup : (gs.closed.map (·.questionId)).Nodup
  openClosedDisjoint : ∀ qid, qid ∈ gs.openQuestions.map Prod.fst →
    qid ∉ gs.closed.map (·.questionId)
  openClean : ∀ qid q, assocLookup qid gs.openQuestions = some q → QuestionClean q
  closedClean : ∀ c, c ∈ gs.closed → QuestionClean c.question
  closedNotOpen : ∀ c, c ∈ gs.closed → c.verdict ≠ Verdict.open

/-- The well-formedness carrier: `SweepReady` plus VC-4 (R-52), no stale open
question under the current franchise and the threshold parameter. The
threshold is carried explicitly because VC-4 is about *that* threshold —
nothing hard-codes a policy (R-46). -/
structure VoteWellFormed (θ : Threshold) (view : GroupView) (gs : VoteState) extends SweepReady view gs : Prop where
  opensOpen : ∀ qid q, assocLookup qid gs.openQuestions = some q →
    verdictOf θ view q = Verdict.open


/-! Every declaration below observes membership through one canonical view.
There is no payload-local franchise for a statement to accidentally read. -/
variable (view : GroupView)
/-! ## Association-list and list lemmas

Slice 1 proved private versions of these in the frozen `KelGroups.Invariants`,
which the required machine may not import (R-41); they are re-proved here over
the transparent definitions in `KelGroups.Types`. -/

section AssocLemmas

variable {κ ν : Type} [BEq κ] [LawfulBEq κ]

private theorem assocErase_sublist' (key : κ) (entries : List (κ × ν)) :
    (assocErase key entries).Sublist entries := by
  induction entries with
  | nil => simp [assocErase]
  | cons entry rest ih =>
      obtain ⟨candidate, value⟩ := entry
      simp only [assocErase]
      split
      · exact List.Sublist.cons (candidate, value) (List.Sublist.refl rest)
      · exact List.Sublist.cons₂ (candidate, value) ih

private theorem assocErase_keys_nodup' (key : κ) (entries : List (κ × ν))
    (h : (entries.map Prod.fst).Nodup) :
    ((assocErase key entries).map Prod.fst).Nodup :=
  (assocErase_sublist' key entries).map Prod.fst |>.nodup h

private theorem assocErase_key_absent' (key : κ) (entries : List (κ × ν))
    (h : (entries.map Prod.fst).Nodup) :
    key ∉ (assocErase key entries).map Prod.fst := by
  induction entries with
  | nil => simp [assocErase]
  | cons entry rest ih =>
      obtain ⟨candidate, value⟩ := entry
      have hn := List.nodup_cons.mp h
      simp only [assocErase]
      split
      · next equal =>
          have hcand : candidate = key := beq_iff_eq.mp equal
          subst candidate
          exact hn.1
      · next different =>
          simp only [List.map_cons, List.mem_cons]
          intro present
          rcases present with equal | inTail
          · exact different (beq_iff_eq.mpr equal.symm)
          · exact ih hn.2 inTail

private theorem assocInsert_keys_nodup' (key : κ) (value : ν) (entries : List (κ × ν))
    (h : (entries.map Prod.fst).Nodup) :
    ((assocInsert key value entries).map Prod.fst).Nodup := by
  simp only [assocInsert, List.map_cons, List.nodup_cons]
  exact ⟨assocErase_key_absent' key entries h, assocErase_keys_nodup' key entries h⟩

private theorem assocAdjust_keys' (key : Key) (f : ν → ν) (entries : List (Key × ν)) :
    (assocAdjust key f entries).map Prod.fst = entries.map Prod.fst := by
  induction entries with
  | nil => rfl
  | cons entry rest ih =>
      obtain ⟨candidate, value⟩ := entry
      simp only [assocAdjust]
      split <;> simp [ih]

private theorem assocAdjust_keys_nodup' (key : Key) (f : ν → ν) (entries : List (Key × ν))
    (h : (entries.map Prod.fst).Nodup) :
    ((assocAdjust key f entries).map Prod.fst).Nodup := by
  rw [assocAdjust_keys' key f entries]
  exact h

private theorem assocLookup_some_mem' (key : κ) (value : ν) (entries : List (κ × ν))
    (h : assocLookup key entries = some value) : (key, value) ∈ entries := by
  induction entries with
  | nil => simp [assocLookup] at h
  | cons entry rest ih =>
      obtain ⟨candidate, current⟩ := entry
      simp only [assocLookup] at h
      split at h
      · next equal =>
          have hcand : candidate = key := beq_iff_eq.mp equal
          subst candidate
          simp only [Option.some.injEq] at h
          subst current
          exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (ih h)

private theorem mem_assocLookup_some' (key : κ) (value : ν) (entries : List (κ × ν))
    (hnd : (entries.map Prod.fst).Nodup) (h : (key, value) ∈ entries) :
    assocLookup key entries = some value := by
  induction entries with
  | nil => cases h
  | cons entry rest ih =>
      obtain ⟨candidate, current⟩ := entry
      have hn := List.nodup_cons.mp hnd
      simp only [List.mem_cons, Prod.mk.injEq] at h
      rw [assocLookup]
      split
      · next eq =>
          have hcand : candidate = key := beq_iff_eq.mp eq
          rw [← hcand] at h
          rcases h with ⟨_, h2⟩ | htail
          · cases h2
            rfl
          · exact absurd (List.mem_map.mpr ⟨(candidate, value), htail, rfl⟩) hn.1
      · next ne =>
          have hcand : ¬(key = candidate) := fun he => ne (by rw [he]; simp)
          rcases h with ⟨h1, _⟩ | htail
          · exact absurd h1 hcand
          · exact ih hn.2 htail

private theorem assocErase_other_lookup (k j : κ) (v : ν) (entries : List (κ × ν))
    (h : k ≠ j) : assocLookup k (assocErase j entries) = assocLookup k entries := by
  induction entries with
  | nil => rfl
  | cons entry rest ih =>
      obtain ⟨candidate, value⟩ := entry
      simp only [assocErase, assocLookup]
      by_cases hc : candidate == j
      · have hcj : candidate = j := beq_iff_eq.mp hc
        by_cases hck : candidate == k
        · have hckj : candidate = k := beq_iff_eq.mp hck
          exact absurd (hckj.symm.trans hcj) h
        · simp [assocErase, assocLookup, hc, hck, ih]
      · by_cases hck : candidate == k
        · simp [assocErase, assocLookup, hc, hck]
        · simp [assocErase, assocLookup, hc, hck, ih]

private theorem assocInsert_other_lookup (k j : κ) (v : ν) (entries : List (κ × ν))
    (h : k ≠ j) : assocLookup k (assocInsert j v entries) = assocLookup k entries := by
  show assocLookup k ((j, v) :: assocErase j entries) = assocLookup k entries
  simp only [assocLookup]
  split
  · next heq =>
      have hjk : j = k := beq_iff_eq.mp heq
      exact absurd hjk.symm h
  · next _ =>
      exact assocErase_other_lookup k j v entries h

private theorem assocInsert_mem_cases (k j : κ) (v w : ν) (entries : List (κ × ν))
    (hmem : (k, v) ∈ assocInsert j w entries) :
    (k = j ∧ v = w) ∨ (k, v) ∈ assocErase j entries := by
  simp only [assocInsert, List.mem_cons] at hmem
  rcases hmem with h | h
  · refine Or.inl ?_
    simp only [Prod.mk.injEq] at h
    exact h
  · exact Or.inr h

private theorem mem_map_fst_erase_of_ne (k j : κ) (entries : List (κ × ν))
    (hne : j ≠ k) :
    j ∈ (assocErase k entries).map Prod.fst ↔ j ∈ entries.map Prod.fst := by
  induction entries with
  | nil => simp [assocErase]
  | cons entry rest ih =>
      obtain ⟨candidate, value⟩ := entry
      simp only [assocErase]
      split
      · next equal =>
          have hcand : candidate = k := beq_iff_eq.mp equal
          subst candidate
          constructor
          · intro hj
            exact List.mem_cons.mpr (Or.inr hj)
          · intro hj
            rcases List.mem_cons.mp hj with heq | ht
            · exact absurd heq hne
            · exact ht
      · next _ =>
          simp [List.map_cons, List.mem_cons, ih]

private theorem mem_map_fst_insert (k j : κ) (value : ν) (entries : List (κ × ν)) :
    j ∈ (assocInsert k value entries).map Prod.fst ↔
      j = k ∨ j ∈ (assocErase k entries).map Prod.fst := by
  simp [assocInsert]

end AssocLemmas

/-! ## Key-list and tally-list lemmas -/

section KeyLemmas

private theorem setInsert_mem_cases (v k : Key) (l : List Key)
    (h : k ∈ setInsert v l) : k = v ∨ k ∈ l := by
  unfold setInsert at h
  split at h
  · exact Or.inr h
  · rcases List.mem_cons.mp h with heq | hin
    · exact Or.inl heq
    · exact Or.inr hin

private theorem nodup_append_mem {α : Type} [BEq α] [LawfulBEq α]
    (l₁ l₂ : List α) (h₁ : l₁.Nodup) (h₂ : l₂.Nodup)
    (hdisj : ∀ a ∈ l₁, a ∉ l₂) : (l₁ ++ l₂).Nodup := by
  induction l₁ with
  | nil => exact h₂
  | cons a rest ih =>
      refine List.nodup_cons.mpr ⟨?_, ih (List.nodup_cons.mp h₁).2
        (fun b hb => hdisj b (List.mem_cons_of_mem _ hb))⟩
      intro hin
      rcases List.mem_append.mp hin with hrest | hl₂
      · exact (List.nodup_cons.mp h₁).1 hrest
      · exact hdisj a List.mem_cons_self hl₂

private theorem setInsert_nodup' (value : Key) (values : List Key)
    (h : values.Nodup) : (setInsert value values).Nodup := by
  simp only [setInsert]
  split
  · exact h
  · next absent =>
      exact List.nodup_cons.mpr ⟨by simpa using absent, h⟩

private theorem mem_erase_inv (a : Key) : ∀ (l : List Key), l.Nodup →
    ∀ b, b ∈ l.erase a → b ∈ l ∧ b ≠ a := by
  intro l
  induction l with
  | nil => intro _ b hb; cases hb
  | cons c rest ih =>
      intro hnd b h
      have hn := List.nodup_cons.mp hnd
      by_cases hca : c = a
      · subst hca
        simp only [List.erase_cons_head] at h
        refine ⟨List.mem_cons_of_mem c h, ?_⟩
        intro hba
        have hbc : b = c := hba
        have hcm : c ∈ rest := hbc ▸ h
        exact hn.1 hcm
      · have hnb : ¬(c == a) := fun heq => hca (beq_iff_eq.mp heq)
        rw [List.erase_cons_tail hnb] at h
        simp only [List.mem_cons] at h
        rcases h with heq | h'
        · exact ⟨List.mem_cons.mpr (Or.inl heq), fun hba => hca (heq ▸ hba)⟩
        · refine ⟨List.mem_cons.mpr (Or.inr (List.erase_subset h')), ?_⟩
          intro hba
          have h'a : a ∈ rest.erase a := hba ▸ h'
          obtain ⟨_, h2⟩ := ih hn.2 a h'a
          exact h2 rfl

private theorem nodup_erase (a : Key) (l : List Key) (h : l.Nodup) : (l.erase a).Nodup := by
  induction l with
  | nil => simp
  | cons b rest ih =>
      have hn := List.nodup_cons.mp h
      by_cases hba : b = a
      · subst hba
        simp only [List.erase_cons_head]
        exact hn.2
      · have hnb : ¬(b == a) := fun heq => hba (beq_iff_eq.mp heq)
        rw [List.erase_cons_tail hnb]
        exact List.nodup_cons.mpr
          ⟨fun hmem => hn.1 (List.erase_subset hmem), ih hn.2⟩

private theorem closed_guard_absent (qid : QuestionId) (records : List ClosureRecord)
    (h : records.any (fun record : ClosureRecord => record.questionId == qid) = false) :
    qid ∉ records.map (·.questionId) := by
  intro hmem
  rw [List.mem_map] at hmem
  obtain ⟨r, hr, hkey⟩ := hmem
  rw [List.any_eq_true.mpr ⟨r, hr, by simp [hkey]⟩] at h
  exact absurd h (by simp)

private theorem assoc_entries_key_unique (entries : List (Key × Question))
    (h : (entries.map Prod.fst).Nodup) :
    ∀ (qe entry : Key × Question), qe ∈ entries → entry ∈ entries → entry.1 = qe.1 →
      entry.2 = qe.2 := by
  induction entries with
  | nil => intro qe entry hqe _; cases hqe
  | cons e rest ih =>
    intro qe entry hqe hentry hkey
    have hn := List.nodup_cons.mp h
    simp only [List.mem_cons] at hqe hentry
    rcases hqe with hqe' | hqet <;> rcases hentry with hentry' | hentryt
    · cases hqe'
      cases hentry'
      rfl
    · cases hqe'
      exact absurd (List.mem_map.mpr ⟨entry, hentryt, hkey⟩) hn.1
    · cases hentry'
      exact absurd (List.mem_map.mpr ⟨qe, hqet, hkey.symm⟩) hn.1
    · exact ih hn.2 qe entry hqet hentryt hkey

private theorem filterMap_keys_nodup (f : QuestionId × Question → Option ClosureRecord)
    (hid : ∀ entry c, f entry = some c → c.questionId = entry.1)
    (entries : List (QuestionId × Question))
    (h : (entries.map Prod.fst).Nodup) :
    ((entries.filterMap f).map (·.questionId)).Nodup := by
  induction entries with
  | nil => simp
  | cons entry rest ih =>
      have hn := List.nodup_cons.mp h
      simp only [List.filterMap_cons]
      cases hf : f entry with
      | none => simp [ih hn.2]
      | some c =>
          simp only [List.map_cons, List.nodup_cons]
          refine ⟨?_, ih hn.2⟩
          intro hin
          rw [List.mem_map] at hin
          obtain ⟨c', hc', heq⟩ := hin
          obtain ⟨entry', hentry', hsome⟩ := List.mem_filterMap.mp hc'
          have h1 : c.questionId = entry.1 := hid entry c hf
          have h2 : c'.questionId = entry'.1 := hid entry' c' hsome
          exact hn.1 (List.mem_map.mpr ⟨entry', hentry', by rw [← h2, heq, h1]⟩)

end KeyLemmas


/-! ## The carrier at the empty state -/

theorem emptyVoteState_sweepReady : SweepReady view emptyVoteState := by
  refine ⟨by simp [emptyVoteState], by simp [emptyVoteState],
    by simp [emptyVoteState], ?_, ?_, by simp [emptyVoteState]⟩
  · intro qid q h
    simp [emptyVoteState, assocLookup] at h
  · intro c h
    simp [emptyVoteState] at h

theorem emptyVoteState_wellFormed (θ : Threshold) : VoteWellFormed θ view emptyVoteState := by
  refine ⟨emptyVoteState_sweepReady view, ?_⟩
  intro qid q h
  simp [emptyVoteState, assocLookup] at h

/-! ## INV-54-POLICYFREE (R-46) -/

theorem verdictOf_threshold_congr (θ θ' : Threshold) (gs : VoteState) (q : Question)
    (h : θ (franchiseSize view) = θ' (franchiseSize view)) :
    verdictOf θ view q = verdictOf θ' view q := by
  cases q with
  | mk kind proposer assents dissents =>
    cases kind with
    | collective =>
        simp only [franchiseSize, franchise, List.length_map] at h
        simp [verdictOf, franchiseSize, franchise, h]
    | permission _ => rfl

/-! ## The sweep -/
private theorem sweepStep_key (θ : Threshold) (gs : VoteState) (entry : QuestionId × Question)
    (c : ClosureRecord) (h : sweepStep θ view entry = some c) : c.questionId = entry.1 ∧
      c.verdict = verdictOf θ view entry.2 ∧ c.verdict ≠ Verdict.open ∧
        c.question = entry.2 := by
  rw [sweepStep] at h
  cases hv : verdictOf θ view entry.2
  · rw [hv] at h
    simp only [Option.some.injEq] at h
    rw [← h]
    simp
  · rw [hv] at h
    simp only [Option.some.injEq] at h
    rw [← h]
    simp
  · rw [hv] at h
    simp at h

private theorem sweepClosures_open_mem (θ : Threshold) (gs : VoteState)
    (entry : QuestionId × Question) :
    entry ∈ (sweepClosures θ view gs).openQuestions ↔
      entry ∈ gs.openQuestions ∧ verdictOf θ view entry.2 = Verdict.open := by
  simp [sweepClosures, List.mem_filter]

private theorem sweepClosures_closed_mem (θ : Threshold) (gs : VoteState) (c : ClosureRecord) :
    c ∈ (sweepClosures θ view gs).closed ↔
      c ∈ gs.closed ∨
        ∃ entry : QuestionId × Question, entry ∈ gs.openQuestions ∧
          sweepStep θ view entry = some c := by
  constructor
  · intro hc
    simp only [sweepClosures, List.mem_append, List.mem_filterMap] at hc
    rcases hc with hold | ⟨entry, hentry, hsome⟩
    · exact Or.inl hold
    · exact Or.inr ⟨entry, hentry, hsome⟩
  · intro h
    rcases h with hold | ⟨entry, hentry, hsome⟩
    · exact List.mem_append.mpr (Or.inl hold)
    · exact List.mem_append.mpr (Or.inr (List.mem_filterMap.mpr ⟨entry, hentry, hsome⟩))

private theorem sweepClosures_sweepReady (θ : Threshold) (gs : VoteState)
    (h : SweepReady view gs) : SweepReady view (sweepClosures θ view gs) := by
  have hopenmem : ∀ entry : QuestionId × Question,
      entry ∈ (sweepClosures θ view gs).openQuestions →
      entry ∈ gs.openQuestions ∧ verdictOf θ view entry.2 = Verdict.open :=
    fun entry he => (sweepClosures_open_mem view θ gs entry).mp he
  have hclosedmem : ∀ c : ClosureRecord,
      c ∈ (sweepClosures θ view gs).closed →
      c ∈ gs.closed ∨ ∃ entry : QuestionId × Question, entry ∈ gs.openQuestions ∧
        c.questionId = entry.1 ∧ c.question = entry.2 ∧
        c.verdict = verdictOf θ view entry.2 ∧ c.verdict ≠ Verdict.open := by
    intro c hc
    rcases (sweepClosures_closed_mem view θ gs c).mp hc with hold | ⟨entry, hentry, hsome⟩
    · exact Or.inl hold
    · obtain ⟨hid, hv, hvne, heq⟩ := sweepStep_key view θ gs entry c hsome
      exact Or.inr ⟨entry, hentry, hid, heq, hv, hvne⟩
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_⟩
  · have hsub : ((sweepClosures θ view gs).openQuestions.map Prod.fst).Sublist
        (gs.openQuestions.map Prod.fst) := by
      refine List.Sublist.map Prod.fst ?_
      have hsf : (gs.openQuestions.filter
          (fun entry : QuestionId × Question => verdictOf θ view entry.2 = .open)).Sublist
          gs.openQuestions := List.filter_sublist
      simpa [sweepClosures] using hsf
    exact hsub.nodup h.openNodup
  · have hres : ((gs.openQuestions.filterMap (sweepStep θ view)).map (·.questionId)).Nodup :=
      filterMap_keys_nodup (sweepStep θ view)
        (fun entry c hc => (sweepStep_key view θ gs entry c hc).1) gs.openQuestions h.openNodup
    have hdisj : ∀ qid ∈ gs.closed.map (·.questionId),
        qid ∉ (gs.openQuestions.filterMap (sweepStep θ view)).map (·.questionId) := by
      intro qid qin hin
      rw [List.mem_map] at hin
      obtain ⟨c, hc, heq⟩ := hin
      rw [List.mem_filterMap] at hc
      obtain ⟨entry, hentry, hsome⟩ := hc
      have hkey := (sweepStep_key view θ gs entry c hsome).1
      exact h.openClosedDisjoint qid
        (List.mem_map.mpr ⟨entry, hentry, hkey.symm.trans heq⟩) qin
    have happend : ((sweepClosures θ view gs).closed).map (·.questionId) =
        gs.closed.map (·.questionId) ++
          (gs.openQuestions.filterMap (sweepStep θ view)).map (·.questionId) := by
      simp [sweepClosures, List.map_append]
    rw [happend]
    exact nodup_append_mem (gs.closed.map (·.questionId))
      ((gs.openQuestions.filterMap (sweepStep θ view)).map (·.questionId))
      h.closedNodup hres hdisj
  · intro qid hopen hc
    rw [List.mem_map] at hopen hc
    obtain ⟨qe, hqe, hqid⟩ := hopen
    obtain ⟨ce, hce, hcid⟩ := hc
    obtain ⟨hentry, hv⟩ := hopenmem qe hqe
    rcases hclosedmem ce hce with hold | ⟨entry, hentry', hid, heq, hv', hne⟩
    · exact h.openClosedDisjoint qid
        (List.mem_map.mpr ⟨qe, hentry, hqid⟩)
        (List.mem_map.mpr ⟨ce, hold, hcid⟩)
    · have hqee : entry.1 = qe.1 := hid.symm.trans (hcid.trans hqid.symm)
      have hsame : entry.2 = qe.2 :=
        assoc_entries_key_unique gs.openQuestions h.openNodup qe entry hentry hentry' hqee
      rw [← hsame] at hv
      rw [hv] at hv'
      exact hne hv'
  · intro qid q hlookup
    obtain ⟨hentry, -⟩ := hopenmem (qid, q)
      (assocLookup_some_mem' qid q (sweepClosures θ view gs).openQuestions hlookup)
    exact h.openClean qid q (mem_assocLookup_some' qid q gs.openQuestions h.openNodup hentry)
  · intro c hc
    rcases hclosedmem c hc with hold | ⟨entry, hentry, hid, heq, hv, hne⟩
    · exact h.closedClean c hold
    · rw [heq]
      exact h.openClean entry.1 entry.2
        (mem_assocLookup_some' entry.1 entry.2 gs.openQuestions h.openNodup hentry)
  · intro c hc
    rcases hclosedmem c hc with hold | ⟨entry, hentry, hid, heq, hv, hne⟩
    · exact h.closedNotOpen c hold
    · rw [hv] at hne ⊢
      exact hne

private theorem sweepClosures_wellFormed (θ : Threshold) (gs : VoteState)
    (h : SweepReady view gs) : VoteWellFormed θ view (sweepClosures θ view gs) :=
  ⟨sweepClosures_sweepReady view θ gs h, by
    intro qid q hlookup
    obtain ⟨hentry, hv⟩ := (sweepClosures_open_mem view θ gs (qid, q)).mp
      (assocLookup_some_mem' qid q (sweepClosures θ view gs).openQuestions hlookup)
    exact hv⟩

/-! ## Per-branch effects -/

private theorem placeBallot_clean (voter : Key) (ballot : Ballot) (question : Question)
    (h : QuestionClean question) : QuestionClean (placeBallot voter ballot question) := by
  obtain ⟨hass, hdis, hdisj⟩ := h
  cases ballot with
  | assent =>
      refine ⟨setInsert_nodup' voter question.assents hass,
        nodup_erase voter question.dissents hdis, ?_⟩
      intro k hk1 hk2
      obtain ⟨h2in, h2ne⟩ := mem_erase_inv voter question.dissents hdis k hk2
      rcases setInsert_mem_cases voter k question.assents hk1 with heq | hin
      · subst heq
        exact absurd rfl h2ne
      · exact hdisj k hin h2in
  | dissent =>
      refine ⟨nodup_erase voter question.assents hass,
        setInsert_nodup' voter question.dissents hdis, ?_⟩
      intro k hk1 hk2
      obtain ⟨h1in, h1ne⟩ := mem_erase_inv voter question.assents hass k hk1
      rcases setInsert_mem_cases voter k question.dissents hk2 with heq | hin
      · subst heq
        exact absurd rfl h1ne
      · exact hdisj k h1in hin

private theorem placeBallot_tally (voter : Key) (ballot : Ballot) (question : Question)
    (k : Key) (h : k ∈ tallyKeysOfQuestion (placeBallot voter ballot question)) :
    k = voter ∨ k ∈ tallyKeysOfQuestion question := by
  cases ballot with
  | assent =>
      simp only [tallyKeysOfQuestion, placeBallot, List.mem_append] at h
      rcases h with h | h
      · rcases setInsert_mem_cases voter k question.assents h with heq | hin
        · exact Or.inl heq
        · exact Or.inr (List.mem_append.mpr (Or.inl hin))
      · exact Or.inr (List.mem_append.mpr (Or.inr (List.erase_subset h)))
  | dissent =>
      simp only [tallyKeysOfQuestion, placeBallot, List.mem_append] at h
      rcases h with h | h
      · exact Or.inr (List.mem_append.mpr (Or.inl (List.erase_subset h)))
      · rcases setInsert_mem_cases voter k question.dissents h with heq | hin
        · exact Or.inl heq
        · exact Or.inr (List.mem_append.mpr (Or.inr hin))

/-! ## Step preservation -/

private theorem effectedState_sweepReady (gs : VoteState) (signer : Key) (event : VoteEvent)
    (h : SweepReady view gs) : SweepReady view (effectedState gs signer event) := by
  cases event with
  | openQuestion questionId kind =>
      have hfresh : QuestionClean (Question.mk kind signer [] []) :=
        ⟨List.nodup_nil, List.nodup_nil, fun _ hk => absurd hk List.not_mem_nil⟩
      by_cases hguard : (lookupQuestion questionId gs).isNone
          && !(gs.closed.any (fun record : ClosureRecord => record.questionId == questionId))
      · have heff : effectedState gs signer (.openQuestion questionId kind) =
            { gs with openQuestions := assocInsert questionId (Question.mk kind signer [] []) gs.openQuestions } := by
          simp [effectedState, hguard]
        rw [heff]
        obtain ⟨hnone, hnotany⟩ := by
          have hguard' : (lookupQuestion questionId gs).isNone = true ∧
              !(gs.closed.any (fun record : ClosureRecord => record.questionId == questionId)) := by
            simpa using hguard
          exact hguard'
        refine ⟨assocInsert_keys_nodup' questionId _ gs.openQuestions
          h.openNodup, h.closedNodup, ?_, ?_, h.closedClean, h.closedNotOpen⟩
        · intro qid hmem
          have hcons : { gs with openQuestions := assocInsert questionId (Question.mk kind signer [] []) gs.openQuestions }.openQuestions = (questionId, Question.mk kind signer [] []) :: assocErase questionId gs.openQuestions := rfl
          rw [List.mem_map, hcons] at hmem
          obtain ⟨e, he, hid⟩ := hmem
          rw [List.mem_cons] at he
          rcases he with heq | he
          · cases heq
            rw [← hid]
            exact closed_guard_absent questionId gs.closed
              (by simpa using hnotany)
          · exact h.openClosedDisjoint qid
              ((assocErase_sublist' questionId gs.openQuestions).map Prod.fst |>.mem
                (List.mem_map.mpr ⟨e, he, hid⟩))
        · intro qid q hlookup
          have hmem := assocLookup_some_mem' qid q
            (assocInsert questionId (Question.mk kind signer [] []) gs.openQuestions) hlookup
          rcases assocInsert_mem_cases qid questionId q (Question.mk kind signer [] [])
            gs.openQuestions hmem with heq | herased
          · obtain ⟨-, heq2⟩ := heq
            cases heq2
            exact hfresh
          · exact h.openClean qid q
              (mem_assocLookup_some' qid q gs.openQuestions h.openNodup
                ((assocErase_sublist' questionId gs.openQuestions).mem herased))
      · have heff : effectedState gs signer (.openQuestion questionId kind) = gs := by
          simp [effectedState, hguard]
        rw [heff]
        exact h
  | cast questionId ballot =>
      cases hlook : lookupQuestion questionId gs with
        | none =>
            have heff : effectedState gs signer (.cast questionId ballot) = gs := by
              simp [effectedState, hlook]
            rw [heff]
            exact h
        | some question =>
            have heff : effectedState gs signer (.cast questionId ballot) =
                { gs with openQuestions := assocInsert questionId (placeBallot signer ballot question) gs.openQuestions } := by
              simp [effectedState, hlook]
            rw [heff]
            have hqmem : (questionId, question) ∈ gs.openQuestions :=
              assocLookup_some_mem' questionId question gs.openQuestions hlook
            refine ⟨assocInsert_keys_nodup' questionId _ gs.openQuestions
              h.openNodup, h.closedNodup, ?_, ?_, h.closedClean, h.closedNotOpen⟩
            · intro qid hmem
              have hcons : { gs with openQuestions := assocInsert questionId (placeBallot signer ballot question) gs.openQuestions }.openQuestions = (questionId, placeBallot signer ballot question) :: assocErase questionId gs.openQuestions := rfl
              rw [List.mem_map, hcons] at hmem
              obtain ⟨e, he, hid⟩ := hmem
              rw [List.mem_cons] at he
              rcases he with heq | he
              · cases heq
                rw [← hid]
                exact h.openClosedDisjoint questionId
                  (List.mem_map.mpr ⟨(questionId, question), hqmem, rfl⟩)
              · exact h.openClosedDisjoint qid
                  ((assocErase_sublist' questionId gs.openQuestions).map Prod.fst |>.mem
                    (List.mem_map.mpr ⟨e, he, hid⟩))
            · intro qid q hlookup
              have hmem := assocLookup_some_mem' qid q
                (assocInsert questionId (placeBallot signer ballot question)
                  gs.openQuestions) hlookup
              rcases assocInsert_mem_cases qid questionId q
                (placeBallot signer ballot question) gs.openQuestions hmem with heq | herased
              · obtain ⟨-, heq2⟩ := heq
                cases heq2
                exact placeBallot_clean signer ballot question
                  (h.openClean questionId question hlook)
              · exact h.openClean qid q
                  (mem_assocLookup_some' qid q gs.openQuestions h.openNodup
                    ((assocErase_sublist' questionId gs.openQuestions).mem herased))
  | renounce questionId =>
      have heff : effectedState gs signer (.renounce questionId) = gs := rfl
      rw [heff]
      exact h

theorem applyVoteEvent_preserves_wellFormed (θ : Threshold) (gs : VoteState)
    (signer : Key) (event : VoteEvent) (h : VoteWellFormed θ view gs) :
    VoteWellFormed θ view (applyVoteEvent θ view gs signer event) := by
  simp only [applyVoteEvent]
  cases hval : validateVoteEvent θ view gs signer event with
  | error _ => exact h
  | ok u =>
      cases u
      exact sweepClosures_wellFormed view θ (effectedState gs signer event)
        (effectedState_sweepReady view gs signer event h.toSweepReady)

private theorem foldFrom_preserves_wellFormed (θ : Threshold) :
    ∀ (events : List (Key × VoteEvent)) (initial : VoteState),
      VoteWellFormed θ view initial → VoteWellFormed θ view (foldFrom θ view initial events) := by
  intro events
  induction events with
  | nil => intro initial h; exact h
  | cons signed rest ih =>
      intro initial h
      exact ih (applyVoteEvent θ view initial signed.1 signed.2)
        (applyVoteEvent_preserves_wellFormed view θ initial signed.1 signed.2 h)

/-- The contractual carrier theorem: every reachable state is well formed. -/
theorem foldVote_wellFormed (θ : Threshold) (events : List (Key × VoteEvent)) :
    VoteWellFormed θ view (foldVote θ view events) :=
  foldFrom_preserves_wellFormed view θ events emptyVoteState (emptyVoteState_wellFormed view θ)

private theorem foldVote_append (θ : Threshold) (pre suffix : List (Key × VoteEvent)) :
    foldVote θ view (pre ++ suffix) = foldFrom θ view (foldVote θ view pre) suffix := by
  simp [foldVote, foldFrom, List.foldl_append]

private theorem sweepClosures_preserves_qid (θ : Threshold) (gs : VoteState)
    (qid : QuestionId)
    (h : qid ∈ gs.openQuestions.map Prod.fst ∨
      qid ∈ gs.closed.map (·.questionId)) :
    qid ∈ (sweepClosures θ view gs).openQuestions.map Prod.fst ∨
      qid ∈ (sweepClosures θ view gs).closed.map (·.questionId) := by
  rcases h with hopen | hclosed
  · rw [List.mem_map] at hopen
    obtain ⟨entry, hentry, hid⟩ := hopen
    by_cases hv : verdictOf θ view entry.2 = Verdict.open
    · have hkept := (sweepClosures_open_mem view θ gs entry).mpr ⟨hentry, hv⟩
      exact Or.inl (List.mem_map.mpr ⟨entry, hkept, hid⟩)
    · have hrecord : sweepStep θ view entry = some
          { questionId := entry.1, question := entry.2, verdict := verdictOf θ view entry.2, cause := closureCause view entry.2 (verdictOf θ view entry.2) } := by
        cases hvv : verdictOf θ view entry.2
        · simp [sweepStep, hvv]
        · simp [sweepStep, hvv]
        · exact absurd hvv hv
      have hc :=
        (sweepClosures_closed_mem view θ gs _).mpr (Or.inr ⟨entry, hentry, hrecord⟩)
      exact Or.inr (List.mem_map.mpr ⟨_, hc, by simpa using hid⟩)
  · rw [List.mem_map] at hclosed
    obtain ⟨c, hc, hid⟩ := hclosed
    have hc' := (sweepClosures_closed_mem view θ gs c).mpr (Or.inl hc)
    exact Or.inr (List.mem_map.mpr ⟨c, hc', hid⟩)

private theorem effectedState_preserves_qid (gs : VoteState) (signer : Key)
    (event : VoteEvent) (qid : QuestionId)
    (h : qid ∈ gs.openQuestions.map Prod.fst ∨
      qid ∈ gs.closed.map (·.questionId)) :
    qid ∈ (effectedState gs signer event).openQuestions.map Prod.fst ∨
      qid ∈ (effectedState gs signer event).closed.map (·.questionId) := by
  cases event with
  | openQuestion questionId kind =>
      by_cases hguard : (lookupQuestion questionId gs).isNone
          && !(gs.closed.any (fun record : ClosureRecord => record.questionId == questionId))
      · have heff : effectedState gs signer (.openQuestion questionId kind) =
            { gs with openQuestions := assocInsert questionId (Question.mk kind signer [] []) gs.openQuestions } := by
          simp [effectedState, hguard]
        rw [heff]
        rcases h with hopen | hclosed
        · refine Or.inl ?_
          rw [mem_map_fst_insert]
          by_cases heq : qid = questionId
          · exact Or.inl heq
          · exact Or.inr ((mem_map_fst_erase_of_ne questionId qid gs.openQuestions heq).mpr hopen)
        · exact Or.inr hclosed
      · have heff : effectedState gs signer (.openQuestion questionId kind) = gs := by
          simp [effectedState, hguard]
        rw [heff]
        exact h
  | cast questionId ballot =>
      cases hlook : lookupQuestion questionId gs with
        | none =>
            have heff : effectedState gs signer (.cast questionId ballot) = gs := by
              simp [effectedState, hlook]
            rw [heff]
            exact h
        | some question =>
            have heff : effectedState gs signer (.cast questionId ballot) =
                { gs with openQuestions := assocInsert questionId (placeBallot signer ballot question) gs.openQuestions } := by
              simp [effectedState, hlook]
            rw [heff]
            rcases h with hopen | hclosed
            · refine Or.inl ?_
              rw [mem_map_fst_insert]
              by_cases heq : qid = questionId
              · exact Or.inl heq
              · exact Or.inr
                  ((mem_map_fst_erase_of_ne questionId qid gs.openQuestions heq).mpr hopen)
            · exact Or.inr hclosed
  | renounce questionId =>
      have heff : effectedState gs signer (.renounce questionId) = gs := rfl
      rw [heff]
      exact h

private theorem applyVoteEvent_preserves_qid (θ : Threshold) (gs : VoteState)
    (signer : Key) (event : VoteEvent) (qid : QuestionId)
    (h : qid ∈ gs.openQuestions.map Prod.fst ∨
      qid ∈ gs.closed.map (·.questionId)) :
    qid ∈ (applyVoteEvent θ view gs signer event).openQuestions.map Prod.fst ∨
      qid ∈ (applyVoteEvent θ view gs signer event).closed.map (·.questionId) := by
  simp only [applyVoteEvent]
  cases validateVoteEvent θ view gs signer event with
  | error _ => exact h
  | ok u =>
      cases u
      exact sweepClosures_preserves_qid view θ (effectedState gs signer event) qid
        (effectedState_preserves_qid gs signer event qid h)

private theorem foldFrom_preserves_qid (θ : Threshold) :
    ∀ (events : List (Key × VoteEvent)) (initial : VoteState) (qid : QuestionId),
      (qid ∈ initial.openQuestions.map Prod.fst ∨
        qid ∈ initial.closed.map (·.questionId)) →
      qid ∈ (foldFrom θ view initial events).openQuestions.map Prod.fst ∨
        qid ∈ (foldFrom θ view initial events).closed.map (·.questionId) := by
  intro events
  induction events with
  | nil => intro initial qid h; exact h
  | cons signed rest ih =>
      intro initial qid h
      exact ih (applyVoteEvent θ view initial signed.1 signed.2) qid
        (applyVoteEvent_preserves_qid view θ initial signed.1 signed.2 qid h)

/-! ## INV-54-DISJOINT (R-57, VC-1) -/

theorem ballots_nodup_disjoint (θ : Threshold) (events : List (Key × VoteEvent)) :
    (∀ qid q, assocLookup qid (foldVote θ view events).openQuestions = some q →
      QuestionClean q) ∧
    (∀ c, c ∈ (foldVote θ view events).closed → QuestionClean c.question) :=
  ⟨(foldVote_wellFormed view θ events).openClean, (foldVote_wellFormed view θ events).closedClean⟩

/-! ## INV-54-NOSTALE (R-52, VC-4) -/

theorem open_questions_are_open (θ : Threshold) (events : List (Key × VoteEvent))
    (questionId : QuestionId) (q : Question)
    (h : assocLookup questionId (foldVote θ view events).openQuestions = some q) :
    verdictOf θ view q = Verdict.open :=
  (foldVote_wellFormed view θ events).opensOpen questionId q h

/-! ## INV-54-PARTITION (R-61, VC-3) -/

theorem questions_partition (θ : Threshold) (events : List (Key × VoteEvent)) :
    ((foldVote θ view events).openQuestions.map Prod.fst).Nodup ∧
    ((foldVote θ view events).closed.map (·.questionId)).Nodup ∧
    (∀ qid, qid ∈ (foldVote θ view events).openQuestions.map Prod.fst →
      qid ∉ (foldVote θ view events).closed.map (·.questionId)) ∧
    (∀ c, c ∈ (foldVote θ view events).closed → c.verdict ≠ Verdict.open) ∧
    (∀ pre suffix qid,
      events = pre ++ suffix →
      (qid ∈ (foldVote θ view pre).openQuestions.map Prod.fst ∨
        qid ∈ (foldVote θ view pre).closed.map (·.questionId)) →
      qid ∈ (foldVote θ view events).openQuestions.map Prod.fst ∨
        qid ∈ (foldVote θ view events).closed.map (·.questionId)) := by
  have hf := foldVote_wellFormed view θ events
  refine ⟨hf.openNodup, hf.closedNodup, hf.openClosedDisjoint, hf.closedNotOpen, ?_⟩
  intro pre suffix qid hev hin
  subst hev
  rw [foldVote_append view]
  exact foldFrom_preserves_qid view θ suffix (foldVote θ view pre) qid hin

/-! ## INV-54-NOEXPIRY (R-54) — semantic preservation premise (R57-07) -/
/-- The observable content of "this signed event preserves the target
question", read off the production step itself: the target question keeps its
exact value. This is a semantic observation over arbitrary events, not a
constructor whitelist — a ballot on another question satisfies it, a ballot on
this one does not.

The franchise and proposer-standing conjuncts this premise carried while the
vote payload owned membership are **gone rather than left standing as
tautologies**: with one writable member store, a *vote* event cannot move the
franchise or a proposer's standing at all, so `franchise view = franchise view`
would be a conjunct that cannot fail. The observation those two conjuncts made
is real, but it now belongs where the franchise can actually change — a base
membership/role transition — and is R62-11's recomputation obligation (T6223,
S62-B). Keeping them here would manufacture confidence in a check that has
nothing left to test. -/
def preservesQuestionDecide (threshold : Threshold) (view : GroupView)
    (gs : VoteState) (signer : Key) (event : VoteEvent)
    (questionId : QuestionId) : Bool :=
  match lookupQuestion questionId gs with
  | none => true
  | some q =>
      decide (lookupQuestion questionId
        (applyVoteEvent threshold view gs signer event) = some q)
/-- R57-07: the no-expiry preservation premise. It holds exactly when the
event preserves the target's ballots. The franchise and proposer-standing
halves moved to the base transition with R62-11; see `preservesQuestionDecide`.
Its truth depends on a semantic observation, never on the event constructor. -/
def PreservesQuestionSemantics (threshold : Threshold) (view : GroupView) (gs : VoteState)
    (signer : Key) (event : VoteEvent) (questionId : QuestionId) : Prop :=
  preservesQuestionDecide threshold view gs signer event questionId = true

instance preservesQuestionSemanticsDecidable (threshold : Threshold) (view : GroupView)
    (gs : VoteState) (signer : Key) (event : VoteEvent)
    (questionId : QuestionId) :
    Decidable (PreservesQuestionSemantics threshold view gs signer event
      questionId) :=
  instDecidableEqBool (preservesQuestionDecide threshold view gs signer event
    questionId) true

theorem no_expiry (θ : Threshold) (events : List (Key × VoteEvent))
    (pre : List (Key × VoteEvent)) (signer : Key) (event : VoteEvent)
    (suffix : List (Key × VoteEvent)) (questionId : QuestionId) (q : Question)
    (hevents : events = pre ++ (signer, event) :: suffix)
    (hopen : assocLookup questionId (foldVote θ view pre).openQuestions = some q)
    (hpres : PreservesQuestionSemantics θ view (foldVote θ view pre) signer event
      questionId) :
    assocLookup questionId
        (applyVoteEvent θ view (foldVote θ view pre) signer event).openQuestions = some q ∧
      verdictOf θ view q = Verdict.open := by
  subst hevents
  have hdec : preservesQuestionDecide θ view (foldVote θ view pre) signer event
      questionId = true := hpres
  simp only [preservesQuestionDecide] at hdec
  rw [lookupQuestion, hopen] at hdec
  simp only [decide_eq_true_eq] at hdec
  have hlook := hdec
  have hform := applyVoteEvent_preserves_wellFormed view θ (foldVote θ view pre) signer
    event (foldVote_wellFormed view θ pre)
  exact ⟨hlook, hform.opensOpen questionId q hlook⟩

/-! ## INV-54-FRANCHISE (R-44, R-45, VC-5) -/

private theorem sweepClosures_tallyKeys (θ : Threshold) (gs : VoteState) (k : Key) :
    (k ∈ tallyKeysOfState (sweepClosures θ view gs) ↔ k ∈ tallyKeysOfState gs) := by
  constructor
  · intro hk
    unfold tallyKeysOfState at hk ⊢
    rcases List.mem_append.mp hk with hopen | hclosed
    · obtain ⟨keys, hkeys, hkin⟩ := List.mem_flatten.mp hopen
      rw [List.mem_map] at hkeys
      obtain ⟨entry, hentry, heq⟩ := hkeys
      obtain ⟨hkept, -⟩ := (sweepClosures_open_mem view θ gs entry).mp hentry
      exact List.mem_append.mpr (Or.inl (List.mem_flatten.mpr
        ⟨keys, List.mem_map.mpr ⟨entry, hkept, heq⟩, hkin⟩))
    · obtain ⟨keys, hkeys, hkin⟩ := List.mem_flatten.mp hclosed
      rw [List.mem_map] at hkeys
      obtain ⟨r, hr, heq⟩ := hkeys
      rcases (sweepClosures_closed_mem view θ gs r).mp hr with hold |
        ⟨entry, hentry, hsome⟩
      · exact List.mem_append.mpr (Or.inr (List.mem_flatten.mpr
          ⟨keys, List.mem_map.mpr ⟨r, hold, heq⟩, hkin⟩))
      · obtain ⟨_, _, _, hq⟩ := sweepStep_key view θ gs entry r hsome
        have heq' : tallyKeysOfQuestion entry.2 = keys := by
          rw [hq] at heq; exact heq
        exact List.mem_append.mpr (Or.inl (List.mem_flatten.mpr
          ⟨keys, List.mem_map.mpr ⟨entry, hentry, heq'⟩, hkin⟩))
  · intro hk
    unfold tallyKeysOfState at hk ⊢
    rcases List.mem_append.mp hk with hopen | hclosed
    · obtain ⟨keys, hkeys, hkin⟩ := List.mem_flatten.mp hopen
      rw [List.mem_map] at hkeys
      obtain ⟨entry, hentry, heq⟩ := hkeys
      by_cases hv : verdictOf θ view entry.2 = Verdict.open
      · have hkept := (sweepClosures_open_mem view θ gs entry).mpr ⟨hentry, hv⟩
        exact List.mem_append.mpr (Or.inl (List.mem_flatten.mpr
          ⟨keys, List.mem_map.mpr ⟨entry, hkept, heq⟩, hkin⟩))
      · have hrecord : sweepStep θ view entry = some
            { questionId := entry.1, question := entry.2, verdict := verdictOf θ view entry.2, cause := closureCause view entry.2 (verdictOf θ view entry.2) } := by
          cases hvv : verdictOf θ view entry.2
          · simp [sweepStep, hvv]
          · simp [sweepStep, hvv]
          · exact absurd hvv hv
        have hc :
            { questionId := entry.1, question := entry.2, verdict := verdictOf θ view entry.2, cause := closureCause view entry.2 (verdictOf θ view entry.2) } ∈
              (sweepClosures θ view gs).closed :=
          (sweepClosures_closed_mem view θ gs _).mpr (Or.inr ⟨entry, hentry, hrecord⟩)
        exact List.mem_append.mpr (Or.inr (List.mem_flatten.mpr
          ⟨keys, List.mem_map.mpr ⟨_, hc, by simpa using heq⟩, hkin⟩))
    · obtain ⟨keys, hkeys, hkin⟩ := List.mem_flatten.mp hclosed
      rw [List.mem_map] at hkeys
      obtain ⟨c, hc, heq⟩ := hkeys
      have hc' := (sweepClosures_closed_mem view θ gs c).mpr (Or.inl hc)
      exact List.mem_append.mpr (Or.inr (List.mem_flatten.mpr
        ⟨keys, List.mem_map.mpr ⟨c, hc', heq⟩, hkin⟩))

/-- R57-03: exact complete-state identity for an arbitrary rejected pair.
No `VoteWellFormed`, reachability, event-kind, or constructor-specific
premise: the production boundary returns the input before both effect and
sweep. -/
theorem inadmissible_is_noop (θ : Threshold) (gs : VoteState) (signer : Key)
    (event : VoteEvent) (error : VoteError)
    (rejected : validateVoteEvent θ view gs signer event = .error error) :
    applyVoteEvent θ view gs signer event = gs := by
  simp only [applyVoteEvent, rejected]

/-- R57-04: the universal non-responsabile corollary. Once a franchise exists,
a signer who is not a current responsabile is inert for **every** `VoteEvent`;
there are no constructor exceptions, and since T6222 there is no membership
constructor left that could have claimed one. -/
theorem nonresponsabile_event_noop (θ : Threshold) (gs : VoteState)
    (signer : Key) (event : VoteEvent) (bootstrapped : franchiseSize view > 0)
    (unauthorized : isResponsabile signer view = false) :
    applyVoteEvent θ view gs signer event = gs := by
  have hs0 : (franchiseSize view == 0) = false := by
    cases hsz : franchiseSize view with
    | zero => rw [hsz] at bootstrapped; simp at bootstrapped
    | succ n => simp [hsz]
  have hval : validateVoteEvent θ view gs signer event =
      .error VoteError.notResponsabile := by
    cases event <;> simp [validateVoteEvent, unauthorized]
  exact inadmissible_is_noop view θ gs signer event _ hval

theorem unfranchised_cast_noop (θ : Threshold) (gs : VoteState) (signer : Key)
    (questionId : QuestionId) (ballot : Ballot)
    (h : isResponsabile signer view = false) :
    applyVoteEvent θ view gs signer (.cast questionId ballot) = gs := by
  have hval : validateVoteEvent θ view gs signer (.cast questionId ballot) =
      .error VoteError.notResponsabile := by
    simp [validateVoteEvent, h]
  simp only [applyVoteEvent, hval]

private theorem tallyKeysOfState_erased_le (gs : VoteState) (qid : QuestionId) (k : Key)
    (hk : k ∈ tallyKeysOfState { gs with openQuestions := assocErase qid gs.openQuestions }) :
    k ∈ tallyKeysOfState gs := by
  unfold tallyKeysOfState at hk ⊢
  rcases List.mem_append.mp hk with hopen | hclosed
  · obtain ⟨l', hl', hkin⟩ := List.mem_flatten.mp hopen
    refine List.mem_append.mpr (Or.inl (List.mem_flatten.mpr ⟨l', ?_, hkin⟩))
    have hsub :
        ((assocErase qid gs.openQuestions).map
            (fun entry => tallyKeysOfQuestion entry.2)).Sublist
          (gs.openQuestions.map (fun entry => tallyKeysOfQuestion entry.2)) :=
      (assocErase_sublist' qid gs.openQuestions).map _
    exact List.Sublist.mem hl' hsub
  · exact List.mem_append.mpr (Or.inr hclosed)

private theorem tallyKeysOfState_insert_cases (gs : VoteState) (qid : QuestionId)
    (placed : Question) (k : Key)
    (hk : k ∈ tallyKeysOfState
      { gs with openQuestions := assocInsert qid placed gs.openQuestions }) :
    k ∈ tallyKeysOfQuestion placed ∨
      k ∈ tallyKeysOfState { gs with openQuestions := assocErase qid gs.openQuestions } := by
  unfold tallyKeysOfState at hk ⊢
  simp only [assocInsert, List.map_cons, List.flatten_cons] at hk
  rw [List.append_assoc] at hk
  rcases List.mem_append.mp hk with hp | hrest
  · exact Or.inl hp
  · exact Or.inr hrest

private theorem effectedState_tally_growth (θ : Threshold) (gs : VoteState)
    (signer : Key) (event : VoteEvent) (k : Key)
    (admitted : validateVoteEvent θ view gs signer event = Except.ok ())
    (hk : k ∈ tallyKeysOfState (effectedState gs signer event)) :
    k ∈ tallyKeysOfState gs ∨
      (signer = k ∧ ∃ qid ballot, event = VoteEvent.cast qid ballot ∧
        isResponsabile k view = true) := by
  cases event with
  | openQuestion questionId kind =>
      by_cases hguard : (lookupQuestion questionId gs).isNone
          && !(gs.closed.any (fun record : ClosureRecord => record.questionId == questionId))
      · have heff : effectedState gs signer (.openQuestion questionId kind) =
            { gs with openQuestions := assocInsert questionId (Question.mk kind signer [] []) gs.openQuestions } := by
          simp [effectedState, hguard]
        rw [heff] at hk
        rcases tallyKeysOfState_insert_cases gs questionId
          (Question.mk kind signer [] []) k hk with hp | her
        · exact absurd hp (by simp [tallyKeysOfQuestion])
        · exact Or.inl (tallyKeysOfState_erased_le gs questionId k her)
      · have heff : effectedState gs signer (.openQuestion questionId kind) = gs := by
          simp [effectedState, hguard]
        rw [heff] at hk
        exact Or.inl hk
  | cast questionId ballot =>
      cases hresp : isResponsabile signer view with
      | false => simp [validateVoteEvent, hresp] at admitted
      | true =>
          have hresp : isResponsabile signer view = true := hresp
          cases hlook : lookupQuestion questionId gs with
          | none =>
              have heff : effectedState gs signer (.cast questionId ballot) = gs := by
                simp [effectedState, hlook]
              rw [heff] at hk
              exact Or.inl hk
          | some question =>
              have heff : effectedState gs signer (.cast questionId ballot) =
                  { gs with openQuestions := assocInsert questionId (placeBallot signer ballot question) gs.openQuestions } := by
                simp [effectedState, hlook]
              rw [heff] at hk
              rcases tallyKeysOfState_insert_cases gs questionId
                (placeBallot signer ballot question) k hk with hp | her
              · rcases placeBallot_tally signer ballot question k hp with heq | hinq
                · exact Or.inr ⟨heq.symm, questionId, ballot, rfl, heq.symm ▸ hresp⟩
                · refine Or.inl (List.mem_append.mpr (Or.inl ?_))
                  have hqmem := assocLookup_some_mem' questionId question
                    gs.openQuestions hlook
                  exact List.mem_flatten.mpr ⟨tallyKeysOfQuestion question,
                    List.mem_map.mpr ⟨(questionId, question), hqmem, rfl⟩, hinq⟩
              · exact Or.inl (tallyKeysOfState_erased_le gs questionId k her)
  | renounce questionId =>
      have heff : effectedState gs signer (.renounce questionId) = gs := rfl
      rw [heff] at hk
      exact Or.inl hk

private theorem tally_keys_franchised_from (θ : Threshold) :
    ∀ (events : List (Key × VoteEvent)) (initial : VoteState) (k : Key),
      k ∈ tallyKeysOfState (foldFrom θ view initial events) →
      k ∈ tallyKeysOfState initial ∨
        ∃ (pre : List (Key × VoteEvent)) (qid : QuestionId) (ballot : Ballot)
          (suffix : List (Key × VoteEvent)),
          events = pre ++ (k, VoteEvent.cast qid ballot) :: suffix ∧
          isResponsabile k view = true := by
  intro events
  induction events with
  | nil => intro initial k hk; exact Or.inl hk
  | cons signed rest ih =>
      intro initial k hk
      have hmid : foldFrom θ view initial (signed :: rest) =
          foldFrom θ view (applyVoteEvent θ view initial signed.1 signed.2) rest := rfl
      rw [hmid] at hk
      rcases ih (applyVoteEvent θ view initial signed.1 signed.2) k hk with
        hmem | ⟨pre, qid, ballot, suffix, hev, hfr⟩
      · have hgate : k ∈ tallyKeysOfState
            (match validateVoteEvent θ view initial signed.1 signed.2 with
              | .ok () => sweepClosures θ view (effectedState initial signed.1 signed.2)
              | .error _ => initial) := hmem
        cases hval : validateVoteEvent θ view initial signed.1 signed.2 with
        | error _ =>
            simp [hval] at hgate
            exact Or.inl hgate
        | ok u =>
            cases u
            simp [hval] at hgate
            rcases effectedState_tally_growth view θ initial signed.1 signed.2 k hval
              ((sweepClosures_tallyKeys view θ _ k).mp hgate) with
              hold | ⟨hsk, qid, ballot, hevc, hfr2⟩
            · exact Or.inl hold
            · have hsigned : signed = (k, VoteEvent.cast qid ballot) :=
                Prod.ext hsk hevc
              exact Or.inr ⟨[], qid, ballot, rest, by simp [hsigned], hfr2⟩
      · refine Or.inr ⟨signed :: pre, qid, ballot, suffix, ?_, hfr⟩
        simp [hev, List.cons_append]

/-- INV-54-FRANCHISE: every key in any tally of a reachable state was a
responsabile at the moment it cast. The witness is the prefix at whose end
the key's ballot was placed; `isResponsabile` there is the cast-time
franchise check the fold's cast branch performs. The key may have lost
standing since — tallies are counted as recorded (V-3, R-53). -/
theorem franchise_of_tallies (θ : Threshold) (events : List (Key × VoteEvent)) (k : Key)
    (hk : k ∈ tallyKeysOfState (foldVote θ view events)) :
    ∃ (pre : List (Key × VoteEvent)) (qid : QuestionId) (ballot : Ballot)
      (suffix : List (Key × VoteEvent)),
      events = pre ++ (k, VoteEvent.cast qid ballot) :: suffix ∧
      isResponsabile k view = true := by
  rcases tally_keys_franchised_from view θ events emptyVoteState k hk with
    h0 | ⟨pre, qid, ballot, suffix, hev, hfr⟩
  · exact absurd h0 (by simp [tallyKeysOfState, emptyVoteState])
  · exact ⟨pre, qid, ballot, suffix, hev, hfr⟩


/-! ### T6223 — a sealed recomputation cannot duplicate a closure

Ratified reliance `INV-62-B-SWEEP-IS-IDEMPOTENT-ON-A-SWEPT-PAYLOAD`. The base
hook sweeps every open question against the post-transition franchise. Two base
changes in a row therefore sweep an already-swept payload, and a closure record
is money-bearing state: a purchase-approval question that closed twice would
appear twice in the append-only log.

`sweepDuplicating` is the named mutant that makes the danger concrete: it
appends the same records but forgets to remove the closed questions from the
open set, so applying it twice writes every record twice.
-/

/-- A question the sweep leaves open contributes no closure record. -/
theorem sweepStep_of_open (threshold : Threshold) (view : GroupView)
    (entry : QuestionId × Question)
    (h : verdictOf threshold view entry.2 = .open) :
    sweepStep threshold view entry = none := by
  unfold sweepStep
  rw [h]

/-- Nothing survives the filter with a closable verdict, so a swept open set
produces no further records. -/
theorem sweep_filterMap_of_swept (threshold : Threshold) (view : GroupView) :
    ∀ l : List (QuestionId × Question),
      (l.filter (fun entry => verdictOf threshold view entry.2 = .open)).filterMap
          (sweepStep threshold view) = []
  | [] => rfl
  | entry :: rest => by
    rw [List.filter_cons]
    by_cases h : verdictOf threshold view entry.2 = .open
    · simp only [h, decide_true, if_true, List.filterMap_cons,
        sweepStep_of_open threshold view entry h]
      exact sweep_filterMap_of_swept threshold view rest
    · simp only [h, decide_false, if_false]
      exact sweep_filterMap_of_swept threshold view rest

/-- The same filter applied twice keeps exactly the same questions. -/
theorem filter_open_idem (threshold : Threshold) (view : GroupView) :
    ∀ l : List (QuestionId × Question),
      (l.filter (fun entry => verdictOf threshold view entry.2 = .open)).filter
          (fun entry => verdictOf threshold view entry.2 = .open)
        = l.filter (fun entry => verdictOf threshold view entry.2 = .open)
  | [] => rfl
  | entry :: rest => by
    rw [List.filter_cons]
    by_cases h : verdictOf threshold view entry.2 = .open
    · simp only [h, decide_true, if_true, List.filter_cons]
      exact congrArg _ (filter_open_idem threshold view rest)
    · simp only [h, decide_false, if_false]
      exact filter_open_idem threshold view rest

/-- **Sealed recomputation is idempotent.** Sweeping a payload that was already
swept at the same threshold and canonical view returns it unchanged: no
question is re-closed and no closure record is appended twice. -/
theorem sweepClosures_idempotent (threshold : Threshold) (view : GroupView)
    (gs : VoteState) :
    sweepClosures threshold view (sweepClosures threshold view gs)
      = sweepClosures threshold view gs := by
  simp only [sweepClosures, filter_open_idem threshold view gs.openQuestions,
    sweep_filterMap_of_swept threshold view gs.openQuestions, List.append_nil]

/-- Named mutant of the production sweep: it writes the closure records but
leaves the closed questions in the open set. Applied twice it duplicates every
record, which is exactly what `sweepClosures_idempotent` forbids. -/
def sweepDuplicating (threshold : Threshold) (view : GroupView) (gs : VoteState) :
    VoteState :=
  { gs with closed := gs.closed ++ gs.openQuestions.filterMap (sweepStep threshold view) }

/-- The mutant is not idempotent whenever the sweep closes anything: the second
application appends the same records again. This is the negative control that
makes `sweepClosures_idempotent` a claim about the production definition rather
than about an empty case. -/
theorem sweepDuplicating_duplicates (threshold : Threshold) (view : GroupView)
    (gs : VoteState)
    (h : gs.openQuestions.filterMap (sweepStep threshold view) ≠ []) :
    sweepDuplicating threshold view (sweepDuplicating threshold view gs)
      ≠ sweepDuplicating threshold view gs := by
  intro hcontra
  apply h
  have hlen := congrArg (fun v => v.closed.length) hcontra
  simp only [sweepDuplicating, List.length_append] at hlen
  cases hF : gs.openQuestions.filterMap (sweepStep threshold view) with
  | nil => rfl
  | cons a t =>
    rw [hF] at hlen
    simp only [List.length_cons] at hlen
    omega

end KelGroups.Vote

/- Axiom evidence for the contractual theorem names; the frozen gate reads
the printed lines, so a theorem that vanishes takes its evidence with it. -/
#print axioms KelGroups.Vote.foldVote_wellFormed
#print axioms KelGroups.Vote.ballots_nodup_disjoint
#print axioms KelGroups.Vote.open_questions_are_open
#print axioms KelGroups.Vote.questions_partition
#print axioms KelGroups.Vote.no_expiry
#print axioms KelGroups.Vote.franchise_of_tallies
#print axioms KelGroups.Vote.verdictOf_threshold_congr
#print axioms KelGroups.Vote.inadmissible_is_noop
#print axioms KelGroups.Vote.nonresponsabile_event_noop
