-- S2-chain-P01.lean — P01 body/consumer chain against REAL KelGroups.Types.
-- FROZEN INSTRUMENT (submission 2; OT2neg + OT2pos — SAME bytes run twice,
-- supersedes the hermetic miniature which is retained unexecuted-supplementary).
-- NEG run: LEAN_PATH resolves a shadow mutant Types.olean first (constant-false
-- body, exact audited bytes compiled from the real file). POS run: LEAN_PATH
-- resolves the clean build olean. Direct `lean` (NOT `lake env lean`) with
-- explicit LEAN_PATH: measured `lake env` appends its paths LAST, which would
-- let the clean olean shadow the mutant silently — determinism requires the
-- explicit order, and each run's outcome authenticates which olean loaded
-- (neg must fail at the helpers; pos must go green).
-- Imports REAL KelGroups.Types ONLY (mutant in neg, clean in pos). Everything
-- else below is copied verbatim: pure lookup helpers (mutant-independent),
-- `comuneId`/`comune_not_a_member` (so the P01 contrast needs no Predicates
-- import, which would drag Step's olean into the neg closure), and the two
-- promoted helper statements/proofs plus the P01 correspondence, byte-identical
-- to production (post-promotion visibility). No Step import anywhere: Step's
-- `comune_cannot_authorize` decide-assertion can neither fire nor mask here —
-- that masking is exactly what this isolation removes, while O5 evidences the
-- mandatory-path enforcement separately.
-- Expect NEG: exit 1 with errors EXACTLY at the two helpers (their proofs read
-- `isMember`'s body through definitional unfolding, which the mutant breaks).
-- Expect POS: exit 0, everything elaborates (same bytes, clean body).
import KelGroups.Types

/-- Membership in an association list yields a lookup hit (no `Nodup` needed).
    Byte-identical copy of the production helper (mutant-independent). -/
private theorem assocLookup_some_mem_nodupfree {κ ν : Type} [BEq κ] [LawfulBEq κ]
    (key : κ) (value : ν) (entries : List (κ × ν))
    (h : KelGroups.assocLookup key entries = some value) : (key, value) ∈ entries := by
  induction entries with
  | nil => simp [KelGroups.assocLookup] at h
  | cons entry rest ih =>
      obtain ⟨candidate, current⟩ := entry
      simp only [KelGroups.assocLookup] at h
      split at h
      · next equal =>
          have keyEq : candidate = key := beq_iff_eq.mp equal
          subst keyEq
          simp only [Option.some.injEq] at h
          subst h
          exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (ih h)

/-- Any entry present in the list is found by lookup (no `Nodup` needed).
    Byte-identical copy of the production helper (mutant-independent). -/
private theorem assocLookup_some_of_mem_nodupfree {κ ν : Type} [BEq κ] [LawfulBEq κ]
    {key : κ} {value : ν} {entries : List (κ × ν)}
    (h : (key, value) ∈ entries) : ∃ w, KelGroups.assocLookup key entries = some w := by
  induction entries with
  | nil => cases h
  | cons entry rest ih =>
      obtain ⟨k, w⟩ := entry
      simp only [List.mem_cons, Prod.mk.injEq] at h
      by_cases heq : k == key
      · exact ⟨w, by simp [KelGroups.assocLookup, heq]⟩
      · rcases h with ⟨hku, _⟩ | htail
        · subst hku
          simp at heq
        · obtain ⟨w', hw'⟩ := ih htail
          exact ⟨w', by simp [KelGroups.assocLookup, heq, hw']⟩

abbrev comuneId : KelGroups.Key := "comune"

def comune_not_a_member (view : KelGroups.GroupView) : Prop :=
  ¬ KelGroups.GroupView.isMember comuneId view

/-- Byte-identical to production `view_mem_of_isMember` (post-promotion). -/
theorem view_mem_of_isMember {view : KelGroups.GroupView} {u : KelGroups.Key}
    (h : KelGroups.GroupView.isMember u view = true) : u ∈ view.members.map Prod.fst := by
  have hs : (KelGroups.assocLookup u view.members).isSome = true := h
  cases hv : KelGroups.assocLookup u view.members with
  | none => simp [hv] at hs
  | some v =>
      obtain hm := assocLookup_some_mem_nodupfree u v view.members hv
      exact List.mem_map.mpr ⟨(u, v), hm, rfl⟩

/-- Byte-identical to production `isMember_of_view_mem` (post-promotion). -/
theorem isMember_of_view_mem {view : KelGroups.GroupView} {u : KelGroups.Key}
    {v : KelGroups.Member}
    (h : (u, v) ∈ view.members) : KelGroups.GroupView.isMember u view = true := by
  obtain ⟨w, hw⟩ := assocLookup_some_of_mem_nodupfree h
  show (KelGroups.assocLookup u view.members).isSome = true
  rw [hw]
  rfl

/-- P01 correspondence, UNCHANGED (contrast): value-parametric truth table,
valid under every `isMember` implementation including the mutant. -/
theorem comune_not_a_member_corr (view : KelGroups.GroupView) :
    comune_not_a_member view ↔ ((!KelGroups.GroupView.isMember comuneId view) = true) := by
  unfold comune_not_a_member
  cases KelGroups.GroupView.isMember comuneId view <;> simp
