-- S2-chain-P01-clean.lean — POSITIVE control twin of S2-chain-P01.lean.
-- Identical bytes except `isMember` carries the production body
-- (`(lookupMember key view).isSome`). Everything must elaborate: exit 0
-- expected. A red negative twin without this green positive twin could fail
-- from its own setup or copy; the pair isolates the body as the difference.
-- FROZEN INSTRUMENT (submission 2, OT2). Single-file elaboration; core-only
-- imports (no project modules are imported, so no oleans beyond the toolchain
-- are needed and no earlier-gate masking is possible).
--
-- Environment: minimal local copies with REAL logic, INCLUDING the production
-- `isMember` body (`(lookupMember key view).isSome`; contrast twin
-- S2-chain-P01.lean carries the audited constant-false mutant). Target statements and proofs below are byte-identical to
-- production `view_mem_of_isMember` (Mirrors.lean:71) and
-- `isMember_of_view_mem` (:81); their qualified names resolve to this local
-- miniature (nothing project-level is imported, so nothing clashes). The P01
-- correspondence is included UNCHANGED as contrast: it still proves, because
-- it cases on a value and never reads the implementation — exactly the v3.1
-- distinction (originals stay valid; sensitivity lives in the promoted rows).
-- Expect: exit 0, everything elaborates (helpers hold on the production body).

namespace KelGroups

abbrev Key := String

structure Member where
  key : Key
deriving DecidableEq, BEq, Repr

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

/-- Is `key` a current member of the canonical relation?
    AUDITED BODY MUTANT (constant-false). Production reads
    `(lookupMember key view).isSome`. -/
def isMember (key : Key) (view : GroupView) : Bool :=
  (lookupMember key view).isSome

end GroupView

end KelGroups

/-- Membership in an association list yields a lookup hit (no `Nodup` needed).
    Byte-identical copy of the production helper. -/
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
    Byte-identical copy of the production helper. -/
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
      · rcases h with ⟨hku, -⟩ | htail
        · subst hku
          simp at heq
        · obtain ⟨w', hw'⟩ := ih htail
          exact ⟨w', by simp [KelGroups.assocLookup, heq, hw']⟩

/-- A key the canonical view counts as member occurs in its member list.
(v3.1 promoted body/consumer obligation: this proof reads `isMember`'s body.) -/
private theorem view_mem_of_isMember {view : KelGroups.GroupView} {u : KelGroups.Key}
    (h : KelGroups.GroupView.isMember u view = true) : u ∈ view.members.map Prod.fst := by
  have hs : (KelGroups.assocLookup u view.members).isSome = true := h
  cases hv : KelGroups.assocLookup u view.members with
  | none => simp [hv] at hs
  | some v =>
      obtain hm := assocLookup_some_mem_nodupfree u v view.members hv
      exact List.mem_map.mpr ⟨(u, v), hm, rfl⟩

/-- A key occurring in the member list counts as member (duplicates harmless).
(v3.1 promoted body/consumer obligation: this proof reads `isMember`'s body.) -/
private theorem isMember_of_view_mem {view : KelGroups.GroupView} {u : KelGroups.Key}
    {v : KelGroups.Member}
    (h : (u, v) ∈ view.members) : KelGroups.GroupView.isMember u view = true := by
  obtain ⟨w, hw⟩ := assocLookup_some_of_mem_nodupfree h
  show (KelGroups.assocLookup u view.members).isSome = true
  rw [hw]
  rfl

abbrev comuneId : KelGroups.Key := "comune"

def comune_not_a_member (view : KelGroups.GroupView) : Prop :=
  ¬ KelGroups.GroupView.isMember comuneId view

/-- P01 correspondence, UNCHANGED (contrast): value-parametric truth table,
valid under every `isMember` implementation including this mutant. -/
theorem comune_not_a_member_corr (view : KelGroups.GroupView) :
    comune_not_a_member view ↔ ((!KelGroups.GroupView.isMember comuneId view) = true) := by
  unfold comune_not_a_member
  cases KelGroups.GroupView.isMember comuneId view <;> simp
