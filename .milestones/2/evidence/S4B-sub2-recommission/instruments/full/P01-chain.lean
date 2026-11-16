import KelGroups.Types

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

abbrev comuneId : KelGroups.Key := "comune"

def comune_not_a_member (view : KelGroups.GroupView) : Prop :=
  ¬ KelGroups.GroupView.isMember comuneId view

theorem view_mem_of_isMember {view : KelGroups.GroupView} {u : KelGroups.Key}
    (h : KelGroups.GroupView.isMember u view = true) : u ∈ view.members.map Prod.fst := by
  have hs : (KelGroups.assocLookup u view.members).isSome = true := h
  cases hv : KelGroups.assocLookup u view.members with
  | none => simp [hv] at hs
  | some v =>
      obtain hm := assocLookup_some_mem_nodupfree u v view.members hv
      exact List.mem_map.mpr ⟨(u, v), hm, rfl⟩

theorem isMember_of_view_mem {view : KelGroups.GroupView} {u : KelGroups.Key}
    {v : KelGroups.Member}
    (h : (u, v) ∈ view.members) : KelGroups.GroupView.isMember u view = true := by
  obtain ⟨w, hw⟩ := assocLookup_some_of_mem_nodupfree h
  show (KelGroups.assocLookup u view.members).isSome = true
  rw [hw]
  rfl

theorem comune_not_a_member_corr (view : KelGroups.GroupView) :
    comune_not_a_member view ↔ ((!KelGroups.GroupView.isMember comuneId view) = true) := by
  unfold comune_not_a_member
  cases KelGroups.GroupView.isMember comuneId view <;> simp

def witnessMember : KelGroups.Member := ⟨"u", "u@audit", [.appRole "buyer"]⟩
def witnessView : KelGroups.GroupView := ⟨[("u", witnessMember), ("u", ⟨"u", "second@audit", []⟩)]⟩
#eval IO.println s!"P01-WITNESS present={KelGroups.GroupView.isMember "u" witnessView} absent={KelGroups.GroupView.isMember "absent" witnessView} duplicates={witnessView.members.length}"
