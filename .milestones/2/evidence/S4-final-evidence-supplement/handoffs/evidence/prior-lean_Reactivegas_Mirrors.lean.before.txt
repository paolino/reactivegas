import Reactivegas.Predicates
import Reactivegas.Step

/-!
# Executable Bool mirrors for the law predicates (S4-B, #66)

For each finite law/state `Prop` in `Reactivegas.Predicates` (except the named
exceptions below) this module ships an independently implemented `Bool` mirror
and proves the exact correspondence `P … ↔ B … = true`.

Rules honoured throughout:
* existing expressions are RELATED, never duplicated: `P01` relates
  `comune_not_a_member` to the existing `KelGroups.GroupView.isMember`, and
  `P07` relates `permissionToClose` to its existing
  `permitted && pending.isEmpty` expression. No new runtime is introduced for
  either (R4);
* equality is decided with `decide` over `DecidableEq` instances, never with
  bare `BEq` (NOTE-003);
* finite list reductions preserve lookup semantics on ARBITRARY states: no
  well-formedness (`Nodup`) premise appears in any statement. Duplicate keys
  are handled by first-match-consistent reductions (`view_mem_of_isMember`,
  `isMember_of_view_mem`), absent keys by the default-balance lemma
  (`bal_absent`) (R14, R15);
* no original definition or theorem is touched (R1, R5).

Named exceptions (covered by `scripts/check-lean-mirrors`, not here):
* `authorizedStep` (P11) — definitional projection onto the existing
  `isResponsabile`; per-constructor relating evidence lives in the checker;
* `stalled` (R0) — already executable (`stalledDecidable`, and it evaluates);
* `Reach` (P13) — NOT-EXECUTABLE, bounded: no arbitrary-`Reach` oracle is
  required under the standing boundary, and that lack is not an
  undecidability proof.
-/

/-- Membership in an association list yields a lookup hit (no `Nodup` needed). -/
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

/-- Any entry present in the list is found by lookup (no `Nodup` needed). -/
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
theorem view_mem_of_isMember {view : KelGroups.GroupView} {u : KelGroups.Key}
    (h : KelGroups.GroupView.isMember u view = true) : u ∈ view.members.map Prod.fst := by
  have hs : (KelGroups.assocLookup u view.members).isSome = true := h
  cases hv : KelGroups.assocLookup u view.members with
  | none => simp [hv] at hs
  | some v =>
      obtain hm := assocLookup_some_mem_nodupfree u v view.members hv
      exact List.mem_map.mpr ⟨(u, v), hm, rfl⟩

/-- A key occurring in the member list counts as member (duplicates harmless).
(v3.1 promoted body/consumer obligation: this proof reads `isMember`'s body.) -/
theorem isMember_of_view_mem {view : KelGroups.GroupView} {u : KelGroups.Key}
    {v : KelGroups.Member}
    (h : (u, v) ∈ view.members) : KelGroups.GroupView.isMember u view = true := by
  obtain ⟨w, hw⟩ := assocLookup_some_of_mem_nodupfree h
  show (KelGroups.assocLookup u view.members).isSome = true
  rw [hw]
  rfl

/-- Balance lookup of an absent key is the default zero. -/
private theorem bal_absent {m : List (KelGroups.Key × Int)} {u : KelGroups.Key}
    (h : u ∉ m.map Prod.fst) : bal m u = 0 := by
  induction m with
  | nil => rfl
  | cons entry rest ih =>
      obtain ⟨k, v⟩ := entry
      rw [bal_cons]
      by_cases hku : k = u
      · subst hku
        exact absurd (List.mem_map.mpr ⟨(k, v), List.mem_cons_self, rfl⟩) h
      · simp only [hku, if_false]
        exact ih (fun hm => h (List.mem_cons_of_mem _ hm))

/-- Member-list occurrence and the canonical membership test coincide. -/
private theorem keys_mem_coe (view : KelGroups.GroupView) (u : KelGroups.Key) :
    (u ∈ view.members.map Prod.fst) ↔ ↑(KelGroups.GroupView.isMember u view) := by
  constructor
  · intro h
    obtain ⟨⟨k, v⟩, he, heq⟩ := List.mem_map.mp h
    have heq' : k = u := heq
    subst heq'
    exact isMember_of_view_mem he
  · intro h
    exact view_mem_of_isMember h

/-- P02 mirror: conservation is already a closed arithmetic equation. -/
def conservationB (s : State) : Bool :=
  decide (sumBal s.casse - sumBal s.conti - escrowSum s.collections = 0)

/-- P02 correspondence. -/
theorem conservation_corr (s : State) : conservation s ↔ conservationB s = true := by
  simp only [conservation, conservationB, decide_eq_true_eq]

/-- P01 correspondence: RELATES the existing `isMember` expression (R4).
No new runtime is introduced. -/
theorem comune_not_a_member_corr (view : KelGroups.GroupView) :
    comune_not_a_member view ↔ ((!KelGroups.GroupView.isMember comuneId view) = true) := by
  unfold comune_not_a_member
  cases KelGroups.GroupView.isMember comuneId view <;> simp

/-- P01 supporting projection: `productionWellFormed` IS that expression
read at the canonical view (definitional, `Step.lean:357`). -/
theorem productionWellFormed_proj (gs : KelGroups.GroupState State) :
    Reactivegas.productionWellFormed gs =
      !KelGroups.GroupView.isMember comuneId (KelGroups.groupView gs) := rfl

/-- P03 mirror: member balances over the member-list keys (duplicates give
redundant identical checks), pledged amounts over the collection lists. -/
def solventB (view : KelGroups.GroupView) (s : State) : Bool :=
  (view.members.map Prod.fst).all (fun u => decide (bal s.conti u ≥ 0)) &&
  s.collections.all (fun col =>
    (col.accepted ++ col.pending).all (fun p => decide (0 ≤ p.amount)))

/-- P03 correspondence: exact on arbitrary states, no well-formedness premise. -/
theorem solvent_corr (view : KelGroups.GroupView) (s : State) :
    solvent view s ↔ solventB view s = true := by
  simp only [solvent, solventB, Bool.and_eq_true, List.all_eq_true, decide_eq_true_eq,
    keys_mem_coe]

/-- P04 mirror: existential over the member-list keys. -/
def insolventB (view : KelGroups.GroupView) (s : State) : Bool :=
  (view.members.map Prod.fst).any (fun u => decide (bal s.conti u < 0))

/-- P04 correspondence: exact on arbitrary states, no well-formedness premise. -/
theorem insolvent_corr (view : KelGroups.GroupView) (s : State) :
    insolvent view s ↔ insolventB view s = true := by
  simp only [insolvent, insolventB, List.any_eq_true, decide_eq_true_eq, keys_mem_coe]

/-- P05 mirror: pairwise uniqueness check over the pledge lists. -/
def uniquePledgesB (col : Collection) : Bool :=
  (col.accepted ++ col.pending).all fun p =>
    (col.accepted ++ col.pending).all fun q =>
      (!decide (p.user = q.user) || decide (p = q))

/-- Pointwise step for P05 (decided Booleans, `DecidableEq Pledge`). -/
private theorem uniquePledges_pt (p q : Pledge) :
    ((!decide (p.user = q.user) || decide (p = q)) = true) ↔
      (p.user = q.user → p = q) := by
  cases h1 : decide (p.user = q.user) <;> cases h2 : decide (p = q) <;> simp_all

/-- P05 correspondence. -/
theorem uniquePledges_corr (col : Collection) :
    uniquePledges col ↔ uniquePledgesB col = true := by
  simp only [uniquePledges, uniquePledgesB, List.all_eq_true, uniquePledges_pt]

/-- P06 mirror: per-collection uniqueness over the state lists. -/
def allUniquePledgesB (s : State) : Bool :=
  s.collections.all fun col => uniquePledgesB col

/-- P06 correspondence. -/
theorem allUniquePledges_corr (s : State) :
    allUniquePledges s ↔ allUniquePledgesB s = true := by
  simp only [allUniquePledges, allUniquePledgesB, List.all_eq_true, uniquePledges_corr]

/-- P07 correspondence: RELATES the existing `permitted && pending.isEmpty`
expression (R4). No new runtime is introduced. -/
theorem permissionToClose_corr (col : Collection) :
    permissionToClose col ↔ ((col.permitted && col.pending.isEmpty) = true) := by
  obtain ⟨id, ref, perm, acc, pend⟩ := col
  cases perm <;> cases pend <;> simp [permissionToClose]

/-- P08 mirror: the `splitUser` outcome determines the held amount. -/
def escrowHeldB (col : Collection) (u : KelGroups.Key) (v : Int) : Bool :=
  match splitUser u col.pending with
  | some (amt, _) => decide (amt = v)
  | none => false

/-- P08 correspondence. -/
theorem escrowHeld_corr (col : Collection) (u : KelGroups.Key) (v : Int) :
    escrowHeld col u v ↔ escrowHeldB col u v = true := by
  unfold escrowHeld escrowHeldB
  cases h : splitUser u col.pending with
  | none =>
      simp
  | some w =>
      obtain ⟨v', r⟩ := w
      simp only [Option.some.injEq, Prod.mk.injEq, decide_eq_true_eq]
      constructor
      · intro ⟨pend, heq, _⟩
        exact heq
      · intro heq
        exact ⟨r, heq, rfl⟩

/-- P09 mirror: every collection's referente differs from `u`. -/
def governanceEnactsB (u : KelGroups.Key) (s' : State) : Bool :=
  s'.collections.all fun c => decide (c.referente ≠ u)

/-- P09 correspondence. -/
theorem governanceEnacts_corr (u : KelGroups.Key) (s' : State) :
    governanceEnacts u s' ↔ governanceEnactsB u s' = true := by
  simp only [governanceEnacts, governanceEnactsB, List.all_eq_true, decide_eq_true_eq]

/-- P10 mirror: the two balance equations, decided separately. -/
def doubleEntryB (s s' : State) (a u : KelGroups.Key) (v : Int) : Bool :=
  decide (bal s'.conti u = bal s.conti u + v) &&
  decide (bal s'.casse a = bal s.casse a + v)

/-- P10 correspondence. -/
theorem doubleEntry_corr (s s' : State) (a u : KelGroups.Key) (v : Int) :
    doubleEntry s s' a u v ↔ doubleEntryB s s' a u v = true := by
  simp only [doubleEntry, doubleEntryB, Bool.and_eq_true, decide_eq_true_eq]

/-- Third conjunct of P12, finitised: checking the balance at every occurring
key is exactly checking it at every key, because absent keys read the default
zero (`bal_absent`). Duplicate keys give redundant identical checks. -/
private theorem canClose_third (s : State) :
    (∀ r : KelGroups.Key, bal s.casse r = 0) ↔
      ∀ r ∈ s.casse.map Prod.fst, bal s.casse r = 0 := by
  constructor
  · intro h r _
    exact h r
  · intro h r
    by_cases hm : r ∈ s.casse.map Prod.fst
    · exact h r hm
    · exact bal_absent hm

/-- P12 mirror: member conti over the member-list keys, no open collections,
cassa over the occurring cassa keys (exact by `canClose_third`). -/
def canCloseGroupB (view : KelGroups.GroupView) (s : State) : Bool :=
  (view.members.map Prod.fst).all (fun u => decide (bal s.conti u = 0)) &&
  (decide (s.collections = []) &&
  (s.casse.map Prod.fst).all (fun r => decide (bal s.casse r = 0)))

/-- P12 correspondence: exact on arbitrary states, no well-formedness premise. -/
theorem canCloseGroup_corr (view : KelGroups.GroupView) (s : State) :
    canCloseGroup view s ↔ canCloseGroupB view s = true := by
  simp only [canCloseGroup, canCloseGroupB, Bool.and_eq_true, List.all_eq_true,
    decide_eq_true_eq, keys_mem_coe, canClose_third]
