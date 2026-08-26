import Reactivegas.Types

/-!
# Economic state of the machine

List-based state: association lists for conti and casse, a list of
open collections holding the escrow. The boot state mirrors the legacy
`bootAnagrafe`, which starts every group with founding responsabili.
-/

/-- An open purchase: escrow at pledge (L3), closure permission (L2). -/
structure Collection where
  id : CollId
  referente : UserId
  permitted : Bool
  accepted : List Pledge
  pending : List Pledge
deriving DecidableEq, Repr

/-- The whole economic state. -/
structure State where
  users : List UserId
  responsabili : List UserId
  /-- user credit accounts -/
  conti : List (UserId × Int)
  /-- cash boxes, one per responsabile -/
  casse : List (UserId × Int)
  collections : List Collection
deriving DecidableEq, Repr

/-- Boot state with `r` as the founding user/responsabile. -/
def State.init (r : UserId) : State :=
  ⟨[r], [r], [], [], []⟩

/-- Balance lookup on an association list; absent means zero. -/
def bal (m : List (UserId × Int)) (u : UserId) : Int :=
  match m with
  | [] => 0
  | (k, v) :: t => if k = u then v else bal t u

/-- **Comune balance** (issue #48): the common fund is the `conti` balance
at the reserved `comuneId` — a reserved non-member account inside
`conti`, never a standalone `State` field. -/
def comuneBal (s : State) : Int := bal s.conti comuneId

/-- **Stall** (issue #48): the comune conto went negative. While stalled
the machine refuses `closePurchase`, `withdraw`, `pledge`,
`acceptPledge` and both departures; `donate` is the sole cure and stays
reachable, as do `failPurchase` and `refusePledge`. `backdonate` is
refused by its own affordability guard, with no separate stall
condition. -/
def stalled (s : State) : Prop := comuneBal s < 0

/-- `stalled` is decidable so step guards can refuse stalled events. -/
instance stalledDecidable (s : State) : Decidable (stalled s) :=
  Int.decLt (comuneBal s) 0

/-- Add `d` to the entry of `u`, appending a fresh entry when absent. -/
def bump (m : List (UserId × Int)) (u : UserId) (d : Int) : List (UserId × Int) :=
  match m with
  | [] => [(u, d)]
  | (k, v) :: t => if k = u then (k, v + d) :: t else (k, v) :: bump t u d

/-- Sum of the amounts in an account list. -/
def sumBal (m : List (UserId × Int)) : Int := (m.map Prod.snd).sum

/-- Sum of the pledged amounts. -/
def sumPledges : List Pledge → Int
  | [] => 0
  | p :: t => p.amount + sumPledges t

/-- Money held in escrow by one collection (accepted plus pending). -/
def escrowOf (c : Collection) : Int := sumPledges c.accepted + sumPledges c.pending

/-- Total money held in escrow across all open collections. -/
def escrowSum (cols : List Collection) : Int := (cols.map escrowOf).sum

/-
Split off the pledge of user `u` from a pledge list: the amount and the
list without it. Fails when the user has no pledge in the list.
-/
def splitUser (u : UserId) : List Pledge → Option (Int × List Pledge)
  | [] => none
  | p :: t =>
    if p.user = u then some (p.amount, t)
    else match splitUser u t with
      | some (v', r') => some (v', p :: r')
      | none => none

/-- Refund every pledge into an account list, one bump per pledge. -/
def refundAll (m : List (UserId × Int)) (l : List Pledge) : List (UserId × Int) :=
  l.foldl (fun acc p => bump acc p.user p.amount) m

/-
Pull the collection with id `c` out of a collection list: the collection
itself together with the list without it. Every collection-modifying
event touches the state through this function; rebuilding is a plain
`cons`.
-/
def pullCollection (c : CollId) : List Collection → Option (Collection × List Collection)
  | [] => none
  | x :: t =>
    if x.id = c then some (x, t)
    else match pullCollection c t with
      | some (y, rest) => some (y, x :: rest)
      | none => none

/-
Detach the collections whose referente is `r`: what remains, together
with all their pledges (to be refunded). Legacy: revoking a responsabile
cancels their open questions (`EventoEliminazioneResponsabile`).
-/
def stripCollections (r : UserId) : List Collection → List Collection × List Pledge
  | [] => ([], [])
  | c :: t =>
    let (rest, ps) := stripCollections r t
    if c.referente = r then (rest, c.accepted ++ c.pending ++ ps)
    else (c :: rest, ps)

/-! ### Arithmetic facts about the helpers -/

theorem sumBal_cons (k : UserId) (v : Int) (t : List (UserId × Int)) :
    sumBal ((k, v) :: t) = v + sumBal t := rfl

theorem bal_cons (k : UserId) (v : Int) (t : List (UserId × Int)) (u : UserId) :
    bal ((k, v) :: t) u = if k = u then v else bal t u := rfl

theorem bump_sum (m : List (UserId × Int)) (u : UserId) (d : Int) :
    sumBal (bump m u d) = sumBal m + d := by
  induction m with
  | nil => simp [bump, sumBal]
  | cons kv t ih =>
    obtain ⟨k, v⟩ := kv
    rw [bump]
    split
    · next h => rw [sumBal_cons, sumBal_cons]; omega
    · next h => rw [sumBal_cons, sumBal_cons, ih]; omega

theorem bal_bump (m : List (UserId × Int)) (u : UserId) (d : Int) :
    bal (bump m u d) u = bal m u + d := by
  induction m with
  | nil => simp [bump, bal]
  | cons kv t ih =>
    obtain ⟨k, v⟩ := kv
    rw [bump]
    split
    · next h => rw [bal_cons, bal_cons, if_pos h, if_pos h]
    · next h => rw [bal_cons, bal_cons, if_neg h, if_neg h, ih]

theorem sumPledges_append (l₁ l₂ : List Pledge) :
    sumPledges (l₁ ++ l₂) = sumPledges l₁ + sumPledges l₂ := by
  induction l₁ with
  | nil => simp [sumPledges]
  | cons p t ih => simp [sumPledges, ih]; omega

theorem refundAll_sum (m : List (UserId × Int)) (l : List Pledge) :
    sumBal (refundAll m l) = sumBal m + sumPledges l := by
  induction l generalizing m with
  | nil => simp [refundAll, sumPledges]
  | cons p t ih =>
    have h1 : sumBal (List.foldl _ (bump m p.user p.amount) t)
        = sumBal (bump m p.user p.amount) + sumPledges t := ih _
    have h2 := bump_sum m p.user p.amount
    show sumBal (List.foldl _ (bump m p.user p.amount) t)
      = sumBal m + (p.amount + sumPledges t)
    rw [h1, h2]
    omega

theorem not_mem_users_of_splitUser_none {u : UserId} {l : List Pledge}
    (h : splitUser u l = none) : ∀ p ∈ l, p.user ≠ u := by
  induction l with
  | nil =>
    intro p hp
    exact absurd hp (by intro hc; cases hc)
  | cons q t ih =>
    intro p hp
    rw [splitUser] at h
    split at h
    · exact Option.noConfusion h
    · next hq =>
      cases hx : splitUser u t with
      | none =>
        rcases List.mem_cons.mp hp with hc | hc
        · subst hc; exact hq
        · exact ih hx p hc
      | some w =>
        obtain ⟨wv, wr⟩ := w
        rw [hx] at h
        exact Option.noConfusion h

private theorem splitUser_sum_lemma {u : UserId} :
    ∀ (l : List Pledge) (v : Int) (r : List Pledge),
      splitUser u l = some (v, r) → sumPledges l = v + sumPledges r := by
  intro l
  induction l with
  | nil => intro v r h; exact Option.noConfusion h
  | cons p t ih =>
    intro v r h
    rw [splitUser] at h
    split at h
    · next hp =>
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      simp [sumPledges]
    · next hp =>
      cases hx : splitUser u t with
      | none => rw [hx] at h; exact Option.noConfusion h
      | some w =>
        obtain ⟨wv, wr⟩ := w
        rw [hx] at h
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        have hihr : sumPledges t = wv + sumPledges wr := ih wv wr hx
        rw [← h.2]
        simp only [sumPledges]
        rw [hihr, ← h.1]
        omega

theorem splitUser_sum {u : UserId} {l : List Pledge} {v : Int} {r : List Pledge}
    (h : splitUser u l = some (v, r)) : sumPledges l = v + sumPledges r :=
  splitUser_sum_lemma l v r h

private theorem splitUser_sublist_lemma {u : UserId} :
    ∀ (l : List Pledge) (v : Int) (r : List Pledge),
      splitUser u l = some (v, r) → ∀ q ∈ r, q ∈ l := by
  intro l
  induction l with
  | nil => intro v r h q hq; exact Option.noConfusion h
  | cons p t ih =>
    intro v r h
    rw [splitUser] at h
    split at h
    · next hp =>
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨-, rfl⟩ := h
      intro q hq
      exact List.mem_cons_of_mem _ hq
    · next hp =>
      cases hx : splitUser u t with
      | none => rw [hx] at h; exact Option.noConfusion h
      | some w =>
        obtain ⟨wv, wr⟩ := w
        rw [hx] at h
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        rw [← h.2]
        intro q hq
        rcases List.mem_cons.mp hq with hc | hc
        · exact List.mem_cons.mpr (Or.inl hc)
        · exact List.mem_cons_of_mem _ (ih wv wr hx q hc)

theorem splitUser_sublist {u : UserId} {l : List Pledge} {v : Int} {r : List Pledge}
    (h : splitUser u l = some (v, r)) : ∀ p ∈ r, p ∈ l :=
  splitUser_sublist_lemma l v r h

theorem escrowSum_cons (c : Collection) (t : List Collection) :
    escrowSum (c :: t) = escrowOf c + escrowSum t := rfl

private theorem pullCollection_id_lemma {c : CollId} :
    ∀ (cols : List Collection) (x : Collection) (rest : List Collection),
      pullCollection c cols = some (x, rest) → x.id = c := by
  intro cols
  induction cols with
  | nil => intro x rest h; exact Option.noConfusion h
  | cons z t ih =>
    intro x rest h
    rw [pullCollection] at h
    split at h
    · next hz =>
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      exact hz
    · next hz =>
      cases hx : pullCollection c t with
      | none => rw [hx] at h; exact Option.noConfusion h
      | some w =>
        obtain ⟨y', rest'⟩ := w
        rw [hx] at h
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        exact h.1 ▸ ih y' rest' hx

theorem pullCollection_id {c : CollId} {cols : List Collection} {x : Collection}
    {rest : List Collection} (h : pullCollection c cols = some (x, rest)) : x.id = c :=
  pullCollection_id_lemma cols x rest h

private theorem pullCollection_sum_lemma {c : CollId} :
    ∀ (cols : List Collection) (x : Collection) (rest : List Collection),
      pullCollection c cols = some (x, rest) →
        escrowSum cols = escrowOf x + escrowSum rest := by
  intro cols
  induction cols with
  | nil => intro x rest h; exact Option.noConfusion h
  | cons z t ih =>
    intro x rest h
    rw [pullCollection] at h
    split at h
    · next hz =>
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      exact escrowSum_cons ..
    · next hz =>
      cases hx : pullCollection c t with
      | none => rw [hx] at h; exact Option.noConfusion h
      | some w =>
        obtain ⟨y', rest'⟩ := w
        rw [hx] at h
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        have hs : escrowSum t = escrowOf y' + escrowSum rest' := ih y' rest' hx
        rw [escrowSum_cons, ← h.1, ← h.2, escrowSum_cons, hs]
        omega

theorem pullCollection_sum {c : CollId} {cols : List Collection} {x : Collection}
    {rest : List Collection} (h : pullCollection c cols = some (x, rest)) :
    escrowSum cols = escrowOf x + escrowSum rest :=
  pullCollection_sum_lemma cols x rest h

private theorem pullCollection_sublist_lemma {c : CollId} :
    ∀ (cols : List Collection) (x : Collection) (rest : List Collection),
      pullCollection c cols = some (x, rest) → ∀ y ∈ rest, y ∈ cols := by
  intro cols
  induction cols with
  | nil => intro x rest h y hy; exact Option.noConfusion h
  | cons z t ih =>
    intro x rest h
    rw [pullCollection] at h
    split at h
    · next hz =>
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      intro y hy
      exact List.mem_cons_of_mem _ hy
    · next hz =>
      cases hx : pullCollection c t with
      | none => rw [hx] at h; exact Option.noConfusion h
      | some w =>
        obtain ⟨y', rest'⟩ := w
        rw [hx] at h
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        rw [← h.2]
        intro y hy
        rcases List.mem_cons.mp hy with hc | hc
        · exact List.mem_cons.mpr (Or.inl hc)
        · exact List.mem_cons_of_mem _ (ih y' rest' hx y hc)

theorem pullCollection_sublist {c : CollId} {cols : List Collection} {x : Collection}
    {rest : List Collection} (h : pullCollection c cols = some (x, rest)) :
    ∀ y ∈ rest, y ∈ cols :=
  pullCollection_sublist_lemma cols x rest h

theorem stripCollections_sum (r : UserId) (cols : List Collection) :
    sumPledges (stripCollections r cols).2 + escrowSum (stripCollections r cols).1
      = escrowSum cols := by
  induction cols with
  | nil => rfl
  | cons c t ih =>
    simp only [stripCollections]
    split
    · next h =>
      dsimp only
      rw [escrowSum_cons, ← ih, sumPledges_append, sumPledges_append]
      simp only [escrowOf]
      omega
    · next h =>
      dsimp only
      rw [escrowSum_cons, escrowSum_cons, ← ih]
      simp only [escrowOf]
      omega
