import Reactivegas.Step

/-!
# The law predicates

One predicate per documented law (issue #45); the prose record in
`docs/design/state-machine.md` references these names.
-/

/-- **L6 Conservation**: `Σ casse − Σ conti − Σ open escrow = 0`. -/
def conservation (s : State) : Prop :=
  sumBal s.casse - sumBal s.conti - escrowSum s.collections = 0

/-- **L7 Solvency**: no account ever goes below zero, and every pledged
amount stays non-negative so refunds can never push anyone under.
Guards in `step` reject any debit that would overdraw; `insolvent` is
therefore unreachable from the boot state (`not_insolvent_of_reach`). -/
def solvent (s : State) : Prop :=
  (∀ u : UserId, bal s.conti u ≥ 0) ∧
  (∀ col ∈ s.collections, ∀ p ∈ col.accepted ++ col.pending, 0 ≤ p.amount)

/-- A negative credit balance. Unreachable from boot since solvency is
enforced; kept as the reported-shape definition for tooling. -/
def insolvent (s : State) : Prop :=
  ∃ u, List.Mem u s.users ∧ bal s.conti u < 0

/-- **L8 Pledge uniqueness**, one collection at a time:
one pledge per user per collection. -/
def uniquePledges (col : Collection) : Prop :=
  ∀ p ∈ col.accepted ++ col.pending,
    ∀ q ∈ col.accepted ++ col.pending, p.user = q.user → p = q

/-- L8 lifted to the whole state. -/
def allUniquePledges (s : State) : Prop :=
  ∀ col ∈ s.collections, uniquePledges col

/-- **L2 Closure permission**: positive closure requires the group's
assent (`permitted`) *and* zero pending pledges. -/
def permissionToClose (col : Collection) : Prop :=
  col.permitted ∧ col.pending = []

/-- **L3 Escrow at pledge**: user `u` currently has `v` held in the
pending pledges of `col`. -/
def escrowHeld (col : Collection) (u : UserId) (v : Int) : Prop :=
  ∃ pend, splitUser u col.pending = some (v, pend)

/-- **L1 Governance enacts**: after enacting the removal of `u`, no open
question (collection) is left with `u` as referente. -/
def governanceEnacts (u : UserId) (s' : State) : Prop :=
  ∀ c ∈ s'.collections, c.referente ≠ u

/-- **L5 Double entry**: a deposit or withdrawal of `v` moves user `u`'s
conto and cashier `a`'s cassa together. -/
def doubleEntry (s s' : State) (a u : UserId) (v : Int) : Prop :=
  bal s'.conti u = bal s.conti u + v ∧ bal s'.casse a = bal s.casse a + v

/-- **AUTH**: every declaration is authored by an elected responsabile.
Stated as the property of any successful step. -/
def authorizedStep (s : State) (e : Event) (_s' : State) : Prop :=
  match e with
  | .addUser a _ | .electResponsabile a _ | .removeResponsabile a _
  | .openPurchase a _ | .grantPermission a _ | .denyPermission a _
  | .deposit a _ _ | .withdraw a _ _ | .transferCassa a _ _
  | .pledge a _ _ _ | .acceptPledge a _ _ | .refusePledge a _ _
  | .correctPledge a _ _ _ | .closePurchase a _ | .failPurchase a _ =>
    isResponsabile s a

/-- Reachability from the boot state through successful steps. -/
inductive Reach : State → Prop where
  | boot (r : UserId) : Reach (State.init r)
  | trans {s : State} {e : Event} {s' : State} :
    Reach s → step s e = some s' → Reach s'
