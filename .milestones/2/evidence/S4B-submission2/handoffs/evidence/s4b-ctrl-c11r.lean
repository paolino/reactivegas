-- S4-B C11 re-establishment (v2 Amendment 1): isolated scratch, control-only.
-- Mutated PROP-RELATUM def (conjunction weakened to disjunction; well-typed
-- Prop) under the production name; production files untouched (Predicates NOT
-- imported, so no clash). The theorem below is byte-identical in name,
-- statement and proof to the production `permissionToClose_corr`. Expect: the
-- DEF elaborates, the ORIGINAL-NAMED theorem fails.
-- Accurate-label note: the production proof is an exhaustive case split over
-- the expression's value shapes (perm × pending), hence vacuous to mutations
-- of the operative projection/core definitions; the demonstrated sensitivity is
-- to a defect in the Prop relatum.
import Reactivegas.State

def permissionToClose (col : Collection) : Prop :=
  col.permitted ∨ col.pending = []

theorem permissionToClose_corr (col : Collection) :
    permissionToClose col ↔ ((col.permitted && col.pending.isEmpty) = true) := by
  obtain ⟨id, ref, perm, acc, pend⟩ := col
  cases perm <;> cases pend <;> simp [permissionToClose]
