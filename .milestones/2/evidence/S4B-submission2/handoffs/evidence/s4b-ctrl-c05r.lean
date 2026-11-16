-- S4-B C5 re-establishment (v2 Amendment 1): isolated scratch, control-only.
-- Mutated PROP-RELATUM def (negation dropped; well-typed Prop) under the
-- production name; production files untouched (Predicates NOT imported, so no
-- clash). The theorem below is byte-identical in name, statement and proof to
-- the production `comune_not_a_member_corr`. Expect: the DEF elaborates, the
-- ORIGINAL-NAMED theorem fails.
-- Accurate-label note: the production proof (`cases isMember <;> simp`) is a
-- truth table parametric in the reused expression's VALUE, hence vacuous to
-- body-mutations of `isMember`; the demonstrated sensitivity is to a defect in
-- the Prop relatum, which is what this control evidences (not a claim about
-- expression-body sensitivity).
import KelGroups.Types
import Reactivegas.Types

def comune_not_a_member (view : KelGroups.GroupView) : Prop :=
  KelGroups.GroupView.isMember comuneId view

theorem comune_not_a_member_corr (view : KelGroups.GroupView) :
    comune_not_a_member view ↔ ((!KelGroups.GroupView.isMember comuneId view) = true) := by
  unfold comune_not_a_member
  cases KelGroups.GroupView.isMember comuneId view <;> simp
