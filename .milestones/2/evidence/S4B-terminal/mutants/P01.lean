import Reactivegas.Step
-- Definitional relatum control only; not executable-expression-body strength.
def comune_not_a_member (view : KelGroups.GroupView) : Prop := KelGroups.GroupView.isMember comuneId view

theorem comune_not_a_member_corr (view : KelGroups.GroupView) :
    comune_not_a_member view ↔ ((!KelGroups.GroupView.isMember comuneId view) = true) := by
  unfold comune_not_a_member
  cases KelGroups.GroupView.isMember comuneId view <;> simp

