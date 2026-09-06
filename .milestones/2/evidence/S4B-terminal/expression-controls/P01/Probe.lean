import Reactivegas.Predicates
theorem comune_not_a_member_corr (view : KelGroups.GroupView) :
    comune_not_a_member view ↔ ((!KelGroups.GroupView.isMember comuneId view) = true) := by
  unfold comune_not_a_member
  cases KelGroups.GroupView.isMember comuneId view <;> simp


def auditTheoremlessB (s : State) : Bool := decide (s.conti = [])

#eval KelGroups.GroupView.isMember "u" {members := [("u", ⟨"u","",[]⟩)]}
#check comune_not_a_member_corr
