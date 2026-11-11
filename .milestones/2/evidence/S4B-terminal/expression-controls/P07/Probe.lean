import Reactivegas.Predicates
theorem permissionToClose_corr (col : Collection) :
    permissionToClose col ↔ ((col.permitted && col.pending.isEmpty) = true) := by
  obtain ⟨id, ref, perm, acc, pend⟩ := col
  cases perm <;> cases pend <;> simp [permissionToClose]


def auditTheoremlessB (s : State) : Bool := decide (s.conti = [])

#eval (stepEvent {members := [("a", ⟨"a","",[.adminRole .publicAdmin]⟩)]} {State.empty with collections := [⟨1,"a",false,[],[]⟩]} (.closePurchase "a" 1) (fun _ _ => false)).isSome
#check permissionToClose_corr
