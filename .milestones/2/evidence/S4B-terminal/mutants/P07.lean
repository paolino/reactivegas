import Reactivegas.Step
-- Definitional relatum control only; not executable-expression-body strength.
def permissionToClose (col : Collection) : Prop := col.permitted ∨ col.pending = []

theorem permissionToClose_corr (col : Collection) :
    permissionToClose col ↔ ((col.permitted && col.pending.isEmpty) = true) := by
  obtain ⟨id, ref, perm, acc, pend⟩ := col
  cases perm <;> cases pend <;> simp [permissionToClose]

