import Reactivegas.Step

/-!
# Observed production refusals (#66 S5)

`Reactivegas.apply` returns `comuneReserved` for two reasons, and these theorems
state both: the aggregate it is given already lists the reserved comune key, or
the integrated step produces an aggregate that does. Weakening either check
falsifies the theorem.
-/

namespace Reactivegas

/-- The production root refuses with `comuneReserved` exactly when the aggregate
it receives is not well formed, or the integrated step succeeds into an
aggregate that is not. -/
theorem apply_comuneReserved_iff (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth)
    (gs : KelGroups.GroupState State) (signer : KelGroups.Key)
    (event : KelGroups.IntegratedEvent Proposal AppEvent) :
    apply θ auth gs signer event = .error ProductionError.comuneReserved ↔
      productionWellFormed gs = false ∨
        ∃ result,
          KelGroups.applyIntegratedEvent (integration θ auth) gs signer event = .ok result ∧
            productionWellFormed result.state = false := by
  unfold apply
  cases hg : productionWellFormed gs
  · simp
  · cases hf : KelGroups.applyIntegratedEvent (integration θ auth) gs signer event with
    | error err => simp
    | ok result =>
      cases hr : productionWellFormed result.state <;> simp [hr]

end Reactivegas
