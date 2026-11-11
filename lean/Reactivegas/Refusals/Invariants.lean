import Reactivegas.Step
import Reactivegas.Lifecycle

/-!
# Observed production refusals (#66 S5)

`Reactivegas.apply` returns `comuneReserved` for two reasons, and these theorems
state both: the aggregate it is given already lists the reserved comune key, or
the integrated step produces an aggregate that does. Weakening either check
falsifies the theorem.

The simulation machine builder (`Reactivegas.Lifecycle.rootWith`) refuses with
`comuneReserved` at the same two points; its theorems below state that, and the
bootstrap rule's unconditional refusal of member removal and role changes.
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

namespace Reactivegas.Lifecycle

/-- The simulation machine builder refuses an aggregate that is not well formed
before it runs any event. -/
theorem rootWith_refuses_unwellformed_aggregate (vote : VoteStep) (after : VotesAfter)
    (gs : KelGroups.GroupState State) (signer : KelGroups.Key)
    (event : KelGroups.IntegratedEvent Proposal AppEvent)
    (h : productionWellFormed gs = false) :
    rootWith vote after gs signer event = .error ProductionError.comuneReserved := by
  simp [rootWith, h]

/-- The simulation machine builder succeeds only into a well-formed aggregate,
from a well-formed aggregate. -/
theorem rootWith_ok_is_wellformed (vote : VoteStep) (after : VotesAfter)
    (gs : KelGroups.GroupState State) (signer : KelGroups.Key)
    (event : KelGroups.IntegratedEvent Proposal AppEvent)
    (result : KelGroups.IntegratedResult State)
    (h : rootWith vote after gs signer event = .ok result) :
    productionWellFormed gs = true ∧ productionWellFormed result.state = true := by
  unfold rootWith at h
  cases hg : productionWellFormed gs
  · simp [hg] at h
  · refine ⟨rfl, ?_⟩
    simp only [hg, if_true] at h
    split at h
    · next res hres =>
      cases hw : productionWellFormed res.state
      · simp [hw] at h
      · simp only [hw, if_true, Except.ok.injEq] at h
        subst h
        exact hw
    · simp at h

end Reactivegas.Lifecycle

namespace KelGroups

/-- In bootstrap mode the validator refuses a member removal whoever signs it:
there is no admin yet to vote one. -/
theorem validateProposal_bootstrapRequiresAdmin_remove {α : Type}
    (validKey : Key → Bool) (config : GroupConfig α) (gs : GroupState α)
    (signer key : Key) (hb : authMode gs = .bootstrap) :
    validateProposal validKey config gs signer (.removeMember key) =
      .error .bootstrapRequiresAdmin := by
  simp only [validateProposal, hb]
  rfl

/-- In bootstrap mode the validator refuses a role change whoever signs it. -/
theorem validateProposal_bootstrapRequiresAdmin_changeRoles {α : Type}
    (validKey : Key → Bool) (config : GroupConfig α) (gs : GroupState α)
    (signer key : Key) (roles : List Role) (hb : authMode gs = .bootstrap) :
    validateProposal validKey config gs signer (.changeRoles key roles) =
      .error .bootstrapRequiresAdmin := by
  simp only [validateProposal, hb]
  rfl

end KelGroups
