import KelGroups.State

namespace KelGroups

variable {α : Type}

inductive ValidationError where
  | notAMember (key : Key)
  | notAnAdmin (key : Key)
  | bootstrapRequiresAdmin
  | memberAlreadyExists (key : Key)
  | memberNotFound (key : Key)
  | proposalNotFound (proposalId : ProposalId)
  | alreadyApproved (key : Key) (proposalId : ProposalId)
  | proposerSelfApproval (key : Key) (proposalId : ProposalId)
  | roleAddPrecondition (name : RoleName)
  | roleRemovePrecondition (name : RoleName)
  | invalidKey (key : Key)
  | reservedKey (key : Key)
deriving DecidableEq, BEq, Repr

instance : BEq (Except ValidationError Unit) where
  beq
    | .ok (), .ok () => true
    | .error left, .error right => left == right
    | _, _ => false

/-- Admin gate shared by the approval validators. Public (not
`private`) so the V-2 preservation proofs can invert successful
validations; behavior unchanged. -/
def requireAdmin (signer : Key) (gs : GroupState α) :
    Except ValidationError Unit :=
  if isAdmin signer gs then .ok () else .error (.notAnAdmin signer)

private def requireMember (key : Key) (gs : GroupState α) :
    Except ValidationError Unit :=
  if isMember key gs then .ok () else .error (.memberNotFound key)

private def requireNotMember (key : Key) (gs : GroupState α) :
    Except ValidationError Unit :=
  if isMember key gs then .error (.memberAlreadyExists key) else .ok ()

private def requireValidKey (validKey : Key → Bool) (key : Key) :
    Except ValidationError Unit :=
  if validKey key then .ok () else .error (.invalidKey key)

private def checkRoleAddition (config : GroupConfig α) (gs : GroupState α) :
    Role → Except ValidationError Unit
  | .adminRole _ => .ok ()
  | .appRole name =>
      match assocLookup name config.roleDefs with
      | none => .ok ()
      | some roleDef =>
          if roleDef.canAdd gs.appFold then .ok () else .error (.roleAddPrecondition name)

private def checkRoleRemoval (config : GroupConfig α) (gs : GroupState α) :
    Role → Except ValidationError Unit
  | .adminRole _ => .ok ()
  | .appRole name =>
      match assocLookup name config.roleDefs with
      | none => .ok ()
      | some roleDef =>
          if roleDef.canRemove gs.appFold then .ok ()
          else .error (.roleRemovePrecondition name)

private def validateRoleAdditions (config : GroupConfig α) (gs : GroupState α) :
    List Role → Except ValidationError Unit
  | [] => .ok ()
  | role :: rest => do
      checkRoleAddition config gs role
      validateRoleAdditions config gs rest

private def roleDifference (left right : List Role) : List Role :=
  left.filter fun role => !(right.contains role)

private def validateRoleRemovals (config : GroupConfig α) (gs : GroupState α) :
    List Role → Except ValidationError Unit
  | [] => .ok ()
  | role :: rest => do
      checkRoleRemoval config gs role
      validateRoleRemovals config gs rest

private def validateRoleChanges (config : GroupConfig α) (gs : GroupState α)
    (key : Key) (newRoles : List Role) : Except ValidationError Unit := do
  -- Faithful redundant lookup: the caller has already required this member.
  let member ← match lookupMember key gs with
    | none => .error (.memberNotFound key)
    | some member => .ok member
  validateRoleRemovals config gs (roleDifference member.roles newRoles)
  validateRoleAdditions config gs (roleDifference newRoles member.roles)

private def validateBootstrapProposal (validKey : Key → Bool) :
    Proposal → Except ValidationError Unit
  | .introduceMember key _ roles => do
      requireValidKey validKey key
      if hasAdmin roles then .ok () else .error .bootstrapRequiresAdmin
  | .removeMember _ => .error .bootstrapRequiresAdmin
  | .changeRoles _ _ => .error .bootstrapRequiresAdmin

private def validateNormalProposal (validKey : Key → Bool) (config : GroupConfig α)
    (gs : GroupState α) : Proposal → Except ValidationError Unit
  | .introduceMember key _ roles => do
      requireValidKey validKey key
      requireNotMember key gs
      validateRoleAdditions config gs roles
  | .removeMember key => requireMember key gs
  | .changeRoles key roles => do
      requireMember key gs
      validateRoleChanges config gs key roles

def validateProposal (validKey : Key → Bool) (config : GroupConfig α)
    (gs : GroupState α) (signer : Key) (proposal : Proposal) :
    Except ValidationError Unit :=
  match authMode gs with
  | .bootstrap => validateBootstrapProposal validKey proposal
  | .normal => do
      requireAdmin signer gs
      validateNormalProposal validKey config gs proposal

def validateApproval (gs : GroupState α) (signer : Key)
    (proposalId : ProposalId) : Except ValidationError Unit := do
  requireAdmin signer gs
  match lookupPending proposalId gs with
  | none => .error (.proposalNotFound proposalId)
  | some pending =>
      -- V-2/A-001: above one current admin the proposer's own approval is
      -- refused at the boundary under its own identity, never `alreadyApproved`
      -- (different meaning: barred by proposer identity, not duplication).
      -- The sole admin's separate approval passes (no threshold special case).
      if signer == pending.proposer && decide (1 < adminCount gs) then
        .error (.proposerSelfApproval signer proposalId)
      else if pending.approvals.contains signer then
        .error (.alreadyApproved signer proposalId)
      else .ok ()


/-! ## The integrated base validators (R62-06, R62-07)

These decide the *integrated* production routes.  `validateProposal` above
stays with the historical fold.

There is no bootstrap arm here.  A group with no admin admits nobody: the
founding admin arrives through the application's already-guarded initial
aggregate, never through a self-admitting first event.  That is what closes
the empty-group bypass the vote machine used to carry.
-/

/-- The sole member-insertion validator.  Three guards, in a fixed order so the
refusal identity is exact: a non-admin signer is `notAnAdmin`, the reserved key
is `reservedKey` — distinguishable from "already a member" because it is
checked first — and an existing key is `memberAlreadyExists`. -/
def validateDirectAdmission (reserved : Key) (gs : GroupState α)
    (signer target : Key) (_email : Email) (_roles : List Role) :
    Except ValidationError Unit :=
  if isAdmin signer gs then
    if target = reserved then .error (.reservedKey target)
    else if isMember target gs then .error (.memberAlreadyExists target)
    else .ok ()
  else .error (.notAnAdmin signer)

/-- Admissibility of a voted base mutation.  Exhaustive over `BaseMutation`:
an added constructor stops this compiling rather than acquiring a default. -/
def validateBaseMutation (gs : GroupState α) (signer : Key) :
    BaseMutation → Except ValidationError Unit
  | .removeMember key => do
      requireAdmin signer gs
      requireMember key gs
  | .changeRoles key _ => do
      requireAdmin signer gs
      requireMember key gs

/-- Admissibility of an approval of a pending base mutation. -/
def validateBaseApproval (gs : GroupState α) (signer : Key)
    (proposalId : ProposalId) : Except ValidationError Unit := do
  requireAdmin signer gs
  match lookupPendingBase proposalId gs with
  | none => .error (.proposalNotFound proposalId)
  | some pending =>
      -- V-2/A-001 integrated bar: same identity rule as `validateApproval`.
      if signer == pending.proposer && decide (1 < adminCount gs) then
        .error (.proposerSelfApproval signer proposalId)
      else if pending.approvals.contains signer then
        .error (.alreadyApproved signer proposalId)
      else .ok ()

def validateBase (validKey : Key → Bool) (config : GroupConfig α)
    (gs : GroupState α) (signer : Key) (event : BaseEvent) :
    Except ValidationError Unit :=
  match event with
  | .propose proposal => validateProposal validKey config gs signer proposal
  | .approve proposalId => validateApproval gs signer proposalId

def validateEvent (validKey : Key → Bool) (config : GroupConfig α)
    (gs : GroupState α) (signer : Key) (event : GroupEvent α) :
    Except ValidationError Unit :=
  match event with
  | .base baseEvent => validateBase validKey config gs signer baseEvent
  | .app _ => if isMember signer gs then .ok () else .error (.notAMember signer)

/-! ## Refusal observations (#66 S5)

Each theorem below says that a public entry point returns one refusal exactly
when the guard that emits it fails, in the precedence the definitions fix:
administrator, then key, then membership, then role preconditions. Weakening a
guard therefore falsifies a theorem. The guards are `private`, so these
statements sit beside them and name only the public entry points. -/

section RefusalObservations

variable (validKey : Key → Bool) (config : GroupConfig α) (gs : GroupState α)
  (signer : Key)

private theorem validateRoleAdditions_error_shape (roles : List Role) (err : ValidationError)
    (h : validateRoleAdditions config gs roles = .error err) :
    ∃ name, err = .roleAddPrecondition name := by
  induction roles with
  | nil => simp [validateRoleAdditions] at h
  | cons role rest ih =>
    cases hc : checkRoleAddition config gs role with
    | error e =>
      simp [validateRoleAdditions, hc, bind, Except.bind] at h
      cases role with
      | adminRole admin => simp [checkRoleAddition] at hc
      | appRole n =>
        cases hl : assocLookup n config.roleDefs with
        | none => simp [checkRoleAddition, hl] at hc
        | some roleDef =>
          cases hd : roleDef.canAdd gs.appFold <;> simp [checkRoleAddition, hl, hd] at hc
          exact ⟨n, by rw [← h, ← hc]⟩
    | ok v =>
      simp [validateRoleAdditions, hc, bind, Except.bind] at h
      exact ih h

private theorem validateRoleRemovals_error_shape (roles : List Role) (err : ValidationError)
    (h : validateRoleRemovals config gs roles = .error err) :
    ∃ name, err = .roleRemovePrecondition name := by
  induction roles with
  | nil => simp [validateRoleRemovals] at h
  | cons role rest ih =>
    cases hc : checkRoleRemoval config gs role with
    | error e =>
      simp [validateRoleRemovals, hc, bind, Except.bind] at h
      cases role with
      | adminRole admin => simp [checkRoleRemoval] at hc
      | appRole n =>
        cases hl : assocLookup n config.roleDefs with
        | none => simp [checkRoleRemoval, hl] at hc
        | some roleDef =>
          cases hd : roleDef.canRemove gs.appFold <;> simp [checkRoleRemoval, hl, hd] at hc
          exact ⟨n, by rw [← h, ← hc]⟩
    | ok v =>
      simp [validateRoleRemovals, hc, bind, Except.bind] at h
      exact ih h

/-- In a group with no admin, an admission whose key `validKey` rejects is refused
as `invalidKey`, and only then. -/
theorem validateProposal_invalidKey_bootstrap (hb : authMode gs = .bootstrap)
    (key : Key) (email : Email) (roles : List Role) :
    validateProposal validKey config gs signer (.introduceMember key email roles) =
        .error (.invalidKey key) ↔ validKey key = false := by
  cases hk : validKey key <;>
    simp [validateProposal, validateBootstrapProposal, requireValidKey, hb, hk, bind,
      Except.bind]
  split <;> simp

/-- Under the normal rule the same refusal needs an admin signer first. -/
theorem validateProposal_invalidKey_normal (hn : authMode gs = .normal)
    (key : Key) (email : Email) (roles : List Role) :
    validateProposal validKey config gs signer (.introduceMember key email roles) =
        .error (.invalidKey key) ↔ isAdmin signer gs = true ∧ validKey key = false := by
  cases ha : isAdmin signer gs
  · simp [validateProposal, requireAdmin, hn, ha, bind, Except.bind]
  · cases hk : validKey key
    · simp [validateProposal, validateNormalProposal, requireAdmin, requireValidKey, hn, ha, hk,
        bind, Except.bind]
    · cases hm : isMember key gs
      · cases h : validateRoleAdditions config gs roles with
        | error err =>
          obtain ⟨n, rfl⟩ := validateRoleAdditions_error_shape config gs roles err h
          simp [validateProposal, validateNormalProposal, requireAdmin, requireValidKey,
            requireNotMember, hn, ha, hk, hm, h, bind, Except.bind]
        | ok v =>
          simp [validateProposal, validateNormalProposal, requireAdmin, requireValidKey,
            requireNotMember, hn, ha, hk, hm, h, bind, Except.bind]
      · simp [validateProposal, validateNormalProposal, requireAdmin, requireValidKey,
          requireNotMember, hn, ha, hk, hm, bind, Except.bind]

/-- The bootstrap rule refuses an admission with no admin role. -/
theorem validateProposal_bootstrapRequiresAdmin_introduce (hb : authMode gs = .bootstrap)
    (key : Key) (email : Email) (roles : List Role) :
    validateProposal validKey config gs signer (.introduceMember key email roles) =
        .error .bootstrapRequiresAdmin ↔ validKey key = true ∧ hasAdmin roles = false := by
  cases hk : validKey key <;> cases hr : hasAdmin roles <;>
    simp [validateProposal, validateBootstrapProposal, requireValidKey, hb, hk, hr, bind,
      Except.bind]

private theorem validateRoleChanges_error_shape (key : Key) (newRoles : List Role)
    (member : Member) (hm : lookupMember key gs = some member) (err : ValidationError)
    (h : validateRoleChanges config gs key newRoles = .error err) :
    (∃ name, err = .roleRemovePrecondition name) ∨ ∃ name, err = .roleAddPrecondition name := by
  simp only [validateRoleChanges, hm, bind, Except.bind] at h
  cases hr : validateRoleRemovals config gs (roleDifference member.roles newRoles) with
  | error e =>
    rw [hr] at h
    obtain ⟨n, hn⟩ := validateRoleRemovals_error_shape config gs _ e hr
    cases h; exact .inl ⟨n, hn⟩
  | ok v =>
    rw [hr] at h
    exact .inr (validateRoleAdditions_error_shape config gs _ err h)

/-- A voted removal of a key that is not a member is refused as `memberNotFound`
for an admin signer, and only then. -/
theorem validateBaseMutation_memberNotFound_remove (key : Key) :
    validateBaseMutation gs signer (.removeMember key) = .error (.memberNotFound key) ↔
      isAdmin signer gs = true ∧ isMember key gs = false := by
  cases ha : isAdmin signer gs <;> cases hm : isMember key gs <;>
    simp [validateBaseMutation, requireAdmin, requireMember, ha, hm, bind, Except.bind]

/-- A voted role change of a key that is not a member is refused likewise. -/
theorem validateBaseMutation_memberNotFound_changeRoles (key : Key) (roles : List Role) :
    validateBaseMutation gs signer (.changeRoles key roles) = .error (.memberNotFound key) ↔
      isAdmin signer gs = true ∧ isMember key gs = false := by
  cases ha : isAdmin signer gs <;> cases hm : isMember key gs <;>
    simp [validateBaseMutation, requireAdmin, requireMember, ha, hm, bind, Except.bind]

/-- The historical removal proposal, under the normal rule. -/
theorem validateProposal_memberNotFound_remove (hn : authMode gs = .normal) (key : Key) :
    validateProposal validKey config gs signer (.removeMember key) =
        .error (.memberNotFound key) ↔ isAdmin signer gs = true ∧ isMember key gs = false := by
  cases ha : isAdmin signer gs <;> cases hm : isMember key gs <;>
    simp [validateProposal, validateNormalProposal, requireAdmin, requireMember, hn, ha, hm,
      bind, Except.bind]

/-- The historical role-change proposal, under the normal rule. -/
theorem validateProposal_memberNotFound_changeRoles (hn : authMode gs = .normal)
    (key : Key) (roles : List Role) :
    validateProposal validKey config gs signer (.changeRoles key roles) =
        .error (.memberNotFound key) ↔ isAdmin signer gs = true ∧ isMember key gs = false := by
  cases ha : isAdmin signer gs
  · simp [validateProposal, requireAdmin, hn, ha, bind, Except.bind]
  · cases hm : isMember key gs
    · simp [validateProposal, validateNormalProposal, requireAdmin, requireMember, hn, ha, hm,
        bind, Except.bind]
    · have hl : lookupMember key gs ≠ none := by
        intro h; simp [isMember, h] at hm
      obtain ⟨member, hmem⟩ := Option.ne_none_iff_exists'.mp hl
      cases h : validateRoleChanges config gs key roles with
      | error err =>
        rcases validateRoleChanges_error_shape config gs key roles member hmem err h with
          ⟨n, rfl⟩ | ⟨n, rfl⟩ <;>
        simp [validateProposal, validateNormalProposal, requireAdmin, requireMember, hn, ha, hm,
          h, bind, Except.bind]
      | ok v =>
        simp [validateProposal, validateNormalProposal, requireAdmin, requireMember, hn, ha, hm,
          h, bind, Except.bind]


/-- Under the normal rule an admission is refused as `roleAddPrecondition name`
exactly when the signer is an admin, the key is valid and new, and `name` is the
first role whose application definition does not allow adding it. -/
theorem validateProposal_roleAddPrecondition_introduce (hn : authMode gs = .normal)
    (key : Key) (email : Email) (roles : List Role) (name : RoleName) :
    validateProposal validKey config gs signer (.introduceMember key email roles) =
        .error (.roleAddPrecondition name) ↔
      isAdmin signer gs = true ∧ validKey key = true ∧ isMember key gs = false ∧
        roles.find? (fun role => match role with
          | .adminRole _ => false
          | .appRole n => match assocLookup n config.roleDefs with
            | none => false
            | some roleDef => !roleDef.canAdd gs.appFold) = some (.appRole name) := by
  have additions : ∀ rs : List Role,
      validateRoleAdditions config gs rs = .error (.roleAddPrecondition name) ↔
        rs.find? (fun role => match role with
          | .adminRole _ => false
          | .appRole n => match assocLookup n config.roleDefs with
            | none => false
            | some roleDef => !roleDef.canAdd gs.appFold) = some (.appRole name) := by
    intro rs
    induction rs with
    | nil => simp [validateRoleAdditions]
    | cons role rest ih =>
      cases role with
      | adminRole admin =>
        simp [validateRoleAdditions, checkRoleAddition, bind, Except.bind, ih]
      | appRole n =>
        cases hl : assocLookup n config.roleDefs with
        | none => simp [validateRoleAdditions, checkRoleAddition, bind, Except.bind, hl, ih]
        | some roleDef =>
          cases hc : roleDef.canAdd gs.appFold <;>
            simp [validateRoleAdditions, checkRoleAddition, bind, Except.bind, hl, hc, ih,
              eq_comm]
  cases ha : isAdmin signer gs <;> cases hk : validKey key <;> cases hm : isMember key gs <;>
    simp [validateProposal, validateNormalProposal, requireAdmin, requireValidKey,
      requireNotMember, hn, ha, hk, hm, additions, bind, Except.bind]

/-- Under the normal rule a role change is refused as `roleRemovePrecondition name`
exactly when the signer is an admin and `name` is the first removed role whose
application definition does not allow removing it. -/
theorem validateProposal_roleRemovePrecondition_changeRoles (hn : authMode gs = .normal)
    (key : Key) (newRoles : List Role) (member : Member)
    (hm : lookupMember key gs = some member) (name : RoleName) :
    validateProposal validKey config gs signer (.changeRoles key newRoles) =
        .error (.roleRemovePrecondition name) ↔
      isAdmin signer gs = true ∧
        (member.roles.filter (fun role => !newRoles.contains role)).find?
          (fun role => match role with
            | .adminRole _ => false
            | .appRole n => match assocLookup n config.roleDefs with
              | none => false
              | some roleDef => !roleDef.canRemove gs.appFold) = some (.appRole name) := by
  have removals : ∀ (name : RoleName) (rs : List Role),
      validateRoleRemovals config gs rs = .error (.roleRemovePrecondition name) ↔
        rs.find? (fun role => match role with
          | .adminRole _ => false
          | .appRole n => match assocLookup n config.roleDefs with
            | none => false
            | some roleDef => !roleDef.canRemove gs.appFold) = some (.appRole name) := by
    intro name rs
    induction rs with
    | nil => simp [validateRoleRemovals]
    | cons role rest ih =>
      cases role with
      | adminRole admin =>
        simp [validateRoleRemovals, checkRoleRemoval, bind, Except.bind, ih]
      | appRole n =>
        cases hl : assocLookup n config.roleDefs with
        | none => simp [validateRoleRemovals, checkRoleRemoval, bind, Except.bind, hl, ih]
        | some roleDef =>
          cases hc : roleDef.canRemove gs.appFold <;>
            simp [validateRoleRemovals, checkRoleRemoval, bind, Except.bind, hl, hc, ih,
              eq_comm]
  have hmem : isMember key gs = true := by simp [isMember, hm]
  cases ha : isAdmin signer gs
  · simp [validateProposal, requireAdmin, hn, ha, bind, Except.bind]
  · simp only [validateProposal, validateNormalProposal, requireAdmin, requireMember, hn, ha,
      hmem, validateRoleChanges, hm, roleDifference, bind, Except.bind, if_true]
    cases hr : validateRoleRemovals config gs
        (member.roles.filter (fun role => !newRoles.contains role)) with
    | error e =>
      obtain ⟨n, rfl⟩ := validateRoleRemovals_error_shape config gs _ e hr
      have hf := (removals n _).mp hr
      simp [hf, eq_comm]
    | ok v =>
      simp only [true_and]
      constructor
      · intro h
        obtain ⟨n, hn'⟩ := validateRoleAdditions_error_shape config gs _ _ h
        cases hn'
      · intro h
        have := (removals name _).mpr h
        rw [hr] at this
        cases this

end RefusalObservations

end KelGroups
