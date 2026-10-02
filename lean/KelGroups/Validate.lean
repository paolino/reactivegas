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

end KelGroups
