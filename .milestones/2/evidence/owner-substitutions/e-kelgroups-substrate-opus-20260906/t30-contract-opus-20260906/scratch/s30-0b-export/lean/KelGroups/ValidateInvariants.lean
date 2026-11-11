/-
  KelGroups.ValidateInvariants — Validation invariant proofs

  Proves that the validation functions accept/reject events
  as specified. Covers bootstrap mode constraints, admin
  requirements, and approval checks.
-/
import KelGroups.Basic
import KelGroups.Validate

namespace KelGroups

-- ============================================================
-- Bootstrap mode theorems
-- ============================================================

/-- IntroduceMember with admin role passes bootstrap. -/
theorem bootstrap_accepts_admin_intro
    (mid : MemberId) (roles : List Role)
    (h : hasAdmin roles = true) :
    validateBootstrapProposal (.introduceMember mid roles)
    = none := by
  simp [validateBootstrapProposal, h]

/-- IntroduceMember without admin role fails bootstrap. -/
theorem bootstrap_rejects_nonadmin_intro
    (mid : MemberId) (roles : List Role)
    (h : hasAdmin roles = false) :
    validateBootstrapProposal (.introduceMember mid roles)
    = some .bootstrapRequiresAdmin := by
  simp [validateBootstrapProposal, h]

/-- RemoveMember always fails bootstrap. -/
theorem bootstrap_rejects_remove (mid : MemberId) :
    validateBootstrapProposal (.removeMember mid)
    = some .bootstrapRequiresAdmin := by
  simp [validateBootstrapProposal]

/-- ChangeRoles always fails bootstrap. -/
theorem bootstrap_rejects_changeRoles
    (mid : MemberId) (roles : List Role) :
    validateBootstrapProposal (.changeRoles mid roles)
    = some .bootstrapRequiresAdmin := by
  simp [validateBootstrapProposal]

-- ============================================================
-- Normal mode: authentication theorems
-- ============================================================

/-- Non-admin signer is rejected for proposals in
normal mode. -/
theorem normal_proposal_requires_admin
    (gs : GroupState) (signer : MemberId) (p : Proposal)
    (hmode : authMode gs = .normal)
    (hadmin : isAdmin signer gs = false) :
    validateProposal gs signer p
    = some (.notAnAdmin signer) := by
  simp [validateProposal, hmode, validateNormalProposal, hadmin]

/-- Introducing an existing member is rejected. -/
theorem normal_introduce_existing_rejected
    (gs : GroupState) (signer mid : MemberId)
    (roles : List Role)
    (hmode : authMode gs = .normal)
    (hadmin : isAdmin signer gs = true)
    (hexists : isMember mid gs = true) :
    validateProposal gs signer (.introduceMember mid roles)
    = some (.memberAlreadyExists mid) := by
  simp [validateProposal, hmode, validateNormalProposal,
    hadmin, hexists]

/-- Removing a non-member is rejected. -/
theorem normal_remove_nonmember_rejected
    (gs : GroupState) (signer mid : MemberId)
    (hmode : authMode gs = .normal)
    (hadmin : isAdmin signer gs = true)
    (hmissing : isMember mid gs = false) :
    validateProposal gs signer (.removeMember mid)
    = some (.memberNotFound mid) := by
  simp [validateProposal, hmode, validateNormalProposal,
    hadmin, hmissing]

-- ============================================================
-- Normal mode: approval theorems
-- ============================================================

/-- Approving a non-existent proposal is rejected. -/
theorem normal_approve_missing_rejected
    (gs : GroupState) (signer : MemberId) (pid : Nat)
    (hadmin : isAdmin signer gs = true)
    (hmissing : gs.pendingProposals.lookup pid = none) :
    validateApproval gs signer pid
    = some (.proposalNotFound pid) := by
  simp [validateApproval, hadmin, hmissing]

/-- Double approval is rejected. -/
theorem normal_double_approve_rejected
    (gs : GroupState) (signer : MemberId) (pid : Nat)
    (pp : PendingProposal)
    (hadmin : isAdmin signer gs = true)
    (hfound : gs.pendingProposals.lookup pid = some pp)
    (happroved : pp.approvals.any (· == signer) = true) :
    validateApproval gs signer pid
    = some (.alreadyApproved signer pid) := by
  simp [validateApproval, hadmin, hfound, happroved]

/-- Admin with valid proposal setup is accepted. -/
theorem normal_valid_admin_proposal_accepted
    (gs : GroupState) (signer : MemberId) (mid : MemberId)
    (roles : List Role)
    (hmode : authMode gs = .normal)
    (hadmin : isAdmin signer gs = true)
    (hfresh : isMember mid gs = false) :
    validateProposal gs signer (.introduceMember mid roles)
    = none := by
  simp [validateProposal, hmode, validateNormalProposal,
    hadmin, hfresh]

end KelGroups
