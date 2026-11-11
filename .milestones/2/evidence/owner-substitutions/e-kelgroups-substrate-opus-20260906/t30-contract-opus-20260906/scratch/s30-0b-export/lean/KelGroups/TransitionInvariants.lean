/-
  KelGroups.TransitionInvariants — Invariant preservation proofs

  Proves that transition functions preserve the key group
  invariants: bootstrap exit, admin counts, majority thresholds.
-/
import KelGroups.Basic
import KelGroups.Transitions
import KelGroups.Invariants

namespace KelGroups

-- ============================================================
-- Tier 1: Straightforward proofs (simp / unfold)
-- ============================================================

/-- Introducing an admin makes adminCount > 0, exiting
bootstrap mode. -/
theorem enact_introduce_admin_exits_bootstrap
    (gs : GroupState) (mid : MemberId) (roles : List Role)
    (h : hasAdmin roles = true) :
    adminCount (enact gs (.introduceMember mid roles)) > 0 := by
  simp only [enact, adminCount, List.filter, h, List.length_cons]
  omega

/-- Introducing an admin increases adminCount by exactly 1. -/
theorem enact_introduce_admin_count
    (gs : GroupState) (mid : MemberId) (roles : List Role)
    (h : hasAdmin roles = true) :
    adminCount (enact gs (.introduceMember mid roles))
    = adminCount gs + 1 := by
  simp only [enact, adminCount, List.filter, h, List.length_cons]

/-- Introducing a non-admin preserves adminCount. -/
theorem enact_introduce_nonadmin_count
    (gs : GroupState) (mid : MemberId) (roles : List Role)
    (h : hasAdmin roles = false) :
    adminCount (enact gs (.introduceMember mid roles))
    = adminCount gs := by
  simp only [enact, adminCount, List.filter, h]

/-- Enact only touches members, not pending proposals. -/
theorem enact_preserves_pendingProposals
    (gs : GroupState) (p : Proposal) :
    (enact gs p).pendingProposals = gs.pendingProposals := by
  cases p <;> simp [enact]

/-- Folding an empty event list yields emptyState. -/
theorem foldGroup_nil (α : Type)
    : foldGroup (α := α) [] = emptyState := by
  simp [foldGroup]

-- ============================================================
-- Tier 2: Majority + tryEnact reasoning
-- ============================================================

/-- adminCount with different pendingProposals is unchanged. -/
@[simp] theorem adminCount_with_pp
    (gs : GroupState)
    (pps : List (Nat × PendingProposal)) :
    adminCount (GroupState.mk gs.members pps) = adminCount gs := by
  simp [adminCount]

/-- enact only depends on members, not pendingProposals. -/
theorem enact_members_eq
    (gs : GroupState)
    (pps : List (Nat × PendingProposal))
    (p : Proposal) :
    (enact (GroupState.mk gs.members pps) p).members
    = (enact gs p).members := by
  cases p <;> simp [enact]

/-- In bootstrap (zero admins), majority is 0. A proposal's
single auto-approval suffices, so it is immediately enacted.
The resulting pendingProposals is empty. -/
theorem bootstrap_proposal_immediately_enacted
    (prop : Proposal) (signer : MemberId) (pid : Nat) :
    (applyPropose emptyState signer prop pid).pendingProposals
    = [] := by
  simp only [applyPropose, tryEnact, emptyState, adminCount,
    majority, List.lookup, List.filter, hasAdmin, List.length]
  simp [enact_preserves_pendingProposals]

/-- With a single admin, the proposer's auto-approval reaches
majority(1) = 1, so the proposal is enacted immediately.
The result's members match a direct enact. -/
theorem single_admin_proposal_enacted
    (gs : GroupState) (signer : MemberId)
    (prop : Proposal) (pid : Nat)
    (hadmin : adminCount gs = 1) :
    (applyPropose gs signer prop pid).members
    = (enact gs prop).members := by
  simp only [applyPropose, tryEnact, List.lookup,
    adminCount_with_pp, hadmin, majority]
  simp [enact_members_eq]

-- ============================================================
-- Tier 3: List induction — eraseP + filter interaction
-- ============================================================

/-- Removing one element via eraseP decreases the filtered
length by at most 1. -/
theorem filter_eraseP_length_ge {α : Type}
    (l : List α) (p : α → Bool) (f : α → Bool) :
    (l.eraseP p |>.filter f).length
    ≥ (l.filter f).length - 1 := by
  induction l with
  | nil => simp
  | cons x xs ih =>
    simp only [List.eraseP]
    cases hp : p x <;> simp only [cond_false, cond_true] <;>
      simp only [List.filter] <;> cases hf : f x <;>
      simp_all [List.length_cons] <;> omega

/-- adminCount after eraseMember drops by at most 1. -/
theorem adminCount_eraseMember_ge
    (gs : GroupState) (mid : MemberId) :
    adminCount { gs with members := eraseMember mid gs.members }
    ≥ adminCount gs - 1 := by
  simp only [adminCount, eraseMember]
  exact filter_eraseP_length_ge gs.members
    (fun m => m.id == mid) (fun m => hasAdmin m.roles)

/-- If adminCount ≥ 2, removing any member keeps
adminCount ≥ 1 (normal mode is preserved). -/
theorem enact_remove_preserves_normal_if_enough_admins
    (gs : GroupState) (mid : MemberId)
    (h : adminCount gs ≥ 2) :
    adminCount (enact gs (.removeMember mid)) ≥ 1 := by
  simp only [enact]
  have := adminCount_eraseMember_ge gs mid
  omega

end KelGroups
