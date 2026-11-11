/-
  KelGroups.FoldInvariants — Fold function invariant proofs

  Proves that app events preserve base state, that approve
  of non-existent proposals is a no-op, and that enact
  operations have the expected effect on member count.
-/
import KelGroups.Basic
import KelGroups.Transitions

namespace KelGroups

-- ============================================================
-- App/Base separation
-- ============================================================

/-- Applying an app event preserves the member list. -/
theorem applyEvent_app_preserves_members
    {α : Type} (gs : GroupState) (x : α) :
    (applyEvent gs (.app x)).members = gs.members := by
  simp [applyEvent]

/-- Applying an app event preserves pending proposals. -/
theorem applyEvent_app_preserves_pendingProposals
    {α : Type} (gs : GroupState) (x : α) :
    (applyEvent gs (.app x)).pendingProposals
    = gs.pendingProposals := by
  simp [applyEvent]

-- ============================================================
-- Approve identity
-- ============================================================

/-- Approving a non-existent proposal leaves state
unchanged. -/
theorem applyApprove_nonexistent_is_identity
    (gs : GroupState) (signer : MemberId) (pid : Nat)
    (h : gs.pendingProposals.lookup pid = none) :
    applyApprove gs signer pid = gs := by
  simp [applyApprove, h]

-- ============================================================
-- Enact effects on member count
-- ============================================================

/-- ChangeRoles does not change the number of members. -/
theorem enact_changeRoles_preserves_member_count
    (gs : GroupState) (mid : MemberId) (roles : List Role) :
    (enact gs (.changeRoles mid roles)).members.length
    = gs.members.length := by
  simp [enact, updateMemberRoles, List.length_map]

/-- IntroduceMember increases the member count by 1. -/
theorem enact_introduce_increases_member_count
    (gs : GroupState) (mid : MemberId) (roles : List Role) :
    (enact gs (.introduceMember mid roles)).members.length
    = gs.members.length + 1 := by
  simp [enact, List.length_cons]

end KelGroups
