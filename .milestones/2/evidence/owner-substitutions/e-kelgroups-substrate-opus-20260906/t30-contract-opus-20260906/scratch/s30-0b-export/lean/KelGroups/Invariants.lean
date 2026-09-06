/-
  KelGroups.Invariants — Formal invariants for the group system

  These theorems capture the design invariants documented in
  the design document. If they compile, the invariants hold.
-/
import KelGroups.Basic

namespace KelGroups

-- ============================================================
-- Invariant 1: Bootstrap mode ↔ zero admins
-- ============================================================

theorem bootstrap_iff_zero_admins (gs : GroupState) :
    authMode gs = .bootstrap ↔ adminCount gs = 0 := by
  simp [authMode]

theorem normal_iff_positive_admins (gs : GroupState) :
    authMode gs = .normal ↔ adminCount gs ≠ 0 := by
  simp [authMode]

-- ============================================================
-- Invariant 2: Empty state is in bootstrap mode
-- ============================================================

theorem empty_is_bootstrap :
    authMode emptyState = .bootstrap := by
  simp [authMode, adminCount, emptyState]

-- ============================================================
-- Invariant 3: Majority properties
-- ============================================================

/-- Majority of 0 is 0 (bootstrap: no votes needed). -/
theorem majority_zero : majority 0 = 0 := by rfl

/-- Majority of 1 is 1 (single admin decides alone). -/
theorem majority_one : majority 1 = 1 := by rfl

/-- Majority of 2: (2+1)/2 = 1 in Nat division.
Note: this means with 2 admins, only 1 approval suffices.
If strict majority (both must agree) is desired, the
Haskell implementation should use a different formula. -/
theorem majority_two : majority 2 = 1 := by rfl

/-- Majority of 3 is 2 (2-of-3). -/
theorem majority_three : majority 3 = 2 := by rfl

/-- Majority is always ≤ n. -/
theorem majority_le (n : Nat) : majority n ≤ n := by
  simp [majority]
  omega

/-- Majority of a positive number is positive. -/
theorem majority_pos (n : Nat) (h : n > 0) :
    majority n > 0 := by
  simp [majority]
  omega

-- ============================================================
-- Invariant 4: Admin removal can trigger bootstrap
-- ============================================================

/-- Removing all members yields bootstrap mode. -/
theorem remove_all_triggers_bootstrap :
    authMode { members := [], pendingProposals := [] }
      = .bootstrap := by
  exact empty_is_bootstrap

-- ============================================================
-- Invariant 5: Adding an admin exits bootstrap
-- ============================================================

/-- A state with at least one admin member is in normal mode. -/
theorem admin_member_means_normal
    (m : Member)
    (h : hasAdmin m.roles = true)
    (gs : GroupState)
    (hm : m ∈ gs.members) :
    authMode gs = .normal := by
  simp [authMode, adminCount]
  exact ⟨m, hm, h⟩

end KelGroups
