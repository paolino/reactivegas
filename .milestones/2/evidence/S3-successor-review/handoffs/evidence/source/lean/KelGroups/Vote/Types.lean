import KelGroups.Types

/-!
# Required vote machine — vocabulary

This subtree is `REQUIRED-OF-SUBSTRATE`, not `FAITHFUL`: it specifies what
`kelgroups#28`/`#30` must become, proved as a model over the production fold.
No `FAITHFUL` claim is made about it and none may be.

The only shared upstream is `KelGroups.Types` (association lists, `Member`,
`Role`, `Admin`, `hasAdmin`), so "responsabile" is the same notion in both
machines by construction. Nothing here imports a faithful-machine transition
module.

Vocabulary only: no state, no transition, and no policy choice presented as
*the* policy. The threshold is a parameter everywhere (R-46); the two named
instances below are exhibits, not defaults.
-/

namespace KelGroups.Vote

abbrev QuestionId := String

/-! ## Verdict (R-49) -/

/-- Exactly three outcomes. `open` is legacy `Indecidibile` and is a distinct
constructor: never negative plus a flag, never an `Option` of a two-valued
type. -/
inductive Verdict where
  | positive
  | negative
  | open
deriving DecidableEq, BEq, Repr

/-! ## Threshold policy (R-46, R-47) -/

/-- A threshold policy maps the current responsabile count to the required
count. Every verdict evaluation takes one explicitly; nothing in the machine,
the state, or any invariant hard-codes a policy. -/
abbrev Threshold := Nat → Nat

/-- Named instance reproducing legacy `maggioranza`'s `(n+1) div 2` arm. A
named exhibit, not a default (R-47). -/
def legacyThreshold (responsabili : Nat) : Nat := (responsabili + 1) / 2

/-- Named instance reproducing legacy `maggioranza`'s `i == 0` arm: a constant
zero. A named exhibit, not a default (R-47). -/
def zeroThreshold (_ : Nat) : Nat := 0

/-! ## Ballot (V-4) -/

/-- The two positions a responsabile can record. -/
inductive Ballot where
  | assent
  | dissent
deriving DecidableEq, BEq, Repr

/-! ## Question kind (R-62) -/

/-- A question is either collective (tallied against the threshold) or a
permission addressed to exactly one designee. The designee is part of the
kind, so a permission question without a designee is not representable. -/
inductive QuestionKind where
  | collective
  | permission (designee : Key)
deriving DecidableEq, BEq, Repr

/-! ## Closure cause (R-55) -/

/-- Why a question left the open set. Slice A produces `tally` and
`franchiseChange`; `proposerDeparted` and `renounced` are carried from Slice A
so Slice B extends rather than redesigns (R-58/R-59 arrive there). -/
inductive ClosureCause where
  | tally
  | franchiseChange
  | proposerDeparted
  | renounced
deriving DecidableEq, BEq, Repr

end KelGroups.Vote
