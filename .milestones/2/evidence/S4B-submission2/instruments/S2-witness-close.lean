-- S2-witness-close.lean — close behaviour witnesses (clean tree).
-- FROZEN INSTRUMENT (submission 2, OT3). Single-file elaboration importing
-- REAL production modules (current oleans; Step/State/Types sources unchanged
-- since the last full build). Every assertion is a `by decide` kernel
-- evaluation that PASSES on the clean tree: an authorized close succeeds with
-- exact collection binding, and the identical unpermitted close is refused.
-- The mutant-direction flip (unpermitted succeeds under the permission-atom
-- mutant) is observed in O4's mandatory-path receipt, never here: this driver
-- establishes the baseline both directions, O4 establishes the defect.
-- Expect: exit 0.
import Reactivegas.Step

def s4bAdm : KelGroups.Member :=
  ⟨"a", "a@x", [KelGroups.Role.adminRole KelGroups.Admin.publicAdmin]⟩
def s4bView : KelGroups.GroupView := ⟨[("a", s4bAdm)]⟩
def s4bColT : Collection := ⟨1, "a", true, [], []⟩
def s4bColF : Collection := ⟨1, "a", false, [], []⟩
def s4bStateT : State := { State.empty with collections := [s4bColT] }
def s4bStateF : State := { State.empty with collections := [s4bColF] }
def s4bAuth : BackdonateAuth := fun _ _ => false

-- exact collection binding on the input side
example : pullCollection 1 s4bStateT.collections = some (s4bColT, []) := by decide
-- authorized close succeeds ...
example : (stepEvent s4bView s4bStateT (.closePurchase "a" 1) s4bAuth).isSome = true := by decide
-- ... and empties exactly that bound collection
example : (stepEvent s4bView s4bStateT (.closePurchase "a" 1) s4bAuth).map
    (fun s' => s'.collections) = some [] := by decide
-- the identical unpermitted close is refused
example : (stepEvent s4bView s4bStateF (.closePurchase "a" 1) s4bAuth).isSome = false := by decide
-- contrast pair: same setups, opposite permission bit
example : (stepEvent s4bView s4bStateT (.closePurchase "a" 1) s4bAuth).isSome = true ∧
    (stepEvent s4bView s4bStateF (.closePurchase "a" 1) s4bAuth).isSome = false := by decide
