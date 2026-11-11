import Reactivegas

/- Deliberately false claim. The run is valid only when Lean rejects it
   because the shipped member-writing transition mutant changed members. -/
theorem auditor_false_member_preservation :
    (match Reactivegas.memberWritingApply with
      | .ok result =>
          result.state.members == Reactivegas.preservationGroup.members
      | .error _ => true) = true := by decide
