import Reactivegas.Invariants

/-- U-CHECK subject (SS-4 frozen): an isolated ELABORATION of a precise
proposition using the fully-qualified name. This is a proof-elaboration
obligation, not `#eval` and not a runtime replay: the timer measures
elaborating `by decide` for this exact statement, nothing else.
Uses Lean 4.25.0 pinned configuration (lean-toolchain:
`leanprover/lean4:v4.25.0`). No `#eval`, no unqualified name, no ellipsis. -/
theorem ss_check_elaboration : Reactivegas.checkSweepIdempotent = true := by decide
