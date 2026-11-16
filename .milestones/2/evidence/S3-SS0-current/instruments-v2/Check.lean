import Reactivegas.Invariants

/-- U-CHECK subject: an isolated ELABORATION of a precise proposition using the
fully-qualified name. This is a proof-elaboration obligation, not `#eval` and not
a runtime replay: the timer measures elaborating `by decide` for this exact
statement, nothing else. -/
theorem ss0_check_elaboration : Reactivegas.checkSweepIdempotent = true := by decide
