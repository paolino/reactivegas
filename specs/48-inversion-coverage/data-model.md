# Data model — #48 inversion coverage amendment

Artifact ceiling: 60 lines / 5 KiB.

## Production data

No production data changes. `Reactivegas.Event` remains the closed fourteen-
constructor economic surface at frozen base `934de7a8`. `State`, `AppEvent`,
`BackdonateAuth`, membership, roles, votes, and composition are unchanged.

## Proof coverage relation

Coverage relates:

- every constructor derived from the `Reactivegas.Event` declaration; and
- every event constructor appearing in the successful `stepEvent` hypothesis
  of a recognized inversion theorem.

The relation is complete exactly when the two derived sets are equal. Duplicate
coverage and theorem references to constructors outside `Event` are reported,
not silently normalized into success.

## State invariants

- **D48-I1 vocabulary fidelity:** the coverage source is the actual `Event`
  declaration, not a second registry.
- **D48-I2 theorem fidelity:** coverage identity comes from the theorem's
  successful-step hypothesis, not its theorem name.
- **D48-I3 inversion fidelity:** success exposes the admitted branch guard and
  the exact successor state.
- **D48-I4 detector reachability:** the ordinary Lean gate executes the check
  and its negative control.
