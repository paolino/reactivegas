# Modules model — #48 inversion coverage amendment

Artifact ceiling: 70 lines / 6 KiB. Responsibility decisions only; signatures
live in `functions-model.md`.

## Changed responsibilities

| Module/artifact | Responsibility in this amendment | Must not own |
|---|---|---|
| `Reactivegas.Invariants` | successful-step inversion theorems for all current economic events | event vocabulary, transition policy, membership, vote, or composition behavior |
| inversion-coverage checker | derive the declared event surface and covered theorem-event surface; reject gaps and prove detector failure | a copied constructor registry, proof bodies, or product policy |
| Lean gate recipe | execute the permanent checker as part of the existing Lean verification boundary | new toolchain selection or dependency policy |

## Unchanged owners

- `Reactivegas.Types` remains the sole economic event vocabulary owner.
- `Reactivegas.Step` remains the sole guard and transition owner.
- `Reactivegas.Composition` remains the route/policy owner.
- `KelGroups` remains the sole membership, role, vote, and base-transition
  substrate owner.

## Dependency direction

The proof module continues to depend on the existing Reactivegas state and step
surface. The coverage checker observes source declarations and theorem
statements; production Lean never depends on the checker. No abstraction is
promoted.
