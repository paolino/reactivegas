# Modules model — #57 structural vote validation

Artifact ceiling: 80 lines / 6 KiB. Responsibility and dependency decisions
only; signatures live in `functions-model.md`.

## Changed responsibilities

| Module | Responsibility in this re-cut | Must not own |
|---|---|---|
| `KelGroups.Vote.Validate` | the one total, exhaustive signer/event authorization decision and existing event-specific admissibility errors | event effects, state mutation, constructor side registries, policy defaults |
| `KelGroups.Vote.Fold` | the sole production boundary that admits an event before both effect and sweep; exact state identity on rejection | duplicated authorization or event-local R-45 guards |
| `KelGroups.Vote.Invariants` | universal rejection identity, universal non-responsabile identity after bootstrap, semantic no-expiry, and inherited production-fold invariants | production definitions or constructor-whitelist proof premises |
| `KelGroups.Vote.Tests` | production-reachable value controls for all current constructors and retained R-45/no-expiry witnesses | definitions imported by production modules |

## Unchanged owners

- `KelGroups.Vote.Event` remains the closed event vocabulary owner.
- `KelGroups.Vote.State` remains the state and sole verdict-site owner.
- `KelGroups.Vote.Types` remains the threshold/verdict/question vocabulary
  owner.
- `KelGroups.lean` already roots every Vote module and remains unchanged.

## Dependency direction

`KelGroups.Vote` may use only Lean core and the existing `KelGroups.Types`
substrate. Nothing under `lean/KelGroups/` imports `Reactivegas.*`; nothing in
this re-cut reaches `KelGroups.Fold`, `KelGroups.Validate`, or
`KelGroups.Invariants` from the frozen faithful machine.

## Promotion

No abstraction is promoted. Authorization and vote-state preservation belong
to the required vote machine; moving either into the shared faithful substrate
would invalidate its frozen fidelity boundary.

