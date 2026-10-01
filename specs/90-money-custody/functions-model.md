# #90 signature model

Only public new signatures are fixed here; local helper organization belongs
to the implementer. Data types and constraints are defined in data-model.md.

| ID | Module | Name and named arguments | Result / effect |
|---|---|---|---|
| F90-CORE | Reactivegas.Economic.Core | step (queries :: Queries) (state :: State frame) (signer :: Key) (event :: CustodyEvent) | Maybe (State frame); pure, exact selected Lean semantics, preserves untouched frame |
| F90-VIEW | Reactivegas.Economic.KelGroups | queriesFromView (view :: GroupView) | Queries; read-only accepted substrate queries |
| F90-ADAPTER | Reactivegas.Economic.KelGroups | stepInView (view :: GroupView) (state :: State frame) (signer :: Key) (event :: CustodyEvent) | Maybe (State frame); production GroupView binding, no duplicated transition policy |

Exports include the model's required constructors/fields, Key and step in Core,
and queriesFromView/stepInView in the adapter. Extra public APIs require a
contract challenge; private helpers do not.
