# Modules model — #62 one membership and role model

Artifact ceiling: 100 lines / 8 KiB. Responsibilities and dependency direction
only; data relations and signatures live in their own models.

## Changed responsibilities

| Module | Owns | Must not own |
| --- | --- | --- |
| `KelGroups.Types` | substrate `Key`, member/role vocabulary, and read-only canonical group view | Reactivegas economy or vote policy |
| `KelGroups.State` | aggregate group state and the sole writable `members` representation | app-event vocabulary or app-owned membership |
| `KelGroups.Event` | generic integrated direct/base/app event vocabulary and observable base-change vocabulary | Reactivegas-specific admission proposal |
| `KelGroups.Integration` | typed app-fold/base-hook contracts and the sole atomic integrated transition boundary | Reactivegas cleanup policy or an app-writable group result |
| `KelGroups.Validate` | direct-command and base-event admission validation against current canonical state | app effects or post-base hooks |
| `KelGroups.Fold` | frozen historical fold used by the accepted historical theorem | the new Reactivegas production entry point |
| `KelGroups.Vote.State` | membership-free open-question and closure payload; verdict interpretation against an explicit group view | member/role storage or threshold defaults |
| `KelGroups.Vote.Event` | question-open/cast/renounce vocabulary only | member admission, removal, or role changes |
| `KelGroups.Vote.Validate` | exhaustive vote-event authorization against an explicit canonical view | base membership authorization or default constructor handling |
| `KelGroups.Vote.Fold` | vote effects and closure recomputation over vote payload plus canonical view | member effects or a private franchise store |
| `KelGroups.Vote.Invariants` | inherited #54/#57 properties re-bound to integrated production reachability | record-literal substitutes for production evidence |
| `Reactivegas.Types` | economic value types, app event sum, and admission-free Reactivegas base proposal sum | `UserId`, membership fields, signer duplication, or voted admission |
| `Reactivegas.State` | economic accounts/collections plus the membership-free vote payload | users, responsabili, roles, or a key bridge |
| `Reactivegas.Step` | app-fold economic behavior and sealed base-change cleanup/recompute policy | base member mutation or independently signable cleanup events |
| `Reactivegas.Composition` | integrated route classification and new concrete base-transition-to-hook theorem | edits to `baseEnacted_threshold_met` |
| `Reactivegas.Trace` / `TraceTests` | integrated signed-event/state serialization, replay, live inventories, and observable controls | alternate state, identity, or route sources |
| umbrella modules | root every production/proof module in the correct namespace | hidden unimported evidence |

`KelGroups.Integration` may be a new module; if an existing module cleanly owns
the same responsibility, the mandate must be versioned before changing this
placement. No additional responsibility module is implied.

## Dependency direction

`KelGroups` depends only on Lean core and its own substrate/vote modules.
`Reactivegas` depends on `KelGroups`, never conversely. The generic integrated
boundary is policy-free: it invokes supplied typed contracts but contains no
Reactivegas event, account, cleanup, or question-kind knowledge.

The Reactivegas instantiation is the only production owner of its restricted
proposal type and base hook. The historical generic fold remains reachable for
its accepted theorem/tests but is not imported or called by the integrated
Reactivegas production root.

## Promotion decisions

- Promote the immutable member/role projection to `KelGroups.GroupView`
  because both generic app folds and the generic vote machine consume it.
- Promote direct admission and base-change vocabulary to KelGroups because
  they describe substrate transitions, while keep the admission-free proposal
  vocabulary in Reactivegas because structural restriction is app-specific.
- Keep economic cleanup and vote-policy composition in Reactivegas; promoting
  them would invert the substrate/application dependency.
- Keep the old fold surface only as historical evidence. It receives no new
  production responsibility.
