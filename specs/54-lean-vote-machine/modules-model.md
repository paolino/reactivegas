# Modules model — #54

Responsibility, dependency direction, and placement only. No bodies, signatures,
or algorithms — see `functions-model.md` and `data-model.md`.

## Dependency direction (the ticket's architectural core)

```text
lean/KelGroups/     substrate. Depends on Lean core ONLY.
        ▲
        │ permitted (Slice 2)
        │
lean/Reactivegas/   application. May depend on KelGroups. Nothing flows back.
```

`lean/KelGroups/` is written to be lifted verbatim into `paolino/kelgroups`.
Every dependency it acquires is a cost paid later, in the operator's words, in
blood. Nothing in it may name a Reactivegas concept — not in a type, not in a
theorem statement, not in a comment that would become false after the port.

The application dimension enters as an **abstract type parameter**, never an
import. That is why R-2 is structurally true and not merely policed.

## New modules (Slice 1)

| Module | Responsibility | May depend on |
|---|---|---|
| `KelGroups.Types` | member/role/admin vocabulary and identifiers | Lean core |
| `KelGroups.Event` | the proposal and group-event vocabulary | `KelGroups.Types` |
| `KelGroups.State` | group condition, pending proposals, admin counting, threshold, auth mode | `KelGroups.Types`, `KelGroups.Event` |
| `KelGroups.Fold` | the transition semantics: propose, approve, enactment attempt, enactment payloads, application-event folding | `KelGroups.State` |
| `KelGroups.Validate` | pre-append validation and its error vocabulary | `KelGroups.State` |
| `KelGroups.Invariants` | VI-1..VI-5 as theorems; VI-6/VI-7 as executed counterexamples | `KelGroups.Fold`, `KelGroups.Validate` |
| `KelGroups.Tests` | the point tests of R-6..R-24 | `KelGroups.Fold`, `KelGroups.Validate` |
| `KelGroups` (root) | imports every module above so `lake build` reaches all of them | all of the above |

The split mirrors the Haskell module split one-to-one. That is deliberate: the
port is a file-per-file correspondence, and a Lean-side reorganisation — however
tidier — makes it a rewrite. `Bootstrap.hs` is the single exception: its two
declarations are folded into `KelGroups.State`, because a Lean module holding
one predicate and one enum earns nothing and the fidelity matrix carries the
correspondence explicitly.

## Changed modules (Slice 1)

| Module | Change | Why |
|---|---|---|
| `lean/lakefile.lean` | declare the `KelGroups` library as an additional default target | without it `lake build` compiles none of the new code and CI is vacuously green |

## Not touched in Slice 1

`lean/Reactivegas/**` — the economic machine is #48's and Slice 2's. Slice 1
proves the substrate stands alone; touching the application would defeat the
demonstration.

## Slice 2 placement (specified, not authorized)

Composition lives under `lean/Reactivegas/`. Preferred design is **structural**:
permission events carry enacted-verdict evidence, so an unauthorized permission
event is unrepresentable rather than rejected. This is the same principle the
project already applies at the UI — remove the option, do not refuse the
attempt — moved one level down into the type system. No new top-level module
family is introduced.

## Abstraction promotion

None. Nothing here is promoted to a shared upstream owner: `KelGroups.*` is
already the most upstream module family in this repository and is destined to
leave it entirely.
