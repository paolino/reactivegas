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

## Slice 2 placement (authorized option D)

| Module | Responsibility | May depend on | Must not own |
|---|---|---|---|
| `Reactivegas.Composition` | closed 18-way event routing; base production-enactment evidence; app production-verdict evidence; executed provenance witnesses; honest model-status metadata | existing `Reactivegas` types plus `KelGroups` faithful and required vote surfaces | economic transition changes, vote-machine changes, cross-channel identity or correspondence |
| `Reactivegas` (root) | import the composition module so ordinary Lean builds elaborate it | `Reactivegas.Composition` | any composition logic |

The direction remains application → substrate. Every module under
`lean/KelGroups/**` is unchanged and continues to import no `Reactivegas.*`.
No second bridge module or shared abstraction is introduced.

## Abstraction promotion

None. Nothing here is promoted to a shared upstream owner: `KelGroups.*` is
already the most upstream module family in this repository and is destined to
leave it entirely.

---

# Modules — Vote-coverage run (2026-08-29)

## Placement decision

The required machine is a **sibling** of the faithful machine, not a revision
of it. Both sit under the `KelGroups` namespace because the requirement they
express is a requirement *on the substrate* (`kelgroups#28`/`#30`), and both
must lift out of this repository together.

```text
KelGroups.Types                     (existing, frozen — shared vocabulary)
        ▲                    ▲
        │                    │
KelGroups.Event/State/       KelGroups.Vote.Types
Fold/Validate/Invariants/            ▲
Tests   (existing, frozen)           │
   = the FAITHFUL machine     KelGroups.Vote.State
                                     ▲
                              KelGroups.Vote.Event
                                     ▲
                              KelGroups.Vote.Fold
                                     ▲
                       ┌─────────────┴─────────────┐
             KelGroups.Vote.Invariants   KelGroups.Vote.Tests
                     = the REQUIRED machine
```

Nothing points left-to-right or right-to-left between the two machines below
`KelGroups.Types`. Neither namespace imports `Reactivegas.*` (R-41).

## Why `KelGroups.Types` is the only shared upstream

It carries the association-list and membership-list vocabulary plus `Member`,
`Role`, `Admin`, and `hasAdmin` — the definition of *responsabile*. Sharing it
is what makes "the franchise is the same notion of admin in both machines" a
structural fact rather than a claim two files happen to agree on. Sharing
anything below it (`Fold`, `Validate`) would let required semantics leak into
the faithful model, which is exactly what R-41's second clause forbids.

## Responsibilities

| Module | Owns | Must not own |
|---|---|---|
| `KelGroups.Vote.Types` | verdict vocabulary, threshold-policy type, named threshold instances, question identity and kind, ballot vocabulary | any state, any transition, any policy choice presented as the policy |
| `KelGroups.Vote.State` | the group-with-questions state, the open-question set, the closure log, franchise extraction, tally reading, verdict evaluation as a function of state | mutation, event interpretation |
| `KelGroups.Vote.Event` | the event vocabulary: open a question, cast a ballot, renounce, admit a member, and the franchise-changing events | any interpretation of those events |
| `KelGroups.Vote.Fold` | the single step function and the list fold; ballot placement; the recompute-and-close sweep run after **every** event; closure-log writing | verdict policy (delegates to State), validation error vocabulary of the faithful machine |
| `KelGroups.Vote.Validate` | ballot/renunciation admissibility and its distinct error vocabulary | anything the fold decides |
| `KelGroups.Vote.Invariants` | VC-1…VC-6 theorems over the production fold; the reachability witnesses | new definitions used by production code |
| `KelGroups.Vote.Tests` | point tests and the executed witnesses of R-48, R-53, R-66 | anything imported by production modules |

## Promotion

No abstraction is promoted into `KelGroups.Types`. The shared vocabulary it
already exposes is sufficient; adding a verdict or threshold notion there would
push required-only semantics into the frozen faithful machine's upstream.

## Dependency-direction enforcement

`nix/lean-dependency-direction.sh` scans `lean/KelGroups` recursively and is
therefore expected to need **no change** (R-42). Coverage of the new subtree is
a demonstration obligation, not an assumption: the gate places a violating
import under `lean/KelGroups/Vote/` and requires a non-zero exit.
