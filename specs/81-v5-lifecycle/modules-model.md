# Modules model — #81

Ceiling 40 lines / 2500 bytes. Changed responsibilities only.

| ID | Module | Responsibility change |
|---|---|---|
| M81-VALIDATE | `KelGroups.Vote.Validate` | sole producer of `notProposer` (renounce) and `notDesignee` (cast on a permission question); stays the single exhaustive admissibility decision. |
| M81-FOLD | `KelGroups.Vote.Fold` | owns both V-5 closure operations: the renounce effect and the proposer-departure closure (F81-CLOSE-PROPOSER). Closure stays removal plus appended record, one operation (R-61). |
| M81-HOOK | `Reactivegas.Step` (`baseHook`) | on `memberRemoved`, invokes F81-CLOSE-PROPOSER for the leaver before the unchanged V-3 sweep over the post view; other base changes unchanged. |
| M81-PROOF | `KelGroups.Vote.Invariants`, `KelGroups.Vote.Tests`, `KelGroups.Mirrors`, `Reactivegas.Invariants` (or a new module under `lean/` registered in the build) | witnesses, mutation-only inversions, mirrors, replaced superseded statements. |

Dependency direction unchanged: `KelGroups` imports no `Reactivegas` module
(`nix/lean-dependency-direction.sh`). Departure semantics stay in the vote
machine; the hook only routes the departing key (D-1 in data-model).

## S81-B

| ID | Module | Responsibility change |
|---|---|---|
| M81-L7-PROOF | new module `Reactivegas.CompositionRefund` (L-7c definitions and theorems; imports `Reactivegas.CompositionRoot`; registered as a root of the existing `ReactivegasComposition` library in `lean/lakefile.lean`), `Reactivegas.CompositionTests` or `Reactivegas.Lifecycle` (witnesses, inversions), `lean/mutants/*` (ledger rows, after PR 98) | L-7a..L-7c witnesses, M-MINT/M-REFUND inversions, the no-stranding theorem and its reachability exhibit. No production module changes. `lean/Reactivegas/CompositionRoot.lean` stays byte-identical to `a436417`: the simulator claim gate pins its sha256 and cites its theorems by line (#70; v3, Q-001 2026-11-08). |
