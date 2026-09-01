# Plan — #48 inversion coverage amendment

Artifact ceiling: 100 lines / 8 KiB.

## Constraints

The frozen machine already has fourteen economic constructors and eight
successful-step inversion theorems. The gap is six constructors, derived from
the declarations rather than trusted from the dispatch list. The change is one
bisect-safe OWNER slice because proof statements and parser reachability need
semantic review. `draft=NONE`.

## Technical strategy

1. Preserve `Event`, `step`, `stepEvent`, membership, vote, and composition
   definitions byte-for-byte.
2. Add one inversion theorem for each member of the derived six-constructor
   gap, exposing the corresponding admitted guard and exact successor state.
3. Add a repository check that derives the event surface and covered event
   arguments directly from source, rejects missing or phantom coverage, and
   runs a derived negative control.
4. Wire that check into the existing Lean gate before the full Lean build.
5. Print theorem-qualified axiom sets, retain zero proof escape hatches, and
   run the complete repository gate.

## Slice S48-I — close inversion coverage

Delivers T4810–T4814 and R48-I01…R48-I07.

Proof/production fence:

- `lean/Reactivegas/Invariants.lean`
- `scripts/check-reactivegas-inversion-coverage`
- `justfile`

A placement, parser-contract, or signature obstruction versions this mandate
before implementation resumes.

## Ordered evidence

1. Baseline: clean `934de7a8`; mechanically derive 14 constructors, 8 covered,
   and the six-name gap; run repository CI.
2. RED: the frozen slice gate fails because the current derived gap is nonempty.
3. Can-fail control: the permanent checker fails after dynamically suppressing
   one derived covered constructor and identifies that exact gap.
4. GREEN: coverage equality, focused Lean build, six axiom reports, absence of
   proof escape hatches, and full repository CI.
5. Fresh audit: statement-level guard/post-state review, detector mutation,
   forbidden-blob comparison, and complete invariant matrix.
6. Acceptance: task-only stamp, exact audited-tree proof, and final local gate
   receipts; no push or PR.

## Risk ledger

| Risk | Permanent control |
|---|---|
| copied constructor list drifts | derive `Event` constructors from the pinned declaration |
| theorem name exists but covers the wrong event | derive coverage from each theorem's `stepEvent` hypothesis |
| checker is present but unreachable | execute it from the normal Lean gate |
| negative control tests a different algorithm | remove one item from the checker's own derived coverage and require failure |
| theorem compiles through an escape hatch | grep both escape spellings and inspect theorem-qualified axiom output |
| inversion statement omits the real guard or state | fresh semantic audit against the corresponding `stepEvent` branch |
| stale issue reintroduces departure semantics | forbidden-surface blob comparison against `934de7a8` |

## Build and audit budget

At most three building audits for the ticket; submission 1 is full-scope and a
single findings-driven repair may receive submission 2. No third submission.
The campaign ledger has one row per declared invariant above.
