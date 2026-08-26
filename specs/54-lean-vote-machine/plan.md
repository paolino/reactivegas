# Plan — #54

## Constraints that shape everything

1. **Fidelity beats elegance.** The model reproduces `/code/kelgroups` at
   `368b596`. A cleaner voting design is a rejection reason, not an improvement.
2. **One-way dependency.** `lean/KelGroups/` must lift out of this repository
   into `paolino/kelgroups` unchanged. `lean/Reactivegas/` may depend on it.
   Only the first direction is gated (spec R-2/R-3, epic NOTE-018).
3. **The build must actually build it.** `lean/lakefile.lean` today declares one
   `lean_lib Reactivegas` as the sole `@[default_target]`. Adding files under
   `lean/KelGroups/` without a second default target produces a **vacuously
   green CI**: `just lean` would pass while compiling none of the new code.
   Slice 1 therefore changes the lakefile, and the gate proves the change took
   effect by making a deliberate error under `lean/KelGroups/` go red.
4. **Zero custom axioms.** `native_decide` is banned (`Lean.ofReduceBool`).
   Point tests use `#guard` / `decide` / `rfl`, which are elaboration-time and
   make the build red when false.
5. **No mathlib.** `lean/lake-manifest.json` has an empty package list. The model
   uses Lean 4 core only (`leanprover/lean4:v4.27.0`). Adding a dependency is a
   contract change requiring a new mandate version, not an implementation call.

## Modelling strategy

Haskell `Map`/`Set` are modelled as association lists and membership-testing
lists, chosen so that **cardinality is faithful**:

- `Map k v` → assoc list with replace-on-insert, delete-by-key, adjust-by-key;
  `size` = length under a key-nodup invariant that the fold preserves.
- `Set k` → list with `insert x l = if x ∈ l then l else x :: l`; `size` =
  length under the nodup invariant (VI-1).

The nodup invariants are not cosmetic: `majority` compares against
`Set.size (approvals pp)` and `adminCount` is `Map.size`, so a model whose list
can carry duplicates silently changes the threshold arithmetic. They are proof
obligations, not comments.

The application dimension stays an abstract type parameter `α` throughout, which
is simultaneously the faithful reading of `GroupState a` and the mechanism that
makes R-2 structurally true rather than merely checked.

## Ordered, bisect-safe slices

### Slice 1 — faithful vote machine *(AUTHORIZED NOW)*

Delivers R-1..R-29. One commit. Touches only:

```
lean/lakefile.lean                          (second default target)
lean/KelGroups.lean                         (root)
lean/KelGroups/*.lean                       (model, invariants, tests)
docs/en/design/kelgroups-vote-machine.md    (fidelity matrix)
mkdocs.yml                                  (one nav entry)
specs/54-lean-vote-machine/tasks.md         (ticket-owner stamp only)
```

`lean/Reactivegas/**` is **not** touched. The composition is Slice 2.

Slice 1 is independent of #48 and may run alongside it.

### Slice 2 — structural composition *(BLOCKED)*

Delivers R-30..R-35. Two hard preconditions:

- **P-1** #48's definitions commit is accepted and both economic consumer
  signatures (purchase approval, voted comune backdonation) are frozen. #48 adds
  the second consumer; a one-consumer interface is obsolete on arrival.
- **P-2** EP-DENY has a ruling. kelgroups has no dissent, rejection, expiry or
  withdrawal event, so `denyPermission` has **no vote-machine source today**.
  R-31 is unstatable for `denyPermission` until that is ruled. Escalated to the
  epic owner at Slice-1 dispatch; it is not a Slice-1 blocker.

Design order is fixed by NOTE-018: evaluate verdict-carrying event types first;
the relational fallback needs a recorded concrete impracticality *before*
implementation.

## Live-boundary consideration

There is no runtime boundary in Slice 1 — the deliverable is a specification
that compiles. The analogous failure mode is a **specification that compiles
without being compiled**, which is why plan constraint 3 and gate leg G2's
negative control exist. That control is this slice's boundary smoke.

## Risks

| Risk | Handling |
|---|---|
| New Lean lib not wired into `lake build` → CI vacuously green | G2 negative control (deliberate error must go red) |
| Boundary scanner with a broken pattern reports "no violations" | G1 detector positive control on a known-present import |
| Boundary scanner over-fires on the legal direction | G1 legal-direction positive control (R-3) |
| Digest injectivity smuggled in as an axiom | R-28 axiom check; injectivity carried as a theorem hypothesis |
| `native_decide` used for point tests | R-28 axiom check will catch `Lean.ofReduceBool` |
| Model drifts to an idealized vote | fidelity matrix (R-25) with per-row Haskell anchors, gate-checked for existence |
| `mkdocs.yml` nav collides with unmerged #47 | one-line adjacent conflict; flagged to the epic owner, trivial to resolve |
