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

### Slice 2 — per-producer composition *(AUTHORIZED)*

Delivers R-30..R-37 in one bisect-safe owner campaign from planning base
`32c63850478c17ac51f622ddbfa17d9b40be29e6`. The architecture is settled by
NOTE-031 and is not reopened: one Reactivegas-side module classifies all 18
events into `direct`, `baseEnacted`, or `appDecided`.

The base route binds the faithful production enactment and its threshold
theorem. The app route binds the required machine's production closure verdict
and exhausts all verdict constructors. Direct events bind neither. No
cross-channel join or identifier bridge exists.

Implementation is fenced to `lean/Reactivegas/Composition.lean` and one import
in `lean/Reactivegas.lean`. The preceding accepted histories and every existing
Reactivegas/KelGroups module remain byte-stable.

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

---

# Plan — Vote-coverage run (2026-08-29)

## Constraints added by this run

1. **Slice 1 is frozen.** The seven `lean/KelGroups/*.lean` modules merged at
   `ccdda83` are not edited. Their shipped fidelity matrix must remain true
   without anyone having to re-verify it.
2. **`REQUIRED-OF-SUBSTRATE`, never `FAITHFUL`.** The new machine specifies
   what `kelgroups#28`/`#30` must become. Claiming it describes existing
   kelgroups is a rejection reason.
3. **No frozen threshold policy.** V-2's two consequences stay open by
   operator ruling. Parameterization is the mechanism that keeps them open;
   picking one and proving theorems that only hold for it would answer a
   product question this chain may not answer.
4. **Absence is load-bearing.** "No expiry" is delivered as a theorem plus the
   absence of any time field, not as a comment. A reviewer must be able to see
   that no transition *could* expire a question.
5. **Same toolchain.** Lean 4 core only, `leanprover/lean4:v4.27.0`, no
   mathlib, `native_decide` forbidden. Adding a dependency is a mandate change.
6. **The vacuous-green trap recurs.** `lean/KelGroups.lean` roots the library;
   a new subdirectory that the root does not import compiles nothing while
   `just lean` reports success. Slice 1 hit this for `lean/KelGroups/`; the
   same control is re-run for `lean/KelGroups/Vote/` (R-43).

## Modelling strategy

The required machine keeps Slice 1's representation choices — association
lists with replace-on-insert, membership lists with insert-if-absent — so that
cardinality remains faithful and the nodup obligations stay provable in the
same style. Three departures, each forced by a ruling:

- **Two tallies, one position.** Ballot placement is a single operation that
  inserts into one list and erases from the other. Disjointness is then an
  invariant of the fold rather than a guard anyone can forget (R-56/R-57).
- **Verdict is a function of state, never a stored field.** Nothing caches a
  verdict, so nothing can hold a stale one. R-51's "recompute on every event"
  becomes an unconditional sweep at the end of the single step function, and
  R-52 states that a stale open question is unreachable.
- **Closure is a write, not a delete.** Removing a question from the open set
  and appending a closure record are one operation. R-61's partition theorem
  is what makes silent deletion unrepresentable rather than merely discouraged.

### The invariant that carries the money argument

R-60 and R-61 exist because a purchase-approval question holds escrow. The
partition theorem is the load-bearing one for the whole run: if a question can
leave the open set without a verdict, the negative continuation never runs and
member funds are stranded against a question that no longer exists. It is
stated over the production fold (R-68) precisely so that no test-only
constructor can satisfy it.

## Ordered, bisect-safe slices

Each slice builds, proves, and is independently audited. `lean/Reactivegas/**`
is untouched throughout.

### Slice A — the tally machine *(AUTHORIZED)*

Delivers R-40…R-57, R-61, R-68…R-71 for the collective path.

```text
lean/KelGroups.lean                       (import the new root)
lean/KelGroups/Vote/Types.lean
lean/KelGroups/Vote/State.lean
lean/KelGroups/Vote/Event.lean
lean/KelGroups/Vote/Validate.lean
lean/KelGroups/Vote/Fold.lean
lean/KelGroups/Vote/Invariants.lean
lean/KelGroups/Vote/Tests.lean
specs/54-lean-vote-machine/tasks.md       (ticket-owner stamp only)
```

Franchise, assent and dissent under one parameterized threshold, the three-way
verdict, one-position-per-responsabile, the unconditional recompute sweep, the
closure log and its partition theorem, and the V-2/V-3 witnesses.

### Slice B — the paths that are not a tally *(AUTHORIZED, follows A)*

Delivers R-58…R-60, R-62…R-67.

Permission questions with a named designee, proposer renunciation, closure on
proposer departure running the negative continuation, and direct member
admission. Extends the same files; adds no new module unless the commit owner
records why a new one is required.

Slice B follows A because it consumes the closure interface A defines. Running
them in parallel would design closure twice.

### Slice C — record honesty *(AUTHORIZED, follows B)*

One correction to `docs/en/design/kelgroups-vote-machine.md`: the shipped
`EP-DENY` row says "Slice 2 cannot derive a deny verdict until this gap is
ruled". V-5 and V-7 ruled it. The row's *fidelity* claim about kelgroups at
`368b596` stays exactly as it is — that statement is still true — and a
pointer to the required machine is added.

**Fence note.** `docs/` is not in the brief's enumerated owned surface, though
it is also not in its forbidden list, and the file is #54's own artifact. The
ticket owner has taken the brief's "you own the exact file fence after
discovery" as authority to include this one file, and has isolated it into its
own slice so the parent can drop it without touching any proof work. Reported
upward explicitly rather than folded in silently.

## What this run does not do

At this historical vote-coverage run boundary, Slice 2 did not start because
it still waited on #48's accepted consumer signatures. NOTE-031 now records
both accepted inputs and authorizes the separate composition campaign above;
the old run itself remains correctly scoped to vote coverage.

## Live-boundary consideration

There is no runtime boundary. The analogous failure is a specification that
elaborates while proving nothing about the semantics it claims — a green
`lake build` over a directory nothing imports, a theorem about a record
literal the fold cannot produce, or a check that no mutation can redden. R-43,
R-68, and R-70 are the three controls against exactly those, and each is a
demonstration obligation, not a statement.
