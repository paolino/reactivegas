# Spec — #48 inversion coverage amendment

Issue: https://github.com/paolino/reactivegas/issues/48

Frozen base: `934de7a8df136d86a8ad2caadbda99af60e58b59`.
Dispatch pointer: `POINTER-1788249740-1981001`.
Artifact ceiling: 120 lines / 10 KiB.

## Authority amendment

The issue body predates merged #62 and is stale where it requires separately
signed departure events. On the frozen base, departures are consequences of
canonical base transitions through the sealed hook; the four old economic
membership/role constructors are absent from `Reactivegas.Event`. This ticket
does not recreate, replace, or modify those events. For this amendment the
frozen repository and dispatch brief outrank the older issue prose.

## Observable outcome

Every constructor currently declared by `Reactivegas.Event` has a theorem that
inverts a successful `stepEvent` and exposes the admitted guard and resulting
state. A permanent check derives the constructor surface from the Lean source,
derives theorem coverage from theorem statements, rejects any gap, and proves
its own detector can fail.

## Requirements

- **R48-I01 — complete inversion surface.** The constructor set is derived
  from the pinned `inductive Event`; the existing inversion set is derived from
  successful-`stepEvent` theorem hypotheses. Their difference is empty after
  the change. No hand-maintained event registry is accepted as coverage.
- **R48-I02 — missing inversions.** Add successful-step inversions for exactly
  the mechanically derived gap: `openPurchase`, `deposit`, `withdraw`,
  `transferCassa`, `donate`, and `backdonate`.
- **R48-I03 — semantic shape.** Each new theorem exposes the guard admitted by
  the corresponding `stepEvent` branch and the exact post-state, matching the
  proof role of the eight existing `step_<name>_inv` theorems.
- **R48-I04 — non-vacuous permanent coverage.** The shipped coverage check
  derives both sides from source, fails when any constructor lacks an inversion,
  reports the derived counts/gap, and includes a negative control that removes
  derived coverage rather than comparing against a copied fourteen-name list.
- **R48-I05 — proof trust.** The accepted scope contains zero `sorry`, `admit`,
  or `sorryAx`. `#print axioms` is run for every new inversion theorem; only
  standard Lean axioms are admissible.
- **R48-I06 — preservation.** The 157 pre-existing theorems still build.
  `baseEnacted_threshold_met`, the merged #62 membership surface, event
  vocabulary, sealed hook, vote machine, composition, and pledge-agency guard
  remain unchanged.
- **R48-I07 — gate reachability.** The permanent coverage check is executed by
  the normal Lean gate, so a present-but-unwired detector cannot report green.

## Invariants and acceptance meaning

| Invariant | Severity | Failure | Success |
|---|---|---|---|
| `INV-48-I-SURFACE` | BLOCKING | a derived `Event` constructor has no successful-step inversion | derived constructor and inversion sets are equal |
| `INV-48-I-GUARDS` | BLOCKING | a new theorem proves less than its branch guard/post-state | each successful branch yields its admitted guard and exact result |
| `INV-48-I-CANFAIL` | BLOCKING | coverage stays green after derived coverage is removed | the negative control produces the expected missing constructor and nonzero exit |
| `INV-48-I-AXIOMS` | BLOCKING | any new theorem depends on `sorryAx` or a custom axiom | all six theorem-qualified axiom reports are clean |
| `INV-48-I-REGRESSION` | BLOCKING | the existing Lean corpus or theorem surface stops building | the full Lean and repository gates remain green |
| `INV-48-I-FENCE` | BLOCKING | membership, departure, vote, composition, toolchain, or pledge-agency semantics change | all forbidden surfaces remain blob-identical to the frozen base |

## Source fence

Implementation writes are limited to:

- `lean/Reactivegas/Invariants.lean`
- one permanent inversion-coverage checker under `scripts/`
- the existing Lean-gate recipe in `justfile`, only to execute that checker
- the ticket-owner task stamp in `specs/48-inversion-coverage/tasks.md`

All other tracked paths are read-only. A need to cross this fence is a contract
challenge, not permission to broaden the ticket.

## Non-goals

- Departure events or any membership, role, base-hook, vote, or composition
  change.
- Toolchain or dependency changes.
- A pledge-agency guard change.
- Refactoring existing inversion theorems or downstream proofs.
- Push, PR creation or mutation, merge, or issue mutation.
