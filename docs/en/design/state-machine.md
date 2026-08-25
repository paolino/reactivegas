# The reactivegas state machine

Issue #45 distils the legacy `Eventi/` reactors into an executable,
machine-checked specification. This page is the prose record of that
specification; every claim below names the Lean predicate or theorem
that proves it. The sources live under
[`lean/Reactivegas/`](https://github.com/paolino/reactivegas/tree/main/lean/Reactivegas)
and build with `lake build` (see `just lean`).

## State

The whole economic state (`Reactivegas.State`) is four association
lists and one list of open collections:

| field | contents |
| --- | --- |
| `users` | registered users |
| `responsabili` | users allowed to open/close collections |
| `conti` | per-user credit balances |
| `casse` | cash boxes, one entry per responsabile |
| `collections` | open purchase collections holding escrow |

A collection (`Reactivegas.Collection`) carries its id, a referente
(responsible user), a closure-permission flag, and two pledge lists:
`accepted` (money already moved) and `pending` (offers not yet accepted).
Balances are plain `Int`; money held in escrow by a collection is the sum
of *both* pledge lists (`escrowOf`), and across all collections
(`escrowSum`).

## Step

The machine is a rejecting step function:
`step : State → Event → Option State` (`Reactivegas.Step`). Each of the
15 legacy events either transforms the state or returns `none`. Guards
encode authorisation inline: opening/closing requires responsabile
status, deposits and withdrawals require the actor's own account, and
pledge/accept/refuse/correct route through `pullCollection`, which fails
when the collection does not exist.

## Laws

The legacy reactors were the behavioural spec; the laws below are the
safety spec. All theorems are proved in `lean/Reactivegas/Invariants.lean`
with **zero** `sorry` and zero custom axioms.

### L1 — Governance enacts removal

Only governance may remove a user. If `step s (removeUser r u)` succeeds,
the event author was authorised by `governanceEnacts`:

* Predicate: `governanceEnacts`
* Theorem: `governance_enacts_remove`

### L2 — Closure needs permission

A collection can be closed only if closure permission was granted:

* Predicate: `permissionToClose`
* Theorem: `close_permission_to_close`

### L3 — Escrow at pledge

Pledging debits the pledger's account immediately and moves the amount
into escrow (`escrowHeld`). After `step s (pledge c u v)` succeeds, the
user balance dropped by `v` and the collection holds exactly `v` more
escrow than before:

* Predicate: `escrowHeld`
* Theorem: `pledge_escrow_debit`

### L4 — Closure spends escrow on the referente

Closing a collection credits its total escrow to the referente:

* Theorem: `close_spends_referente`

### L5 — Double entry

Deposits and withdrawals preserve the global conservation identity via
double entry: the account change is mirrored by the cash-box change.
`deposit_double_entry` and `withdraw_double_entry` produce the witness
pair required by `doubleEntry`.

### L6 — Conservation (flagship)

Money is neither created nor destroyed: for any reachable state, the sum
of all account balances equals the boot capital minus escrow held by open
collections. The full per-event preservation theorem covers all 15 events
in a single induction-free case analysis:

* Predicate: `conservation`
* Theorem: `conservation_preserved`

### L7 — Insolvency is reachable but explicit

The spec admits that insolvency can happen, and exhibits it rather than
assuming it away. `badState` is a concrete reachable state whose accounts
cannot cover the escrow; `insolvency_reachable` derives `Reach badState`
from the boot state and `insolvency_example` decides the shortfall:

* Predicate: `insolvent`
* Theorems: `badState`, `insolvency_reachable`, `insolvency_example`

### L8 — One pledge per user per collection

Within one collection, a user appears at most once among all pledges
(`uniquePledges`); globally this holds when it holds initially
(`allUniquePledges`). Pledging preserves uniqueness
(`pledge_preserves_allUnique`), and a second pledge by the same member is
rejected outright (`pledge_rejected_when_member`).

### AUTH — Authorised steps

Every successful step is authorised: the acting user holds the rights the
event demands. `authorizedStep` is the gate predicate and
`step_authorized` proves it for all 15 events:

* Predicate: `authorizedStep`
* Theorem: `step_authorized`

## Verifying

```sh
just lean        # cd lean && lake build
nix develop -c just ci   # includes the lean build
```
