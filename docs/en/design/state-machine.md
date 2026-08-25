# The reactivegas state machine

Issue #45 distils the legacy `Eventi/` reactors into an executable,
machine-checked specification. This page is the prose record of that
specification: each law is introduced by a user story — a concrete thing
a real group member might do — followed by the precise property the Lean
code proves about every possible execution, not just the story. The
sources live under
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

### AUTH — Only entitled actors can act

**User story.** Bruno is an ordinary member. He tries to close Elena's
collection, withdraw from someone else's account, and promote himself to
responsabile. Every one of those attempts bounces: the machine simply
returns `none`.

**What is proved.** Authorisation is not a UI convention but a theorem.
`authorizedStep` states that whenever `step s e = some s'`, the event's
author holds exactly the rights the event demands — membership where mere
membership suffices, responsabile status where power does.
`step_authorized` proves it case by case for all 15 events, so no future
edit to `step` can silently drop a guard without breaking this proof.

* Predicate: `authorizedStep`
* Theorem: `step_authorized`

### L1 — Governance enacts removal

**User story.** The group votes to expel Carl, a member who owes money on
an open purchase. Only the governance process — an authorisation signed
off at the top level — may perform the removal. A random responsabile
cannot delete users to make their pledges disappear.

**What is proved.** If `step s (removeUser r u)` succeeds, then the
authorisation evidence carried by the event satisfies `governanceEnacts`.
Removal is never a side effect of anything else.

* Predicate: `governanceEnacts`
* Theorem: `governance_enacts_remove`

### L2 — Closure needs permission

**User story.** Elena opened a collection for a bulk olive-oil order.
Before she closes it — locking the pledges and paying the supplier — the
group must grant closure permission, typically once the goods arrive and
the price is confirmed. Without that click, closing is impossible.

**What is proved.** Whenever a close event succeeds, the resulting state
still satisfies `permissionToClose` for that collection: the permission
flag was set *before* the close. `close_permission_to_close` rules out
the entire class of "closed while still provisional" bugs.

* Predicate: `permissionToClose`
* Theorem: `close_permission_to_close`

### L3 — Escrow at pledge

**User story.** Anna pledges €30 towards the olive oil. She expects her
account to show €30 less immediately — the money is committed, even
though nobody has spent it yet — and she expects to see her offer listed
in the collection. She does not expect the group's total wealth to change.

**What is proved.** After a successful pledge, three things hold at once
and are proved together (`pledge_escrow_debit`): Anna's balance dropped
by exactly `v` (`bal_bump`), the collection now holds exactly `v` more
escrow than before, witnessed by `escrowHeld` via `splitUser`, and the
global books still balance because the debit moved into escrow rather
than vanishing.

* Predicate: `escrowHeld`
* Theorem: `pledge_escrow_debit`

### L4 — Closure pays out to the referente

**User story.** The oil arrives, permission is granted, Elena closes the
collection. The pooled money — say €450 pledged by twelve members — is
credited in one movement to Elena's account as referente, who then pays
the supplier. No member's pledge can be silently redirected elsewhere.

**What is proved.** `close_spends_referente` shows that the closed
collection's full escrow appears as a credit to `col.referente` — and
only there — when the close event succeeds.

* Theorem: `close_spends_referente`

### L5 — Double entry for cash movements

**User story.** Marco deposits €50 into his account via Daniela, a
responsabile: €50 leaves Daniela's cash box and appears in Marco's
account. When he later withdraws, the mirror image happens. At no point
does Marco gain without the cash box losing, or vice versa.

**What is proved.** Both movements produce the witness required by the
`doubleEntry` predicate: the pair of balancing changes. The theorems
construct that witness directly from the `bal_bump` lemma applied to each
side of the ledger.

* Predicate: `doubleEntry`
* Theorems: `deposit_double_entry`, `withdraw_double_entry`

### L6 — Conservation (flagship)

**User story.** Months pass. Members join and leave, dozens of purchases
open and close, money sloshes between accounts, cash boxes and escrow.
The treasurer's invariant stays trivially true: the machine never minted
or burned a cent — total balances always equal the boot capital minus
what is parked in open collections.

**What is proved.** This is the heart of the specification. `conservation`
formalises the treasurer's claim for any reachable state, and
`conservation_preserved` proves that **every one of the 15 events**
preserves it. There is no induction over history to trust: each case
reduces to arithmetic facts about the list helpers (`pullCollection_sum`,
`splitUser_sum`, `refundAll_sum`, …) discharged by `omega`. Any code
change that leaks or duplicates money breaks the build.

* Predicate: `conservation`
* Theorem: `conservation_preserved`

### L7 — Insolvency is reachable but explicit

**User story.** A catastrophic scenario: members withdraw their credit,
leaving the group owing more escrow than the accounts hold. The legacy
system never claimed this cannot happen — and honesty is cheaper here
than a false guarantee. The spec admits insolvency, exhibits it, and lets
downstream code decide how to react.

**What is proved.** `badState` is a concrete, hand-written state whose
accounts cannot cover its escrow. `insolvency_reachable` derives
`Reach badState` from the boot state by an explicit chain of legal steps
— so the bad state is not a fantasy but genuinely constructible — and
`insolvency_example` decides the shortfall by computation alone
(`by decide`). The negative result is itself machine checked.

* Predicate: `insolvent`
* Theorems: `badState`, `insolvency_reachable`, `insolvency_example`

### L8 — One pledge per user per collection

**User story.** Anna, excited about the olive oil, tries to pledge twice.
The second attempt is refused outright: within one collection a user
speaks with one wallet. This keeps refund logic simple — a member's stake
in a purchase is a single well-defined amount.

**What is proved.** `uniquePledges` states the uniqueness property inside
one collection and `allUniquePledges` lifts it to the whole state. Three
theorems close the loop: `pledge_rejected_when_member` shows the duplicate
pledge returns `none` rather than corrupting the lists;
`uniquePledges_pend_cons` shows that when a pledge *is* accepted onto a
uniqueness-respecting collection, uniqueness survives the cons; and
`pledge_preserves_allUnique` lifts that to whole-state preservation, so
uniqueness holds forever if it holds at boot.

* Predicates: `uniquePledges`, `allUniquePledges`
* Theorems: `pledge_rejected_when_member`, `uniquePledges_pend_cons`,
  `pledge_preserves_allUnique`

## Verifying

```sh
just lean        # cd lean && lake build
nix develop -c just ci   # includes the lean build
```
