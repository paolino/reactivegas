# V-5 vote lifecycle and S-12 refusals — #81

Authority: [issue 81](https://github.com/paolino/reactivegas/issues/81);
operator rulings V-5 (proposer renounce/departure closes negatively) and
S-12 (2026-10-01: non-proposer renounce and non-designee permission ballot are
refused). S-12 supersedes the issue's "Out of scope" section for its two items.
Base: Lean `890a74f1c4c34b52c55b5d941c78c94fa504e005`.
Ceiling: S81-A sections 100 lines / 7000 bytes; S81-B its own (below). Every invariant is BLOCKING:
a closure is the wire through which escrow is refunded (#76), so each row
reaches money.

## Stories

- A proposer who renounces their question closes it, negatively, and the
  closure is kept with its cause.
- A proposer who leaves the group takes their open questions with them, closed
  negatively, in the same transition as the departure, while every other
  question is still recomputed against the new franchise (V-3).
- Only the proposer can renounce; only the designee can answer a permission
  question. Anyone else is refused and nothing changes.

## Acceptance

| ID | Must hold | Can-fail mutant that must be shown failing |
|---|---|---|
| L-1 | a `renounce` by the question's proposer removes it from the open set | renounce leaves it open |
| L-2 | a base transition removing the proposer from the group closes every open question they proposed, inside that same integrated transition | closes on a later step, or not at all |
| L-3 | both V-5 closures carry `verdict = .negative` | closes `.positive` |
| L-4 | causes are exactly `.renounced` (renounce) and `.proposerDeparted` (departure) | records `.tally` |
| L-5 | each V-5 closure is appended to `VoteState.closed` with the question as it stood | closes and discards the record |
| L-6 | the V-5 rule closes no question but the renouncing/departing proposer's own | closes every open question on the V-5 trigger |
| L-6a | on a departure, the proposer's question closes `.negative`/`.proposerDeparted` *and*, in the same transition, an unrelated question crossing its threshold under the post franchise closes with `.franchiseChange`; both retained | collapses both to one cause, or suppresses either closure |
| L-6b | a renounce under unchanged franchise leaves unrelated questions unaffected | closes an unrelated question on a bare renounce |
| R-1 | a `renounce` by a responsabile who is not the proposer is refused with `VoteError.notProposer`; vote payload and integrated state unchanged | accepts it |
| R-2 | a `cast` on a permission question by a responsabile who is not its designee is refused with `VoteError.notDesignee`; state unchanged | records the ballot |
| L-7 | escrow held against the closed question is refunded; no balance is stranded — rows L-7a..L-7c, slice S81-B below | see S81-B |

## Invariants

| ID | Observable truth |
|---|---|
| INV81-REACH | every row L-1..L-6b, R-1, R-2 has a compiled witness through the production path: `KelGroups.Vote.applyVoteEventChecked`/`foldVote` **and** the integrated `Reactivegas` path (`appFold`/`voteApply` for vote events; the committed base transition running `baseHook` for departure). A structural or build-time observation is not a witness. |
| INV81-CANFAIL | every mutant in the table above is a permanent, compiled, mutation-only definition judged by the *same* oracle that accepts production, and that oracle evaluates to rejection on it (repository inversion convention, e.g. `voteApplyBypass`/`checkVoteApplyBypassCaught`). |
| INV81-ORDER | first-error identity: `notResponsabile`, then `questionNotFound`, then `notProposer`/`notDesignee`. |
| INV81-REFUSAL-INERT | a refused event reaches neither its effect nor the sweep; the integrated path reports `Except.error`, never a successful identity. |
| INV81-V3 | the post-base sweep still runs on every base change; L-6 is not satisfied by narrowing it. |
| INV81-WF | the existing vote well-formedness, partition, no-expiry, idempotence and franchise theorems remain proved over the new machine. Only statements that encode the superseded behaviour (renounce no-op; non-designee ballot recorded) are replaced, each by its ruled counterpart. |
| INV81-TYPES | `ClosureRecord`, `ClosureCause`, `VoteError`, `VoteState`, `Question`, `VoteEvent` and `BaseHook` definitions are unchanged (contract shared with #76). |
| INV81-CORPUS | `lean/corpus/*` bytes unchanged and `just lean-corpus-verify` green. |
| INV81-MIRRORS | every new Prop-valued definition reconciles under `scripts/check-lean-mirrors`. |

## Non-goals

- NG-1 A role change that demotes the proposer without removing them from the
  group is not a departure (issue scope: "removing the proposer from the group").
- NG-2 *(S81-A only; delivered by S81-B)* L-7 refund.
- NG-3 Haskell production and replay (#67/#75); `docs/en/design` (#71).
- NG-4 An "abstain" action or any withdrawal-from-tally reading of renounce.

## Slice S81-B — L-7 refund (2026-11-08)

Base origin/master `17d6a7a` (#76 PR 93 merged). Desk ruling A-002 (2026-11-08):
the refund is #76's negative permission continuation as built. Every negative
closure of a permission-bound question mints a negative authorization
(`syncAuth`); a responsabile's `denyPermission c` spends it through
`denyByClosure`, which dissolves `c` and refunds every accepted and pending
pledge to its owner. **The refund therefore requires a responsabile's
`denyPermission`, as for every negative closure** (carried to the operator by
the desk as a product note). Section ceiling: 45 lines / 4000 bytes.

| ID | Must hold | Can-fail mutant that must be shown failing |
|---|---|---|
| L-7a | after a proposer `renounce` closes a question bound to permission on collection `c` holding accepted and pending pledges, a responsabile's `denyPermission c` through the production root succeeds; each pledger's conto rises by exactly their pledged sum, `c` is absent, no escrow and no authorization for `c` remain | M-MINT: the closure mints no authorization for this cause |
| L-7b | the same after the proposer's departure, in both cases: proposer ≠ `c`'s referente (refund through `denyPermission c`), and proposer = `c`'s referente (refunded inside the departure transition; no authorization outlives `c`) | M-MINT for `.proposerDeparted`; M-REFUND (below) |
| L-7c | no stranding, quantified over every production history (guarded boot, then root steps): a present collection whose bound permission question was closed negatively — by any cause, at least `.renounced` and `.proposerDeparted` — holds a live negative authorization for it, and spending that authorization refunds every pledge to its owner. "Bound" is the binding in force when the closure was recorded; a reused collection id inherits nothing | M-MINT (the predicate is false on a named reachable history under the mutant); M-REFUND: `denyPermission` under that authorization leaves a pledge unrefunded |

| ID | Observable truth |
|---|---|
| INV81-L7-REACH | L-7a/L-7b witnesses run through the production root (`Reactivegas.apply`, e.g. the #76 helpers `run`/`runBase`) on reachable histories; L-7c's antecedent is shown reachable by a named history that contains a V-5 negative closure with escrow present. |
| INV81-L7-CANFAIL | M-MINT and M-REFUND are permanent, compiled, mutation-only definitions judged by the same oracle that accepts production (INV81-CANFAIL convention), each evaluating to rejection on a named reachable history. |
| INV81-L7-LEDGER | once #66 S3 (PR 98) is on master: every new theorem has a row in `lean/mutants/ledger.json` (`KILLED`/`HELPER`; an `OPEN` row with reason and attempted mutants), and the rendered ledgers do not drift (CI job "Lean mutant ledger"). |
| INV81-L7-CONTRACT | no production definition changes and no type change: `denyByClosure`, `syncAuth`, `pruneAuth`, `baseHook`, `appFoldCore`, `ClosureRecord`/`ClosureCause` byte-unchanged; every #76 witness byte-unchanged. If L-7c cannot be stated or proved without such a change, or a counterexample exists on the base, `BLOCKED` with the history before any production edit. INV81-TYPES, INV81-CORPUS, INV81-MIRRORS stand. |

Non-goals: NG-5 refund atomic with the closure (A-002 rejected it); NG-6
backdonation-bound questions (a negative backdonation authorizes nothing and
holds no escrow); NG-7 Haskell, corpus rows (#75), `docs/en/design` (#71).
