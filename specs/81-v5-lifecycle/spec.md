# V-5 vote lifecycle and S-12 refusals — #81

Authority: [issue 81](https://github.com/paolino/reactivegas/issues/81);
operator rulings V-5 (proposer renounce/departure closes negatively) and
S-12 (2026-10-01: non-proposer renounce and non-designee permission ballot are
refused). S-12 supersedes the issue's "Out of scope" section for its two items.
Base: Lean `890a74f1c4c34b52c55b5d941c78c94fa504e005`.
Ceiling: this artifact 100 lines / 7000 bytes. Every invariant is BLOCKING:
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
| L-7 | *(open, gated on #76)* escrow held against the closed question is refunded | not delivered here |

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
- NG-2 L-7 refund (needs #76's closure→economy wire).
- NG-3 Haskell production and replay (#67/#75); `docs/en/design` (#71).
- NG-4 An "abstain" action or any withdrawal-from-tally reading of renounce.
