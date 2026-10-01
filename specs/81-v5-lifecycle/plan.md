# Plan — #81

Ceiling 60 lines / 4000 bytes.

## Strategy

The vote machine already declares the vocabulary (`ClosureCause.renounced`,
`.proposerDeparted`, `VoteError.notProposer`, `.notDesignee`); this ticket makes
each constructor produced under its ruling, with no type change. Admissibility
gains the two S-12 refusals in `validateVoteEvent`; the renounce effect becomes
a closure; departure closure is a vote-machine operation invoked by the sealed
post-base hook on `memberRemoved`, before the V-3 sweep over the post view.
Running the V-5 closure first is what makes the proposer's question carry
`.proposerDeparted` (L-6a) rather than whatever the sweep would record.

## Slices

One bisect-safe slice, S81-A, topology OWNER: all rows share the closure
surface and the refusal boundary, and splitting them leaves an intermediate
master where renounce closes but anyone may trigger it.

## Proof surface

Permanent Lean witnesses compiled by `lake build` (gate row G2), following the
repository's established convention: `decide`/`#guard`-backed `check*` Bools
over named fixtures, theorems where a quantified statement is cheap, and
mutation-only definitions judged by the same oracle (INV81-CANFAIL).

## Live boundaries and contracts

- Vote closure → economic effect (#76, PR 93): no type change (INV81-TYPES).
  New negative closures are what #76's negative-permission continuation will
  consume; L-7 stays open until #76 lands.
- Frozen corpora (#86): bytes must not change (INV81-CORPUS). A corpus
  emission difference is a BLOCKED question to the desk, never a re-emit.
- `Reactivegas.baseHook` is also edited by #76; textual conflict is resolved
  by whichever lands second, without semantic fork.
- Re-base on #92 when it lands (checker repair).

## Gate

`gate.sh` v1 (ignored, runtime-owned): `git diff --check` over the slice range,
then the verbatim CI steps `just lean-toolchain-contract`, `just lean`,
`just lean-corpus-verify` under `nix --quiet develop --command`.
gate-authors=NONE (operator team: 3 Opus); synthesized by the ticket owner.
