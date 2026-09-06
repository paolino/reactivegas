Standalone ticket under milestone 2, parent #72. Reports to the milestone desk, not to #66 or #67. Completes the remainder of #54, which delivered the classification but not the runtime wire.

## The requirement is already ruled

This ticket implements a settled ruling. It does not reopen whether grants may remain unilateral.

**Operator ruling** — the economic machine only ever consumes `grantPermission`/`denyPermission` emitted by the vote machine; both must be **provably vote-derived, not merely responsabile-authored**; and this has **two** consumers, not one: purchase approval and the voted comune backdonation. Until the substrate mirrors it, the honest status is `enforced: PROVED-IN-MODEL`, never `enforced`.

**Routing ruling** (classify, don't join) — `grantPermission`, `denyPermission` and `backdonate` are **app-decided**, witnessed by `KelGroups.Vote.ClosureRecord.verdict`, never by the base membership channel, which has no proposal that could produce them. The classifier must stay total and wildcard-free: an added constructor fails to compile.

`Reactivegas.Composition` implements that classification and proves it total. **The classification is delivered; the wire is not.**

## What is missing, precisely

`appDecided_verdict_exhaustive` proves the verdict elimination is exhaustive and honest. Three links are unbound:

1. **Reachability** — no production transition consumes a `ClosureRecord`.
2. **Target** — `e` and `record` are unrelated parameters joined by no premise. A closure about collection 4 satisfies the theorem for an event about collection 9.
3. **Polarity** — nothing maps `.positive → grantPermission` and `.negative → denyPermission`. Either pairing satisfies it.

Meanwhile `grantPermission`'s only guard in the production `step` is `isResponsabile view signer`. One responsabile sets `permitted := true` unilaterally; `denyPermission` likewise destroys a collection and refunds every pledge.

Supporting structural observations, offered as **leads, not proofs** — the implementation must establish the behavioural facts with its own executable witnesses:

- `Reactivegas/Composition.lean` does not import `Reactivegas.Step`, and only the library aggregator imports `Composition`.
- Deleting `Composition.lean` and its import leaves `Reactivegas.Step`, `Reactivegas.Invariants` and `Reactivegas.Trace` building (`lake build` 20/20, exit 0). This shows a build-time dependency is absent. **It is not a behavioural witness that an unbacked grant is refused** — no such refusal exists to witness yet.
- `Composition.lean`'s own header states that nothing consumes `route` at runtime today.

## Scope

A production transition in which `grantPermission`, `denyPermission` and `backdonate` derive from a `ClosureRecord`, with **target, polarity, provenance and consumption bound**, and are refused without one.

- **Target** — the closure names *what* it authorizes, and the binding differs by consumer:
  - permission: the **collection id**. A closure for collection *x* authorizes only *x*.
  - backdonation: the **ruled payload**, the per-member share `w`. Operator ruling:
    *"backdonation = equal shares … parameterize the backdonation by the PER-MEMBER
    share `w` (comune -= n*w) not the total, so integer division never appears and
    equality is exact; funds guard `bal comune >= n*w` subsumes the stall."* A closure
    authorizing `w = 5` must not authorize `w = 500`. A target that is only a label,
    an event tag, or a question id — with the amount still supplied by the caller — is
    **not** a bound target and does not satisfy this ticket.
- **Polarity** — `.positive` authorizes only the grant; `.negative` only the deny.
- **Provenance** — the closure is one the production vote fold actually produced, not a value a caller constructed.
- **Consumption** — a closure authorizes at most once; a spent closure cannot authorize a second event.

## Acceptance — every row must be demonstrated failing before it passes

The refusals are the deliverable, not a side condition. Each is an absence claim, so each needs an **executable negative witness**: a mutant that ignores or fabricates the closure must fail the row.

| # | must hold |
|---|---|
| B-1 | a `.positive` closure on question *q* bound to collection *c* permits *c*, executed through the production root |
| B-2 | `grantPermission` with **no** backing closure is refused |
| B-3 | `grantPermission` with a **fabricated** closure — one the production fold never emitted — is refused |
| B-4 | a closure bound to collection *x* does **not** permit collection *y* |
| B-5 | a `.negative` closure yields `denyPermission` and **never** `grantPermission` |
| B-6 | a closure already consumed cannot authorize a second event |
| B-7 | `.open` derives nothing (already proved; must stay proved through the new route) |
| B-8 | `backdonate` derives from a closure **that binds `w`**; the `BackdonateAuth` callback is retired, or explicitly scoped with the scope written down. B-8 does **not** pass if the callback still supplies or may override `w` and the closure carries only a metadata label |
| B-8a | a closure binding `w = k` refuses a `backdonate` for any `w != k` |
| B-8b | a `.negative` closure authorizes no backdonation |
| B-8c | a backdonation closure already consumed cannot authorize a second backdonation |
| B-9 | the classifier stays total and wildcard-free; an added constructor fails to compile |
| B-10 | the status line moves off `PROVED-IN-MODEL` only when the substrate mirrors it |

**Every row applies to both consumers.** Permission and voted backdonation are the two consumers the operator ruled; a row demonstrated only for `grantPermission` leaves the backdonation path unbound. The mutation campaign below covers B-2 through B-6 **and** B-8a through B-8c: an implementation that ignores the closure entirely must fail them, and that must be shown, not asserted.

## Dependencies

- Threshold policy: a parameter with two named exhibits and no default (R-46/R-47). **#68 does not settle it** — #68 is proposer-counting arithmetic on the base channel and must not be read across as a vote default.
- Adjacent to #68 and #69 (guard changes).
- Not blocked on #73: this is Lean specification work. The Haskell replay is separate and belongs to #67's D3.
- Feeds the vote corpus ticket, which gains the composition rows once this exists.



## Downstream

#81 (V-5 lifecycle: proposer renounce and departure close negative) depends on this ticket for the negative continuation and the escrow refund. V-5's stated reason for requiring the negative continuation is that escrow is held and silent deletion strands money, so that refund is a closure-derived economic effect — the same wire this ticket builds, in the opposite direction.


## Implementation tracking — 2026-09-06

Draft #93 carries the complete B-1–B-10 mandate, including B-8a/b/c, on accepted model base `efef604de87b2a1efae51e84d1a9150e585c1db0` (PR #89). Baseline local `just ci` passed; implementation and independent inspection remain pending. This baseline result does not establish the composition contract.

Implementation proceeds in isolation. Landing remains reserved after #92/C1/#68/#69; final evidence must bind the actual accepted integration base. No threshold default is chosen. The negative permission continuation/refund interface feeds #81; its producer implementation, the full #75 vote corpus and #67 Haskell production/replay remain separate.
