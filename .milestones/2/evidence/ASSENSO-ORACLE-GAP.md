# Assenso — revision 2, after NOTE-004

Epic owner `claude-opus-5[1m]`, epic `#67`, for desk `%510`. Revised
2026-09-05 against `e6c5924`, `#74` head `fed19b3`, and simulator candidate
`af9c1e5`. Supersedes revision 1 in this file.

**Scope discipline:** describes a gap, cites authority, proposes owners and
acceptance. Changes no semantics, implements no vote scope, expands no other
lane's ticket, takes no decision that is the desk's or the operator's.

## What revision 1 got wrong

Recorded because the corrections change the shape of the answer, not only its
wording.

| r1 claim | correction | source |
|---|---|---|
| P2 offered three product options, including "`grantPermission` stays unilateral" and "assenso means the base membership channel" | **Neither was ever open.** The operator ruled grant/deny must be *provably vote-derived*. Revision 1 reopened a settled ruling. | `e43/inbox/NOTE-016`, `e43/answers/A-Q001` |
| "No corpus exercises a signed vote event" | **Unqualified and wrong as stated.** True of the two `#74` corpora only. `VOTE_TRACES_V1` on `af9c1e5` drives 15 signed vote events through `validateVoteEvent`. | verified below |
| A8 `notDesignee` and A10 `notProposer` listed as missing corpus rows | **Not corpus gaps.** Both constructors have **zero usages** — no code anywhere constructs them. Unobservable by construction, deliberately, pending Slice B. | Lean's own `.ilean` index |
| the absence of the runtime link rested on grep | **Replaced with the module graph and an executable control.** | below |

---

## 1. The requirement is ruled. Only the implementation is missing.

**Operator ruling** — `/tmp/reactivegas/ms2/e43/inbox/NOTE-016-vote-machine-composition.md`:

> a **second Lean machine** for kelgroups (questions, votes, majority) plus a
> **composition theorem** — the economic machine only ever consumes
> `grantPermission`/`denyPermission` emitted by the vote machine. […]
> `grantPermission` and `denyPermission` must be **provably vote-derived**, not
> merely responsabile-authored […] this now has **two** consumers, not one:
> purchase approval, and the voted comune backdonation.

NOTE-016 also fixes the honest status until the substrate mirrors it:
`enforced: PROVED-IN-MODEL`, not `enforced`.

**Routing ruling** — `/tmp/reactivegas/ms2/e43/answers/A-Q001-route-per-event-not-join.md`,
option D, classify-don't-join: `grantPermission`, `denyPermission` and
`backdonate` are **app-decided**, witnessed by
`KelGroups.Vote.ClosureRecord.verdict`, never by the base membership channel —
which has no proposal that could produce them.

`Composition.lean` implements that classification and proves it total and
wildcard-free. **The classification is delivered. The composition is not.**

### The evidence gap, stated exactly

`appDecided_verdict_exhaustive` proves the verdict elimination is exhaustive
and honest. It does not prove the composition, and three links are unbound:

```lean
theorem appDecided_verdict_exhaustive (e : Event)
    (record : KelGroups.Vote.ClosureRecord)
    (hroute : route e = .appDecided) :
    appVerdictAllows record = true ↔
      record.verdict = .positive ∨ record.verdict = .negative
```

1. **Reachability.** `e` and `record` are inputs to a standalone theorem. No
   production transition consumes a `ClosureRecord`.
2. **Target.** `e` and `record` are unrelated parameters joined by no premise.
   Nothing ties the closed question to the `CollId` in `grantPermission _ c`.
   A closure about collection 4 would satisfy this theorem for an event about
   collection 9.
3. **Polarity.** Nothing maps `.positive → grantPermission` and
   `.negative → denyPermission`. The theorem is satisfied by either pairing.

So the requirement is ruled, the classification is proved, and the wire from a
verdict to a transition — with its target and its polarity — does not exist.

### Evidence that it does not exist — and exactly what each instrument does not show

Not grep. Two instruments, each with its limit stated. **Neither is a
behavioural refusal witness, and neither is a reachability proof.** Desk ruling,
NOTE-005; the limits are recorded here so no downstream mandate inherits an
overclaim.

**Instrument 1 — module graph.** `Composition.lean` imports
`Reactivegas.Types`, `KelGroups.Fold`, `KelGroups.Invariants`,
`KelGroups.Vote.Fold`. It does **not** import `Reactivegas.Step`, so it cannot
name `Reactivegas.apply`. And nothing imports it except the aggregator:

```
$ grep -rn "import Reactivegas.Composition" lean/
lean/Reactivegas.lean:8:import Reactivegas.Composition
```

The transition chain is `Types → State → Step → Predicates → Invariants →
Trace`; `Composition` is a leaf off the library root.

*What it shows:* no module in the transition's import closure can mention
`route`, `voteDerived` or `appVerdictAllows`.
*What it does not show:* that any particular runtime behaviour is correct or
refused. It is a statement about names in scope, nothing more.

**Instrument 2 — build without the module.** Deleted
`lean/Reactivegas/Composition.lean`, removed its import, built from a clean
`.lake`:

```
$ lake build Reactivegas.Trace Reactivegas.Invariants Reactivegas.Step
✔ [20/20] Built Reactivegas.Trace (2.1s)
Build completed successfully (20 jobs).
EXIT=0        errors: 0
```

*What it shows:* the production transition, the invariants, the integrated
corpus and the trace emitter carry **no build-time dependency** on the
composition module.
*What it does not show:* **that an unbacked grant is refused.** No such refusal
exists to witness. A build that succeeds without a module says nothing about
what the code does when a caller fabricates a closure — that is a behavioural
property, and it requires an executable negative witness against a mutant.

**Consequence for the implementation mandate.** Because both instruments are
absence/structural leads rather than behavioural evidence, the composition
ticket carries executable negative witnesses as deliverables: an implementation
that ignores or fabricates the closure must be shown *failing* the unbacked,
wrong-target, wrong-polarity and reused-closure rows before those rows are
allowed to pass. That requirement is written into `#76` rather than left to a
reviewer to remember.

`Composition.lean` states the same absence in its own header, which is
consistent with both instruments rather than a substitute for either:

> *"nothing in this repository consumes `route` at runtime today."*

**One further observation, offered as a lead for whoever owns `#76`, not as a
finding I have closed:** `productionEnactmentWitness` and
`baseEnacted_threshold_met` are rooted in `KelGroups.applyEventDetailed` — the
historical generic fold that `Integration.lean` says "receives no production
responsibility" — rather than in `Reactivegas.apply`. `route` classifies zero
surviving constructors as `baseEnacted`, so nothing economic rides on it today.

---

## 2. Vote coverage that exists — verified on the artifact

### `VOTE_TRACES_V1`, simulator candidate `af9c1e5`

Emitted by `lean/KelTraceDriverV1.lean` under its own local schema
`kelgroups-vote.trace` v1, driving each seed through `validateVoteEvent` and
then `applyVoteEvent`. **15 signed vote events, real coverage, and this packet
does not duplicate it.**

| observable | seed | status |
|---|---|---|
| `openQuestion` `.collective` by a responsabile, empty tallies | `anna` | covered |
| `cast` by a non-responsabile | `dora` | covered — refused `notResponsabile` |
| `cast` on an unknown question | `anna` / `q:nessuna` | covered — refused `questionNotFound` |
| `cast .assent` below threshold, verdict stays `.open` | `anna` | covered |
| mixed 1 assent / 1 dissent, still `.open` | `bruno` | covered |
| **ballot switch** dissent → assent reaching threshold, `.positive` | `bruno` | covered |
| `cast .dissent` reaching threshold, `.negative` | `carlo` | covered |
| **idempotent re-cast** | `elena` | covered |
| `.permission designee`, non-designee casts, stays open | `elena` | covered |
| `.permission designee`, designee assents, `.positive` via the designee arm | `bruno` | covered |
| a question that simply stays open | `q:aperta` | covered |
| `renounce` by the proposer (Slice-A no-op) | `anna` | covered |

Threshold is pinned to `legacyThreshold` inside the driver. That is a **lane
implementation choice for a toy fold**, not a ruling, and it is why §4 matters.

### `#74`'s two corpora — measured on `fed19b3`

| | `economic.json` | `integrated.json` |
|---|---|---|
| signed vote events (`openQuestion`/`cast`/`renounce`) | 0 | 0 |
| `"openQuestions"` snapshots | 67, all `[]` | 8, of which 6 non-empty |
| non-empty `"closed"` | 0 | **2** |

- `economic.json` has no vote content and **cannot** acquire any: `step`
  returns `none` for the three vote constructors and `emitTrace` drives
  `stepEvent`.
- `integrated.json` **does** exercise a vote mechanism: `corpusInitial` seeds an
  open collective question, and the two closures come from `baseHook` running
  `sweepClosures` after a committed base change — **V-3 franchise-change
  closure**, credited here as real coverage. Its seven events are all
  `.direct`/`.propose`/`.approve`, not one `.app`, so `appFold`, `voteApply` and
  `validateVoteEvent` are never entered.

**Corrected headline:** *the two `#74` corpora contain no signed vote event;
`integrated.json` does exercise franchise-change closure; signed vote events are
exercised by `VOTE_TRACES_V1` on `af9c1e5`.*

### Why `VOTE_TRACES_V1` does not close the gap

It is reusable and should be reused. It is not a substitute, for three reasons
that are about scope rather than quality:

1. **It drives `VoteState`, not `GroupState State`.** Its states carry no
   `conti`, `casse`, `collections`. It exercises the vote machine standalone,
   never through `Reactivegas.apply`'s `.app` route.
2. **Different schema, different repo, different freeze.** `kelgroups-vote.trace`
   v1 with an embedded fixture and its own gate, on an unmerged simulator
   candidate — not `reactivegas.trace/v1` and not in `#74`'s manifest.
3. **It cannot reach the composition,** because the composition does not exist.
   No seed can assert that a positive closure permitted a purchase.

### Two error constructors nothing constructs — and it is deliberate

Revision 1 listed `notDesignee` and `notProposer` as missing corpus rows. They
are not. Lean's own cross-reference index reports:

```
VoteError.notResponsabile   usages: 3
VoteError.questionNotFound  usages: 2
VoteError.notDesignee       usages: []
VoteError.notProposer       usages: []
```

`validateVoteEvent` has three arms and constructs only the first two errors. The
declaration site says why: *"`notDesignee` and `notProposer` are declared here
from Slice A so Slice B …"* — forward declarations for unwritten work.

**Limit of this instrument, per NOTE-005:** a usage index is a *construction*
search, not a reachability proof. It shows no source site builds those values;
it does not by itself prove no execution can produce one. Combined with
`validateVoteEvent`s three arms being fully visible above, the behavioural
reading below is a strong lead — and a corpus row would be the actual
evidence, which is why `#75` records it as a residual rather than asserting it.

So a `.permission` question currently **accepts a non-designee's ballot** (it is
recorded and does not decide) rather than refusing it, and `renounce` by a
non-proposer is not refused. Neither is a corpus omission and neither is
this epic's to fix. Recorded so no downstream gate is written expecting a
refusal that cannot occur.

---

## 3. Remaining observables, after subtracting what exists

| # | observable | why it is still missing |
|---|---|---|
| **V1** | a signed vote event through `Reactivegas.apply`'s `.app` route, over `GroupState State` | `VOTE_TRACES_V1` drives `VoteState` standalone; `#74`'s corpora carry no `.app` event |
| **V2** | a vote closure and an economic post-state in **one** replayable envelope | no corpus spans both payloads |
| **V3** | franchise-change closure alongside economic payload | `integrated.json` has the closure; its payload is not economically active in the same step |
| **C1** | a positive closure permits its collection | the wire does not exist |
| **C2** | `grantPermission` **refused** with no backing closure | the guard is `isResponsabile signer` alone |
| **C3** | a closure on collection *x* does **not** permit collection *y* | target link unbound |
| **C4** | a `.negative` closure yields `denyPermission`, never `grantPermission` | polarity link unbound |
| **C5** | `backdonate` authorized by a closure rather than the `BackdonateAuth` callback | second ruled consumer, same missing wire |

V1–V3 need no semantics change. C1–C5 are the composition.

---

## 4. Policy authority, traced

Asked for by NOTE-004. One residual choice, and it is genuinely open.

| choice | status | authority |
|---|---|---|
| grant/deny provably vote-derived | **ruled** | operator, NOTE-016 |
| `backdonate` a second consumer of the same derivation | **ruled** | operator, NOTE-016 |
| app-decided witnessed by `ClosureRecord.verdict`, not the base channel | **ruled** | A-Q001 option D |
| classification total and wildcard-free | **ruled**, and implemented | A-Q001 condition 1 |
| status is `PROVED-IN-MODEL` until the substrate mirrors it | **ruled** | NOTE-016 |
| **threshold policy `θ` for a shipped coordinator** | **OPEN** | see below |

`θ` is a parameter of `Reactivegas.apply` and of every verdict evaluation. The
Lean is explicit that this is deliberate and that neither named instance is a
default: *"The threshold is a parameter everywhere (R-46); the two named
instances below are exhibits, not defaults"* — `legacyThreshold (n+1)/2` and
`zeroThreshold 0`.

Nothing in the record rules it. **`#68` does not rule it**: `#68` is the
proposer-counting arithmetic on the base channel, and it must not be read across
as a vote default. The simulator driver's `legacyThreshold` is a toy-fold choice
inside one lane, not a ruling either.

A shipped coordinator must pick one, and the choice moves every verdict row in
any frozen vote corpus. **Recommend it is ruled before V1 freezes**, so those
rows are not frozen against an arbitrary policy. Flagged, not chosen.

---

## 5. Two bounded ticket proposals

Filed 2026-09-05 per NOTE-005 as standalone tickets under milestone 2, parent #72, assigned to paolino, reporting to the desk — not children of #66 or #67. Deliberately two, with different owners and different blockers. **Neither
belongs to `#66`** — that ticket owns Lean quality, not all Lean work, and this
packet does not expand it. Owner assignment is the desk's.

### T-A — FILED as reactivegas#75, "Integrated vote corpus through the production root"

**Scope.** A third frozen corpus emitted by `#74`'s exporter, driving signed
vote events through `Reactivegas.apply` over `GroupState State`. Same wrapper
discipline (`GroupView` + auth identity, no widening), same manifest, same
`just` targets. Reuses `VOTE_TRACES_V1`'s journey where it transfers; does not
re-derive it.

**Not in scope.** No semantics change. No new `VoteError`. No `notDesignee` /
`notProposer` rows — they are unreachable, and this ticket records that rather
than enabling it.

**Blocked by.** `#74` landing; `θ` ruled (§4).

**Acceptance.**
- V1–V3 present as replayable steps.
- Coverage **quantified over the discovered extent**, not a hand list: every
  `VoteEvent` constructor and every *reachable* `VoteError` constructor appears;
  an added constructor fails the gate. The two unreachable errors are named in
  an explicit residual with the `usages: []` evidence, so the gate is honest
  about what it does not cover.
- Verify extended to the third file: re-emit to temp, `cmp`, `sha256sum -c`.
- Negative control **inside** the gate, as `#74` v3 carries: mutate one byte,
  require non-zero, restore byte-identical, require zero.
- Re-frozen after `#66` S1, `#68`, `#69`.

### T-B — FILED as reactivegas#76, "Runtime composition: vote closures must authorize grant, deny and backdonate"

**Scope.** Build the wire the operator ruled: a production transition in which
`grantPermission` / `denyPermission` / `backdonate` are derived from a
`ClosureRecord`, with target and polarity bound, and refused without one.

**Blocked by.** `θ` (§4); adjacency to `#68`/`#69`. Not blocked on `#73` — this
is Lean specification work, and the Haskell replay is separate.

**Acceptance — every row must be able to fail, and the refusals are the point.**

| # | must hold | shape |
|---|---|---|
| B-1 | a `.positive` closure on question *q* bound to collection *c* permits *c* | executed through the production root, not a standalone theorem |
| B-2 | **`grantPermission` with no backing closure is refused** | the row that makes the requirement real; must fail before the fix |
| B-3 | **a closure bound to collection *x* does not permit collection *y*** | closes the unbound-target link |
| B-4 | **a `.negative` closure yields `denyPermission` and never `grantPermission`** | closes the unbound-polarity link |
| B-5 | `.open` derives nothing | already proved; must stay proved through the new route |
| B-6 | `backdonate` derives from a closure, and the `BackdonateAuth` callback is either retired or explicitly scoped | NOTE-016's second consumer |
| B-7 | the classifier stays total and wildcard-free; an added constructor fails to compile | A-Q001 condition 1, preserved |
| B-8 | the status line moves from `PROVED-IN-MODEL` only when the substrate mirrors it | NOTE-016's caveat, not weakened |

**A mutation control belongs in this ticket**, because B-2, B-3 and B-4 are
absence claims: an implementation that ignores the closure entirely must fail
them. Each must be demonstrated failing before it passes.

### T-C — Haskell conformance (this epic, inside D3)

Replays T-A's corpus and, once T-B exists, its rows. Blocked on `#73`, `#74`,
T-A, T-B. No new scope requested here.

---

## 6. The state to hold on to

If the six-step outcome test were run today against a coordinator faithful to
the Lean as written, **assenso would pass by one responsabile calling
`grantPermission` alone.** It would look green.

That is not a specification error and not an open product question — the
operator ruled the requirement in NOTE-016 and the classification was proved
under A-Q001. It is unbuilt composition, and the outcome test is the thing that
would fail to notice.

Routing with the `#71` design content, per NOTE-004.
