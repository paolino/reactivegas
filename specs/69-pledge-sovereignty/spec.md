# Member sovereignty over their own pledge — #69

Authority: [issue 69](https://github.com/paolino/reactivegas/issues/69); operator ruling
2026-09-05: *a member may change or withdraw their own pledge freely while it is `pending`; once
the referente has accepted it, changes require the referente.* The issue's acceptance paragraph
(pending withdrawal removes the row) binds. #48's departure design is superseded and not built.
Base: origin/master `222ad9a` (#68, #76, #81, #66 S3+S5 landed).
Model position: Inside — revises `Reactivegas.step` arms `.pledge` and `.correctPledge`
(`lean/Reactivegas/Step.lean`) under the ruling above; every other declaration keeps its meaning.
Class: changes behaviour (Lean model). Ceiling: this file 120 lines / 9000 bytes.

## Stories

- A member pledges for themselves; nobody else can pledge in their name.
- While the pledge is pending, its owner raises it (if their conto covers the difference),
  lowers it, or withdraws it (amount 0): the row disappears and the money returns to the conto.
- Once the referente has accepted the pledge, the owner can no longer touch it; the referente's
  correction works exactly as before.

## Ruled guards (the whole change)

| ID | Event | Guard set after #69 (everything else in the arm unchanged) |
|---|---|---|
| G-PLEDGE | `pledge u c v` signed by `a` | `a == u` replaces `isResponsabile view a`; kept: `isMember u view`, no accepted/pending row of `u` in `c`, `0 < v`, `bal conti u ≥ v`, not stalled |
| G-ACC | `correctPledge u c v'`, `u` has an accepted row in `c` | byte-equivalent to the base arm: `isResponsabile a`, `c.referente == a`, `0 ≤ v'`, `bal conti u + (v - v') ≥ 0`; zero keeps an accepted row (unchanged) |
| G-PEND | `correctPledge u c v'`, `u` has no accepted row and a pending row `v` in `c` | `a == u`, `0 ≤ v'`, `bal conti u + (v - v') ≥ 0`; effect: conti `u` += `v - v'`; `v' > 0` replaces the pending row by `⟨u, v'⟩`; `v' = 0` removes it; `accepted` untouched |
| G-NONE | `correctPledge` with no row of `u` in `c` | refused (unchanged) |

Regime choice: accepted is looked up first, so G-ACC is the base behaviour on every state, not
only on reachable ones. Decision D-STALL: no stall guard is added to G-PEND (the issue names the
guard set; G-ACC has none either). Recorded for the operator in the handoff, not a blocker.

## Acceptance

Every row is BLOCKING (each reaches money). "Root" = the production root `Reactivegas.apply`,
driven through the #76 helpers (`run`/`runBase` in `CompositionTests.lean`) from a guarded boot.

| ID | Must hold, as a compiled witness through the root | Can-fail mutant shown failing |
|---|---|---|
| J-CREATE | member `m` signs `pledge m c v`, affordable: accepted; `c.pending` holds `⟨m,v⟩`; conti `m` −v | M-RESP: pledge keeps the responsabile guard |
| J-RAISE | owner raises a pending pledge within their conto: accepted, row = v', conti −(v'−v) | M-PEND-OFF: correction still reads only `accepted` |
| J-LOWER | owner lowers a pending pledge: accepted, conti +(v−v') | M-PEND-OFF |
| J-WITHDRAW | owner corrects pending to 0: `u` has no row in `c.pending` nor `c.accepted`; conti +v | M-ZERO-ROW: a zero pending row is kept; M-PROMOTE: withdrawal writes an accepted row |
| J-UNBLOCK | withdrawing the last pending pledge of a permitted collection lets the referente's `closePurchase c` succeed; with another pending pledge left it is still refused | M-ZERO-ROW |
| J-SWITCH | after the referente's `acceptPledge u c`, the owner's `correctPledge u c _` is refused; the referente's succeeds with the base effect | M-ACC-SELF: accepted regime admits the owner |
| R-IMPERSONATE | refused, root state unchanged: a responsabile's `pledge u c v` for `u ≠` signer; another member's, and the referente's, `correctPledge u c v'` on `u`'s pending pledge | M-RESP; M-PEND-ANY: pending regime ignores the signer |
| R-UNAFFORD | owner raises a pending pledge by more than their conto: refused, unchanged | M-SOLV: solvency guard dropped from G-PEND |
| R-NEGATIVE | `pledge` with `v ≤ 0`, `correctPledge` pending with `v' < 0`: refused, unchanged | M-NEG: `0 ≤ v'` dropped from G-PEND |

## Invariants

| ID | Observable truth |
|---|---|
| INV69-REACH | every row above has a witness whose verdict is computed by the root on a reachable history; a structural or build-time observation is not a witness |
| INV69-CANFAIL | each mutant is a permanent, compiled, mutation-only definition judged by the same oracle that accepts production, and that oracle evaluates to rejection on it (repository inversion convention, e.g. `checkVoteApplyBypassCaught`) |
| INV69-SCOPE | conservation (`conservation_preserved`), solvency (`solvent_preserved`, `reach_solvent`) and uniqueness (L8, `allUniquePledges`) stay proved over the new step with their statements unchanged; each gains the new arm in its proof, not a narrowed hypothesis |
| INV69-THEOREMS | new theorems state the ruled guards: a successful `pledge` has signer = `u`; a successful pending correction has signer = `u` and leaves `accepted` unchanged; a successful correction of an accepted row is the base arm's effect; a zero pending correction leaves no row of `u` |
| INV69-LEDGER | every new theorem has a row in `lean/mutants/ledger.json` (`KILLED`/`HELPER`, or `OPEN` with reason and attempted mutants); renderings do not drift; CI "Lean mutant ledger" projected ≤ 110 min, else BLOCKED before adding rows |
| INV69-SUPERSEDED | only statements, witnesses and fixtures that encode the superseded behaviour (responsabile pledges for another, member cannot correct pending) are replaced, each by its ruled counterpart; seed traces keep their intent with the pledge re-signed by its owner |
| INV69-CORPUS | `lean/corpus/economic.json` and `corpus.sha256` are re-exported only by `just lean-corpus-export` from re-signed seeds; `integrated.json` byte-unchanged; Haskell replay extent (`CorpusSpec`, deposit=7) unchanged; the PR carries the CorpusSpec run showing the same custody counts (desk A-001) |
| INV69-SIM | desk A-001 option B, bounded: `lean/TraceDriverV1.lean` seeds re-signed by the pledger; `economics-simulator-core.mjs` `pledge`/`correctPledge` guards equal G-PLEDGE/G-ACC/G-PEND/G-NONE row by row with the same refusal reasons; trace fixture re-emitted and claim-gate pins re-derived by the repository commands; existing simulator gates green and unchanged in what they check; no page/UI change, no generator or page-check change |
| INV69-MIRRORS | every new Prop-valued definition reconciles under `scripts/check-lean-mirrors`; inversion coverage stays green |
| INV69-CONTRACT | no type change: `Event`, `AppEvent`, `State`, `Collection`, `Pledge`, `StepError`, `LiveAuth` unchanged; #76/#81 witnesses unchanged except INV69-SUPERSEDED |

## Non-goals

- NG-1 #48's departure design (superseded by the issue).
- NG-2 UI legibility of pending vs accepted, page changes, new simulator features (#70); this lane hands off event shapes, refusal reasons and where pending vs accepted is observable.
- NG-3 Haskell production (Haskell follows Lean, later ticket); `docs/en/design` (#71).
- NG-4 Stall semantics of corrections (D-STALL).
