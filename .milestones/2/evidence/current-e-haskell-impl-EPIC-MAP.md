# Epic map — `paolino/reactivegas#67` (Haskell implementation on kelgroups)

Owner `claude-opus-5[1m]`, pane `%504`, runtime root
`/tmp/reactivegas/ms2/e-haskell-impl/`. Parent: milestone owner, pane `%37`.
Parent epic issue `#72`. Base `origin/master` = `e6c5924`.
Resurrection-grade; rebuilt from durable state only.

Assessment: `handoffs/D1-ASSESSMENT.md`. State: **BLOCKED on Q-001.**

## The epic artifact — already satisfied

| field | value |
|---|---|
| artifact | the `server` bundle, `reactivegas-server-<tag>-linux-x86_64.tar.gz` |
| entry point | `app/Main.hs` → `Server.Service.main` |
| release path | `.github/workflows/release.yml`, release-please manifest mode, built **from the tag** |
| PR-side proof | `.github/workflows/build-artifacts.yaml`, same `scripts/release/package-release-artifact`, explicitly not a release |
| current published version | `v2021.12.0`, pre-release, 2026-08-26 |
| cadence declared | one at D2 (proves the new core starts), one at D5 (the outcome test) |
| merges upward into | the milestone artifact — it **is** the milestone artifact; no epic fork is created |

`resolve-epic` requires an epic-scoped runnable artifact released at day 0. That
condition was met before this epic opened. **No fork is created and none is
retired at close** — creating one here would add a second artifact shape with
nothing keeping it in step with the released one.

## Exporter branch — PARKED, completed (2026-09-05)

The exporter line of this epic is **finished and parked**. `#86` landed and closed
completed; `#74` and PR `#78` closed superseded with evidence preserved; `t86` is
`COMPLETE`, its lane watcher retired, its worktree and evidence intact and undeleted.

Post-merge `CI 33967518058` and `Release 33967518054` are **SUCCESS** at `d670323`
(desk-verified). My own earlier in-progress observations stand as historical.

**Wake conditions — what would reopen this branch:**

| wake | why |
|---|---|
| `#68` or `#69` lands | changes what the corpora emit; the frozen bytes must be **re-emitted and re-frozen** under the original fence |
| `#66` S2–S5 lands something that moves trace metadata | same — a recorded re-emission, never a silent one |
| `#75` accepts the R3.1 replay-context contract | the sidecar ships with `#75` and binds to these corpora |
| `#76` or `#81` semantics land | vote-derived authorization and V-5 closures change the corpus content |
| a consumer needs the corpus as a **final** oracle | it is provisional today; that promotion is a separate decision |

Until one of those fires, this branch needs nothing. **Publication and deployment
remain unauthorized**; the corpus stays provisional at `73a077fc…` / `1f173aec…`.

## Children

| slice | issue | owner | outcome | depends on | proof | state |
|---|---|---|---|---|---|---|
| **D0c** exporter | **#86** (superseding closed #74) | `t86-exporter-successor`, seat `muse`, pane `%529` — **terminal, retained-history** | both corpora as frozen artifacts + failing-closed hash gate, live-value binding, committed-CI verifier, declared tooling | none | **LANDED** PR #87, merge `d6703231` |
| **D0/D0b** substrate | **#73** | **not mine** — sibling epic `e-kelgroups-substrate`, owner `%532`, **authorized and in progress** since 10:57:46Z; reports to the desk | `kelgroups#28` + `#30` | Q-001 ruled | kelgroups test-suite green | outside fence |
| **D2** | — | none | `GroupState AppState`; `Key` identity; one membership store | #73 | the new core starts | **blocked** |
| **D3** | — | none | 17-event app fold + replay harness (repo’s first Haskell test-suite) | #73, D2, **#68 + #69** (its corpus producer #86 has landed)  | `cabal test` replays the frozen corpus step for step | **blocked** |
| **D4** | — | none | election → collection → pledge → assenso → purchase → refund | D3 | the six steps driven locally | planned |
| **D5** | — | none | release, then the stranger test from a clean directory | D4 | published asset fetched and driven, no source touched | planned |

D2 is the one child in this epic that adds nothing a person can run. Spent
deliberately and recorded; D3 makes it runnable. A second such child means the
epic is sliced by layer and must be re-cut.

## Exporter landed 2026-09-05 — and what it does not settle

PR #87 merged as `d67032313acf3699cc50358a057391b88d002192`, single parent
`4a6cd87…`, tree `d033effe3292fd4f7f2b1ac0dca46461d69088ee` byte-identical to the
audited candidate; GitHub signature `verified=true`. `#86` closed completed; `#74`
closed superseded with candidates never accepted and evidence preserved; PR `#78`
closed undelivered.

**Exporter acceptance with explicitly provisional corpus dependencies — not final
vote or economic conformance.** D2 and D3 are unaffected: they still wait on an
*accepted* substrate interface (`#73`).

Corpus bytes stay provisional at `73a077fc…` / `1f173aec…`; `#68`, `#69`, the
`#76`/`#81` semantics and the `#75` replay context remain, and any accepted
upstream integration needs a **recorded** re-emission under the original fence.

Named residual: inherited merge `bc44998` fails the commit-gate subject shape,
accepted as an exact hygiene exception rather than rewriting audited history.

**Execution-provenance deviation, recorded not smoothed.** The landing used a shell
CLI squash with `--match-head-commit`, gated by a **hand-rolled three-condition shell
guard**, after the exact triple was re-verified at action time. The named
`guard_merge` tool has **no execution record**; the raw invocation and output are
preserved at `handoffs/MERGE-EXECUTION-PROVENANCE.md`.

**Narrowed per NOTE-032** — my first statement overreached. The evidence
establishes that the commissioned Pi/muse child did not have the required callable
tool and that the Claude PreToolUse hook was not installed in it. It does **not**
establish that only Claude seats can invoke the guard: the Codex desk has the
merge-guard MCP tools too, and **hook installation and MCP tool availability are
different facts** which I conflated.

**Prevention adopted:** before dispatching a tool-specific mandatory instruction,
verify the actual executor can invoke the named tool. If the child cannot, merge
execution is retained in this parent seat after the exact-SHA grant and the child's
frozen ready packet. If no authorized executor is capable, return that as a concrete
blocker — never a silent CLI substitution.
## Contract registry

| contract | producer | consumers | stable version | release signal | enforcing check |
|---|---|---|---|---|---|
| `KelGroups.Integration` surface (26 names; **19 landed via kelgroups#28, 7 vote-machine names outstanding in #30**) |  `paolino/kelgroups` (#28, #30), tracked as reactivegas#73 in its own lane | this epic (D2, D3) | **none — unimplemented; lane authorized and in progress, contract not yet accepted** | kelgroups release/tag | **NONE** |
| `reactivegas.trace/v1` envelope | `lean/Reactivegas/Trace.lean` | simulator lane, this epic (D3) | v1, frozen **in Lean source only** |  **#86 built it — LANDED** | `TraceTests.lean` today; #74 adds a failing-closed hash gate |
| integrated corpus (7 base steps) | `lean/Reactivegas/Invariants.lean` | this epic (D3) | unversioned |  **#86 built it — LANDED** | `just lean-corpus-gate` today; #74 adds the artifact gate |
| one-membership-store invariant | `lean/KelGroups/Integration.lean` (#62) | D2 | proved in Lean | n/a | **NONE in Haskell** |
| the six-step outcome test | milestone | D5 | n/a | published release | **NONE** — never yet run against a new core |
| `AppState = { conti, casse, collections, votes }` | `lean/Reactivegas/State.lean` (#62) | D2, D3 | proved | n/a | **NONE in Haskell** |
| B1 / B2 — proposer is not an assent; pledge sovereignty | operator, tracked as reactivegas#68 and #69 | D3 corpus freeze | ruled, **not yet landed** | the amendments merge to master | Lean proofs once landed; corpus re-frozen after |

Five of seven rows enforce nothing today. That is the honest state: this epic
consumes contracts that exist as Lean proofs and as nothing else.

## Invariant ledger

| shape | current instance | mechanism enforcing it |
|---|---|---|
| one membership store, one insertion path | `INV-62-DIRECT-ONLY`, structural in `BaseMutation` | Lean types; **nothing in Haskell** |
| app fold writes payload only, never a group | `IntegratedAppFold` return type | Lean types; **nothing in Haskell** |
| a committed base change and its consequences are one transition | `commitBaseChange` runs the sealed hook | Lean; **nothing in Haskell** |
| the reserved comune key is never a member | `productionWellFormed`, checked pre and post | Lean; **nothing in Haskell** |
| refusal cannot advance state | `foldIntegrated` keeps the aggregate on error | Lean; **nothing in Haskell** |
| specification and implementation agree step for step | the conformance oracle | **does not exist as an artifact** |

Recurring shape across every row: *the invariant is a Lean type, and the
Haskell has no counterpart.* D2 and D3 are where each acquires a mechanism, or
acquires a named residual saying it did not.

## Rulings absorbed (A-001…004, milestone owner, 2026-09-05)

| q | ruling |
|---|---|
| Q-001 | kelgroups gets **its own lane under the milestone owner**, not a sub-lane of mine. No vendoring — a second implementation of one substrate contract is the exact shape this milestone has spent weeks deleting. Milestone 2 now spans two repos; escalated to the operator. D2/D3 stay BLOCKED and I say so. |
| Q-002 | The `lean/` prohibition was **mis-scoped to a directory**; it meant *do not change the model*. Additive `lean/` work authorized: `lean_exe`, frozen corpus files, hash gate. Stop and escalate if it needs a theorem, guard, `stepEvent`, `Trace`, or state type. |
| Q-003 | `Voci/` **out of scope**; record as an explicit non-goal with its reason. Whether the Lean's silence is deliberate goes to the operator. |
| Q-004 | **#68 and #69 land before the corpus is frozen.** The exporter's *format* is independent and proceeds now. |

## Upstream status — authorized, in progress, not delivered (2026-09-05)

The cross-repository permission question is **settled**: the operator authorized
a team on kelgroups. Verified independently rather than relayed — epic runtime
root `/tmp/reactivegas/ms2/e-kelgroups-substrate`, owner `START` at 10:57:46Z in
pane `%532` on the muse pin, first child `t28-app-api` dispatched 11:00:10Z to
`%534`, ordering `#28` then `#30` with independent audit, both panes live.

**Partially landed 2026-09-06.** `kelgroups#28` merged (`933e385d…`); the contract
went from **0 of 26** names at D1 to **19 of 26**, the seven absent being exactly
the vote machine (`Verdict`, `Ballot`, `ClosureRecord`, `foldVote`, `sweepClosures`,
`Question`, `Threshold`) — `kelgroups#30`, untouched. `IntegratedAppFold` and
`BaseHook` landed faithful to the Lean contract with a real rejection channel, which
resolves the load-bearing D1 blocker; no cross-repo contract change arises.

**A landed slice is not an accepted interface.** An authorized lane is not an
against the reconciled contract, and may not vendor or duplicate upstream
implementation.

**That epic reports to the desk, not to me.** All contract changes and handoffs
cross through the desk. I do not direct, question or supervise it.

Boundaries that did not move: client API compatibility needed by `#28`/`#30` is
authorized, the broader Reactivegas browser UI (`#84`) is not; `#76`/`#81`
semantics are not anticipated; no shipped threshold default is ruled.

## Product boundaries carried from #43 (NOTE-012)

Preserved explicitly because they constrain D4 and D5 and are easy to lose:

- **Multi-gruppo MVP is multiple server instances.** D4 exposes one gruppo per
  instance and must not grow a multi-tenant surface.
- **Migration of the live 2018 gruppi is out of scope** — a follow-on milestone.
- **The milestone completion boundary is wider than the six-step coordinator
  test.** The milestone description reads *"Done when a released coordinator +
  browser client + CLI implement the laws end-to-end for one gruppo"*, so D5
  satisfying the stranger test is necessary and **not sufficient**. Browser (#84)
  and CLI (#83) are inside the boundary.
- **#70's current JS acceptance is unchanged.** #82 builds the wasm core; it does
  not replace the simulator and nothing authorizes that.
- **#43 remains unrespawned.** Its requirements are carried by ticket, not by
  reviving the epic.

Full reconciliation, with the one supersession and the one unresolved item:
`handoffs/EPIC43-REQUIREMENT-MAP.md`.

## Corrections I made and had to retract (dated)

Kept visible rather than edited away, because the error shapes matter.

**2026-09-05 — the browser client does not need a new repository.** I reported
that `paolino/kelgroups-client` does not exist and that the browser client
therefore entailed a third repository needing a cross-repo ruling. False. I ran
`gh repo view` and `gh repo list` and never opened the kelgroups tree.
`client/kelgroups-client/` is a package inside `paolino/kelgroups` at HEAD
`368b596`, with keys, transport, an HTTP API client, a shell app and a nix
bundle. **Absence of a repository is not absence of a component** — the same
inference shape I had been correcting in others.

What the package genuinely lacks, measured: the Reactivegas UI, and `core.wasm`
integration. And it carries `Client/Fold.purs`, a PureScript reimplementation of
the base fold — a second implementation of base semantics that #43's
"semantics come from core.wasm" is in tension with. That is #84's substance.

**2026-09-05 — the artifact question was not architecture.** I flagged #43's
`kelgroups-server`-with-the-app-plugged-in against the existing `server` bundle
as an unresolved architecture question. It is not: the operator ruled the
Haskell implementation on kelgroups as the backend, and a working legacy release
does not reopen that. Measured: kelgroups' library is polymorphic and its
`app/Main.hs` links `KelGroups.Trivial` directly, so an app is supplied at link
time in an executable's `Main`. What remains is a **delivery choice** for D4 and
#51 — which package produces the coordinator executable, and whether the released
artifact keeps its current name and `bin/server` entrypoint. Packaging, not
product.

Full detail: `handoffs/EPIC43-REQUIREMENT-MAP.md`.

## Standing rules absorbed

**No lane writes into `docs/en/design/` while `#71` is rewriting it** (milestone
owner, 2026-09-05, A-006). Design-record content is produced as a handoff and
routed to the milestone owner, who delivers it to `#71` as required sections.
Milestone-wide, not specific to this epic. Two handoffs are queued under it:
`handoffs/NON-GOAL-voci.md` and `#74`s `handoffs/CORPUS-COVERAGE.md`.

## Open questions

None of mine. One sequencing question raised to the milestone owner: where the
`Voci/` non-goal entry lands, given `#71` is rewriting the design record.

## Resume record

- **Parent desk:** `%510` (gpt-6-astra) as of NOTE-003. Full resurrection detail
  lives in `handoffs/RESUME-FRAGMENT.md`; the lines below are the summary.
- **Launch:** epic owner pane `%504`, window `reactivegas-e67-t74-corpus-exporter`,
  worktree `/code/reactivegas-haskell-impl`, branch `feat/haskell-on-kelgroups`,
  base `e6c5924`, tree clean.
- **Skill chain:** `orchestrator-contract` → `epic-orchestrator` → `resolve-epic`
  → `worker-protocol` → `tmux-orchestrator`.
- **Child:** `t74-corpus-exporter`, pane `%505`, seat `muse`, launch command
  `muse --approve` (wrapper pins `pi --provider opencode-go --model
  muse-spark-1.3-contributor --thinking xhigh`; rejects overrides, exit 64;
  refuses degraded launch, exit 69). Auditor family resolved to `grok`.
  Root `/tmp/reactivegas/ms2/e-haskell-impl/t74-corpus-exporter/`.
- **Stage:** D1 delivered and published to `#67`; `#74` filed and active; D2/D3
  blocked on `#73`.
- **Accepted commits:** none.
- **Next action:** supervise `t74-corpus-exporter` — read its artifacts, not its
  events. Accept `#74` at ticket altitude; do not merge without authorization.
  Re-freeze the corpus after `#68` and `#69` land.
