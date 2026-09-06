> **SUPERSEDED HISTORY — DO NOT EXECUTE.**
> Written 2026-09-05T07:2xZ. Its instructions address a live `t74` and base
> `e6c5924`, both long superseded. The canonical current record is
> `RESUME-FRAGMENT.md` (sha256 `8722514f55e6543b3aa367a988f00216eb973936423196b405c02da7fcc1b960`).
> Body below is unaltered; its own sha256 as copied was
> `536eb4c997277ef6f53508c6fbfeac52ce8f576bb2584172d52f5de6441d0ce7`.

---

# Resume fragment — epic `#67`, for desk `%510`

Current as of 2026-09-05T07:2xZ. Resurrection-grade: everything needed to
rebuild this seat is here or named here.

## This seat

| | |
|---|---|
| role | epic owner, `paolino/reactivegas#67` (parent `#72`, milestone "Reactivegas on kelgroups") |
| argv | `claude --dangerously-skip-permissions --model 'claude-opus-5[1m]'` |
| pane | `%504` |
| window | `reactivegas:5`, named `reactivegas-e67-t74-corpus-exporter` |
| worktree | `/code/reactivegas-haskell-impl`, branch `feat/haskell-on-kelgroups`, base `e6c5924`, **tree clean — this epic has committed nothing** |
| runtime root | `/tmp/reactivegas/ms2/e-haskell-impl/` |
| parent | **desk `%510` (gpt-6-astra)** as of NOTE-003; previously `%37` |
| skill chain | `orchestrator-contract` → `epic-orchestrator` → `resolve-epic` → `worker-protocol` → `tmux-orchestrator` |

## Child

| | |
|---|---|
| worker | `t74-corpus-exporter`, ticket owner for `#74` |
| pane | `%505` |
| seat | `muse` — launch command **exactly** `muse --approve` |
| verified identity | wrapper pins `pi --provider opencode-go --model muse-spark-1.3-contributor --thinking xhigh`; rejects provider/model/thinking/api-key overrides (exit 64); refuses degraded launch when the model is absent from the catalog (exit 69). Live argv confirmed in-pane before `START` was admitted. |
| runtime root | `/tmp/reactivegas/ms2/e-haskell-impl/t74-corpus-exporter/` |
| worktree | `/code/reactivegas-issue-74`, branch `feat/74-corpus-exporter`, base `e6c5924` |
| its commit owner | `muse`, pane `%506` (operator ruling suspends the T.O./owner alternation fence for this seat pair) |
| its auditor | **`grok`**, resolved by `alternate-authoritative-cli --seat commit-auditor muse muse`. Never `muse`, never `glm`. Not yet spawned. |
| head | `fed19b3` |
| gate | `gate.sh`, untracked+ignored, v3 hash `66ea7cb62676d3405b503b6bb1204cedff5f0d01f557e411159353f3b343816a`, backup at `t74-corpus-exporter/evidence/gate.sh.v3` |

## Stage

- **D1 assessment** delivered (`handoffs/D1-ASSESSMENT.md`) and published to
  `#67` as an issue-body edit, not a comment.
- **`#74`** active. Head `fed19b3` carries `CorpusExport.lean`, `lean_exe`,
  `lean/corpus/{economic,integrated}.json`, `corpus.sha256`, and the verify
  target wired into `just ci`. Wrappers bounded to the approved
  `GroupView` + auth shape. Negative control ran both ways.
- **Not yet done on `#74`:** fresh `grok` auditor on `fed19b3`; ticket-owner
  journal entry for `fed19b3`; `handoffs/CORPUS-COVERAGE.md` promoted out of
  the grandchild root and amended to a three-input freeze list. All three
  ordered in `t74-corpus-exporter/inbox/NOTE-003`, acknowledged 07:23:34Z.
- **D2, D3** blocked on `#73`. No ticket owner dispatched for either. No
  vendoring, no implementation in `kelgroups` from this epic.
- **D4, D5** planned, unstarted.

## Exact next action

Supervise `t74-corpus-exporter` through: journal reconciliation → fresh `grok`
auditor on `fed19b3` → findings → acceptance packet at ticket altitude. Do not
merge; no push/PR/merge authority without explicit desk authorization. Then
hand the acceptance packet to `%510`.

## Missing dependencies, in the order they bite

| # | dependency | blocks | owner |
|---|---|---|---|
| 1 | `#66` S1 — `Trace.lean` `Name.mkSimple` repair | the **final** corpus freeze; `#74`'s bytes already carry one wrong `"declaration":"UNPROVED"` on guard `withdraw` | Lean lane |
| 2 | `#68`, `#69` | the final corpus freeze | Lean lane |
| 3 | `#73` — the kelgroups substrate contract (0 of 26 names exist) | **D2 and D3 entirely** | its own lane under the desk |
| 4 | the assenso wire — no executable path from a vote closure to `grantPermission` | the milestone's own outcome test | **operator ruling**; see `handoffs/ASSENSO-ORACLE-GAP.md` |
| 5 | threshold policy `θ` — a parameter with two named exhibits and no default | any frozen vote verdict, and the shipped coordinator | **operator ruling** |
| 6 | `Voci/` non-goal routing into `#71` | nothing; recorded | desk |

## Durable artifacts in this root

| file | what it is |
|---|---|
| `handoffs/D1-ASSESSMENT.md` | the assessment, its evidence table, and the port-vs-rewrite verdict |
| `handoffs/ASSENSO-ORACLE-GAP.md` | the bounded assenso packet: measured coverage, missing observables, proposed owners, executable acceptance |
| `handoffs/NON-GOAL-voci.md` | the `Voci/` non-goal, routed to `#71` via the desk |
| `handoffs/RESUME-FRAGMENT.md` | this file |
| `EPIC-MAP.md` | epic map, contract registry, invariant ledger, standing rules |
| `STATUS.md` | append-only journal |
| `questions/Q-001…004`, `answers/A-001…006` | the ruled decisions |

## Standing rules absorbed

- No lane writes into `docs/en/design/` while `#71` rewrites it; content routes
  to the desk as handoffs.
- No push, PR, merge, or issue comment without explicit desk authority.
- `reactivegas.trace/v1` is preserved; no widening beyond the approved
  `GroupView` + auth wrapper without escalating first.
- Every commit gets a journal line; the parent verifies by diffing `git log`
  against `STATUS.md`. This caught `fed19b3`.
