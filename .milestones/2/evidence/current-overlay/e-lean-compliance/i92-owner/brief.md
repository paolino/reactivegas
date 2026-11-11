# BRIEF — #92 integration repair, implementation owner

Commissioned by quality epic owner `%503` under NOTE-080/083/084.
Issue: https://github.com/paolino/reactivegas/issues/92 (milestone 2, `chore`).

You are a **fresh** seat with no inherited context. Read your inputs yourself.

## Identity — verify before START

| | |
|---|---|
| model | `muse-spark-1.3-contributor`, thinking `xhigh`, provider `opencode-go` |
| bases | quality base `efef604de87b2a1efae51e84d1a9150e585c1db0` (tree `caaa0488f39a6afb2553680a11fd6bfd86d1c90b`); C1 candidate `48f76d96eb0975ec6c21cc5ba490af196d4882fa` |

Own a post-cursor `START` via
`/code/llm-settings/shared/skills/worker-protocol/scripts/status-event`. **Known
tag vocabulary only** — `START`, `NOTE`, `BLOCKED`, `RESUMED`,
`GATE-PASS`/`GATE-FAIL`, `COMMIT`, `PUSHED`, `COMPLETE`. **Invent no tag.**
**Every way you stop carries `COMPLETE` or `BLOCKED`** — a progress summary is not
a terminal event, and a parent can only wait on a tag it knows.

## The defect — already localized, do not re-derive it

`scripts/check-lean-mirrors` derives its tracked set **dynamically** but imports a
**fixed** set of roots into its generated driver.

- `s4bTrackedModules` derived live, substituted at `:416`.
- Generated driver imports: a **fixed seven-root block** at `:58-64` — `KelGroups`,
  `Reactivegas`, `Reactivegas.Mirrors`, `KelGroups.Mirrors`,
  `Reactivegas.CorpusExport`, `Reactivegas.CorpusGate`, `Reactivegas.TraceTests`.
- Import-reach completeness at `:266-274` requires every tracked module to be in
  the imported closure.

**The failure class, precisely — not universally:** a **newly tracked registered
top-level root absent from the driver's actual import closure**. A module
transitively reachable from those seven resolves normally — `lean/Reactivegas.lean`
imports `Reactivegas.Types`, `.State`, `.Step`, `.Predicates`, `.Invariants`,
`.Trace`, `.Composition`. `lean/TraceDriverV1.lean` and `lean/KelTraceDriverV1.lean`
are top-level and reachable from none.

**Executed evidence** in `inputs/BRANCH-RED-just-ci.log`, lines 2976–2977:
`MIRROR-IMPORT-REACH-GAP KelTraceDriverV1` and `… TraceDriverV1`.
`inputs/BASE-GREEN-just-ci.log` shows the same command exit 0 on the base with an
**identical discovered census**.

**Two things this is not.** There is **no diagnostic defect** — the checker named
both offenders; `MIRROR-CHECK-FAILED` is only the terminal summary. That behaviour
already works and is **asserted as a regression, not built**. And there is **no
registration defect** — the two driver modules are legitimate; **never deregister
them**.

## Budget — TEN units, shared, frozen

**A mandatory `nix develop --quiet -c just ci` is ONE substantive execution**,
including its recipe and dependency expansion. Internal CI stages are **not**
charged recursively. Each separate aggregate invocation counts again; ad hoc runs
count separately. **Never wrap independent runs or retries into one script to
relabel them as one unit.**

Your allocation is **I1–I6**. I7/I8 belong to two blind inspectors, I9/I10 are
contingent on the single repair bounce. **No raises.** A setup failure is logged
and uncharged **only** when it fails before reaching the assertions and does no
product work; anything that reaches the product and fails is charged.

| # | unit |
|---|---|
| **I1** | `just ci` on an isolated **combined** tree at both exact candidates, **before any edit** — the RED must be exactly the two named reach gaps and nothing else |
| **I2** | `just ci` on that tree **after** the repair |
| **I3** | `just ci` on a tree carrying an **independently introduced import-reach omission** — must still fail, **naming the actual omitted identity** (regression assertion) |
| **I4** | `just ci` on a tree carrying a **checker-disable/bypass attempt** — must **not** pass the assurance gate |
| **I5** | `just ci` on a tree carrying an **invalid-import / missing-artifact** setup condition — must stay distinguishable from a *reached* reconciliation failure, with its own named diagnostic |
| **I6** | `just ci` on the **quality-only** candidate at `efef604d` after the repair — original mirror correspondence and classification still hold at their existing scope |

Each control gets **its own** run on a tree carrying **that control's single
injected condition**. A control is never folded into another run.

Enumerate your actual command schedule before spending. If it cannot fit, **return
the exact branch still unexecuted before spending** — do not launch an unfunded
command.

## Acceptance

- The C1 candidate and its registered drivers pass the committed mandatory CI path
  with your repair, **retaining every existing CI check and the driver
  registration**.
- A properly registered added project root is supported **according to declared
  ownership** — **no ad hoc name-list exception**, and **no exclusion of legitimate
  project sources**. A hardcoded `TraceDriverV1`/`KelTraceDriverV1` allowance is a
  failed submission.
- The regression assertion holds: an independently introduced omission still fails
  **naming the actual omitted identity**; a disable/bypass control does not pass.
- Setup failure stays distinguishable from a reached reconciliation failure, each
  with a **named** diagnostic rather than a generic one.
- Original mirror correspondence and classification requirements stay tested at
  their existing scope. **No semantic or count quota shortcut. No weakening of any
  upstream requirement to obtain green.**

## Fences

Own surface: `scripts/check-lean-mirrors` and, if strictly required, the `justfile`
wiring. **No new product semantics. No `docs/en/design/` writes. No simulator-lane
edits and no driver deregistration** — if the evidence turns out to require a
change only to simulator-owned registration, **stop and return that disposition**;
it is not yours to make.

No push, no PR, no merge, no issue comments. **Local delivery only** — your own
`STATUS.md` and `handoffs/`. Nothing to any human composer. No contact with any
other seat.

## Submissions

**At most two, with one adjudicated repair batch between them.** Submission 1 goes
to two blind inspectors; `%503` adjudicates **once** and sends **one** batch. Each
submission writes to its **own** directory, frozen with a `MANIFEST.sha256`
carrying **no self-entry**.

Demonstrate the final integration in an **isolated combined tree bound to BOTH
exact candidates**, and keep that evidence **distinct** from the quality-only
candidate CI.

This closes nothing. #66, #72 and #92 stay open; base S4 acceptance is **not**
retracted.
