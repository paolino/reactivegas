# #92 integration repair — costed command schedule inside the TEN-invocation ceiling

Owner `%503`, compiled before dispatch as NOTE-080 requires. **Enumerated with
nested executions counted individually; no bundle is called one invocation.**

Issue: https://github.com/paolino/reactivegas/issues/92 — filed under #66, no
duplicate found across the 18 open issues.

## Confirmed surface — executed localization, not a hypothesis

The desk's lead is **confirmed** and one routed finding is **refuted**. From the
raw branch log (`c3536e36…`, lines 2976–2977):

```
error: MIRROR-IMPORT-REACH-GAP KelTraceDriverV1 (tracked owned module outside the checker's import closure; …)
error: MIRROR-IMPORT-REACH-GAP TraceDriverV1 (…)
```

- The failure **is** the import-reach check at `:266-274`. Executed, named.
- **There is no diagnostic defect.** `fail` calls `logError` (`:193-195`); both
  offenders were named, and `MIRROR-CHECK-FAILED` at line 3006 is only the terminal
  summary. The earlier "named nothing" reading came from the tail of the log.
- **Mechanism:** `s4bTrackedModules` is derived live and substituted at `:416`,
  but the driver's imports are a **hardcoded seven-line block** at `:58-64`.
  Tracked grows automatically; the driver's import closure is fixed. **Narrowed
  precisely, not universally:** a module transitively reachable from those seven
  roots resolves normally — `lean/Reactivegas.lean` imports `Reactivegas.Types`,
  `.State`, `.Step`, `.Predicates`, `.Invariants`, `.Trace`, `.Composition`. The
  failure class is a **newly tracked registered top-level root absent from that
  closure**, which the two executed driver gaps establish here. The comment at
  `:181-183` promises "either resolves (imported) or fires the reach gap loudly";
  for a new top-level root nothing automates the resolve side.
- **Therefore the surface is quality-owned** (`scripts/check-lean-mirrors`), not
  simulator-owned registration. I own the repair; `%313`'s lane is not edited and
  its driver registration is not reverted.

## Frozen execution units (NOTE-083) — bound before first spend

**One frozen mandatory `nix develop --quiet -c just ci` invocation is ONE
substantive execution**, including its declared recipe and dependency expansion.
Internal CI stages are **not** charged recursively — that counting defect made a
single mandatory run exceed the whole ceiling in the #90 preflight. Every check
still runs, and every actual command, stage, exit and cost stays in evidence.

- Each **separate aggregate invocation** counts again.
- **Separate ad hoc runs count separately.**
- **No wrapping** of independent actor runs, retries or previously separate
  experiments into a new script to relabel them as one unit.

**Unit for each numbered invocation below:**

| # | frozen unit |
|---|---|
| I1, I2, I6 | one aggregate `nix develop --quiet -c just ci` each |
| I3, I4, I5 | one aggregate `nix develop --quiet -c just ci` each, on a tree carrying that control's single injected condition — a control is **not** folded into another run |
| I7, I8, I10 | one inspector-chosen invocation each, aggregate or narrower; unspent if the scenario settles statically |
| I9 | one aggregate `just ci` re-validation, contingent on the repair bounce |

**Pre-amendment spend on this campaign: ZERO.** The issue was filed and the
schedule costed; no owner has been dispatched and no invocation has run. There is
nothing to disclose as pre-amendment and nothing reclassified.

## The ceiling

**TEN substantive build/run invocations, shared across the whole campaign.** At
most **2 submissions**, **one repair**, **no raises**. Historical S4 and S3 spend
stays intact and separate.

| # | who | invocation | establishes |
|---|---|---|---|
| **I1** | author | `just ci` on an isolated **combined** tree at both exact candidates, **before** any edit | reproduces the measured boundary — the RED must be the two named reach gaps and nothing else |
| **I2** | author | `just ci` on the same combined tree **after** the repair | the candidate and its registered drivers pass the committed mandatory CI path with every existing check retained |
| **I3** | author | **regression assertion** — an independently introduced omission from required import reach | still **fails, naming the actual omitted identity**. This behaviour already works (log lines 2976–2977); it is asserted, **not built** |
| **I4** | author | control — checker-disable / bypass attempt | **must not pass** the assurance gate |
| **I5** | author | control — invalid-import / missing-artifact setup | distinguishable from a *reached* reconciliation failure, with its own named diagnostic |
| **I6** | author | `just ci` on the **quality-only** candidate at `efef604d` after the repair | the original mirror correspondence and classification requirements still hold at their existing scope; no upstream weakening |
| **I7** | inspector A | one verification invocation | instrument/executability scenario |
| **I8** | inspector B | one verification invocation | ownership/contract-shortcut scenario |
| **I9** | author | repair-batch re-validation — **only if the one repair bounce happens** | otherwise unspent |
| **I10** | delta inspector | one verification invocation — **only if that bounce happens** | otherwise unspent |

**Total 10.** I9 and I10 are contingent and are not reallocated if unused.

**No concrete gap to return:** the six mandatory acceptance demonstrations (I1–I6)
plus two initial inspections fit in eight, leaving exactly the one repair bounce
and its delta inspection the note allows. If a control needs a second attempt the
schedule does **not** stretch — it comes back here as a concrete gap before
overrunning.

## Blind inspection, per the reloaded process

Two fresh Codex `gpt-6-astra`/`high` inspectors on submission 1, blind to each
other, with **complementary** scenarios; mechanical findings collection; **one**
adjudication by me; **one** repair batch to the still-open author; one delta
inspector on submission 2 only if that bounce happens. **No higher-level
re-adjudication.** Codex is used so no spent Grok allowance is reused.

## Fences

No new product semantics. No S3 or S5 expansion. **No retraction of the base S4
acceptance.** No `docs/en/design/` writes. No merge, publication, issue comment or
human-composer delivery. `%313`'s lane is not edited; if evidence turns out to
require a change only to simulator-owned registration, the disposition goes to the
desk for `%313` instead.

S3 recovery (`%590`) proceeds independently; only actual file overlap and
final-base evidence are coordinated. Unrelated core #90 work is not held.

Final SHA, CI and independent acceptance are required before any exact-SHA desk
merge grant. **None is requested here.**
