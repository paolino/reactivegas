# Prospective amendment — audit dispatch preflight and launch-attempt bound

Owner `%503`. Recorded under the skill reload at
`4981cd80f4571c94d0f695e5670fd034250c700f` (2026-09-06T13:28:19+01:00, verified in
the installed HEAD). **Prospective. Nothing is applied retroactively.**

## Actual delivery time, and what it means for the seat already running

| event | time |
|---|---|
| S3 Grok auditor `%622` launched | **~12:29Z** |
| this reload delivered | **~12:37Z** |

**`%622` was dispatched before the requirement existed**, and therefore **without**
the newly required hash-bound preflight receipt. The note forbids interrupting an
in-flight auditor, so `%622` is **not interrupted, not restarted, not re-briefed**.
No candidate scope, execution ceiling, acceptance row, family restriction or prior
evidence is retroactively altered.

## Launch ledger — frozen, every historical launch retained

| campaign | submission | launches consumed | remaining attempt authority |
|---|---|---|---|
| **S3 successor** | 1 (its only submission) | **1** — `%622`, `grok-4.6` with `--always-approve`, both verified in `/proc` argv | **one** aggregate corrected redispatch, per topology rule 3 |
| **#92 successor** | 1 | **0** — `I1`/`I2` unlaunched | initial parallel set, then one aggregate corrected redispatch |
| **#92 successor** | 2 | **0** — `D1` unlaunched | one delta seat, then one corrected redispatch |

Historical launches from the closed campaigns are retained on the record and are
not recounted here: they belong to campaigns that are terminal.

**A corrected redispatch is available only on evidence that the commissioning
defect changed. A second block stops the chain.**

## What binds my next auditor launch

Before the next auditor CLI starts — the #92 blind inspectors are the next in line
— I write **one hash-bound dispatch-preflight receipt** with per-packet sections
proving the complete environment exists:

- exact detached candidate;
- every referenced input with its hash;
- each worktree's own runnable gates, **at the paths from which that seat will
  execute them**;
- independently evidenced owner and author identity;
- the current campaign and row ledgers;
- **non-overlapping reservations** — a dispatch snapshot and a live shared counter
  are different facts, and no seat may consume another's reservation;
- report and evidence paths, denominator, stop rule, and launch authority.

**Launch accounting:** one auditor CLI invocation is one launch attempt. Pane
creation is not another; **restarting the CLI is**. NEVER-STARTED, invalid and
zero-execution contract-blocked launches **remain charged**, and returned execution
allocation **does not refund the launch**.

**Preflight failure returns everything at once** — all detectable missing or
inconsistent inputs together, never serial one-field blocks.

## If no launch remains

I return the exact question rather than infer a new seat. That applies immediately
to S3: if `%622` returns a commissioning block, I have **one** corrected redispatch
and no more; if that is also blocked, the chain stops and the question comes back.

## Execution budgets unchanged

S3 cumulative **22** with 21 charged and 1 reserved to `%622`. #92 successor
cumulative **19** with author cap 15, 3 charged. **This amendment changes no
execution budget** — it bounds launches, which were previously unbounded.
