# S62-SIM-C1R — prospective launch-attempt amendment (skills `4981cd8`)

**Prospective only.** Delivered to this lane 2026-09-06. It amends **nothing**
retroactively: candidate scope, execution ceilings, acceptance rows, family
restrictions and prior evidence are all unchanged. Recorded **before** the next
auditor launch, as the reload requires.

## Historical launches — retained in full, none reinterpreted away

| # | seat | CLI invoked? | reached START | outcome | charge under `4981cd8` |
|---|---|---|---|---|---|
| L0 | pane for `commit-auditor-c1r-codex-s1` | **no** — `codex-raw` does not exist on this host, the exec failed and the pane died instantly | no | launch never happened | **AMBIGUOUS — see below** |
| L1 | `%560`, codex `gpt-6-astra`/high | yes | **yes** | `AUDIT-CONTRACT-BLOCKED`, ledger unbound, **0/5 and 0/30 executed** | **CHARGED** — zero-execution contract blocks stay charged |
| L2 | `%562`, codex `gpt-6-astra`/high | yes | **yes** | `AUDIT-RESULT verdict=findings`, partial, 5/5 and 7/30 executed | **CHARGED** |

*(An earlier, separate campaign launched `%535` grok for the retired S62-SIM
submission 2; that is a different campaign's history and is not counted here.)*

**L0 is the ambiguous one and I will not resolve it in my own favour.** The rule
says pane creation alone does not consume an attempt, while a CLI invocation
does — and here the binary did not exist, so no CLI ran. It also says a launch
stays charged when the seat never reaches `START`. Those two clauses point
opposite ways for a failed `exec`. **Submission 1 therefore consumed either 2 or
3 launch attempts**, and I am not choosing the smaller number by preference.

**Returned execution does not refund a launch:** L1 returned its full 5/30
allocation, and that allocation was reused by L2 — but its *attempt* is spent.

## Remaining per-submission attempt authority — the actual question

The frozen C1R contract predates attempt accounting. It authorises **two
submissions** and **10 substantive / 60 targeted**, and says nothing about
launch attempts. So there is **no frozen attempt authority to carry forward**,
and the new default (submission 2 = one initial delta seat + one corrected
redispatch) **does not match this campaign**, whose submission 2 is a fresh
**FULL** audit by explicit exception, not a delta seat.

**The exact question, returned rather than inferred:**

> For `S62-SIM-C1R` submission 2 — a fresh FULL audit under the recorded
> existing-campaign exception, with 5 substantive / 53 targeted remaining — **how
> many launch attempts are authorised**, and is **L0 charged**?

**I will not infer a seat.** No auditor CLI is launched until that authority is
frozen. If the answer is that submission 1 exhausted the attempts, that is a
blocker to return, not a reason to quietly open one more.

## What I will do at the next launch, whatever the answer

Write **one hash-bound preflight receipt before any CLI is invoked**, with
per-packet sections proving the complete environment exists: exact detached
candidate, referenced inputs and hashes, each worktree's own runnable gates,
independently evidenced owner/author identity, the current campaign and row
ledgers, non-overlapping reservations, report and evidence paths, denominator,
stop rule and launch authority. The auditor's own preflight must return **all**
detectable missing or inconsistent inputs **together**, not as serial one-field
blocks — which is precisely how L1 was spent.

**Relevance to L1:** it blocked on a single missing field (the campaign row
ledger). Under this amendment that whole class is caught by my preflight receipt
before a CLI is ever invoked, and an auditor that still finds gaps returns them
in one batch. L1's cost is exactly the failure this change exists to prevent.
