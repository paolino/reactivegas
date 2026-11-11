# Campaign ledger — S62-SIM-C1R (simulator C1 re-bind, successor campaign)

Authored by ticket owner `%313`. **The retired `campaign-ledger-S62-SIM.md`
(`cb48443e…`) is preserved unchanged and is NOT this campaign's ledger.** Its
30-build accounting, its rows and its submission-2 identity at `280b67f` remain
historical evidence. Nothing there is refunded, netted, or overwritten.

## Campaign identity

| field | value |
|---|---|
| ticket / slice | `#70` C1 / **`S62-SIM-C1R`** |
| audit subject base | `6879970fdb1a797263843387e14704eaa1e3a2e7` |
| candidate | `9717405e52664c9a520fcd0c65edb4e90612110a` |
| accepted base integrated | `3590c0015b84fd58004bf6fb44dd18b107304c48` (S2R, PR88) |
| instrument | `gate-v16-one-membership.sh` `705231918134a9a9194e22b2f8378f6b0b1476798432914a04ed48a386793556` |
| reconciliation | `AUDITOR-BUDGET-RECONCILIATION-v2-9717405e.md` `4ad03cae…` |
| campaign state | **open** — submission 1 dispatched; no mutation campaign has started |
| submissions | up to **TWO**, each a fresh **FULL** audit |

**The subject is the whole prefix, not the integration slice.** The owner's
`PROOF-COMPLETE range=3590c001..9717405e` describes the rebase span only.

## Budget

| counter | owner | auditor |
|---|---|---|
| substantive | **28 / 28** (spent) | **5** submission 1, **10** total |
| targeted | **37 / 40** | **30** submission 1, **60** total |

Charge: full `gate-v16` = **4** (v14 body as one unit + three nested full ui-gate
suites); cold `just ci` = **1**. Focused probes keep their genuine class. Any
additional whole command counts separately.

## Ceiling-raise ledger

| step | authority |
|---|---|
| owner 12 → 18 | NOTE-078 |
| owner 18 → 20 | NOTE-079 |
| owner 20 → 22 | NOTE-083 |
| owner 22 → 24 | NOTE-084 |
| owner 24 → 28 | RESUME-SIMULATOR-COST-DISPOSITION |
| auditor 8 → 10 substantive | NOTE-084 (targeted 60 unchanged) |

**Five owner increases**, all desk-ordered, none self-granted. The Q-001
accidental full run was **counted spend, not a raise**. The desk authorised this
audit under that complete history as a **task-specific exception** to the generic
commit-auditor two-raise/third-increase termination rule; it grants no further
budget to anyone.

## Submission accounting

| field | value |
|---|---|
| submissions used | **0** |
| current dispatch | **submission 1**, auditor `%560`, codex `gpt-6-astra`/high |

## §A — initial row set (owner-authored). ALL ROWS REOPENED.

**No inherited acceptance.** Every former PASS from the retired campaign is
**evidence input**, never a granted row. A row is closed only by this campaign's
own verification.

| row | invariant | severity | state |
|---|---|---|---|
| INV-1 | one membership store | BLOCKING | open |
| INV-2 | identity is the substrate `Key` | BLOCKING | open |
| INV-3 | authority is the canonical view | BLOCKING | open |
| INV-4 | the fourteen and only the fourteen | BLOCKING | open |
| INV-5 | consequences ride the sealed hook | BLOCKING | open |
| INV-6 | a zero balance is read, never stored | BLOCKING | open |
| INV-7 | the two-stage election flow deletes itself | BLOCKING | open |
| INV-8 | the pin manifest sees what it claims to see | BLOCKING | open |
| INV-9 | fixtures re-emitted, not hand-edited | BLOCKING | open |
| INV-10 | the threshold is transcribed, not anticipated | BLOCKING | open |
| INV-11 | nothing already green regresses | BLOCKING | open |
| R-GEO | geometry / permalinks | BLOCKING | open |
| R-CIT | linkable Lean citations resolve | BLOCKING | open |
| R-ITA | Italian visible copy, no banned vocabulary | BLOCKING | open |
| R-LAY | layout / rendered chrome | BLOCKING | open |
| C-KEY | substrate string keys survive every derived UI control | BLOCKING | open |
| C-CHROME | enumerated render classes validated without deleting the evidence | BLOCKING | open |

C-KEY and C-CHROME are **property classes, not instances.** A repair that only
reddens a named site has not closed them — that is the most expensive lesson of
this campaign and it is why these two exist.

## §B — auditor row verdicts (append-only, auditor-owned)

The auditor appends here **and** in its own `report.md`. This file is left
writable for that purpose; **§A above is owner-authored and must not be edited**
— its integrity is checkable against the hash recorded by the ticket owner in the
lane STATUS at the moment of binding.

Append rows as:

```
| row | severity | verdict | mutation state | evidence path | probe sha256 |
```

Nothing below this line yet.


### Submission 1 — auditor %562 administrative handback under NOTE-A1

Verdict FINDINGS, PARTIAL. No inherited acceptance; no row promoted from aggregate GREEN. Stopped by NOTE-A1; substantive 5/5, targeted 7/30 actually completed. All uncompleted assessments stay OPEN/UNJUDGED.

Pre-append ledger sha256 c2e5628318bdbac950ef7d1401564f16d1ecf0fbdb77287b0ba32267dbd681b8. Report /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/report.md sha256 bfc2b8c2fe3da9e7b3c4100740df622e1c9e09fa359f731d52ea0d4ada2173fb.

| row | severity | verdict | mutation state | evidence path | probe sha256 |
|---|---|---|---|---|---|
| INV-1 | BLOCKING | UNJUDGED | OPEN | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/report.md | not independently frozen per row |
| INV-2 | BLOCKING | UNJUDGED | OPEN | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/report.md | not independently frozen per row |
| INV-3 | BLOCKING | FAIL | BLOCKED | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/evidence/T7-authority.log | ce66e7dac7bd71b6adc66ef9d4fc5183cfea841d2cbe91c0cbcc2b7987901b7c |
| INV-4 | BLOCKING | UNJUDGED | OPEN | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/report.md | not independently frozen per row |
| INV-5 | BLOCKING | UNJUDGED | OPEN | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/report.md | not independently frozen per row |
| INV-6 | BLOCKING | UNJUDGED | OPEN | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/report.md | not independently frozen per row |
| INV-7 | BLOCKING | UNJUDGED | OPEN | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/report.md | not independently frozen per row |
| INV-8 | BLOCKING | UNJUDGED | OPEN | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/report.md | not independently frozen per row |
| INV-9 | BLOCKING | UNJUDGED | OPEN | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/report.md | not independently frozen per row |
| INV-10 | BLOCKING | UNJUDGED | OPEN | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/report.md | not independently frozen per row |
| INV-11 | BLOCKING | FAIL | BLOCKED | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/evidence/T4-geometry.log | ce66e7dac7bd71b6adc66ef9d4fc5183cfea841d2cbe91c0cbcc2b7987901b7c |
| R-GEO | BLOCKING | FAIL | BLOCKED | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/evidence/T4-geometry.log | ce66e7dac7bd71b6adc66ef9d4fc5183cfea841d2cbe91c0cbcc2b7987901b7c |
| R-CIT | BLOCKING | UNJUDGED | OPEN | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/report.md | not independently frozen per row |
| R-ITA | BLOCKING | UNJUDGED | OPEN | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/report.md | not independently frozen per row |
| R-LAY | BLOCKING | UNJUDGED | OPEN | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/report.md | not independently frozen per row |
| C-KEY | BLOCKING | FAIL | BLOCKED | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/evidence/T5-derive-bracket.log + T6-derive-dot.log | ce66e7dac7bd71b6adc66ef9d4fc5183cfea841d2cbe91c0cbcc2b7987901b7c |
| C-CHROME | BLOCKING | UNJUDGED | OPEN | /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-c1r-codex-s1b/report.md | not independently frozen per row |

Totals: 17 rows; 4 FAIL/BLOCKED, 13 OPEN/UNJUDGED, 0 KILLED, 0 RESIDUAL. Campaign OPEN; stopped=administrative-handback (not set-point/tail). The listed probe hash is the final handback freeze, not a claim of a per-invocation pre-execution freeze. Findings F-01 signer binding, F-02 reachable packing overlap, F-03 syntax-sensitive handler discovery; remaining class assessments unfinished.
