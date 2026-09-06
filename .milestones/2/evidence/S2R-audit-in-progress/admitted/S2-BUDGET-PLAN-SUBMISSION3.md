# S2 submission 3 — command budget, itemized before resuming

Required by NOTE-024 **before** the owner resumes and **before** the audit is
commissioned. Both budgets are itemized against the actual required work, and
each is reported as fitting or as a concrete gap.

## Cap state, recorded as the desk requires

| | |
|---|---|
| submissions | **2/2 exhausted**, then **explicit desk extension 2 to 3**. **No fourth.** |
| owner build/gate ceiling | **11 to 14**, second and **final** build-ceiling raise. **10 spent**, leaving **4** attempts *including failures* |
| owner targeted elaborations/probes | **at most 16**, separately counted, **including failed setups** |
| auditor build/gate attempts | **at most 6** |
| auditor targeted elaborations/probes | **at most 24**, separately counted |
| automatic raises | **none**, either side |
| gate hash at `561347d` | `cd67ade9bc137f87` |

## Owner: the four remaining attempts, itemized

**The F-003 RED is already executed and receipted** by the submission-2 audit
(`03-just-lean-std-import.log`, sha256 `0d3f25ef...`, plus `probe-extra-std.out`).
The owner does **not** re-spend a build re-establishing it. That is what makes
four sufficient.

| # | attempt | closes |
|---|---|---|
| 1 | repair, then the mandatory path with the **clean** `Std.Data.DHashMap`-importing registered root | valid extension **passes**, **import retained**, module **swept** |
| 2 | mandatory path with the **poisoned** equivalent (`axiom` plus a theorem using it) | **rejected for the axiom dependency** |
| 3 | mandatory path with a **genuinely project-owned** module withheld from the environment | **B-minus-S on a project-owned module**, not on the misclassified `Std` one |
| 4 | final full local CI | acceptance receipt |

S-minus-B, truncation, zero-discovery and panic controls are **probe-level** and
draw on the 16-elaboration allowance, not on the four.

**Verdict: four fits, with zero slack.** Any failed attempt consumes one. On a
genuine gap the owner reports the **specific command and cost** before spending
beyond the ceiling; it does not improvise.

## Auditor: six attempts against the FULL mandate

Scope is **`4a6cd87` to the final SHA across the entire unaccepted candidate**;
prior receipts are inputs, not inherited acceptance.

| # | attempt | closes |
|---|---|---|
| 1 | cold full CI at the final SHA, `.lake` initially absent | acceptance receipt plus cold provenance |
| 2 | mandatory path, clean `Std`-importing registered root | F-003 valid extension, import retained, swept |
| 3 | mandatory path, poisoned equivalent | axiom-dependency rejection |
| 4 | mandatory path, project-owned module withheld | B-minus-S on a **project-owned** module |
| 5 | mandatory path, **an existing theorem made sorry-bodied** | **the row the previous audit left independently unclosed** |
| 6 | **rebuilt base** `4a6cd87` | compiled `Expr` equality for the licensed renames **and** the compiled base consumer scan for the removed wrappers |

**Verdict: six covers the mandate, with zero slack, on one condition** — attempt
6's rebuilt base must serve **both** the `Expr` equality and the consumer scan.
Both read the same base environment, so one build answers both. That is stated to
the auditor as the plan rather than discovered mid-run.

T-zero and shadowing are addressed **without** a build; see below.

**No concrete gap is returned.** If the auditor finds one it reports the specific
command and cost rather than commissioning itself a knowingly incomplete audit.

## T-zero and shadowing, classified as NOTE-024 requires

| item | status |
|---|---|
| **T-zero** — zero S, zero B or zero T must fail | **BINDING.** Mandate row **A4** in the owner brief. Close it or report it as a blocker; it is **not** an auditor proposal |
| **toolchain-name shadowing** — a project module shadowing a toolchain name | **NOT binding.** It originated as the commit owner's **declared limit 2** in SUBMISSION-2 and was carried into the auditor brief as something to press. It is an **additional proposed invariant** and must be reported as such |

Neither may be silently counted as killed while unexecuted.
