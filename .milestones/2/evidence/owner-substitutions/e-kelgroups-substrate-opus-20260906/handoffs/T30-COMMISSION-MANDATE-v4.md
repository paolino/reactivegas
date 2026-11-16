# T30-COMMISSION-MANDATE-v4 — kelgroups #30 full implementation, commissioned

Epic owner `%532` (Opus), kelgroups `#29` / Reactivegas `#73`.
Authority: desk `NOTE-007-COMMISSION-T30-FULL-IMPLEMENTATION` (2026-09-06), read in full.
Supersedes the superseded statements named in §2 **only**; everything not named here stands unchanged.
**All prior texts are preserved as history and none is rewritten.**

## 1. What is commissioned

The **complete `#30` implementation**, not a slice-0 permission and not another proposal round.

Retained in full and undiminished: every `R30` row; `S1`–`S6`; every `REQ` and per-identity obligation;
client **adapt-only**; integrated refusal, persistence and replay; closure evidence; and the review
obligations. Ruled-but-unlanded dependencies stay **distinctly recorded** as dependencies, never folded into
delivered scope. `#29` and Reactivegas `#73` remain OPEN until their full outcomes are met. `#33`/`#34`
remain downstream and are not dispatched by this grant.

## 2. Superseded statements — replaced here, not left for the worker to adjudicate

Each of these is **history, preserved**; where a current instruction conflicts, **this table governs**.

| superseded text | replaced by |
|---|---|
| r8's single-slice / "no S30-2" statement | the approved decomposition in §4: `S30-0a/0b` → extent declaration → behavioural rows → closure |
| the Muse **ticket-owner** assignment | ticket owner is **Opus `claude-opus-5[1m]`/high at `%572`**; Muse is the **commit owner** |
| the r8 **leg** as gate input | the **demonstrated r9 leg**, sha `69c529ca22e1a798e1ffb1810902243d9f3c2a9223da50d1eef66b2a39833a25` |
| "old fixture status: unexecuted" | pf8r2 executed: `SUITE: PASS (baseline=GREEN setup-failures=0 mispredicts=0)`; synthetic 5/5, closed |
| TAXONOMY-v1 in contract §8 and the command-map block | **TAXONOMY-v2**, versioned per §3 |

No other r8 content is superseded. Product semantics are **not widened** by any of this.

## 3. Version the contract once — TAXONOMY-v2

The ticket owner produces **one** versioned mandate + command map adopting TAXONOMY-v2. r8 and all prior
evidence are preserved byte-intact beside it.

Required in that versioning:

- **Name A10's corrected promised exit** (mapped RED(1); v1 delivered 3) and **name A27 as the newer
  control**. State explicitly that **v1 behaviour here is source-derived, not an executed comparison** —
  pf8r2 ran v2 only; no v1 execution exists to cite.
- **Preserve every error identity and diagnostic.** An accumulated RED must never become a misleading
  no-verdict label, and must never become a PASS, because a later step refuses. v2 changes only the status a
  consumer branches on after a verdict has been rendered; it removes no reason line and merges no identity.
- Do not widen product semantics anywhere in the versioning.

## 4. Sequence and exact boundaries

**S30-0a / S30-0b first**, against accepted kelgroups `933e385df2f2a251bb54a08bb7663f0d41fafb64`, in an
**isolated cold worktree/export**:

- real `KelGroups.Event` and `KelGroups.Server.JSON` metadata / selection / freshness evidence;
- freeze **actual paths, commands, toolchain, input hashes, expected observations, receipt capture and
  per-command time bounds** before running;
- a setup failure or an ambiguous selector is **charged and returned** — it is not retried into silence, and
  it yields **no claim of Vote coverage**;
- **do not** run the full frozen Vote extent against a base where it is absent and count the predictable
  refusal as this preflight. (`1-fileset-hs` is a D-1 `fail`; that RED is a foregone conclusion, not evidence.)

**On successful prerequisite observations, proceed directly** to the extent declarations and the full
behavioural implementation through **one bounded commit owner**. There is **no report-and-park checkpoint**
between them and none is to be invented.

Candidate tests and metadata checks must ultimately use the **actual Vote modules and the final candidate**.
Reusing toolchain knowledge across module sets is legitimate; **transferring uniqueness, mapping or verdict
evidence across module sets is not.**

### Freeze prerequisites are temporal

Bind requirements, expected controls and scripts **before subject execution**; demonstrate the
**candidate-dependent** rows **before GREEN/acceptance**. A test of missing Vote outputs cannot be a
prerequisite for permission to create those outputs. **No obligation is waived by this ordering** — every row
is still owed, only later. Candidate-specific outputs and emitted pins are **evidence on the actual
candidate**, never assumed facts at the initial freeze.

### Channel independence — do not fabricate the expected signature

Source/metadata-channel independence remains **required**. Note the correction: **an unexported source edit
is not guaranteed to leave an entire `ghc --show-iface` dump unchanged.** Either bind a **concrete can-fail
observation**, or **return the design finding honestly**. Do not fabricate stability, and do not silently
normalize away load-bearing data merely to obtain the expected signature.

### Frozen semantics

Reactivegas is frozen at `3590c0015b84fd58004bf6fb44dd18b107304c48` for current accepted semantics.
**No new Lean edits. No threshold default. No expiry. No dormant refusal producers. No `#81`/`#76`
implementation by anticipation.** If `#68` or another accepted relevant base lands, perform an explicit
overlap/fit assessment first — **no automatic extra budget and no acceptance inheritance.**

## 5. Budgets — explicit, cumulative, nothing invisible

| seat | ceiling | notes |
|---|---|---|
| **Owner** | **28 substantive whole-project operations / 22 targeted probes**, cumulative, from product spend **0** | the accepted 26-unit candidate envelope **plus** `S30-0a`/`S30-0b`, which **add to** and never replace candidate `B3`/`B22a` |
| **Author submissions** | **max 2 total** (one normal repair), **sharing the same 28/22 cumulative ceiling** | **no separate repair build pool** |
| **Auditor** | **25 substantive / 24 targeted**, cumulative **across max 2 fresh per-submission audits** | full original mandate, all required independent directions and controls |

Counting rules, binding:

- **Every whole-project build/test/CI invocation counts.**
- **Hash-only source restore is not a fresh verification build**; if one is run anyway it is **itemized and
  charged**.
- **A narrow probe that compiles more than its declared scope is a whole-project operation** and is counted
  as one. Probes must not hide builds.
- **No parallel heavy builds.**
- **No automatic raise or reset** on branch, model or submission change.
- If implementation or restore/build grouping **cannot fit**, return the **exact additional operation and
  scope before spending beyond the bound** — do not overrun and reconcile afterwards.
- The auditor inherits **no** semantic acceptance from the old synthetic PASS. Conditional `A-REBIND` remains
  **unspent with its reason recorded** while its dependency is unlanded. Named reserves are **not** permission
  to replay unexplained failures until green. If a second full audit cannot fit the remaining cumulative
  budget, **report before dispatch** — do not waive rows and do not borrow an owner result.
- Synthetic 5/5 and all S28 history stay **separate and spent**. **No sixth synthetic run.**

## 6. Seats

| role | seat | pin |
|---|---|---|
| ticket owner | `%572`, under this epic owner | `claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high` |
| commit owner | **one** Muse | `muse --approve` (Pi / opencode-go / `muse-spark-1.3-contributor` / xhigh) |
| independent auditor | **one fresh** per admitted submission | Codex `gpt-6-astra`, effort **high**, **explicit in live argv** |

Grok only if eligible under the existing one-family-seat cap. **Never Muse/GLM/Claude auditing.**
Verify launch, active model and effort, and require the seat's **own post-cursor `START`** before admitting
any claim. **No provider fallback or model substitution by helper default.** Existing terminal roots are
**retained inputs, never resumed audit contexts.**

## 7. Delivery authority

- **Local commits: authorized.**
- **After full local acceptance and a qualifying independent audit: push and DRAFT PR authorized**, with
  real remote CI at the exact head, factual scope and residuals, and **no closing keywords** for `#29`/`#73`.
- **No merge and no release authority.** Return the exact candidate/base/tree, the full audit/receipt/cost
  packet and remote CI **for desk merge authorization**.
- **No issue comments.** Necessary issue **body** updates stay factual and are read back.
- **No deployment, publication, or `#33`/`#34` dispatch** from this grant.

## 8. Supervision

Terminal-aware supervision through the immediate child only. **Prove wake delivery for any new watcher.**
**Never wait from a cursor taken after the result** — retain the pre-dispatch cursor and consume all existing
records first. **One event per substantive phase**, and every stop is terminal, `PARKED` with an exact wake
condition, or `BLOCKED` with a question.

Handback in the ordinary next report, with **no extra desk approval to continue authorized work**: the frozen
mandate/gate hashes, the admitted commit-owner identity and `START`, and the **actual first compiler result**.
Local reports only — `handoffs/`, own `STATUS.md`, and a pointer in `/tmp/reactivegas/ms2/inbox/`.
Never the desk composer.
