# RESUME — simulator lane `%313`. THE current restart-safe record.

**This file supersedes every other resume fragment in this lane.** The previous
root `RESUME.md` and `resume.md` are preserved verbatim as
`SUPERSEDED-RESUME.md.442a0186.md` and `SUPERSEDED-resume.md.4395582e.md`; both
described `af9c1e5` / `origin/master e6c5924` / `gate-v12` / a live `%517`
auditor / a future grok second audit, **none of which is current**. `.orch/resume.md`
is likewise superseded by this file. Do not resurrect from any of them.

---

## 1. Identity and live seats — verified, not recalled

| pane | seat | state |
|---|---|---|
| `%313` | ticket owner (this seat), claude `claude-opus-5[1m]`, effort high | active |
| `%540` | commit owner, muse | **PARKED**, write-idle |
| `%562` | commit auditor, codex `gpt-6-astra`/high | **TERMINAL — DO NOT RESTART** |

Window `reactivegas:3` → `reactivegas-ms2-t-simulator-c1r-audit`.
`%315`, `%535` and the other historical seats are **retained history, terminal**.

**Exact replayable launch commands** (live argv, read from the running processes):

```
%540  pi --provider opencode-go --model muse-spark-1.3-contributor --thinking xhigh --approve
%562  codex --dangerously-bypass-approvals-and-sandbox \
        -C /code/reactivegas-sim-fable-audit-c1r-s1 -m gpt-6-astra -c model_reasoning_effort=high
```

**Terminal auditor contexts must never be restarted.** A new submission gets a
new pane, root and process. Terminal roots on disk:
`commit-auditor-c1r-codex-s1b`, `commit-auditor-c1r-codex-s1-CONTRACT-BLOCKED`,
`commit-auditor-codex-geometry-s1`, `commit-auditor-s62sim-codex-s1b`,
`commit-auditor-s62sim-grok-s2`.

## 2. Base, candidate and tree — verified

| field | value |
|---|---|
| accepted master | `3590c0015b84fd58004bf6fb44dd18b107304c48` (S2R / PR88) |
| repo / branch | `/code/reactivegas-sim-fable`, `feat/economics-simulator-fable` |
| **HEAD** | `886016cc6f29e8c1ddb2b2dee55129ecd18b48c4`, tree **clean** |
| **audited candidate** | `9717405e52664c9a520fcd0c65edb4e90612110a` — **UNACCEPTED** |
| audit subject | `6879970fdb1a797263843387e14704eaa1e3a2e7 .. 9717405e` (whole prefix) |
| audit worktree | `/code/reactivegas-sim-fable-audit-c1r-s1` (detached at the candidate) |

**HEAD is above the audited candidate.** `9127e452` (F-01), `5ee08ed9` (F-02,
**rejected, retained, not carried on**), `f85ff597` (F-03), `886016cc` (docs) are
**unaudited repair commits**. They close nothing. **No history cleanup.**

## 3. Executed work and terminal verdicts

**Submission 1 of 2 — CONSUMED. `FINDINGS`, `completeness=partial`.**
Report `bfc2b8c2fe3da9e7b3c4100740df622e1c9e09fa359f731d52ea0d4ada2173fb`.
Rows: **4 FAIL/BLOCKED** (INV-3, INV-11, R-GEO, C-KEY), **13 OPEN/UNJUDGED**,
0 KILLED, 0 RESIDUAL. The auditor's own cold CI exited 0 and v16 ran GREEN —
**no row was promoted to PASS on aggregate green.**

- **F-01** (INV-3) — repaired, refusal `author-mismatch`, **real file-picker path
  exercised**. Not accepted; needs audit.
- **F-02** (INV-11 / R-GEO) — **repair REJECTED**; implementation **HELD** on a
  pending operator decision.
- **F-03** (C-KEY) — repaired, spelling set + fail-closed dynamic keys. Not
  accepted; needs audit.

One earlier seat returned `AUDIT-CONTRACT-BLOCKED` (ledger unbound, my omission),
spending 0/5 and 0/30 — **it did not consume a submission**.

## 4. Frozen instruments and artifacts

```
gate-v16-one-membership.sh              70523191…86793556   certified 9717405e
gate-v15 1b6a0011…  gate-v14 1164ae9d…  (immutable, historical)
AUDITOR-BUDGET-RECONCILIATION-v2        4ad03cae…651f63a
campaign-ledger-S62-SIM-C1R.md          c2e56283…dbd681b8   §A owner, §B auditor
campaign-ledger-S62-SIM.md              cb48443e…           retired, untouched
C1-DEFENSIVE-REPAIR-PROPOSAL v1/v2      1162b52e… / 46e9132d…
C1-REPAIR-v3-CORRECTIONS…               c06c91a9…
C1-GEOMETRY-v4-QUANTITATIVE-CORRECTIONS 3cbed566…
C2-READINESS-PACKET v1/v2               72d24761… / 3dbd74eb…
auditor report                          bfc2b8c2…da2173fb
auditor probe.mjs (frozen at handback only) ce66e7da…
forged-session regression artifact      a00067f9…c00c9fd
```

## 5. Spend and ceilings — no reset, nothing refunded

| counter | state |
|---|---|
| owner substantive | **28 / 33** — 5 available (exceptional sixth raise, this repair only) |
| owner targeted | **48 / 48** — **0 available**; includes a recorded **41/40 overrun** |
| parent (me) | **4 substantive / 10 targeted** from the grant; historical parent spend **UNKNOWN, not zero** |
| auditor | **5 / 10** substantive, **7 / 60** targeted spent |
| submission 2 proposal | 5 substantive / 53 targeted — **a proposal, not taken** |

Six owner raises total (12→18→20→22→24→28→33). Every invocation counts at its
actual layer, **including failed, setup and warm retries**; a focused mode that
runs a full suite is a full suite.

## 6. Pending decisions — declared, NOT approvals

- **Operator geometry ruling.** Expand both rings with pan/scroll, or fixed rings
  with paged purchases. **Neither ruled. Silence is not approval and elapsed time
  infers nothing.** Measured: fixed ring 132 at separation 92 gives **8** slots;
  grown to just inside `MEMBER_R=220` gives only **12**; disjoint-area bound
  `N·π·42² ≤ 760·680` → **N ≤ 93** (loose, sound). The original constraints are
  **incompatible with unbounded n**.
- **F-02 control gap.** Whichever option is chosen, F-02 needs can-fail controls
  and the owner has **0 targeted**. Gap returned, **not granted**.

## 7. Preparation, separate from executed work

**C2 readiness v2** (`3dbd74eb…`) — **preparation only**, nothing built or
committed. Owner 12/60 and auditor 6/40 are **proposals**. Open C2 question: Node
is **not declared** in `nix/project.nix`/`flake.nix`, so its CI provision is not
reproducible from the repo (this is *not* a claim Node is absent).

## 8. Exact next permissible action

**Nothing executable is authorized right now.** Wait for the desk on:

1. the **operator geometry ruling** → then F-02 design, then repair; and
2. the **F-02 control-gap** decision.

Then, in order: owner repairs F-02 → I re-derive, falsify and freeze a successor
gate (its focused modes must invoke the **same operative predicate** as the full
run and prove their wiring cannot be silently removed) → owner's final gate + CI
on its 5 remaining substantive → **fresh FULL submission-2 audit**, new seat, over
the **whole 17-row subject**, only once a concrete complete command plan is frozen
within its allowance.

**Never:** restart `%562` or any terminal auditor; infer the geometry answer;
push, PR, merge or publish. C1 stays ahead of #68/#69; C2/C3/C4 remain required.

## 9. Landing order — CHANGED 2026-09-06

**S4 may land before C1.** The C1-next-landing reservation is superseded
(`NOTE-S4-NEXT-LANDING-20260906`, `7ce475d2…`). Reason: C1 is parked on an
unanswered operator geometry choice plus a control-budget gap, its auditor is
terminal, and it has no authorized executable work — so holding S4 behind that
open product decision no longer protects a live audit.

This is **scheduling only**: no candidate acceptance, no merge authority, and it
does **not** make S4 acceptance close `#66`. **C1 still precedes `#68` and
`#69`.** S3/S5/`#71` and all milestone requirements stay required.

**On an actual announced S4 landing — not before:**

- assess the **incoming base and the exact overlap** before the later authorized
  C1 final run;
- **no S4 candidate is anticipated** and none is fetched or inspected now;
- **no existing receipt transfers to new bytes automatically** — every receipt
  stays a receipt on the SHA it was taken at;
- any additional necessary cost is **returned, never silently taken**;
- the next C1 audit already must cover the **full repaired candidate including
  its actual accepted base**.

**Forward cost, flagged now and not spent:** an S4 landing would add another
integration on top of the existing gap. Integration itself is targeted (the
`d670323` and `3590c001` integrations each cost the owner **0 substantive**), but
re-validation is gate (4) + `just ci` (1) = **exactly the 5 substantive the owner
has left**, and F-02 still needs can-fail controls against **0 targeted**. The
S4 landing does not change that gap; it stacks an integration on it.

## 10. Geometry — RULED 2026-09-06T07:58:16.147Z. Section 6's pending decision is CLOSED.

Operator answered **"expand"**: expanding **both** member and purchase rings on a
**pannable/scrollable** canvas, all purchases displayed together, controls
readable. The fixed-rings/pagination alternative was **not** selected. Section 6
above is superseded on this point and retained as history.

Authority and amended requirements:
`handoffs/C1-GEOMETRY-AMENDMENT-AND-F02-CONTROL-PLAN.md` `ed23b066…`;
ledger §A-AMENDMENT-1 in `campaign-ledger-S62-SIM-C1R.md` `ee47f4f862af…`.

Still binding: member angles stable per membership; members outside the purchase
ring; separation and legibility retained. **Forbidden:** semantic cap,
pagination, illegible shrink, unreachable interaction. **No row closed or
downgraded** — all 17 remain required, F-01/F-03 included.

**Budget:** owner targeted **48 → 60** (48 spent, **12 available**, no retry
reserve); owner substantive **28/33**, 5 reserved for the final gate + CI **on
the actual accepted base**; parent unchanged at 4/10 with historical spend
intact — **no fresh counter**.

**Next permissible action (supersedes §8):** owner continues the C1 repair —
F-02 under the amendment, using the 8-of-12 control plan. Then I re-derive and
freeze the successor gate (same operative predicates in focused and full modes,
plus omitted/bypassed-wiring controls). Then the funded final gate + CI on the
accepted base. Then a fresh **FULL** submission-2 audit over the whole 17-row
subject — **only** on a concrete full-command fit and desk admission.
**No self-wake, no further design checkpoint.**

## 11. Skills reload — 2026-09-06, carry into every successor packet

Skills reloaded from `/home/paolino/.codex/skills` (symlinks resolve to
`/code/llm-settings/shared/skills`) at revision
`a55a25a2f50af0195b96de5d10d04feef2e80ac6`.

**Any successor seat — auditor or owner — receives this in its packet rather than
being resurrected for a reload.**

Applicable discipline: supervision is **event-driven and bounded** (one long wait
per turn, arm the harness Monitor, never a loop of short waits); verify a child's
claim against the artifact it **names** through a **one-line** check (hash, exit
code, digest) — a supervisor does not read artifacts into its own context; ≤20
API calls/hour outside a transition; parked seats exit; terminal workers retire
independently of acceptance.

**Two deferred, deliberately:**

- supervisors should run at the **standard context pin, not `[1m]`**; this seat
  is `[1m]`, so conforming is a scheduled **context turnover**, not a mid-flight
  switch — this record is restart-safe for it;
- terminal-worker retirement is **overridden** by the standing provider-stop
  order to keep all roots, panes and trees in place. `%562` stays unretired until
  that order is lifted.

**No default amends the active campaign:** frozen mandates, 17 rows, candidate
identities, cumulative budgets (including the 41/40 overrun) and the
codex-or-grok family fence are unchanged.

## 12. Audit process — rewritten at `af60ac2`, 2026-09-06

Loaded `af60ac2f01dfd0199eb02e146b29f873c2b0cd94`, superseding the `a55a25a2`
baseline. New shape: **two or three parallel blind inspectors** on submission 1,
each with its own clean detached worktree and its own fault scenario and
**nothing from any other inspector**; **mechanical** findings collection
(`commit-auditor/scripts/collect-findings`); **one** ticket-owner adjudication
into a hash-bound `REPAIR-BATCH.md` — the only judgment step; inspectors
stopped/archived before disposition; **one delta inspector** on submission 2;
**one declared campaign execution budget**, never a fresh default per inspector.

**Applies to NEW eligible campaigns only.** `S62-SIM-C1R` is frozen mid-flight
and keeps: all **17 rows**, the **explicit full-candidate scope**, the
two-submission contract with submission 1 consumed, the **10/60** auditor budget
(**5 substantive / 53 targeted** remaining), the **codex-or-grok** fence, and all
historical spend — including the **41/40** and **14/12** overruns — with **no
retroactive refund or reset**.

**Transition consequence, recorded so it is not discovered late:** C1R's
remaining **5 substantive cannot fund parallel inspectors** — each needs a full
gate (4) plus a cold `just ci` (1) — so 2–3 would require 10–15. This is not a
conflict, because C1R keeps its single-fresh-FULL-auditor shape; it is the reason
the new default must not be retrofitted onto this campaign without an actual
amendment and its own budget.

**Newly compiled briefs carry the updated process.** Terminal seats receive it in
their successor packet; no terminal auditor is restarted and no parked
implementer is woken for a reload.

## 13. PARKED on the mirror blocker — 2026-09-06

**State:** candidate **`48f76d9`** on accepted base `efef604d`, tree clean.
**`gate-v17` GREEN end to end** (first time). **`just ci` RED** at S4's
`check-lean-mirrors`. C1 is **not accepted**.

**Budgets, final for this leg:** owner **37/37**, parent **5/5 substantive,
7/10 targeted**. No extra run or audit dispatch is implied by the current
disposition.

**Established by the authorized base control** (`evidence/base-ci-control/`,
log `f343eaeb…`): base `just ci` **exit 0** with `MIRROR-CHECK-OK … tracked=29`,
against an **identical** branch census (`rows=19 exceptions=4 discovered=24
promoted=2`) that then fails **naming nothing**. This **rules out the
larger-predicate-census hypothesis**.

**Claim strength, corrected:** it does **not** by itself prove the
**import-reach** mechanism. That remains the *candidate* surface — the branch's
two tracked owned modules `lean/TraceDriverV1.lean` and
`lean/KelTraceDriverV1.lean` — **not a demonstrated cause**. The checker also
violated its own contract by exiting without naming an offender.

**Routed:** the checker extension/diagnostic finding goes to quality owner
`%503` as one bounded new campaign — it must confirm the responsible surface,
support legitimate registered roots, retain independent omission detection, and
name the offending identity. **No C1 reimplementation or deregistration is
requested**, and none will be volunteered.

**Wake condition:** the desk's **verified quality repair / accepted-base
announcement**, **or** a concrete diagnosis assigning a **simulator-owned**
correction. This is a **scoped wait with evidence — not acceptance of C1.**

**`%540` stays alive and parked.** It is **not** to be retired merely because a
generic skill default now recommends retirement; it is preserved under the
standing authority, with this resume kept current for a later **safe turnover**.

**Preserve:** `48f76d9`, the v17 GREEN receipts, the branch CI RED, and the base
control receipts. The full-audit contract (17 rows, whole prefix from
`6879970f`, codex-or-grok fence, schedule `60a8e612`) and C2/C3/C4 all stand.
