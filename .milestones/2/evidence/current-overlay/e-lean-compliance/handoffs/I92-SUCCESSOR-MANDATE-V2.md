# #92 successor mandate v2 — exact commands, ceilings and topology

Owner `%503` (ticket owner, `claude`). Compiled under the desk release of
2026-09-06. **Every row below gives cwd, argv, input identity, expected polarity
and charging rule.** Labels are not commands.

## Identities, rebound

| | |
|---|---|
| accepted master (new base) | `890a74f1c4c34b52c55b5d941c78c94fa504e005`, tree `0f40463de294d7b0438dbec0a30c7590b5a19262` |
| historical prior base | `efef604de87b2a1efae51e84d1a9150e585c1db0` — **historical only** |
| C1 | `48f76d96eb0975ec6c21cc5ba490af196d4882fa`, tree `3d202f01f369d3fde2b8187074ebf6d08ff416f5` |
| C1 delta | C1 **relative to its merge base `efef604d`** |

Verified independently by me before compiling: `origin/master` equals
`890a74f1…`/`0f40463d…`; `efef604d` **is** an ancestor; the #90 delta is **19**
paths, the C1 delta **15**, and their changed-path **overlap is 0**. That makes the
combined construction feasible — **it does not pre-accept its output.**

## The two artifacts

**Quality-only landing candidate**, built **directly on `890a74f1…`**, touching
**exactly** these four paths and nothing else:

```
scripts/check-lean-mirrors
scripts/lake-roots/lakefile.lean
scripts/lake-roots/Main.lean
scripts/lake-roots/.gitignore
```

**`justfile` is fenced out.** Existing accepted CI wiring already reaches the
checker. If a separately measured reason later requires recipe wiring, **stop and
return it** — this mandate does not approve it.

**Combined integration evidence tree**, constructed deterministically from base
`890a74f1…` + the exact quality delta + the exact C1 delta, and **never** the #92
landing subject. Record the combined tree hash and the exact construction command.

**`F1` binds only to the quality landing head. `INT` binds only to the combined
evidence tree.**

## Rows — cwd, argv, polarity, charge

`Q` = quality-only candidate worktree. `C` = combined evidence worktree. `T` =
isolated disposable tool fixture. Every aggregate is charged **once** including its
recipe and dependency expansion; nested stages are never bundled.

| # | cwd | argv | expected | charge |
|---|---|---|---|---|
| **N1** | `T` | `nix develop --quiet -c bash -lc '<build lakeRoots exe; run it against the fixture workspace>'` | native build + Lake config evaluation + exact `LAKE-EVALUATED-ROOT` output; **hash-bound exact target bytes or a deterministic reconstructible delta**, plus exact command, env/toolchain binding, output, result | 1, charged on any outcome reaching the product |
| **A1** | `Q` | `nix develop --quiet -c just ci` | **RED** naming the omitted root; mirror subject reached | 1 |
| **A2** | `Q` | `nix develop --quiet -c just ci` | **GREEN**; dotted-default, explicit-inline, next-line-opener each **built and covered** | 1 |
| **A3** | `Q` | `nix develop --quiet -c just ci` | **RED** naming the actual omitted identity | 1 |
| **A4** | `Q` | `nix develop --quiet -c just ci` | bypass **does not pass** the assurance gate | 1 |
| **A5** | `Q` | `nix develop --quiet -c just ci` | invalid-import distinguishable from a reached reconciliation failure, own named diagnostic | 1 |
| **A6** | `Q` | `nix develop --quiet -c just ci` | **GREEN**, census identical to `890a74f1…` | 1 |
| **A7** | `Q` | `nix develop --quiet -c just ci` | **RED** from the independently enumerated **compiled** set | 1 |
| **A8R** | `Q` | `nix develop --quiet -c just ci` | the **shipped** axiom reconciliation path fires its **production B-minus-S refusal and names the identity** | 1 |
| **A8G** | `Q` | `nix develop --quiet -c just ci` | with **only** that production refusal disabled, the named refusal **disappears** or the mandatory assurance path **accepts incorrectly**. **A separate self-refusing driver is disallowed.** | 1 |
| **INT** | `C` | `nix develop --quiet -c just ci` | the **complete committed mandatory CI path** passes on the combined tree, registered drivers included — **not a narrow driver command** | 1 |

**Fixture rule, campaign-wide:** any row whose premise is *"this source is part of
the project"* stages the complete disposable fixture in its isolated worktree and
records staged paths **and porcelain before** the aggregate. Never commit or push a
fixture. Never stage generated build output.

**Stop rule:** stop at the **first required failure**, return the exact unfinished
branch, spend nothing further. No retry, no setup margin, no staging after the fact.

## Ceilings — recomputed, and they close

| allocation | units |
|---|---|
| initial author rows N1, A1–A7, A8R, A8G, INT | **11** |
| I1 + I2 | ≤ **2** |
| conditional repair **R1–R10** | 10 |
| conditional **N1R** — **only if native-tool bytes change**; a checker-only repair does not spend it | 1 |
| conditional **D1** | 1 |
| **F1** | 1 |

`11 + 2 + (10+1) + 1 + 1 = ` **26 total**; `11 + 11 = ` **22 author**; no-repair
branch `11 + 2 + 1 = ` **14**. Matches the released ceilings exactly.

**Unused conditional units stay unused. No retry or setup margin. No execution
traded for prose or inherited evidence.** Historical evidence may be supplied as
input and must be re-established by exact byte and provenance checks.

## Families — three actual CLIs

| role | family |
|---|---|
| ticket owner | `claude` — `%503` |
| commit owner | **`grok`, `grok-4.6`** — one Grok seat for this ticket |
| blind inspectors I1, I2, conditional D1 | **`codex`, `gpt-6-astra`, effort `high`** |

A model alias is **not** another family; the Sol-then-Astra shape I proposed
violated alternation. `%615` is terminalized after its zero-execution artifacts are
preserved, and is **not** the successor author. Auditor panes, processes, sessions
and roots are fresh and distinct.

## Launch topology — ceiling 5

Submission 1: **two** initial blind launches plus **at most one** aggregate
corrected redispatch, only after evidence a **commissioning defect changed**.
Submission 2: **one** delta launch plus **at most one** corrected delta redispatch
under the same condition.

**A CLI invocation consumes an attempt even at zero substantive executions. A
returned execution allocation does not refund an attempt. A second contract block in
a chain is terminal.**

## Packets

`audit-packet` **format V2** at shared revision
`6aa0ad7ce39caa4e47a5c428947a3c32383a4173`, with the `llm-settings` working tree
**verified clean before each freeze**. Every I1/I2/D1 packet names specialization
**`commit-auditor`**, declares every applicable specialization input and **every
executable as `TOOL`**, and binds **exactly one** input to each of
`dispatch-preflight-receipt`, `current-campaign-ledger`, `current-row-ledger`.

**The dispatch receipt must substantively reconcile the two ledgers, identities,
reservations, denominator, exact commands, paths and stop rule. Presence is not
completeness** — and the omission that ended the last campaign was mine.

**Sequence:** freeze I1 → launch → durably charge → **update the canonical ledgers**
→ freeze I2 at the next ordinals. **Never freeze both from one snapshot.**
Target-pane `verify`/`preflight` results go in a **separate launch receipt cited in
`START`**, never appended to the sealed receipt.

## Landing order

**The quality-only candidate must be accepted and landed before C1 is rebased and
audited.** The combined tree is evidence and cannot carry C1 into the #92 landing.
No push, PR, issue comment, merge, `#66` closure, simulator landing or
product-semantics change is authorized here.
