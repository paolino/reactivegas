# Auditor brief — S2 submission 3, candidate `b0c2cdb`, FRESH FULL audit

**Seat:** `codex`, launched `codex -m gpt-6-astra -c model_reasoning_effort=high`
— **both model and reasoning effort pinned in argv**, not inherited from config.
**State your live argv, PID and START in your first journal line.** A prior audit
in this lane pinned only the model; the desk requires both explicitly.

Fresh context, fresh root, **`.lake` initially absent**.

**Family:** the author is `muse`. **`grok` is already used for S2 and may not be
reused; `claude` may not be selected.** You are `codex`, a fresh context — the
earlier `codex` context that produced F-001/F-002 is gone.

**Read-only. No repair. No contact with the commit owner.**

## Upward reporting — local files only

**Never type, paste, send-keys or send a pointer into milestone desk pane `%510`
or any other human seat**, for any purpose including an acknowledgement. Operator
correction, 2026-09-05: a pane paste simulates operator keystrokes and is not
delivery.

Deliver to `handoffs/AUDIT-REPORT.md` in this directory, hashed into
`handoffs/HASHES.txt`, and journal a `STATUS.md` event naming path, hash and your
exact next state. **That is delivery.** No gist, no push, no external artifact,
no publication. Anything you spawn inherits this rule.

## Identity and caps

| field | value |
|---|---|
| accepted base | `4a6cd87fcbc3e4a536bbc9f240f5efe5704022af` |
| **candidate** | **`b0c2cdb`** on `chore/66-s2-axiom-gate`, worktree `/code/reactivegas-66-s2` |
| rejected predecessors | `5745a2c` (F-001, F-002), `561347d` (F-003) — preserved with their audits |
| repair delta | `561347d..b0c2cdb`, `scripts/check-lean-axioms` only |
| mandate | **v3 `a8e18e478ca8d063`** = brief ∥ AMENDMENT-1 ∥ AMENDMENT-2; v1 `0a1db9887ccc9d8f`, v2 `7cfb7aec95a37448` preserved |
| gate script sha256 at candidate | `c83ae5647485018e72eef85bd217dfe6ad5202fba224d4e6bb9680dd0f25feb5`, superseding `cd67ade9bc137f87` |
| submissions | **3 of 3 — extension consumed. No fourth exists** |
| owner spend | builds **12/14**, probes **15/16**; **both raises consumed, nothing left to grant** |
| **your caps** | **≤6 build/gate attempts, ≤24 separately counted targeted elaborations/probes. No automatic raise** |

Owner packets: `../commit-owner-s2-muse/handoffs/SUBMISSION-{1,2,3}.md`. Prior
audits: `../candidate-auditor-s2-codex/`, `../candidate-auditor-s2r-grok/`.
**Prior receipts are inputs, not inherited acceptance.**

## Your six attempts — budgeted before dispatch, and they fit with zero slack

The ticket owner itemized this in `../handoffs/S2-BUDGET-PLAN-SUBMISSION3.md`.
**Attempt 6 must serve two purposes**; that is the plan, not something to
discover mid-run.

| # | attempt | closes |
|---|---|---|
| 1 | cold full `nix develop --quiet -c just ci` at `b0c2cdb`, `.lake` absent | acceptance receipt + cold provenance |
| 2 | `just lean`, **clean** registered root **importing `Std.Data.DHashMap`** | F-003: passes, **import retained**, module **swept** |
| 3 | `just lean`, poisoned equivalent (`axiom` + theorem using it) | rejected **for the axiom dependency** |
| 4 | `just lean`, a **genuinely project-owned** module withheld | **`B \ S` on a project-owned module** — not on the `Std` module F-003 proved misclassified |
| 5 | `just lean`, an **existing theorem made `by sorry`** | **the row the first audit left independently unclosed** |
| 6 | **rebuilt base `4a6cd87`** | compiled **`Expr` equality** for the licensed renames **and** the compiled **base consumer scan** for the removed wrappers — both read the same base environment |

**If six cannot cover the mandate, report the concrete command and cost gap
rather than running a knowingly incomplete audit.** Do not exceed silently.

## The repair to attack

`B = env \ closure(import Lean)` was a **guess about toolchain contents, not
provenance**: the 1707-module closure holds 261 `Std.*` internals yet excludes
public `Std.Data.DHashMap`, so a clean `Std`-importing root failed at `B \ S`.

**Claimed replacement — required row 6, the heart of the repair:** per-module
**compiled-artifact resolution through the loader's own `LEAN_PATH` search
order**. Each elaborated module name maps to its olean relative path; the first
`LEAN_PATH` entry whose file exists wins; a hit under the repository root and
outside Lake's `packages/` footprint is **project-built**; a hit elsewhere
(toolchain lib, dependency checkouts) is **dependency**; **no hit is a named
finding, exit 1**.

**Attack it:**

1. **Is it actually provenance, or a differently-shaped guess?** The rule now
   depends on `LEAN_PATH` order and on the repo-root / `packages/` boundary.
   Break each.
2. **Shadowing.** The owner says shadowing follows loader order. What happens
   when the *same module name* resolves under both the repo root and a
   dependency path? Which wins, and is that the right answer?
3. **The fail-closed branch.** "No hit ⇒ named finding, exit 1" — **fire it.**
   An unexercised fail-closed branch is not a fail-closed branch.
4. **`packages/` boundary.** Is Lake's dependency footprint actually at that
   path in this tree, and what happens if it moves or is empty?
5. **Dependencies must not be misclassified as project sources**, and project
   sources must not be misclassified as dependencies. Both directions.
6. **`S \ B` and `B \ S` must both remain live findings.** Neither may be
   unrepresentable by construction.
7. **The original truncation and zero/discovery controls must still be alive.**

## Rows the previous audits left open — close or report as blockers

Do not silently count an unexecuted required row as killed.

- existing-theorem `sorry` control through `just lean` (attempt 5);
- **rebuilt-base** compiled `Expr` equality for the licensed renames (attempt 6);
- compiled **base consumer scan** for the removed wrappers (attempt 6);
- **T-zero — zero S, zero B or zero T must fail — is a BINDING mandate row
  (A4)**, not an auditor proposal. Close it or report it as a blocker;
- **toolchain-name shadowing is NOT binding** — it originated as the owner's
  declared limit and is an **additional proposed invariant**. Report it as such.

## One advisory already accepted by the desk — do not re-litigate

**`CI-T-SHARED-FILTER`.** Both T derivations share `thmInfo` and B membership:
**two views of one inventory, not two independent theorem sources.** Skip-both
(`count=1212`) and T-side `KelGroups.Types` shrink (`count=1162`) are
**demonstrated survivors**. The desk accepted this as a **named advisory limit**.

Verify it still holds and is stated honestly in the packet. **Do not accept any
description of the gate as having two independent theorem sources, or as
resistant to common-filter omissions.** Current enumerator agreement is evidence
about the current tree, not proof of future checker completeness.

## Scope

**FULL: `4a6cd87..b0c2cdb`, the entire unaccepted candidate** — the axiom gate,
the A2′ census with no quota, Row B renames and dead re-exports, Row C, the
fence, and every original and amended requirement. The repair scope was narrow;
**the audit scope is not**.

## Return

One integrated verdict. Findings ranked, each with the command that produced it.
An explicit list of rows you could **not** independently close, and whether each
is binding. Name the worktrees for the ticket owner to retire. State your exact
build and probe spend against 6 and 24.
