# BRIEF — fresh independent S4-B audit (replacement commission)

One coherent packet. It replaces the commission that returned
AUDIT-CONTRACT-BLOCKED; it is not a patch on that seat's Q-001. **No field here
is left for you to infer from a process elsewhere.**

Load the shared `auditor` role plus `commit-auditor` and `lean-auditor`, and
check their dispatch requirements against this packet **before** your START. If
a required field is still missing, that is a contract gap to return — not
something to reconstruct.

## Dispatch bindings (commit-auditor §"required fields")

| field | value |
|---|---|
| ticket | **#66**, slice **S4-B** (Prop/Bool mirrors, correspondence proofs, mandatory checker) |
| `ticket_owner_pane` (commissioner) | **`%503`** |
| `owner_pane` (implementation owner) | **`%547`** — `pi`, PID/PGID **1493708**, START Sat Sep 5 13:40:16 2026, worktree `/code/reactivegas-66-s4b`, verified live at dispatch |
| named ticket window | **the window **named** `lean-quality` in session `reactivegas` (index is volatile — root reorganises windows; bind by NAME and re-resolve with `tmux list-panes -a`)** — both panes above are in it |
| your pane / window | **a dedicated window **named** `rg-s4b-audit2` (currently `%563`)** — see the express exception below |
| model / effort | `codex -m gpt-6-astra -c model_reasoning_effort=high` — both pinned in argv; verify from `/proc` |
| **submission number** | **1** of 2 (owner submissions 0/2 consumed before this packet) |
| report path | `handoffs/AUDIT-REPORT.md` in **your** runtime root |
| candidate | **`189e1ed306f8f8e8bcdd11eeab4fc5657a518fc8`**, unchanged |
| base | **`3590c0015b84fd58004bf6fb44dd18b107304c48`** (accepted `master`, landed S2R) |
| your worktree | `/code/reactivegas-66-s4b-audit2`, detached at the candidate, verified clean, **zero oleans** |

Because this is **submission 1**, there is no rejected candidate SHA and no
prior *semantic* report to inherit. The predecessor seat produced **no verdict**:
it exited pre-START on an authority gap.

### Express exception to the same-window rule — written, not inferred

`commit-auditor` requires your `$TMUX_PANE` to differ from both named panes and
to resolve to the same ticket window. **Your pane will instead sit in a dedicated
audit window.** That departure is **expressly authorized for this slice** by the
desk artifact `QUALITY-S4B-TERMINAL-CONTRACT-RECOVERY-20260905`, item 2:
*"Dedicated audit window is expressly authorized for this slice; write that
exception and exact relationship."*

Exact relationship: commissioner `%503` and implementation owner `%547` are both
in the window **named** `lean-quality` in session `reactivegas` (index is volatile — root reorganises windows; bind by NAME and re-resolve with `tmux list-panes -a`); your window is a sibling in the same session,
holding only your seat. The rule's purpose — a distinct, non-reused seat with no
pane sharing between owner and auditor — is satisfied more strongly, not less.
Record the exception and its authority in your report.

## Campaign ledger and carried counters — established from record, not assumed

**Campaign ledger path:**
`/tmp/reactivegas/ms2/e-lean-compliance/commit-owner-s4b-muse/STATUS.md`
(append-only), with `handoffs/SUBMISSION.md`, `handoffs/EVIDENCE.sha256` and
`handoffs/evidence/` (186 archived files).

| counter | value | provenance |
|---|---|---|
| **auditor `builds_spent` / `builds_budget`** | **0 / 8 substantive**, **0 / 60 targeted** | Exactly **one** S4-B auditor seat has ever existed (`candidate-auditor-s4b-codex`). It is terminal, its own journal records `spend=0-substantive,0-targeted`, and its preflight ran no build. **This zero is measured from that record, not defaulted.** |
| auditor budget scope | **8 / 60 TOTAL across BOTH submissions**, not per submission | grant note in `admitted/` |
| owner spend | **8/8 substantive, 42/60 targeted** | owner ledger — **this is the owner's, and is never substituted for yours** |

**Ceiling-raise ledger for this ticket, complete:**

| # | ceiling | change | authority |
|---|---|---|---|
| 1 | owner substantive | **6 → 8**, for C4 and C26 only | `QUALITY-S4B-TWO-COMMAND-GAP-GRANTED-20260905` (in `admitted/`) |
| — | owner targeted | 60, **never raised** | — |
| — | owner submissions | 2, **never raised** | — |
| — | **auditor 8 / 60** | **never raised** — zero increases | set at commission |

**Count: one ceiling increase ticket-wide.** Losing the predecessor seat added no
submission and refunded no historical work.

Predecessor disposition: `AUDIT-CONTRACT-BLOCKED` + `COMPLETE`, report
`f151d6f4cc4cca337c2c826ed1e436a687c900caf6f3cb613a7cd25cac4175a0`, semantic
verdict **unjudged**, spend 0/0. Preserved in `admitted/` as an input. **Do not
resume that context or reuse its seat.** S4-A and S4-B histories are separate;
Phase A is an input, never inherited acceptance.

## Authority and precedence — frozen in `admitted/` with `MANIFEST.sha256`

`INSTRUMENT-v2-OPERATIVE.md` `2214ff8a0d25f47afded7b7215e9873b5a237d97caea55eb72b1d8f884c5ca4f`
is the **operative acceptance instrument**, together with
`AMENDMENT-NOTE-002` `2cd32f05…`, `AMENDMENT-NOTE-001-reconciliation-v2`
`72a47113…`, `AMENDMENT-NOTE-004-landed-base` `7b57b4e8…` (binds base
`3590c001`), and `S4-CONTRACT-ORIGINAL-REQUIREMENTS` `f872255f…`.

`INSTRUMENT-v1-SUPERSEDED` `44c48239…` is **history, not authority**.
`OWNER-BRIEF` `b4a79201…` records what the owner was told; **its citations of
instrument v1 and base `4a6cd87` are stale**.
`SUBMISSION-ADMITTED` `363999bd…` is **the claim under audit, not the test**.

Verify with:

```
cd /tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-codex-r2/admitted
sha256sum -c MANIFEST.sha256
```

`2214ff8a…` also appears in the owner's own submission header, so it
cross-checks independently of me. **Gate mechanism:** the candidate-bound
`just lean` / `just ci` recipes are the authoritative gate for this slice; there
is no separate `gate.sh` contract and none is expected.

## Scope

**The complete original S4 requirements plus operative v2 and all normative
amendments** — not only the nineteen correspondence rows and not only the
owner's selected 26. **Every candidate requirement remains falsifiable**, and no
row is closed by implication, by aggregate green, or by the owner's assertion.

## Mutation fence — typed

**WRITABLE:** your worktree `/code/reactivegas-66-s4b-audit2` (mutations, builds,
temporary drivers, restoration); your own runtime root; a separately retained
deliberate checker-control copy if the operative instrument requires one.

**NOT WRITABLE:** the reference candidate `189e1ed…` and
`/code/reactivegas-66-s4b`; the frozen `admitted/` instruments; any other lane's
worktree or runtime; anything remote — no push, PR action, comment, gist,
publication, deployment or merge. The branch is unpushed and stays so.

**Every mutation preserves its raw mutant and evidence of final restoration.** A
mutation you cannot show you reverted is a finding against your own run.

## Before START

Enumerate your **actual full command set** against the carried **8 / 60** and
confirm it fits, or **return the exact gap before executing anything**. Failed
and warm calls count; reads, greps, `git` interrogation and hashing are free.

Record at START from live inspection: PID/PGID, full argv, cwd, `git rev-parse
HEAD`, olean count, `$TMUX_PANE`, wall clock. Your START must postdate this
brief.

Label every conclusion **new-execution**, **unchanged-input with byte identity**,
or **inspection**. Deliver one verdict per row — CLOSED / OPEN / PARTLY — each
with its establishing command, observation and method label.

Local files only; contact no other seat. Report what you find, including that the
candidate is sound if it is. Your findings, severities and verdict are yours
alone.
