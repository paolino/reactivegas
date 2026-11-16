# Brief — ticket owner `t30-s30-2d-opus-20260906` (kelgroups #30, S30-2D evidence-only re-cut)

You are a **fresh ticket-owner successor**. The previous ticket owner `%572` is **COMPLETE and must not be
resurrected**; its runtime root is preserved read-only as evidence. You are not a continuation of its
conversation — you are a new seat with the same ticket.

## Objective

One observable outcome: **S30-2D runs its single authorized audit and returns a terminal verdict with per-row
attribution**, or returns a concrete mismatch **before** launch. Nothing else.

## Identity and runtime

| field | value |
|---|---|
| role | ticket orchestrator, kelgroups `#30`, slice **S30-2D** (evidence-only re-cut) |
| your runtime root | `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-s30-2d-opus-20260906/` |
| parent | epic owner `%532`, root `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/` |
| your pane | assigned at dispatch; verify it yourself from `$TMUX_PANE` |
| your pin | `claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high` (operator role-substitution pin governing this lane) |
| predecessor root (READ-ONLY evidence) | `<parent>/t30-contract-opus-20260906/` — never modify, never resurrect `%572` |

Skills: `orchestrator-contract`, `ticket-orchestrator`, `resolve-ticket`, `context-compiler`,
`worker-protocol`, `tmux-orchestrator`, `auditor`, `commit-auditor`, `verification`, `invariants`,
`gate-script`, `haskell`, `nix`. Reload them at shared revision `6aa0ad7ce39caa4e47a5c428947a3c32383a4173`.

You are not alone in the codebase; do not revert edits made by others.

## First act — reconcile from live evidence, not from this brief

**Before commissioning anything**, reconcile the retained commit-owner identity and the candidate **from live
process and worktree evidence**, and record what you observed:

- `%607` is the retained Muse commit owner (`pi --provider opencode-go --model muse-spark-1.3-contributor
  --thinking xhigh --approve`, pid 59239 at my last look). It stays **parked, write-idle, not woken, and
  without write authority.** Verify it; do not message it.
- candidate `bdeba37a093c59a579c27735a41c0c560ec70f1c`, tree `2988c76009832c1a71b4cda47ce2a14a51d0c53b`,
  accepted base `9762ad4db50f370348ea71abd44f7e969349d4b4`. **Byte-identical and unjudged.**
- S30-2C terminal report `8dd61e2368c8232d0c1cd9811f66fa02cb97c88c6c38f8ad3266b38a4161e44b`.

Treat every figure in this brief as a claim to verify, including the counters.

## Authority — S30-2D

| item | value |
|---|---|
| campaign substantive ceiling | **4** |
| one auditor allocation | **4** |
| launch attempts | **0/1 — exactly one launch** |
| product submissions | **0** |
| repair batches / candidate-write authority | **0** |
| seat | one **fresh `codex` `gpt-6-astra`, effort `high`**, alternate to the retained `muse` commit owner |

Historical counters stay historical: ticket-wide audit spend **15/25**, owner operations **17/28**, exhausted
implementation submissions and repair bounce unchanged. **Ten arithmetically unspent ticket-wide audit units
are capacity, not authority** — do not draw on them, and do not let anyone (including me) call such a draw a
reallocation.

Scope: the **same four ordered executions and finite denominator** from S30-2C. **Six active blocking
invariant rows plus `CTRL-1` reopened for actual evidence.** The two inherited requirement kills are carried
as **named history and remain challengeable**, never counted as fresh executions.
**This is not broad ticket discovery.**

## Packet — V2 at `6aa0ad7c`

Working-tree fact I verified and you should re-verify: `HEAD == origin/main == 6aa0ad7c`; the tree is **not**
globally clean (`claude/settings.json`, `codex/config.toml`), but **`shared/skills` is clean** and the
executed `audit-packet` on-disk sha256 `4398936bae023f1b129b776ce789d65629931ef6208030206f34b14a7f705f42`
**equals** its committed value (`AUDIT_PACKET_SPEC_V2`, 20766 bytes). **`HEAD == origin/main` alone is
insufficient** — bind the executed bytes and the role profile.

**Create a new versioned V2 packet. Never overwrite, convert or reinterpret S30-2C's V1 artifacts.**

Before freeze, complete and hash-bind: the **dispatch-preflight receipt**; the **current campaign ledger**
carrying all historical spend/attempts plus this new authority; the **current row ledger** carrying every
active, inherited and open row. Declare each as an `INPUT`, then bind **exactly once**:

```text
ROLE\tdispatch-preflight-receipt\t<declared-input-label>
ROLE\tcurrent-campaign-ledger\t<declared-input-label>
ROLE\tcurrent-row-ledger\t<declared-input-label>
```

Name specialization `commit-auditor` and add every specialization `REQUIRE`/`ROLE` pair. Declare **every
mandatory executable** as `TOOL`. The packet also binds exact candidate/tree/base, rejected predecessor and
exact repair delta, retained terminal report, immutable open row set, mandate, gate, instruments,
denominator, output paths, stop rule, commissioner/author/auditor identities, and the new authority.

Proposed counters: resource spent `0`, ceiling `4`, reserved `4`; seat allocation `4`; submission attempts
spent `0`, ceiling `1`, dispatch attempt `1`; campaign launches spent `0`, dispatch launch `1`.
**If the schema distinguishes product submissions from auditor dispatch attempts, record product submissions
as zero and use the required schema values without relabelling history. Return an exact schema conflict
before launch rather than inventing a count.**

The preflight receipt must **substantively prove** the clean detached exact-candidate worktree, the gate at
the seat's actual path, complete row extent, historical and new counters, the non-overlapping four-command
schedule, family separation, launch configuration, and report/evidence paths. **It must not cite a manifest
that does not yet exist.**

## Launch sequence — and the circularity trap

Freeze **only after all inputs are final**. Create the target pane **without starting the CLI**; run V2
`verify` and `preflight` **from that pane's exact cwd and environment**. Store those results plus the
manifest hash in a **separate launch receipt**.

**The READY/PREFLIGHT results are NOT appended to the sealed pre-launch receipt — that would invalidate it
circularly.** They live in the separate launch receipt, which `START` cites.

Only then launch the CLI, and require a post-cursor `START` **matching the frozen attempt and launch
ordinals**.

## Tool semantics — corrected, and the correction matters

**Optional convenience tools are auditor choices.** An unsupported option or unavailable convenience tool is
**recorded and adapted within the same launch at zero candidate cost** when an equivalent exists.
**The generic claim that every command failure is terminal is REMOVED** — it was over-broad, it came from my
predecessor note, and it terminalized S30-2C on a `--help` usage error against a present, working helper.
**Required candidate commands retain their exact fail-and-return semantics.**

## Terminal conditions

**Exactly one launch. A setup or optional-helper problem is not repaired by relaunch.** Any **candidate
finding or command failure returns for desk disposition** through me, because no implementation submission
remains. If the audit **passes every active row**, return the exact acceptance packet upward — **do not
push, open a PR, merge, close `#30`/`#29`/`#73`, comment, or start the next slice.**

If the packet and four-command schedule **fit exactly**, dispatch and continue through the one terminal audit
and **one** ticket-owner adjudication **without another checkpoint**. **If anything does not fit, return the
concrete mismatch before launch.**

## Reporting

Journal one event per substantive phase via `status-event`. Every stop is `COMPLETE`, `BLOCKED  Q-NNN`, or a
park whose wake names **another party and artifact** — never your own next turn. Local delivery only:
`handoffs/`, your `STATUS.md`, and a pointer in `/tmp/reactivegas/ms2/inbox/`. **Never the desk composer.**
