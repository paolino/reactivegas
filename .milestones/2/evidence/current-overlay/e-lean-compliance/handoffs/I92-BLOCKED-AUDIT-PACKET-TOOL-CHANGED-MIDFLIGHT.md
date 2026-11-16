# #92 I1 blocked — the sealing tool changed mid-flight, uncommitted

Owner `%503`. **Returned as a real contract conflict**, which is the one thing the
continue-note said to stop for. **No unit spent by I1. Nothing worked around.**

## What happened, in order, with times

| time | event |
|---|---|
| ~14:51 | I froze the I1 packet with `audit-packet freeze`. It printed **`AUDIT-PACKET-READY`**, manifest `67192452…`, `inputs=14`. |
| ~14:51 | I ran `audit-packet verify` immediately. It printed **`AUDIT-PACKET-READY`** again. |
| **14:58:07** | **`shared/skills/auditor/scripts/audit-packet` was modified on disk** (file mtime). |
| 13:58:45Z | I1 STARTed, ran its **own** pre-START verify as its brief required, got **exit 1, NOT-READY**, and **did not start subject inspection**. |
| now | My own re-verify returns `AUDIT-PACKET-NOT-READY: packet spec requires at least one TOOL record`. |

## The cause is not tampering with my packet

The seal did **not** detect a changed input. It failed because **the tool now
requires a record type that did not exist when the packet was sealed**.

`/code/llm-settings` is at commit `3bd353c1` — the same commit named in the reload
note — but the script is **uncommitted-modified in the working tree**:

```
 M shared/skills/auditor/SKILL.md
 M shared/skills/auditor/scripts/audit-packet      (+68 / −6)
 M shared/skills/auditor/tests/run.sh
 M shared/skills/ticket-orchestrator/SKILL.md
?? shared/skills/auditor/scripts/select-runtime-tool
```

The uncommitted diff adds a **mandatory `TOOL<TAB>label<TAB>executable` spec
record** (`[[ $tools -gt 0 ]] || die "packet spec requires at least one TOOL
record"`), plus a new untracked helper `select-runtime-tool` that resolves a
requested runtime against bounded alternatives.

So my packet was sealed against the tool as it existed at 14:51 and is being
verified against a **different, uncommitted** version seven minutes later.

## What I have not done, and why

**I have not added a `TOOL` record and re-frozen.** Doing so would bind this
campaign's audit packet to an **uncommitted, unannounced** tool state that no one
else can reproduce or audit — and a packet whose validity depends on one
developer's working tree is not a seal, it is a coincidence.

**I have not modified `/code/llm-settings`.** It is not my lane, and the standing
rule for it is explicit: everything on `main`, committed and pushed, always. The
ruling behind that rule names exactly this failure — a stale file on disk while the
working change lives only in an uncommitted tree.

**I have not restarted or re-briefed I1.** It behaved correctly: it verified from
its own seat, refused a NOT-READY seal, and stopped before inspecting. That is the
mechanism doing its job.

## Ledger, unchanged

Campaign spend **14/20 author, 14/24 total**. **I1 spent nothing.** Under the
launch-attempt rule its launch **is** charged — one CLI invocation, one launch
attempt, and a zero-execution contract-blocked launch remains charged — so
submission 1 now has **one launch consumed** and, per topology, **one aggregate
corrected redispatch remaining**, available only on evidence that a commissioning
defect changed. **That evidence plainly exists here**, but the redispatch is not
mine to spend without the tool question settled, because a re-freeze today would
seal against the same uncommitted state.

Submission 1 candidate `580e3d5f…` / tree `3c40cdd7…` is untouched and frozen.

## What I need

1. **Commit and push the `audit-packet` change**, or revert it, so packets seal
   against a reproducible tool. Either is fine; the current state is not.
2. Then rule whether I re-freeze the I1 packet with the required `TOOL` record —
   naming which executables the record should bind — and whether that re-freeze
   consumes the remaining corrected redispatch or is treated as a commissioning
   correction outside it.

**No push, PR, merge, acceptance or `#66` closure follows.** `%615` is idle and
intact; S3 remains terminal and not accepted.
