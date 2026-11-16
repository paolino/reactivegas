---
name: tmux-orchestrator
description: "Visible-worker transport for tmux: bind to the current conversation pane, name and construct role layouts, choose and launch a supported CLI, deliver durable file pointers with acknowledgement, inspect liveness, recover/reset panes, and repair layout drift. Use whenever an orchestrator runs child agents in tmux. Load orchestrator-contract and worker-protocol for role and communication semantics; this skill owns pane mechanics only."
---

# Tmux Orchestrator

This skill makes delegated work visible. It owns tmux transport, not the work
contract:

- [[orchestrator-contract]] — authority, supervision, escalation, acceptance
- [[worker-protocol]] — briefs, STATUS, Q/A, inbox, monitor, archival
- altitude role skills — milestone, epic, ticket, commit-owner, or auditor duties

Read `references/recovery.md` when a pane stalls, dies, needs correction, or is
reused. Read `references/recursive-dispatch.md` when a child will orchestrate
its own children. Read `references/red-flags.md` during a protocol audit.

## Active-goal guard

Bind one tmux lane to one declared goal: repository, issue/PR/epic, worktree,
branch, runtime root, and worker set.

Before acting on new input, classify it:

- same goal or lane management → proceed;
- explicit retarget → park/close old workers and persist status before reuse;
- clearly different goal → refuse in this pane and name the correct lane;
- ambiguous → inspect identity facts, then ask only if still unresolved.

Do not spawn, edit, push, or alter remote metadata until the request belongs
to this lane.

## Bind to this conversation

Use `$TMUX_PANE`, never the attached client's ambient focus:

```sh
parent_pane=${TMUX_PANE:?run inside the tmux pane serving this conversation}
parent_window=$(tmux display-message -p -t "$parent_pane" \
  '#{session_name}:#{window_index}')
tmux list-panes -t "$parent_pane" \
  -F '#{pane_id} #{session_name}:#{window_index}.#{pane_index} #{window_name} cmd=#{pane_current_command} path=#{pane_current_path}'
```

An untargeted `tmux display-message -p` can return whichever pane a human
client happens to be viewing, which may be another orchestrator. Every
split, move, layout, capture, and focus command uses a stable pane ID such as
`%471`.

After every spawn or move, list panes through `"$parent_pane"` and prove the
child is in the same window. If not:

```sh
tmux join-pane -d -h -s <worker-pane> -t "$parent_pane"
```

Then re-list. Never respond to a missing worker by spawning a duplicate.

## Window names

The name answers what the lane owns:

| Lane | Pattern |
|---|---|
| milestone singleton | `<repo>-ms<id>-<goal>` |
| epic with current ticket | `<repo>-e<epic>-t<ticket>-<goal>` |
| standalone ticket | `<repo>-no-epic-t<ticket>-<goal>` |
| unresolved operation | `<scope>-no-epic-t-unknown-<goal>` |

Rename as soon as IDs are known and whenever the active ticket/goal changes:

```sh
tmux rename-window -t "$parent_window" <name>
```

Generic names such as `codex`, `worker`, or `orch` are temporary setup state.

## Canonical layouts

Milestone owners are singleton windows. Epic execution uses a quadrant:

```text
┌────────────────────┬────────────────────┐
│ epic owner         │ ticket owner       │
├────────────────────┼────────────────────┤
│ commit owner       │ work slot          │
│                    │ draft tool / auditor│
└────────────────────┴────────────────────┘
```

Standalone OWNER tickets use the ticket owner above a commit-owner plus
sequential work-slot row. The ticket owner launches the commit owner. If the
brief permits one draft-tool attempt, the commit owner creates the work-slot
pane and owns it until the draft process is stopped, evidenced, archived, and
the pane is killed. After `PROOF-COMPLETE`, the ticket owner creates a new
work-slot pane for a fresh auditor while the commit owner remains parked and
write-idle.

The two roles never overlap in the work slot and never reuse a process,
conversation, runtime root, or worktree. On an authorized repair, the auditor
is already closed; the owner resumes alone, then the ticket owner creates a new
fresh auditor pane for the new SHA.

A ticket owner's LIGHT slice uses only the work slot. Leave the commit-owner
position absent; do not invent a second worker for symmetry. A later OWNER
escalation resets that pane and launches a fresh commit owner. It never reuses
the LIGHT worker's context as an owner, draft, or auditor.

Adjacent LIGHT slices may reuse a sanitized CLI-native session under
[[orchestrator-contract]]'s context lease. Terminate the prior process and
sandbox first, archive its root, create the new worktree/root/brief, and
relaunch the resume/capsule ID through the new sandbox in the same pane. Mount
only the exact sanitized session artifact needed for resume, read-only; never
mount the CLI's whole home or credential store. Require a new
`START context=REUSED`. If the CLI cannot resume without live-process reuse,
ambient credentials, or writable old state, context reuse is unavailable.

Build an OWNER lane in ownership order. Start from the ticket-owner pane; do
not create a new window unless the operator explicitly requests a separate
lane. Standing authoritative families are `claude`, `codex`, `grok`, `glm`, and
`muse`.
Eligibility is membership in that set plus the alternation constraints
and the role fence in [[orchestrator-contract]]. Prefer `glm` for the
commit-owner seat when its probationary role and secrets fence allow; otherwise
prefer `grok` when the secrets bar and the unmetered-family cap allow. There is
no standing draft-tool family. Do not encode the set as a provider-flip table.

Determine the current seat's actual CLI family before splitting. A different
model name from the same provider is not an alternate agent. Derive each edge
mechanically from the standing family set, the current family, and any explicit
seat exclusions:

```sh
commit_owner_cli=$(
  /code/llm-settings/shared/skills/tmux-orchestrator/scripts/alternate-authoritative-cli \
    --seat commit-owner "$ticket_owner_cli"
)
commit_auditor_cli=$(
  /code/llm-settings/shared/skills/tmux-orchestrator/scripts/alternate-authoritative-cli \
    --seat commit-auditor "$commit_owner_cli"
)
```

The helper chooses the first remaining family eligible for that seat.
Commit-owner order is `glm`, `grok`, `muse`, `claude`, `codex`. Auditor order is
`grok`, `claude`, `codex`; **neither `glm` nor `muse` is ever an auditor**. To keep all three seats
distinct, or to apply the unmetered-family cap, pass already seated families
as exclusions:

```sh
commit_auditor_cli=$(
  /code/llm-settings/shared/skills/tmux-orchestrator/scripts/alternate-authoritative-cli \
    --seat commit-auditor "$commit_owner_cli" "$ticket_owner_cli"
)
```

The helper normalizes `codex-raw` as the Codex family and returns
`NO-AUTHORITATIVE-ALTERNATE` with exit 66 when exclusions exhaust the set.

The ticket owner creates the distinct commit-owner pane itself; it never
executes the owner role in the T.O. pane or through a hidden in-process child:

```sh
commit_owner_pane=$(tmux split-window -d -v -t "$ticket_pane" -c <worktree> \
  -P -F '#{pane_id}' "$commit_owner_launch")
```

`commit_owner_launch` must be the mapped alternate CLI. Immediately verify
same-window placement, deliver the durable pointer with `send-pointer`, and
require `START mode=COMMIT-OWNER pane=$commit_owner_pane ... alternate=true`.
Until that exact post-cursor acknowledgement exists, classify the role as
`NEVER-STARTED`; pane presence or a busy spinner is insufficient.

After committing the complete RED bundle, the commit owner may create the
one-shot draft-tool pane beside its own pane:

```sh
draft_pane=$(tmux split-window -d -h -t "$commit_owner_pane" -c <draft-worktree> \
  -P -F '#{pane_id}' "$draft_tool_launch")
```

The ticket owner must not issue that command. After the draft's terminal
result, the commit owner stops it, archives its root, and kills `draft_pane`.

After the owner submits and parks, the ticket owner creates the fresh auditor
in the now-free work slot. `auditor_launch` must use the mapped alternate CLI
from the commit owner, even when that is the same CLI family as the T.O.:

```sh
auditor_pane=$(tmux split-window -d -h -t "$commit_owner_pane" -c <audit-worktree> \
  -P -F '#{pane_id}' "$auditor_launch")
```

The commit owner must not issue the auditor command. After its terminal report,
the ticket owner archives the auditor root and kills `auditor_pane`. Require
`START mode=COMMIT-AUDITOR pane=$auditor_pane ... alternate=true` before
admitting any verdict. Every repaired submission gets another newly created
pane and process; never reset or reuse the prior auditor pane.

Always use `-d`; an attached spawn steals focus and can receive the operator's
next keystroke. Verify geometry:

```sh
tmux list-panes -t "$parent_pane" \
  -F '#{pane_id} left=#{pane_left} top=#{pane_top} w=#{pane_width} h=#{pane_height}'
```

When nested under an epic quadrant, matching column widths and row heights are
the postcondition. Standalone tickets may have the ticket-owner top row and
two-pane implementation row. See `references/recovery.md` for drift repair.

## Choose and launch the CLI

The parent brief chooses the CLI, model, and reasoning effort before the split.
Default to the parent's CLI only when no higher contract overrides it. Never
guess across conflicting instructions.

Production rules:

- launch interactively in the exact worktree;
- enable the CLI's non-interactive approval/bypass mode;
- pin model and reasoning effort explicitly;
- pass no task prose as a Codex CLI argument;
- record the exact launch command in the durable brief/resume record;
- launch a standing authoritative family (`claude`, `codex`, `grok`, `glm`, `muse`)
  into the seat that family is eligible for, preferring `glm` for commit owner
  when its probationary role and secrets fence allow, then `grok` when the
  secrets bar and unmetered-family cap allow;
- keep `draft=NONE` unless a new operator ruling names an exact replacement
  family, harness, model, effort, and fence.

LIGHT is an exception to bare approval-bypass launching. A gate can detect
persistent source drift but cannot undo transient writes, credential reads,
network access, or resource bursts. Launch LIGHT only through an OS-enforced
sandbox supplied by its immediate parent brief:

- only allowlisted implementation paths and the worker STATUS/evidence root
  are writable;
- gate, oracle, specs, and all other repository paths are read-only;
- ambient credentials and unneeded network access are unavailable;
- PID, CPU, memory, disk, and wall-time limits are enforced externally.

If the available CLI/host transport cannot enforce that profile, LIGHT is
unavailable and must use OWNER. Do not treat approval bypass or a prompt-only
path fence as a sandbox.

A draft-tool candidate is never accepted mechanically: the strong commit
owner stops it, independently freezes its disposable worktree, reads the
complete candidate, repairs it, and owns the final code. Use an OS-enforced
sandbox when an approved one exists. When the operator explicitly permits a
replacement launcher, record whether its fence is preventive or detective,
use a fresh disposable worktree/runtime, grant no commit/push authority, and
inspect repository, process, and credential-sensitive state before integration.

### Sandbox launch and attestation contract

The same contract governs a parent verifier executing unreviewed LIGHT code.
For verification, candidate source, gates, Git metadata, and evidence are
read-only; only a fresh private build/runtime root is writable.

The durable brief names an approved host-specific launcher by absolute path
and version/digest, its exact profile arguments, and a parent-owned attestation
path outside every worker-writable root. The launcher—not the worker—must emit:

- role/run ID, exact command digest, start/end times, observed exit, and parent
  PID;
- UID/GID, PID/mount/network namespace IDs, cgroup ID, and resolved
  device/inode for every mounted root;
- complete read-only and writable root sets, with link-resolution checks;
- absence of ambient credential mounts/environment and the enforced network
  policy;
- PID/CPU/memory/disk/wall limits, observed usage, and terminal
  process/cgroup cleanup.

The immediate parent runs the launcher's independent attestation verifier and
binds the resulting attestation hash in STATUS. A worker-authored statement,
profile prose, namespace creation without mount proof, or exit code alone is
not attestation. The launcher must fail closed when it cannot enforce or
measure a field.

This transport skill does not designate a universal sandbox backend: host
mount, cgroup, credential, and network policy differ. If the parent cannot name
an already approved launcher and verifier satisfying this exact contract,
LIGHT and unreviewed LIGHT verification are unavailable on that host.

Before LIGHT acceptance or draft-tool materialization, terminate the
interactive CLI, revoke any writable mounts/cgroup, and verify no owned process
remains. A `COMPLETE` STATUS line does not revoke live write authority.

Known command shapes:

```sh
# Codex
codex-raw --dangerously-bypass-approvals-and-sandbox \
  -C <worktree> -c model_reasoning_effort=<effort>

# Claude — the model string is quoted and carries the [1m] context suffix
claude --dangerously-skip-permissions \
  --model 'claude-opus-5[1m]' --effort <effort>

# GLM — probationary commit-owner only. The wrapper sources the Z.AI key and
# fixes provider=zai, model=glm-5.3-flash, thinking=max.
glm --approve

# Kimi
kimi --auto -m <approved-kimi-model> \
  --skills-dir /code/llm-settings/shared/skills

# Grok — standing authoritative family; pinned to grok-4.6
grok --always-approve -m grok-4.6

# muse — standing authoritative family for TICKET-OWNER and COMMIT-OWNER only,
# never an auditor. The wrapper pins provider/model/effort and refuses to launch
# degraded if the model is missing from the catalog (exit 69), so never call
# `pi` directly for a muse seat.
muse --approve
# reports: harness=pi provider=opencode-go model=muse-spark-1.3-contributor effort=xhigh

```

Resolve approved model IDs from the parent brief or a shape above, not from a
stale copied example and **not from the local session default**. A bare
`claude` inherits whatever `~/.claude/settings.json` pins, which is a global
setting no lane controls: a research-class small-context model pinned there
silently becomes the model of every authoritative seat launched without
`--model`. Name the model in every launch.

Two ways the Claude model string goes wrong, both of which produce a seat that
looks correct in `ps` and is wrong in the pane:

- **Dropping the `[1m]` suffix.** `claude-opus-5` and `claude-opus-5[1m]` are
  different session pins. Long-lived orchestrator and commit-owner seats are
  exactly the ones that exhaust context, so the suffix is part of the approved
  identifier, never an optional decoration.
- **Leaving it unquoted.** `[1m]` is a bracket glob to the shell. Unquoted it
  is at the mercy of the launching directory and `nullglob`/`failglob`
  settings. Always write `--model 'claude-opus-5[1m]'`.

### GLM model and harness pinning

GLM is approved only as a probationary commit owner through the `glm` wrapper.
The family is `glm`; the harness is Pi. Do not record `pi` as the family because
Pi can drive many providers and models. The wrapper rejects provider, model,
thinking, and API-key overrides and always launches:

```text
harness=pi provider=zai model=glm-5.3-flash effort=max
```

Use `glm --approve` for an interactive tmux worker. The role must report the
full identity in START. GLM cannot audit its own or another owner's work and
cannot touch production secrets; dispatch a fresh eligible auditor after every
GLM candidate.

### Grok model pinning

Grok is approved for **`grok-4.6` only**. Older models (`grok-4.5`) and other
aliases offered by `grok models` are **not approved** for any Grok seat. Always
pin `-m grok-4.6` explicitly rather than relying on account defaults:

```sh
grok --always-approve -m grok-4.6
```

Record the launch command in the resume record **exactly as it must be
replayed**, including the quotes. That recorded line is what an operator or a
successor pastes to resurrect the seat, so an approximation there becomes the
next seat's real launch.

New or historically low-discipline CLIs start on low-blast-radius work and
receive tighter diff/liveness checks. Coding ability never waives
[[orchestrator-contract]] acceptance.

## Dispatch

For each worker:

1. Initialize its [[worker-protocol]] root at dispatch time.
2. Write the complete durable `brief.md`.
3. Bind the parent pane and refresh the window name.
4. Spawn detached with the recorded launch command.
5. Verify same-window placement.
6. Start the worker-protocol monitor as a foreground/event wait.
7. Deliver only a short pointer and require a fresh acknowledgement.
8. Restore focus to the parent pane.

For commit owners and auditors, steps 4–7 are mandatory authority
preconditions, not optional visibility. The T.O. records the returned pane ID
before delivery and rejects a `START` whose reported pane or CLI family does
not match the launch. A direct function/subagent call cannot substitute for
the split, pointer, and acknowledgement.

Initialize:

```sh
worker_root=$(/code/llm-settings/shared/skills/worker-protocol/scripts/init-worker \
  <parent-root> <worker-id>)
```

After writing `"$worker_root/brief.md"`, deliver it:

```sh
/code/llm-settings/shared/skills/tmux-orchestrator/scripts/send-pointer \
  <worker-pane> "$worker_root/brief.md" "$worker_root/STATUS.md" \
  '  START  '
```

`send-pointer` uses a tmux buffer, settles the paste, retries submission, and
then waits for a post-cursor STATUS acknowledgement. Its success is the
dispatch postcondition. A visible paste or busy spinner without `START` is not
a dispatched worker.

For a CLI whose interactive prompt cannot consume a pointer reliably, pass the
same short pointer through that CLI's supported interactive-initial-prompt
flag, then still require the STATUS acknowledgement. Never put the full brief
in an argument or long TUI paste.

## Monitor and liveness

Run:

```sh
TMUX_NOTIFY_TARGET="$parent_pane" \
  /code/llm-settings/shared/skills/worker-protocol/scripts/monitor-workers <parent-root>
```

Use an event tool or a foreground blocking command; a background process that
prints while the orchestrator sleeps does not wake it.

STATUS is content. `capture-pane` is only for liveness:

```sh
tmux capture-pane -pt <worker-pane> -S -15
```

Routine captures stay near 10–15 lines and at most once per minute per worker.
Use wider scrollback only after a detected anomaly. Look for a running tool,
idle prompt, unsent input, approval dialog, wrong model, repeated loop, or API
failure. Do not make design or acceptance decisions from half-finished pane
reasoning.

**The child's own STATUS journal is the liveness signal. Pane state is not.**
Every pane-derived measure is a proxy and each one produces false alarms in a
different direction:

| Proxy | Fails when |
|---|---|
| pane exists | the process is alive but idle with input never submitted |
| pane CPU | the CLI is blocked on a model response and burns almost nothing |
| unsent composer text | the worker was told to ignore unattributed input and is working |
| pane content unchanged | the worker is thinking inside one long turn |

So gate every stall verdict on **journal age first**: a child that has journaled
recently is working, whatever the pane looks like. Use the proxies only to
*classify* a child that has already gone quiet — unsent composer text plus a
stale journal is a real stall; either one alone is not.

A monitor built on a proxy will be rebuilt repeatedly, each time after it misses
or invents an incident. Before arming one, run it against a known-good and a
known-bad state and require it to distinguish them.

On `NEVER-STARTED` or `STALE`, classify and act immediately. Supervise only
the immediate child; wake an intermediate orchestrator instead of touching
its descendants.

## Corrections and answers

Write the correction, answer, or recovery instruction to the child's durable
brief/inbox/answer file first. Then send its pointer and require the matching
acknowledgement:

```sh
/code/llm-settings/shared/skills/tmux-orchestrator/scripts/send-pointer \
  <worker-pane> <absolute-instruction-file> <absolute-STATUS.md> \
  '  (RESUMED|NOTE)  '
```

Actionable corrections interrupt at a safe boundary. Queueing is reserved for
passive information whose delay cannot invalidate the current step; the inbox
file remains the durable backstop. See `references/recovery.md`.

Never use raw `paste-buffer` followed by an unverified bare Enter in
production dispatch. Never treat an answer file as delivered until `RESUMED`
appears.

**A nudge is a dispatch.** "Keep going", "continue with X" — anything that makes
a worker take another turn — goes through `send-pointer` with the acknowledgement
handshake, exactly like a brief. There is no message short enough to type
directly; the dropped keystroke does not care how long the text was. If the
intent is purely to continue, write a one-line inbox note whose whole body is
that instruction and deliver it the same way.

**A worker that needs a nudge every turn was given its instruction in pieces.**
Repeated "continue" dispatches are a symptom, not a workflow: each buys one turn,
and every handoff is another chance for delivery to fail. Give the worker the
complete remaining scope and an explicit terminal condition — succeed, block, or
stop at capacity — then arm one wait and send nothing until it reaches one of
them.

## Reuse and respawn

At a context boundary, use the CLI's native reset command and verify a fresh
session before the next pointer. If reset semantics are unknown or the next
run changes CLI/model/effort, close and respawn the pane.

Pane reuse never means runtime-root reuse. Archive the accepted old root,
create a fresh one, write a fresh brief, and require a new `START`.

Pane death loses conversation but not the protocol. Reconstruct from the
worktree, brief, STATUS, Q/A, inbox, and resume record, then respawn with the
same recorded command. See `references/recovery.md`.

## Completion

When a child logs `COMPLETE`:

1. the immediate parent independently accepts at its own altitude;
2. on acceptance, move the runtime root under `.archived/`;
3. reset/close the pane or create a correction run;
4. record the accepted artifact in the parent's STATUS.

For ticket slices, [[resolve-ticket]] owns acceptance. For child tickets,
[[resolve-epic]] owns ticket-level completion.

## Report

After dispatch report the window name, pane IDs/roles, worktree, worker IDs,
runtime roots, monitor state, and restored parent focus.

After completion report accepted commit/artifact, verification command and
exit status, residual risk, and whether panes were reset, closed, or retained.

## Red flags

- an untargeted tmux identity command;
- spawning into a new or wrong window;
- missing `-d` on a split;
- task prose passed to Codex as a CLI argument;
- a worker called running without a post-cursor `START`;
- a commit owner or auditor run inline, outside tmux, or in a reused pane;
- a same-family T.O.→owner or owner→auditor handoff;
- a GLM seat outside the commit-owner role, without `harness=pi
  provider=zai model=glm-5.3-flash effort=max`, or touching production secrets;
- raw unacknowledged paste+Enter delivery;
- pane transcript used as durable content;
- background-only supervision;
- parent controlling a grandchild;
- context or runtime root reused across slices;
- duplicate workers created instead of repairing layout.
