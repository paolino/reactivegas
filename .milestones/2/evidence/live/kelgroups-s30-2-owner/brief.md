# Brief — ticket preparation owner `t30-contract-opus-20260906` (kelgroups #30)

You replace the terminal Muse ticket owner of `t30-contract` under an operator-ordered role
substitution (`/tmp/reactivegas/ms2/artifacts/ROLE-SUBSTITUTION-OPUS-20260906.md`). This changes
**staffing only**. Scope, product semantics, caps, counters and acceptance are unchanged and carry
over exactly. You are the sole current ticket owner for kelgroups `#30` preparation.

## Objective

One observable outcome: **the bounded synthetic fixture campaign is finished and handed back** —
either the single remaining authorized repair rerun is executed with complete preserved evidence, or
a reasoned decision that it would establish nothing, plus in both cases the exact next compiler/product
prerequisite and cost under the unchanged original `#30` scope, or a precise blocker.

## Identity and runtime

| field | value |
|---|---|
| role | ticket orchestrator, planning altitude (CONTRACT PREPARATION ONLY) |
| worker ID | `t30-contract-opus-20260906` |
| your runtime root | `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/` |
| parent scope | epic `paolino/kelgroups#29`, epic owner pane `%532`, root `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/` |
| your pane | `%572` (reactivegas:12.2), cwd `/code/kelgroups` |
| your launch | `claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high` |
| preserved predecessor root | `/tmp/reactivegas/ms2/e-kelgroups-substrate/t30-contract/` — **READ-ONLY, never modify, never delete** |
| worktrees (READS ONLY) | `/code/kelgroups` (main `933e385d`, clean), `/code/reactivegas` (master `3590c001`) |
| upstream issue | `paolino/kelgroups#30` (OPEN). `#28` LANDED+CLOSED, `#33`/`#34` filed-blocked, `#29` + Reactivegas `#73` OPEN |

Quote your pane, launch command, model (`claude-opus-5[1m]`), effort (`high`) and this brief's sha256
in your `START`. Verify the pane and argv yourself (`$TMUX_PANE`, `ps -o args -p <pane_pid>`) — do not
copy them from this file.

Required skill load chain: `orchestrator-contract`, `ticket-orchestrator`, `resolve-ticket`
(planning phases only — no implementation/dispatch chapters), `context-compiler`, `worker-protocol`,
`tmux-orchestrator`, `verification`, `invariants`, `gate-script`, `haskell`, `nix`, `lean4` (read-only).

You are not alone in the codebase; do not revert edits made by others.

## Current state (verified by the epic owner at 2026-09-06T05:56Z, fresh commands)

- kelgroups `main` = `933e385df2f2a251bb54a08bb7663f0d41fafb64`, `/code/kelgroups` clean. `#28` LANDED (PR#32).
- Accepted Reactivegas Lean input = `3590c0015b84fd58004bf6fb44dd18b107304c48`.
- Predecessor journaled `HANDOFF-READY` 2026-09-06T05:52:30Z; its pane was idle, no children, nothing running.
- **pf8 invocation 1 DID execute** (the substitution order's "0 spent" premise was wrong; the predecessor
  corrected it in its own handoff and the evidence tree confirms it).
  `scratch/pf8/ev/SUITE.log` = `===== SUITE: FAIL (setup-failures=0 mispredicts=17) =====`;
  28 case directories under `scratch/pf8/cases/` with `stdout`/`stderr`/`exit` preserved.
- Epic-owner read of the raw evidence (A1, the baseline case). **These are quoted observations, not a
  diagnosis. The epic owner endorses no diagnosis of the 17 mismatches — that verdict is yours to reach
  independently.** They are listed only so you start from the raw streams rather than from prose:
  - `DRIFT-FAIL: 1-clean-hs: uncommitted bytes in kelgroups tree` — fixture dirt (untracked `dist-newstyle`).
  - `DRIFT-FAIL: 3-fresh: ... Types.hi OLDER than pre-build marker` — copy-order vs marker-mtime nondeterminism.
  - `DRIFT-REFUSE: 4-missing: no live dump for KelGroups.Vote.Types` → exit 3. This cascade is why 24 of 28
    cases exit 3 rather than exercising their intended domain observation.
  - `stderr`: `T30-DRIFT-LEG-r8.sh: line 53: id: No such file or directory` / `differently.: command not found`
    — the missing `#` on a comment continuation. Two noise lines, no verdict impact.
  - `setup-failures=0` was reported alongside those failing rows, so that counter alone is not evidence that
    the fixture was sound. Whether that is a defect in the instrument is part of what you judge.

## Constraints (binding, carried unchanged — no part of this is new)

**T30 preparation fence.** FORBIDDEN: product/compiler builds, Lean/Haskell/client compilation, tests,
mutations, product gate runs, semantic mutation, product edits in any repo, pair/worker dispatch of any
kind, integration grant, commits, push, PR, merge, release, issue comments, gists, network/credential use.
`draft=NONE`. Product-build spend is **0** and stays 0.

**Campaign budget — read the receipts, do not re-derive from prose.**
- Current campaign (authorized by `artifacts/KELGROUPS-T30-R7-BOUNDED-EXECUTION-DISPOSITION-20260906.md`):
  max **2** harness invocations. **1 spent** (pf8 inv1). **1 repair rerun remains.**
- Historical pf1: **2** failed invocations, retained separately, permanently spent, never refunded.
- Aggregate historical+new ceiling **4**; **3 consumed**. No quiet third in the current campaign.
- A new root, model or ticket refunds nothing and grants no new submission.
- Owner 26/24 and auditor 25/24 are **PROPOSALS**, not grants. `#30` implementation and audit are UNGRANTED.

**Evidence preservation.** Every failed run is preserved byte-identical. Never overwrite `scratch/pf8/`,
`scratch/pf7/`, `scratch/pf1/` or any `handoffs/` artifact in the predecessor root. Any repair works in a
**new** tree inside **your** root (`scratch/pf8r/`), seeded by copying from the predecessor root. No
re-execution merely to show work: spend the rerun only if it changes what can be concluded.

**Launch authority is not yours.** As with invocation 1, the epic owner performs the mechanical preflight
and issues the binding receipt (bound sha256 of runner + gate leg, exact command, exact CWD). You do not
launch invocation 2 until that binding exists in your `inbox/`. This is the epic owner's mechanical
preflight, not another desk checkpoint.

**Settled, do not reopen.** Composition/lifecycle authority; the `#68` V-2 ruling as settled-but-unlanded
(rebind boundary, never anticipation); no shipped theta default (threshold is a parameter, exhibits are not
defaults); unruled `notProposer`/`notDesignee` stay UNSCHEDULED; `#76`/`#81` content neither implemented
ahead of Lean nor marked permanently out of the eventual contract — state both explicitly per row.
`#68`/`#76`/`#81` changes bind only after accepted upstream landings. No Reactivegas economic
implementation in kelgroups, no vendoring.

**Honest scope of any green.** Passing the synthetic suite means the exercised shell/git plumbing on
synthetic fixtures — never compiler compatibility, compiler discovery, semantic coverage, mapping
completeness or product readiness. The r8 runner and gate leg were text-reviewed only.

## Task

1. **Admit.** Verify your own pane/argv/model/effort and this brief's hash. Read, in order:
   this brief → the predecessor's `handoffs/OWNER-SUBSTITUTION-HANDOFF.md` → its `STATUS.md` tail →
   its `brief.md` (`f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`) → its full inbox
   (`NOTE-001`…`NOTE-009`, `PREFLIGHT-INVOCATION-1-BINDING.md`, `SUBSTITUTION-ORDER.md`) →
   `handoffs/T30-CONTRACT-r8.md`, `T30-COMMAND-MAP-r8.md`, `T30-DRIFT-LEG-r8.sh`, `scratch/pf8/run.sh` →
   the pf8 inv1 raw evidence. Then `START`.
2. **Consume the inv1 evidence yourself.** Do not accept the predecessor's diagnosis or the epic owner's
   summary above as fact — both are leads. Read the raw streams and say per required mechanism whether it
   is ESTABLISHED, REFUTED, or UNESTABLISHED by inv1, with the exact file:line evidence.
3. **Decide the rerun on merit.** Either (a) design and write the repair in a new `scratch/pf8r/` tree
   (seeded by copy; predecessor bytes stay frozen), with per-case predictions and the exact command, and
   file it for the epic owner's preflight binding; or (b) return a reasoned decision that the rerun would
   establish nothing beyond inv1, naming what would be needed instead. Either is a complete outcome.
4. **On receiving the binding receipt**, execute exactly the bound command once, preserve complete raw
   streams, exit codes and script/fixture identities, and journal the actual counter.
5. **Hand back** the complete experimental result plus your own assessment: which required mechanisms are
   established, which remain unestablished, and **the exact next compiler/product prerequisite and its
   cost** under the unchanged original `#30` scope. Any residual scope or assurance claim stays explicit.

Do not stop after intake to announce future actions, and do not invent an approval checkpoint between
steps you are already authorized to take. Steps 1–3 need no further permission.

## Verification commands and expected evidence

- Identity: `echo $TMUX_PANE`; `ps -o args -p $(tmux display-message -p -t $TMUX_PANE '#{pane_pid}')`
  → exactly `claude --dangerously-skip-permissions --model claude-opus-5[1m] --effort high`.
- Bases: `git -C /code/kelgroups rev-parse HEAD` → `933e385d…`; `git -C /code/kelgroups status --porcelain`
  → empty; `git -C /code/reactivegas rev-parse HEAD` → `3590c001…`.
- Artifact identity: `sha256sum` every packet/runner/leg you rely on or produce; quote the hash in STATUS.
- Campaign: `cat <predecessor>/scratch/pf8/ev/SUITE.log` and the per-case `exit`/`stdout`/`stderr`.
- Preservation control: before and after any work, `sha256sum` a sample of predecessor pf8 case streams and
  show they are unchanged.

## Authority

Commit/push authority: **NONE**. No commits, no push, no PR, no merge, no issue comments, no release,
no publication. Writes are confined to your own runtime root. Reads are unrestricted within
`/code/kelgroups`, `/code/reactivegas` and the preserved predecessor root.

## Questions, inbox, reporting, stop conditions

- Blocked → `questions/Q-NNN-<slug>.md` + `BLOCKED  Q-NNN-<slug>`, then park. The epic owner answers in
  `answers/A-NNN-<slug>.md`; acknowledge with `RESUMED  Q-NNN-<slug>`.
- Epic-owner corrections and the preflight binding arrive in your `inbox/`; acknowledge `NOTE  NOTE-NNN read`.
  Check unread inbox before every phase, before any execution, and before `COMPLETE`.
- Journal every event with
  `/code/llm-settings/shared/skills/worker-protocol/scripts/status-event <your-root>/STATUS.md <TAG> "<msg>"`.
  Never hand-write a timestamp. `START` is acknowledgement, not progress.
- **Every stop is terminal (`COMPLETE`), `PARKED: <reason>; wake=<exact condition>`, or `BLOCKED Q-…`.**
  Include `COMPLETE` for a capacity-limit stop with a handoff path — silence is the one unacceptable ending.
- **Upward reporting is local files only**: your `handoffs/` + `STATUS.md`, plus a pointer file in
  `/tmp/reactivegas/ms2/inbox/`. **Never type, paste or send-pointer into desk `%510` or any human composer.**
  Propagate this rule to anything you write.

## Escalate to the epic owner (`%532`) when

Scope, budget or authority must change; a required input contradicts another with no settled ruling; the
repair needs a capability outside this fence; or the campaign cannot conclude within the remaining single
rerun. Name the exact competing evidence. Do not reopen settled rulings, and do not infer a shipped theta.
