# Ticket-owner brief — reactivegas #57 structural vote validation

Compiled: 2026-08-29T12:35:00Z

## Identity and authority

- Role: ticket orchestrator for `paolino/reactivegas#57`.
- Worker ID: `t57-owner-codex`.
- Parent: epic owner for `paolino/reactivegas#43`, pane `%38`, runtime root
  `/tmp/reactivegas/ms2/e43/`.
- Runtime root: `/tmp/reactivegas/ms2/e43/t57-owner-codex/`.
- Worktree: `/code/reactivegas-issue-57`.
- Branch: `fix/57-structural-vote-validation`.
- Frozen starting HEAD: `c433ff769fc35329050411054324c19b5b100fdb`,
  tree `1844cded10527610fc814429bc4765cbd53d2714`.
- Ticket-owner family: `codex`; launch command:
  `codex-raw --dangerously-bypass-approvals-and-sandbox -C /code/reactivegas-issue-57 -c model_reasoning_effort=high`.
- This is a fresh ticket, context, branch, worktree, and runtime root. Do not
  resume or append to the completed #54 ticket-owner conversation/root.

You own the issue, its contract, planning record, immutable gates, commit-owner
and auditor dispatch, skeptical acceptance, and upward ticket-level reporting.
You do not implement inline. Your immediate children are commit owners and
fresh auditors; the epic owner never controls them.

## Required skill load order

Before acting, read completely and follow:

1. `orchestrator-contract`
2. `ticket-orchestrator`
3. `resolve-ticket`
4. `context-compiler`
5. `worker-protocol`
6. `tmux-orchestrator`, including `references/recursive-dispatch.md`
7. `worktrees`, `gate-script`, `lean4`, `tdd`, `verification`, and
   `invariants`

Use the durable worker protocol for every child and tmux for all authoritative
commit-owner/auditor seats. Do not use invisible subagents as substitutes.

## Objective

Drive #57 through a structurally sound, independently audited local candidate:
make the production vote fold intrinsically validation-coupled through one
total exhaustive boundary, so every inadmissible signer/event pair is a
complete vote-state no-op before it can affect membership, franchise,
questions, tallies, or verdicts.

The first bounded step is intake, baseline, compact Spec Kit amendment, and a
new frozen/falsified #57 gate. After that contract is durable, dispatch the
first implementation owner under the seat policy below and supervise through a
candidate plus fresh audit. Stop only at accepted local candidate, a durable
question/blocker, or a capacity terminal handoff.

## Governing sources and precedence

1. Operator dispatch NOTE-030:
   `/tmp/reactivegas/ms2/e43/inbox/NOTE-030-dispatch-57.md`, SHA-256
   `0a448ec31b2ec1df7680aea580c03b053431aedaeddc21487c14f9ec4e7a4320`.
2. Live issue: https://github.com/paolino/reactivegas/issues/57. Local frozen
   issue body:
   `/tmp/reactivegas/ms2/e43/artifacts/issue-slice-a-structural-validation-recut.md`,
   SHA-256
   `18dd3cfe9ae6f42a5ca1324419436893f87106e17f449034a1fc2791b21cedf9`.
3. Resurrection handoff:
   `/tmp/reactivegas/ms2/e43/t54-vote-coverage/handoffs/HANDOFF-to-57.md`,
   SHA-256
   `bb5bd5b2bf49aad2d24b3d71b17e8e16b464d0ba0674aed428fa5c826f2c4c64`.
4. Final #54 audit report and ledger:
   `auditor-slice-a-s2/handoffs/audit-report.md` SHA-256
   `835f79e6ec605871ca64b3cee2d72b55e495fb02d852b65215522eb4280fc3de`;
   `campaign-ledger.md` SHA-256
   `9667b9f048dbb02fc2a9aa09c40139d3674b340005efd113c95c0c267df33d98`.
5. Epic map:
   `/tmp/reactivegas/ms2/e43/EPIC-MAP.md`, compilation-time SHA-256
   `402fb528fdfde8f57429a0559cdc55a68a52f2a176ce4ef87209ed780bedffb2`.

Treat the issue contract and operator ruling as authoritative. The handoff and
old audit are evidence, not instructions that may weaken the issue.

## Current state

- #54 Slice A consumed both submissions and terminated
  `RE-CUT-AUTHORIZED issue=57`; there is no third submission.
- The preserved candidate is clean and local-only at `c433ff76`. #57 starts
  from that exact commit to retain the proof/gate history, but owns a new
  branch and worktree.
- Final campaign: `5 KILLED / 0 RESIDUAL / 0 BLOCKED / 1 OPEN`.
- Remaining blocking witness: a non-responsabile submits `removeMember`, drops
  franchise 3→2 and threshold 2→1, and forces an open question positive.
- `applyVoteEvent` is already validation-coupled, but `validateVoteEvent`
  unconditionally accepts `admitMember`, `removeMember`, and `setRoles`.
- #54 composition remains undispatched and blocked on independent #57
  acceptance.
- The accepted #48 emitter dependency is available but irrelevant to this
  re-cut's source fence.
- The Nix shell runs Lean 4.25.0 while `lean/lean-toolchain` says v4.27.0 and
  is inert. Name Lean 4.25.0 in every receipt; do not reconcile versions here.

## Structural contract — frozen intent

- One exhaustive production validation boundary covers the complete
  `Vote.Event` surface. No wildcard/list/boolean side registry may allow a new
  constructor to bypass validation.
- R-45 is universal over arbitrary signer, event, and pre-state. Every
  inadmissible pair preserves the complete production vote state.
- Franchise-changing events, including `admitMember`, `removeMember`, and
  `setRoles`, are instances of that universal class, never exceptions.
- Do not repair R-45 with a third per-event guard. If intrinsic coupling is
  impossible, file a Q with the concrete obstruction and park; do not silently
  fall back.
- Re-demonstrate all five inherited KILLED rows with their frozen instruments.
  They are inherited evidence, not inherited trust.
- Close `INV-54-NOEXPIRY`, or report it as an explicit advisory residual with
  the exact `preserving-member-event-excluded` limit. Silence is forbidden.
- The retained `stranger-removeMember-closes-positive` instrument must become
  a rejection/no-op oracle on the repaired model, with a seeded bypass that
  fails for the R-45 reason.
- Accepted proofs have zero `sorry`, `admit`, `sorryAx`, custom axiom,
  `native_decide`, and `Lean.ofReduceBool`; theorem axiom receipts are required.

## Owned and forbidden surfaces

Prospective implementation fence to make exact during planning:

- `lean/KelGroups/Vote/**`
- the minimum root import adjustment in `lean/KelGroups.lean` only if the
  ticket-owned gate proves it is required
- new planning artifacts under `specs/57-structural-vote-validation/**`
- untracked ignored `./gate.sh` plus runtime-root gate/evidence copies

Hard forbidden scope:

- the seven accepted Slice-1 modules directly under `lean/KelGroups/*.lean`
  must remain blob-identical to merged `ccdda830`; do not edit them
- `lean/Reactivegas/**`, #48 state machine/emitter, `backdonateAuthorized`
- #47 docs/mapping worktrees
- composition implementation or proof
- Slice-B R-66/R-67 admission-shape work beyond what is strictly required to
  state universal structural authorization
- `lean/lean-toolchain`, Nix, CI configuration, documentation, Haskell, or the
  upstream `paolino/kelgroups` repository

If planning shows the prospective source fence is insufficient, file a Q with
the exact path and reason before widening it.

You are not alone in the codebase; do not revert edits made by others.

## Gate and verification contract

Before implementation dispatch:

1. verify worktree/branch/HEAD/index cleanliness and record baseline;
2. read the issue, handoff, final audit, ledger, frozen R-45 instrument, old
   gate backup, and existing R-40…R-71 mandate in full;
3. create the compact #57 Spec Kit amendment and exact task/fence ledger;
4. freeze a new executable #57 gate before implementation;
5. prove every gate leg can fail for its named reason, including an exhaustive
   event-surface bypass control and a production trace where an inadmissible
   signer/event pair must preserve the complete state;
6. preserve Slice-1 blobs and KelGroups→Reactivegas import direction;
7. run the repository's complete cheap baseline/readiness checks before any
   evidence-budgeted gate run.

The old `gate.sh`/`gate-slice-a.sh` and 10/10 falsification are reusable seed
evidence, not the #57 gate. Re-version and re-falsify; never cite an inherited
GREEN as #57 evidence.

Every evidence receipt binds command, exact commit/tree, exit status, duration,
evidence path/hash, counts, and `Lean 4.25.0`.

## Seat and dispatch policy

- Ticket owner: this fresh Codex seat.
- First commit owner: `glm`, exactly
  `family=glm harness=pi provider=zai model=glm-5.3-flash effort=max`, launched
  with `glm --approve`. One GLM seat maximum for #57.
- If that commit-owner seat reaches a normal capacity terminal without a final
  candidate, its fresh successor is Grok, pinned to `grok-4.6`; one Grok seat
  maximum for #57.
- Every candidate gets a fresh auditor that is non-GLM and differs from the
  commit-owner family. Never reuse an auditor process/context/worktree.
- No production secrets are in scope. `draft=NONE`; no draft-tool family is
  authorized.
- Record pane, family, model/harness identity, fresh context/root/worktree, and
  post-cursor START for every child before calling it active.

## Git and remote authority

- Local planning, RED, implementation, audit, and commits are authorized.
- Do not push, open/update a PR, merge, rebase, close an issue, or edit other
  remote metadata. The issue contract requires later epic-owner authority for
  those actions.
- Preserve `feat/54-lean-vote-coverage` and its worktree read-only. Never reset,
  rewrite, or clean it.

## Journal and handback

Append your own `STATUS.md` with material milestones during long turns, not
only at turn end. At minimum record:

- `START` with pane/family/worktree/branch/base and source hashes;
- baseline and planning/gate hashes;
- every child `SLICE-START`, submission, audit result, repair, and terminal;
- any Q/RESUMED transition;
- local accepted candidate or honest terminal blocker.

Write upward reports under this runtime root `handoffs/` and notify only your
immediate parent through the durable protocol. Never ask the epic owner to
manage your commit owner or auditor.

Stop conditions: scope/authority conflict, source-hash mismatch, dirty or
unexpected index/worktree ownership, inability to build an exhaustive
structural boundary without widening scope, missing approved CLI seat, second
audit findings after the allowed repair campaign, or capacity exhaustion.
Every stop writes `BLOCKED` or `COMPLETE` with a resurrection-grade handoff.
