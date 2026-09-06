# Brief — ticket owner `t30-contract` (kelgroups #30, CONTRACT PREPARATION ONLY)

Role: ticket orchestrator (planning altitude). Worker ID: `t30-contract`.
Parent scope: epic `paolino/kelgroups#29`, epic owner Muse pane `%532`
(reactivegas:12 `kelgroups`), runtime root
`/tmp/reactivegas/ms2/e-kelgroups-substrate/`.
Your runtime root: `/tmp/reactivegas/ms2/e-kelgroups-substrate/t30-contract/`.
Worktree for READS: `/code/kelgroups` (main @ `933e385d`, clean) and
`/code/reactivegas` Lean tree (read-only). You make NO writes outside your
own runtime root. Upstream issue: `paolino/kelgroups#30` (OPEN). Siblings:
#28 LANDED+CLOSED, #33/#34 filed-blocked, #29 epic OPEN.

Required skill load chain: `orchestrator-contract`, `ticket-orchestrator`,
`resolve-ticket` (planning phases only — NO implementation/dispatch
chapters), `context-compiler`, `worker-protocol`, `tmux-orchestrator`,
`verification`, `invariants`, `gate-script`, `haskell`, `nix`, `lean4`
(read-only: read the Lean spec, never edit it).
You are not alone in the codebase; do not revert edits made by others.

## Authority (preparation-only fence — binding)

- Seat: `muse --approve` (Pi/opencode-go/muse-spark-1.3-contributor/xhigh).
  Standing fence persists: commit owners Muse, auditors Codex-or-Grok (never
  Muse/GLM/Claude) — but you commission NEITHER; `draft=NONE`.
- ALLOWED: read-only source/tool inspection (reads, greps, `git log/diff/
  rev-parse`, `gh issue view`); writing YOUR OWN planning/gate artifacts
  under your runtime root; freezing operational classifications (named below)
  once the actual command map exists.
- FORBIDDEN: Lean/Haskell/client COMPILATION, tests, mutations, gate runs
  of any kind; pair/worker dispatch of any kind; product edits (any repo);
  commits, push/PR/merge/release, issue comments; budget spend of any kind.
  Initial execution spend is 0 and stays 0. Figures 18/24 + 12/24 are
  ESTIMATES, not allowances. Normal read-only contract work needs no further
  permission. If actual measurement becomes necessary: return ONE bounded
  command request (exact command + classification + cost) — never another
  guessed fit table, never silent execution.
- Base: kelgroups main `933e385d`; Lean `3590c001` (zero-diff verified in
  Vote/Integration/State/Validate vs `4a6cd87` — re-verify at your freeze).
  Do NOT resume the terminal #28 ticket root or any author/auditor context.

## Inputs (read all before writing; newest governs on conflict)

- `handoffs/T30-MANDATE-v3.md` + `handoffs/T30-REQUIREMENT-MAP-v3.md`
  (epic v3 planning; v1/v2 retained as history).
- Current bodies: kelgroups #30 (dated 2026-09-06 correction operative),
  #29 (dated Lean correction operative), #33, #34.
- Accepted Lean: `lean/KelGroups/Vote/{Types,State,Event,Validate,Fold}.lean`
  + `lean/KelGroups/{Integration,State,Validate}.lean` @ `3590c001`;
  V-2 ruling (settled, paolino/reactivegas#68 OPEN — rebind boundary, never
  anticipation); paolino/reactivegas#81 body (proposer scope + explicit
  unruled exclusions); R3.1 replay contract (threshold = test input).
- LANDED S28 interface on main (exact SHAs in EPIC-MAP): converge to it,
  never redesign it.

## Terms (non-negotiable, from the commissioning note)

- Leave current accepted behaviours INTACT in your contract. V-2 rebind
  after #68 is a SEPARATE dependency on the actual approval path (name its
  concrete boundary; no blanket-block language).
- Unruled `notProposer`/`notDesignee` producing semantics stay UNSCHEDULED
  (preserved boundary, no promise, no dependency edge).
- Later #76/#81 lifecycle/economic content: NEITHER silently implemented
  ahead of Lean NOR marked permanently out of the eventual substrate
  contract (state both explicitly per row).
- Gate properties need can-fail controls at ACTUAL integrated/client/
  persistence boundaries — never source-token searches or shrinkable
  fixture inventories. Absent-API import failure is NOT behavioural
  evidence (name the narrower interface-existence claim instead).
- Operational classifications + full allowance proposal freeze ONLY after
  the actual command map exists (named rows → concrete commands/controls
  with classes + sharing rationale + cold/final validation + audit
  obligations + discovery bounds or a measurement request).

## Deliverable (output contract)

`t30-contract/handoffs/T30-CONTRACT-r1.md` + frozen requirement-to-command/
control map + candidate-independent initial gate design (fence, legs,
kill-attribution rules, evidence bindings, spend classes) + realistic
proposed ceilings for implementation authorization — with the FULL inherited
#30 client/integration/replay/closure scope preserved. Then `COMPLETE`
(planning handoff) or `BLOCKED` with a concrete gap. No outline-only
checkpoint will be accepted as completion.

## Questions / inbox / escalation / reporting

- Blocked → `questions/Q-NNN-<slug>.md` + `BLOCKED Q-NNN`, park for my
  answer. My corrections → your `inbox/` + wake, ack `NOTE NOTE-NNN read`.
- Check unread inbox before every phase and before `COMPLETE`.
- Upward reporting: LOCAL FILES ONLY (`handoffs/` + `/tmp/reactivegas/ms2/
  inbox/` pointer + STATUS journal). NEVER type/paste into desk `%510` or
  any human composer. Stops are terminal or `PARKED: <reason>;
  wake=<condition>` or `BLOCKED Q-...`.

Brief sha256 (this file): recorded by owner at dispatch; quote it in START.
