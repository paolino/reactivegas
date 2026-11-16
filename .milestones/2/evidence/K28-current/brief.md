# Brief — ticket owner `t28-app-api` (kelgroups #28, slice S28-1)

Role: ticket orchestrator. Worker ID: `t28-app-api`.
Parent scope: epic `paolino/kelgroups#29`, epic owner Muse pane `%532`
(`reactivegas:8` `kelgroups-e29-t28-substrate`), runtime root
`/tmp/reactivegas/ms2/e-kelgroups-substrate/`.
Your runtime root: `/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/`
(`brief.md` = this file, `STATUS.md`, `questions/`, `answers/`, `inbox/`,
`handoffs/`).
Worktree: reuse `/code/kelgroups-issue-28`, branch `feat/28-generalize-app-api`
(current `6ec0248`, clean, zero own commits, verified unowned 2026-09-05).
First action after START: `git fetch origin`, verify `origin/main ==
368b596fef0b6d393c2ac7afc631d236c55d86d1`, rebase the branch onto it,
journal the rebase (old+new SHA) in STATUS. Own that worktree exclusively;
no second ticket owner acts on #28.
Upstream issue: `paolino/kelgroups#28`. Epic: `#29`. Sibling: `#30` (NOT
yours; you plan its dependency surface only). Reactivegas handoff: `#73`.

Required skill load chain (in order): `orchestrator-contract`,
`ticket-orchestrator`, `resolve-ticket`, `context-compiler`,
`worker-protocol`, `tmux-orchestrator` (+ `references/recursive-dispatch.md`
since you will orchestrate your own pair), `verification`, `invariants`,
`gate-script`, `haskell`, `nix`, `lean4` (read-only: read the Lean spec,
never edit `lean/` semantics without epic ruling).
You are not alone in the codebase; do not revert edits made by others.

## Authority and fences

- Standing milestone override permits Muse ticket AND commit owners (same
  family). Your team: you (ticket owner, `muse --approve` =
  Pi/opencode-go/muse-spark-1.3-contributor/xhigh) + ONE distinct Muse commit
  owner (separate pane, separate runtime root, separate worktree-derived
  branch state — never inline, never hidden subagent) + ONE fresh independent
  auditor per candidate: first auditor MUST be Codex
  `codex-raw --dangerously-bypass-approvals-and-sandbox -C <worktree> -c
  model_reasoning_effort=high` with model `gpt-6-astra` (record exact live
  argv + model/effort in START; verify `ps` before admitting). Later Grok
  only within the one-grok-seat cap and an explicit full brief; auditors are
  NEVER Muse/GLM/Claude. No hidden implementation subagents, no driver draft
  grant: `draft=NONE`.
- Visible tmux workers only: commit owner + each fresh auditor in distinct
  panes of `reactivegas:8` via `tmux split-window -d`, `send-pointer`
  delivery, post-cursor `START` acknowledgement (pane + family + alternate
  check). Pane presence is not dispatch.
- Owned surface: `lib/KelGroups/{Event,State,Validate,Fold,Types,Store}.hs`
  (+ minimal `Bootstrap.hs`/`Server.hs` adaptation if the type change forces
  it), `test/` additions for the new properties, `kelgroups.cabal` test
  wiring if needed, `Trivial.hs` kept compiling as degenerate instance.
  Necessary existing client type/API adaptations to keep `just ci` green
  (`build-client`/`test-client`) are INCLUDED if the library type change
  breaks them — adapt, do not redesign. Forbidden: Reactivegas browser UI,
  wholesale `Client/Fold.purs` deletion or base-fold redesign (belongs to
  #84), Reactivegas economics in kelgroups, vendoring, repo creation,
  issue/PR comments/reviews/gists, deployment, publication,
  release-please merge. `Trivial` stays working but is NOT evidence of
  nondegenerate capability.
- Commit authority: local SIGNED implementation commits (journal every commit
  incl. rebase/amend with SHA+subject); factual issue-body planning updates
  and draft PRs ONLY after full local `just ci` GREEN on the exact head.
  NO merge: exact-SHA merge authority remains at milestone desk `%510`; you
  execute `guard-merge` only after written authorization. No gate bypass, no
  blanket test exclusion to make integration green.
- Budgets (binding, per initial #28 slice): ≤2 submissions; YOU ≤8
  substantive full build/gate attempts total + ≤24 explicitly counted targeted
  probes; one fresh FULL independent audit per candidate, ≤5 substantive audit
  builds + ≤20 targeted probes per audit. Journal every failed setup/attempt
  with command + exit + cause. No automatic raises — return a concrete
  workload/cost gap to the epic owner before exceeding. No parallel heavy
  builds inside the ticket.

## Source of truth (frozen; reconcile BEFORE freezing your contract)

Accepted spec = Reactivegas accepted Lean @ `4a6cd87` + later operator
rulings. Current kelgroups #28/#30/#29 bodies are STALE INPUTS:

1. `LEDGER.md` (2026-09-05T10:50Z) + `PLAN-TO-MILESTONE-2.md` + epic
   `EPIC-MAP.md` (`/tmp/reactivegas/ms2/e-kelgroups-substrate/EPIC-MAP.md`) —
   read all three first.
2. `e-haskell-impl/handoffs/D1-ASSESSMENT.md` (0/26 gap is historical measure,
   not a whitelist), `EPIC43-REQUIREMENT-MAP.md` (R9c/R11 corrections),
   `ASSENSO-ORACLE-GAP.md` (composition wire missing; C1–C5 belong to
   reactivegas#76 — do NOT build them here).
3. `lean/KelGroups/{Types,State,Validate,Fold,Integration}.lean` +
   `Vote/{Types,State,Event,Validate,Fold}.lean` (read exact modules; discover
   the complete relevant extent via imports/consumers, not an allowlist).
4. `questions/A-V2-AND-PLEDGE-AGENCY.md` + `t68-proposer-assent/answers/A-001*`:
   V-2 zero-open/proposer-selfbar is SETTLED but UNLANDED — freeze against
   current accepted base, plan explicit rebind after #68 lands, never
   anticipate candidate semantics.
5. `#75/#76/#81` + `REPLAY-CONTEXT-CONTRACT.md`: threshold is a parameter
   (exhibits are not defaults); replay context is test input, not a shipped
   default.
6. Dated corrections (2026-09-05, binding): #28's `appOnBase` total +
   "base never rolls back" sketch is STALE — implement the sealed atomic
   `BaseHook` (`Except`, `commitBaseChange` discards whole transition on hook
   refusal) + rejecting integrated route. #30's single-admin immediate
   enactment / generic proposals / rejection-expiry is STALE — reconcile
   against direct/base/app routes, `QuestionKind`, explicit `ClosureRecord`,
   no-expiry/retention. #29's "legacy laws authoritative" is STALE — Lean +
   later rulings win. Do NOT smuggle refusals from dormant `notDesignee` /
   `notProposer` (zero construction sites) and do NOT assume a theta default.

If an authority conflict is unresolved by existing rulings: BLOCK with
`questions/Q-NNN-<slug>.md` + exact competing evidence. Do not reopen settled
rulings. Contract work + #28 implementation proceed WITHOUT waiting for
reactivegas #66 repairs or #71 prose.

## Objective (S28-1, the one observable outcome of this run)

A real nondegenerate application (a test-only demo instance inside `test/`,
NOT a new shipped executable) has distinct state/event types, sees signer +
read-only sole membership view, gets its domain refusal enforced BEFORE
durable append, and observes base changes through the sealed atomic hook —
proven by the frozen gate below on the exact candidate commit.

## Constraints

- One writable membership store; admission ONLY via direct command (no votable
  admission representable in types). `pendingBase` typed by a non-admitting
  mutation; historical `pendingProposals`/`introduceMember` kept but receives
  NO production responsibility on the new path (name the boundary explicitly).
- `IntegratedAppFold`: signer → pre/post `GroupView` → AppState → AppEvent →
  `Except AppError AppState`. `BaseHook`: change → pre/post views → state →
  `Except`. `IntegratedEvent` keeps BaseProposal/AppEvent distinct params.
  `IntegratedResult` carries `Option BaseChange` evidence. `GroupView` is the
  SOLE membership projection (no second list, no app-side mutation).
- Refused events leave modeled state AND the durable log unchanged. Any
  claimed rollback is tested AFTER a tentative base change + a failing hook.
- `validate`/`fold` share the single step so accept/replay can never disagree;
  replay of an accepted KEL never rejects.
- No hardcoded discovery quotas. No `admit`-style escape hatches. Full
  `just ci` (build, tests, Lean `lake build`, client build+test, format,
  cabal-fmt, lint) green on the exact head; verify the ACTUAL committed
  automation wiring (the command CI runs), not a local alias.
- Second coherent slice S28-2 (durable-boundary hardening) is planned, max 2
  slices total, justified by independently reviewable behavior — never to avoid
  full acceptance. S28-1 must already exercise the real append/replay boundary
  (`Store`/KEL), not only a pure helper.

## Contract rows → witnesses + can-fail mutants (derive BEFORE implementing)

For EACH row, freeze one reachable positive witness, one refusal witness, one
atomicity/replay witness, and one specific mutant you will show going RED:

| row | positive | refusal | atomicity/replay | mutant (must go RED) |
|---|---|---|---|---|
| distinct types + signer + GroupView | nondegenerate demo app compiles with `e ≠ s`, authorizes by signer via view | non-member app event refused `NotAMember` | replay of accepted KEL reproduces identical app state | conflate `e=s` — demo app fails to compile |
| rejecting step before append | `Store` append path calls validate-then-append; accepted event durable | domain-invalid app event (`Left`) never appended (log byte-identical) | refused-event log + folded state both unchanged after retry + replay | bypass validate in append path — refusal test goes RED |
| atomic hook | base change + hook success commits state + `Some change` evidence | failing hook rejects whole transition (state discarded, `Err`) | tentative base change + failing hook → pre-state AND pre-log restored | ignore hook `Err` and keep post-base — atomicity test goes RED |
| direct-only admission | direct admit by admin inserts member | votable admission unrepresentable (type-level; attempted encoding fails) | pending store typed non-admitting; historical path marked non-production | re-add admission constructor to voted mutation — direct-only proof/test goes RED |
| validate/fold agreement | accepted event folds to same state via both paths | — | property: accept→replay never disagrees (QuickCheck over generated traces) | fork the step copies — agreement property goes RED |
| no client-decided authority | demo verdicts come only from the integrated boundary | out-of-band app-state write refused/ignored | — | second decision path — authority test goes RED |

Freeze this table (with exact names/files) as your ticket contract before the
commit owner writes behavior-changing code. Changes from later #68/#76/#81
landings require explicit rebind + revalidation.

## Frozen gate G28-1 (immutable for this slice; hash-bound header)

`gate.sh` (untracked, ignored, executable contract — see `gate-script` skill)
MUST run, in order, on the exact candidate head, with `set +e` capture so
failing commands stay diagnosable:

1. `git status --porcelain` empty of tracked paths (before AND after).
2. `git rev-parse HEAD` recorded; gate header hash equals the executable bytes.
3. `cabal build all -O0` (or `just build`) — exit 0.
4. `cabal test all -O0 --test-show-details=direct` (or `just test`) — exit 0,
   with the six new property groups above all present and passing (quantify
   over the discovered extent — `grep -c` the property names from the test
   tree, never a hardcoded count).
5. Each frozen mutant above applied one at a time → the corresponding property
   goes RED (non-zero exit naming the property); mutant reverted afterwards.
   Record mutant diff hash + failing output.
6. `just ci` FULL (format-check, cabal-fmt check, hlint, build, test, `lake
   build`, client build, client test) — exit 0 on the exact head.
7. `Trivial` instance still compiles and its existing tests pass (degenerate
   presence only — explicitly NOT counted as a nondegenerate witness).

RED bundle (commit owner, before GREEN): failing gate output on the pre-change
base proving the new properties fail for absence (not for typos), with the
frozen gate hash. GREEN: same gate exit 0 on the candidate + `PUSHED` nothing
(no push without epic authorization; draft PR only after GREEN + audit).

## Task (bounded, in order)

1. `START` in `STATUS.md` (brief hash, pane, argv, family) — you are not
   started until then.
2. Rebase worktree as directed; journal SHAs. Verify toolchain (`just --list`,
   `cabal --version`, `lake --version`, `node --version` for client).
3. Freeze your ticket contract (witness/mutant table + G28-1 `gate.sh`) and
   record `NOTE GATE-FROZEN gate=<sha256> head=<sha>`.
4. Dispatch your distinct Muse commit owner (visible pane, full brief, RED
   first), supervise immediate child only, verify every material claim against
   artifacts (read files, not events; low-discipline briefs specify
   mechanism).
5. On `PROOF-COMPLETE`, freeze the candidate, spawn the fresh Codex
   `gpt-6-astra/high` auditor (new pane, new root, full brief, read-only),
   admit verdict only after its pane-bound `START`; on findings, one repair
   cycle then a FRESH auditor for the new SHA. Max 2 submissions.
6. Hand back: candidate SHA, gate receipts (RED+GREEN), audit report path,
   issue-body correction text (dated, against accepted rulings), draft-PR
   readiness, S30 dependency notes, residual risks. Then `COMPLETE`.

## Output contract

`COMPLETE <sha> ready-for-review` + `handoffs/S28-1-CANDIDATE.md` containing:
exact candidate SHA + branch, RED/GREEN gate logs (paths + hashes), witness /
mutant table with file:line pointers, audit verdict (hash-bound, independent),
`just ci` full log hash, issue-body correction proposal (dated), S30
dependency enumeration, and explicit residual risks. No merge performed.

## Questions / inbox / escalation

- Blocked → `questions/Q-NNN-<slug>.md` + `BLOCKED Q-NNN` and park; you own
  prompt delivery of my answers (`answers/A-NNN`) and require your children's
  `RESUMED`. I own answers and delivery; desk `%510` owns scope/cost/
  destructive/product-intent escalations.
- My corrections arrive in `inbox/NOTE-NNN-*.md` (+ `send-pointer` wake);
  acknowledge `NOTE NOTE-NNN read` before relying on new instructions.
- Check unread inbox before every new phase, before every expensive command,
  before freezing evidence, before `COMPLETE`.
- Supervision: foreground wait on your immediate children's STATUS (their
  journals are the liveness signal; `capture-pane` ≤15 lines/min for liveness
  only). Prove a known-stale alarm reaches YOU (name the receiving mechanism
  + demonstrate receipt); monitor only active roots.
- Upward reporting: LOCAL FILES ONLY (`handoffs/` + `/tmp/reactivegas/ms2/
  inbox/` pointer + STATUS journal). NEVER type/paste/send-pointer into desk
  `%510` or any human chat composer (`UPWARD-REPORTING-LOCAL-ONLY.md`,
  binding). Propagate to your children.
- Stop conditions: two parents on one child; grandchild managed directly;
  orchestrator writing implementation; scope change without durable ruling;
  relayed claim without artifact verification; worker called running without
  `START`; lane reused without fresh root/context; `glm` outside commit-owner
  role. Every stop is terminal or `PARKED: <reason>; wake=<condition>` or
  `BLOCKED Q-...`.

Brief sha256 (this file): recorded by owner at dispatch; you quote it in START.
