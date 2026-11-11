# S2 SHADOW + OT RESOLVED COMMANDS — concrete artifact for binding (NOTE-020)

**No run executed to produce this file.** Every command below is resolved and
literal: cwd, full argv, input/output paths, search paths as set, clean-input
identities with hashes, shadow construction and order. Owner reads, hashes and
binds this file; execution follows binding. Spend at writing: nothing.

## 0. Fixed identities

- Worktree `/code/reactivegas-66-s4b`, branch `chore/66-s4b-mirrors`, base
  `3590c001`, repair commits through `b667648`, tree clean (verified at hand).
- Runtime root `/tmp/reactivegas/ms2/e-lean-compliance/commit-owner-s4b-muse/`,
  instruments under `instruments/`, receipts under `handoffs/evidence/`.
- `NIXDEV` = `nix develop --quiet -c`; `LEANLIB` =
  `/code/reactivegas-66-s4b/lean/.lake/build/lib/lean` (O1-fresh full build
  oleans; manifest `instruments/S2-clean-olean-manifest.sha256`, 29 files).
- `P01WORLD` = `/tmp/s2shadow-P01` (holds ONLY the mutant `Types.olean`;
  nothing else is ever written there). `P07WORLD` = `/tmp/s2shadow-P07` (holds
  ONLY the mutant `Step.olean`). Separate clean owned worlds: P07neg can never
  resolve retained P01-mutant bytes because they live in a different directory
  that is not on its search path. Each world is built by `rm -rf <WORLD> &&
  mkdir -p <WORLD>/…` INSIDE its compile argv (stale exclusion by
  construction, visible in the receipt) — `mkdir -p` alone is never cited.
- Direct-`lean` deviation rationale (retained receipt
  `instruments/S2-lean-env-search-order.receipt.txt`): `lake env` places
  project paths BEFORE inherited `LEAN_PATH` (observed order: project,
  toolchain, marker-last), so a shadow entry can never win under `lake env`.
  Deterministic shadow-first loading requires explicit `LEAN_PATH`; each
  neg/pos outcome authenticates which olean loaded (neg must fail at its
  targets; pos must go green). `lake env lean` stays for OT3/OT4 (no shadow).

## 1. Shadow construction (free file ops inside the counted compiles)

- Separate worlds `P01WORLD=/tmp/s2shadow-P01` and `P07WORLD=/tmp/s2shadow-P07`.
  Each world is built by `rm -rf <WORLD> && mkdir -p <WORLD>/…` INSIDE its
  compile argv (stale exclusion by construction, visible in receipts); P01
  world fully retires (restored, verified) before the P07 world is created —
  the two worlds never coexist, so P07neg cannot resolve retained P01-mutant
  bytes.
- Mutant application: `git apply <ABS-DIFF>` with cwd worktree root (tracked
  files only); restore after each compile: `git checkout -- <files>` +
  `git status --short` verify (empty). No mutant persists past its compile.
- Key clean-input olean hashes (from the manifest; full list in the file):
  `KelGroups/Types.olean 3fd0e27e…`, `Reactivegas/State.olean e2dd1fb6…`,
  `Reactivegas/Step.olean c3a0e0ef…`, `KelGroups/Integration.olean 1e0d83cd…`,
  `KelGroups/Vote/Fold.olean a233b9e1…`, `KelGroups/Vote/Types.olean 37478bce…`.
- `KelGroups/Types.lean` has ZERO project imports (verified): its compile
  resolves nothing but the toolchain sysroot.

## 2. Targeted operations (9 total: 8 sheeted + 1 granted OT4-retry; 44 spent, 7 remaining, 51/60 exact fit, gap NONE)

### OT3 — clean close witnesses
- cwd: `/code/reactivegas-66-s4b/lean`.
- argv: `nix develop --quiet -c lake env lean /tmp/reactivegas/ms2/e-lean-compliance/commit-owner-s4b-muse/instruments/S2-witness-close.lean`
- inputs: driver (`531eb3e9…`), dep oleans (current `.lake`, Step/State/Types
  sources unchanged — genuine per byte-diff).
- search paths: `lake env` defaults (no shadow). Expects exit 0.
- receipt: `handoffs/evidence/S2-OT3.log`.

### OT4retry — compiled-identity census (retry; original S2-OT4.log retained as the counted failure)
- cwd, argv shape: as OT3 with `instruments/S2-census.lean` (`f4d4b64a…` current, incl. the sortUndecided→fail repair; prior hashes superseded by content change, each delta journalled).
- inputs: driver + full lib env (kind census unaffected by visibility
  promotion — same kinds/counts; documented). Expects exit 0 with
  `S2-CENSUS-OK` (identity sets printed; no frozen counts).
- receipt: `handoffs/evidence/S2-OT4retry.log` (the counted `S2-OT4.log` failure is never overwritten).

### SH-P01compile — mutant Types olean (world P01WORLD)
- cwd: `/code/reactivegas-66-s4b`. setup (free): `git apply
  /tmp/reactivegas/ms2/e-lean-compliance/commit-owner-s4b-muse/instruments/S2-mut-isMember-false.diff`
  (`85e250ac…`; single hunk, constant-false body).
- argv: `nix develop --quiet -c bash -c 'cd /code/reactivegas-66-s4b/lean && rm -rf /tmp/s2shadow-P01 && mkdir -p /tmp/s2shadow-P01/KelGroups && LEAN_PATH=/code/reactivegas-66-s4b/lean/.lake/build/lib/lean lean -DautoImplicit=false -o /tmp/s2shadow-P01/KelGroups/Types.olean KelGroups/Types.lean'`
- inputs: mutant `KelGroups/Types.lean` (worktree tempfile state; restorable);
  dep oleans: NONE (zero project imports — `LEAN_PATH` set but unconsulted).
- outputs: `/tmp/s2shadow-P01/KelGroups/Types.olean` (sha recorded in receipt).
- restore (free, same operation): `git checkout -- lean/KelGroups/Types.lean`
  + verify clean. Charge: 1 targeted (single-file compile, no dependency
  rebuild, no whole-project/test execution — T6/T7 precedent).
- receipt: `handoffs/evidence/S2-SH-P01compile.log` (exit 0 + output sha).

### SH-P01neg — helpers fail against real mutant Types
- cwd: `/code/reactivegas-66-s4b/lean`.
- argv: `nix develop --quiet -c bash -c 'cd /code/reactivegas-66-s4b/lean && LEAN_PATH=/tmp/s2shadow-P01 lean -DautoImplicit=false /tmp/reactivegas/ms2/e-lean-compliance/commit-owner-s4b-muse/instruments/S2-chain-P01.lean'`
- inputs: driver (`b64a4cabaa8630b99761a191ad11884fffcb4029bc961cdd3b2cc91fd59dbcba`), shadow Types.olean (compile receipt),
  toolchain sysroot (implicit). No Step import anywhere: the comune assertion
  can neither fire nor mask.
- search paths: EXACTLY `/tmp/s2shadow-P01` (nothing else set; sysroot implicit).
- expects exit≠0 with errors EXACTLY at `view_mem_of_isMember` +
  `isMember_of_view_mem` (P01-orig proves alongside as contrast).
- receipt: `handoffs/evidence/S2-SH-P01neg.log`.

### SH-P01pos — same bytes, clean body
- argv: as neg with `LEAN_PATH=/code/reactivegas-66-s4b/lean/.lake/build/lib/lean`.
- inputs: same driver, clean Types.olean (manifest `3fd0e27e…`). Expects exit 0.
- receipt: `handoffs/evidence/S2-SH-P01pos.log`.

### SH-P07compile — mutant Step olean (world P07WORLD)
- cwd: `/code/reactivegas-66-s4b`. setup (free): `git apply
  /tmp/reactivegas/ms2/e-lean-compliance/commit-owner-s4b-muse/instruments/S2-mut-close-perm.diff`
  (`95edbfa3…`; permission atom + print-only eval witness).
- argv: `nix develop --quiet -c bash -c 'cd /code/reactivegas-66-s4b/lean && rm -rf /tmp/s2shadow-P07 && mkdir -p /tmp/s2shadow-P07/Reactivegas && LEAN_PATH=/code/reactivegas-66-s4b/lean/.lake/build/lib/lean lean -DautoImplicit=false -o /tmp/s2shadow-P07/Reactivegas/Step.olean Reactivegas/Step.lean'`
- inputs: mutant `Reactivegas/Step.lean`; dep oleans per manifest (Types,
  State, Integration, Vote.* — all clean, unchanged). outputs: shadow
  Step.olean (sha in receipt). restore + verify (free).
- receipt: `handoffs/evidence/S2-SH-P07compile.log` (exit 0 + output sha).

### SH-P07neg — chain fails against real mutant Step
- cwd: `/code/reactivegas-66-s4b/lean`.
- argv: `nix develop --quiet -c bash -c 'cd /code/reactivegas-66-s4b/lean && LEAN_PATH=/tmp/s2shadow-P07:/code/reactivegas-66-s4b/lean/.lake/build/lib/lean lean -DautoImplicit=false /tmp/reactivegas/ms2/e-lean-compliance/commit-owner-s4b-muse/instruments/S2-chain-P07.lean'`
- inputs: driver (`075f6f22e9c920615068e452058dc4df69c4cb73db3cfab9aa15e64251559253`), shadow Step.olean + clean dep oleans
  (consistent: shadow compiled against exactly these). No TraceTests import:
  trace decide-flips can neither fire nor mask (those are O4 evidence).
- search paths: EXACTLY `/tmp/s2shadow-P07:<lib>` in that order. P01-mutant
  bytes live in `/tmp/s2shadow-P01`, which is nowhere on this path.
- expects exit≠0 with the failure AT `step_close_inv` (`close_guard_inv`
  proves; `close_permission_to_close` elaborates only via the broken link;
  P07-orig proves).
- receipt: `handoffs/evidence/S2-SH-P07neg.log`.

### SH-P07pos — same bytes, clean Step
- argv: as neg with `LEAN_PATH=/code/reactivegas-66-s4b/lean/.lake/build/lib/lean`.
- Expects exit 0. receipt: `handoffs/evidence/S2-SH-P07pos.log`.

## 3. Order (fixed, single executable order)

OT4retry → SH-P01compile → SH-P01neg → SH-P01pos → SH-P07compile →
SH-P07neg → SH-P07pos → (O-phase: O1-retry, O2, O3, O4, O5, noop, O6).
Targeted batch needs no O-prerequisite (current `.lake` valid for all its
inputs — verified manifest; each temp mutant restores byte-identical before
the next step). P01 world fully retires (restored, verified) before P07
world is created; the two worlds never coexist. (OT3 done.)

## 4. Fit (one remaining-cost table)

| row | spent | remaining need | allowance | gap |
|---|---|---|---|---|
| substantive | 9/16 (8 hist + O1-failed, preserved) | O1retry,O2,O3,O4,O5,noop,O6 = 7 | 16-9 = 7 | NONE (exact fit) |
| targeted | 44/60 (42 + OT3-green + OT4-failed, preserved) | OT4retry + SH×6 = 7 ops | 51-44 = 7 (+1 granted NOTE-024) | NONE (exact fit) |

No compression (every op distinct and required), no reclassification
(compiles stay proposed-targeted by single-file scope + retained T6/T7
precedent; fallback 16+/14 not invoked), no silent spend. Gap status: NONE open —
the +1 targeted (OT4-retry side) was granted NOTE-024 (51 allocation; 44 spent,
7 remaining exact fit). Syntax-repair commit `b667648` + clean-olean manifest
(`S2-clean-olean-manifest.sha256`, 29 files, O1-built) bound as gate inputs;
O1's module builds retained at proven scope (never whole-O1 success).
