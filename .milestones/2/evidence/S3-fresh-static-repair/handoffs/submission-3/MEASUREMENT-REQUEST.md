# FROZEN MEASUREMENT REQUEST r3 (submission 3) — reviewable form, no execution under this mandate
To commissioner %503. Supersedes the v1 request (7-vs-8 chains, 18-vs-12 arithmetic, deferred
diffs/argv/target all repaired here). Dependent costing stays STOPPED. No Lean/build/probe is run
by the repair owner under current authority. No allowance is granted by this file.

## 1. Counts that contain their own instructions
- Unmeasured chains: 8 (C-VOTEFOLD, C-VOTEVAL, C-VOTESTATE, C-VALIDATE, C-INTEGRATION, C-FOLD,
  C-KSTATE, C-RSTATE). C-STEP already anchored by R-BUILD2.
- Timed invocations: 8 mutant builds + 8 restore reruns + 1 cold build + 1 isolated check
  elaboration = 18. Proposed ceiling: 18 timed invocations. This EXCEEDS Phase-1's 3-build ceiling
  and is stated as excess requiring explicit numeric grant; it is not trimmed to fit.
- No representative sampling: 8 chains are 8 distinct dependency-closure classes; measuring fewer
  would drop a required class, not establish it. Classes are preserved, never called equivalent.

## 2. Prerequisite (exact, verified before each cycle)
Scratch checkout `<scratch>` of 3590c0015b84fd58004bf6fb44dd18b107304c48, detached; before AND after
each cycle: `git -C <scratch> rev-parse HEAD` prints the SHA above and `git -C <scratch> status
--porcelain` prints empty. Candidate worktree /code/reactivegas-66-s3-repair never touched.

## 3. Per-chain cycles (exact argv, cwd, diff, observable)
Common argv: `lake build`, cwd `<scratch>/lean`. First receipt line of the campaign: `lake --version`
prints a 4.25.0 toolchain (pins to the R-BUILD2 toolchain class; retained lean path
/nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean). Kinds reported separately per
invocation (incremental production rebuild / proof-check elaboration RED-or-GREEN / restore replay),
never averaged. Restore after each cycle: `git -C <scratch> checkout -- .`, rerun `lake build` to
retained GREEN (R-BUILD3 pattern), porcelain empty.
- C-VOTEFOLD: diff Vote/Fold.lean:76 filter predicate → `fun _ => True`. Observable: sweep-lemma
  REDs with failing-obligation lines (cf. open_mem iff, idempotent equation).
- C-VOTEVAL: diff Vote/Validate.lean:58 `if isResponsabile signer view then .ok () else .error
  VoteError.notResponsabile` → `.ok ()`. Observable: nonresponsabile/openQuestion hval REDs.
- C-VOTESTATE: diff Vote/State.lean:89 `≥` → `>` (assents-length comparison). Observable: verdict
  flip REDs in sweep/tally obligations.
- C-VALIDATE: diff Validate.lean:145 `if isAdmin signer gs then` → `if True then`. Observable:
  admission-ok/inversion REDs with hadmin shape mismatch.
- C-INTEGRATION: diff Integration.lean:210 `change := none` → `change := some
  (BaseChange.memberAdmitted "measure")`. Observable: app-route projection REDs.
- C-FOLD: diff Fold.lean:37 `≥` → `>`. Observable: threshold-equation/split-shape REDs.
- C-KSTATE: diff State.lean:50 `(adminCount gs + 1) / 2` → `adminCount gs + 1`. Observable: majority
  decide REDs.
- C-RSTATE: diff State.lean (Reactivegas) refundAll body `l.foldl (fun acc p => bump acc p.user
  p.amount) m` → `m`. Observable: deny/fail fund-equation REDs.
- U-COLD: argv `lake build` after full clean of <scratch>/lean/.lake build outputs; retained full log
  with per-module times (cures the missing cold log, F-07).
- U-CHECK (named target): scratch file <scratch>/check-eval.lean containing
  `import Reactivegas.Invariants` + `#eval checkSweepIdempotent`; argv `time lake env lean
  <scratch>/check-eval.lean`, cwd `<scratch>/lean`; retained wall time (isolated decide elaboration,
  cures the mixed 2s command, F-07).

## 4. History preserved
R-BUILD2 (exact 1-line diff + RED at Invariants.lean:407), R-BUILD3, R-CORPUS, R-TRACE retained as
anchors; this request extends, never rewrites, them. The 4-against-3 overrun stays on the record.
The F-07 historical isolation gap (LEAN_PATH named the candidate worktree) stands retrospectively
and is cured prospectively by the scratch checkout above.
