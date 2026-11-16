# SS0-RETURN-v2 — full sequence executed once (bounded retry of v1 setup failure)

v1 packet (`evidence/run/`, `handoffs/SS0-RETURN.md`) preserved untouched.
v2 runner repairs ONLY the cwd/flake-resolution transport defect; subject, atom,
targets and expectations unchanged. Terminal: runner exit 0, wall 42 s
(nominal budgets 360+140+140+55, worst case 755 < 900 ceiling). No S3
conclusions drawn; read against raw exits and loaded artifacts.

## Per-operation record

| op | named target | exit | duration | outcome class |
|---|---|---|---|---|
| OP1 cold baseline (substantive) | `lake build Reactivegas.Invariants` via `instruments-v2/cold.sh` | 0 | 15980 ms | setup-baseline GREEN — `Build completed successfully (19 jobs)`, 19 oleans in `op1-oleans.txt`; not a theorem result |
| OP2 single-atom mutant (substantive) | same named target, atom applied (`op2-applied.diff` sha `8c0b9e5d…`, 1 dirty path) | 1 | 19819 ms | **semantic** — statement failure inside `step_grant_inv` (see evidence); nonzero exit alone is NOT the basis, the diagnostic is |
| OP3 restore + matching build (substantive) | same named target, restored (`op3-restored-status` empty = clean) | 0 | 3125 ms | setup-restoration GREEN — `Build completed successfully (19 jobs)` |
| OP4 U-CHECK (targeted) | `lake env lean Check.lean` via `instruments-v2/check.sh` | 0 (OP4-ACTUAL) | 2476 ms | setup-elaboration GREEN — silent stdout/stderr beyond ACTUAL-CWD lines = clean elaboration of `ss0_check_elaboration : Reactivegas.checkSweepIdempotent = true := by decide`; not `#eval`, not a runtime replay |

CWD transport (the v1 defect) verified repaired: every op log opens with
`ACTUAL-CWD-AT-COMMAND-BOUNDARY: /code/reactivegas-66-s3-ss0-scratch` and
`ACTUAL-CWD-BEFORE-LAKE(-BEFORE-LEAN): …/lean` from `pwd`, not echoed strings.

## OP2 evidence (the only RED; expected observed result, not a stop)

1. **Mutated `Reactivegas.Step` COMPILED** — `op2.stdout:118`
   `ℹ [17/19] Built Reactivegas.Step (1.3s)`, followed by Step axiom infos
   (`:473–476`). The run measured a semantic kill, not a compile error.
2. **Single error diagnostic, inside the named obligation** — the ONLY
   `error:`-severity diagnostic in either OP2 log (`op2.stdout:126`):
   `error: Reactivegas/Invariants.lean:209:4: Type mismatch`, where
   `Eq.symm hx` has the mutated post-state
   (`collections := col :: rest`) but is expected to have the statement's
   concluded type with `permitted := true`. `op2.stderr` adds only the
   expected dirty-tree warning plus consequential `error: build failed`.
   This is a **statement** failure: the concluded equation is false under the
   atom, so the proof term no longer typechecks — not tactic brittleness.
3. **Reach past the unchanged obligation** — zero diagnostics in the
   `step_deny_inv` span (`:211–229`); `info:` diagnostics at `:1639–1641` and
   `:2351–2353` appear AFTER the error in the log, demonstrating elaboration
   continued past `:211` to end of file. No source-order-only claim is made;
   this is established by the log ordering above.

## Exact limits (returned, not smoothed over)

- **L1 — `:209:4`, not literally `:197`.** The theorem statement
  `theorem step_grant_inv` begins at `:197`; the failing proof line
  `exact hx.symm` is at `:209:4`. The runner's `op2-classification.txt`
  `:19[0-9]` grep MISSED the error (it caught only an unrelated
  `KelGroups/Vote/Invariants.lean:190` warning); the `:209:4` diagnostic was
  established by direct log inspection above. Obligation identity
  (`step_grant_inv`) holds by source span `:197–209`, confirmed against the
  restored source — but anyone re-grepping for a literal `:197` error will not
  find one.
- **L2 — reach is absence-of-error plus continuation, not a positive marker.**
  Neither `step_deny_inv` nor `step_grant_inv` is NAMED anywhere in the OP2
  logs, and no affirmative "`step_deny_inv` proved" line exists. Reach of
  `:211` is established by (a) exactly one error diagnostic in the whole log
  (at `:209:4`) + (b) post-`:211` elaboration evidence (`:1639+`, `:2351+`
  infos). Proof-of-deny by absence-of-error is the limit of what the log shows.
- **L3 — `check.sh` elaborates the v1 path.** `instruments-v2/check.sh`
  invokes `instruments/Check.lean`, not `instruments-v2/Check.lean`. Both files
  are byte-identical (sha `f84ad938…`, verified pre-run), so the U-CHECK
  subject is unchanged — recorded, not hidden.

## Preservation / budget

- Scratch: HEAD `3590c0015b84fd58004bf6fb44dd18b107304c48`, porcelain clean
  (`final-status` = HEAD line only); restored by the runner, no verification
  build added by the worker (OP3's build is the runner's frozen step).
- Full retry executed all four ops: cumulative **9 substantive / 4 targeted**
  per commissioner accounting. Zero further retries or explorations; no Lean or
  lake invocation exists outside the four inside the runner.

## Packet contents

- Journal: `STATUS.md` (machine-stamped via `status-event`).
- v1 (preserved): `evidence/run/`, `handoffs/SS0-RETURN.md`.
- v2: `evidence/run-v2/` (25 files incl. `identity.txt`, per-op
  exit/ms/stdout/stderr, `op1-oleans.txt`, `op2-applied.diff`,
  `op2-dirty-status`, `op2-classification.txt`, `op3-restored-status`,
  `runner.stdout/stderr`, `final-status`), hash ledger `evidence/run-v2.sha256`,
  and this file `handoffs/SS0-RETURN-v2.md`.
