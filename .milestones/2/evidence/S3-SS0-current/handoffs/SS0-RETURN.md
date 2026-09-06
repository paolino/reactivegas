# SS0-RETURN — stopped at OP1 setup failure (no retry)

Runner executed once per frozen sequence. Stop semantics applied by the runner:
OP1 non-zero → BLOCKER, exit 91. Scratch restored without another verification
build. No S3 conclusions drawn; packet read against raw exits and artifacts.

## Per-operation record

| op | named target | exit | duration | outcome class |
|---|---|---|---|---|
| OP1 cold baseline (substantive) | `lake build Reactivegas.Invariants` via `instruments/cold.sh` under `nix develop` | 1 | 63 ms | **setup** — Lean/lake never invoked |
| OP2 single-atom mutant (substantive) | same named target, atom applied | — not executed | — | — (stop before OP2) |
| OP3 restore + matching build (substantive) | same named target, restored | — not executed | — | — (stop before OP3) |
| OP4 U-CHECK (targeted) | `lake env lean instruments/Check.lean` | — not executed | — | — (stop before OP4) |

## Exact blocker

`evidence/run/op1.stderr` (151 bytes, verbatim):

```
path '/tmp/reactivegas/ms2/e-lean-compliance/measure-owner-s3-ss0' does not contain a 'flake.nix', searching up
error: could not find a flake.nix file
```

`run-ss0.sh` invokes `nix develop --quiet --no-write-lock-file -c bash
"$R/instruments/cold.sh"` with cwd = the runtime root, which contains no
`flake.nix`; `nix develop` resolves the flake from the invoking directory
*before* `cold.sh`'s internal `cd /code/reactivegas-66-s3-ss0-scratch/lean`
ever runs. `nix` errored in 63 ms. Corroboration in `evidence/run/identity.txt`:
outside the devShell only `nix` is on PATH (`lake`/`lean` absent, `LEAN_PATH`
unset), and env contains no candidate/repair-worktree reference — so no
toolchain existed to fall back to, and none was invoked. `op1.stdout` is empty
(0 bytes). This is a **setup** outcome: not semantic (no theorem evaluated),
not proof-script (no proof checked).

## Preservation state (read-only checks, no compiler invocation)

- Scratch HEAD still `3590c0015b84fd58004bf6fb44dd18b107304c48`; porcelain 0 lines.
- `final-status` empty (0 lines) — runner's `git checkout -- .` restore held.
- The intended devShell inputs exist and were untouched:
  `/code/reactivegas-66-s3-ss0-scratch/flake.nix`,
  `/code/reactivegas-66-s3-ss0-scratch/lean/lakefile.lean`.
- The frozen atom (`SS0-atom.diff`) was never applied; `op2-applied.diff` /
  `op2-dirty-status` were never created (absent from `evidence/run/`).

## Budget accounting

- Spent: **1 substantive** (failed OP1; setup failures consume the operation).
- Unexecuted: OP2 + OP3 substantive, OP4 targeted.
- Against brief accounting (historical 5 substantive / 3 targeted): cumulative
  **6 substantive / 3 targeted** if the commissioner counts the consumed OP1.
  No static submission count touched. No retry taken; runner directory
  `evidence/run/` preserved as-is for commissioner inspection.

## Packet contents

- Journal: `STATUS.md` (machine-stamped via `status-event`).
- Raw evidence: `evidence/run/` (`identity.txt`, `op1.exit`, `op1.ms`,
  `op1.stdout`, `op1.stderr`, `runner.stdout`, `runner.stderr`, `final-status`).
- This file: `handoffs/SS0-RETURN.md`.
