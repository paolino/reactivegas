# PREFLIGHT BINDING — invocation 1 launch authority (epic-issued)

From: epic owner `%532`. Date: 2026-09-06. Mechanical preflight complete:
runner reviewed line-by-line against NOTE-009's five defects — all fixed
(case-root `/.` copies every branch; no set-e swallow with structural
CASE_ENV_OK + nested-copy gates; A11 dump-only poisoning; single exit
taxonomy; A18/A22 bound-overlay reads).

## Bound artifacts (sha256)

- Runner: `t30-contract/scratch/pf8/run.sh` = `62025c179e85b6ab…`
- Gate leg under test: `handoffs/T30-DRIFT-LEG-r8.sh` = `f0afa32b4fbb…`
- Fixtures: generated at runtime by setup_tree (no frozen fixture bytes to
  bind; template construction reviewed in-runner).

## Exact invocation command (invocation 1 of ≤2)

CWD `/tmp/reactivegas/ms2/e-kelgroups-substrate/t30-contract/`:
`bash scratch/pf8/run.sh`
(unique output dirs per invocation: pf8 tree is invocation-1's; any repair
rerun copies to a NEW tree first — never overwrite these streams.)

Counter: synthetic-preflight 1 of 2 (own counter; historical 0 product
builds stays 0). Zero product/compiler execution authorized or possible
here (shell+git plumbing, synthetic fixtures, owned scratch only).
Failure spends the authorized repair rerun in-scope; no quiet third.
Passing means exercised synthetic plumbing ONLY.

Launch authority GRANTED for invocation 1 on these exact bindings.
