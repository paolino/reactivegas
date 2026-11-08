# Plan — #66 S3

## Strategy

One bisect-safe slice. Machine-readable mutant catalogue and ledger tracked
under `lean/`; a tracked runner, invoked by a new `just` recipe and a new CI
step, that discovers the compiled extent, applies each mutant in isolation,
observes positioned diagnostics, validates every ledger claim, runs the
negative controls, restores the tree and checks the rendered ledgers.

## Decisions (each answers a finding of the terminal S3 campaign)

| id | decision | answers |
|---|---|---|
| D-1 | No historical receipt is reused. Every kill is observed by CI on the head. | F-03, D3 of the old mandate |
| D-2 | Extent and closures come from the elaborated environment, not from source text. | F-06 |
| D-3 | Ownership of a theorem by a definition is membership of that definition in the theorem's **statement** closure. Proof-only dependencies do not make ownership. | F-02 (semantic relevance, not byte precision) |
| D-4 | The required atom extent is the compiled refusal vocabulary (R-9) plus whatever single atoms the REQUIRED theorem rows need for a kill. Guard conjuncts are reached through the theorems that state them (e.g. `step_*_inv`); a conjunct no theorem states is visible as a guard mutant that survives. No Cartesian product, no manufactured pairings. | F-01 |
| D-5 | `HELPER` is computed (R-4), never asserted; no static recipe stands in for an elaborated witness. | F-07 |
| D-6 | Every runner self-check carries a CI negative control (R-10); a check derived from its own input is a defect. | `setupAndRestoreIncluded` |
| D-7 | `OPEN` and `SURVIVED` are honest outcomes, not CI failures; their disposition is the desk's. | old D5 |

## Constraints

- Lean toolchain as pinned (`lean/lean-toolchain`), run through the repo's
  Nix dev shell, as every existing Lean CI step is.
- CI wall time: the mutant step may live in its own job; its wall time on the
  pushed head's CI run must be ≤ 60 min. Measured, never estimated.
- The runner is fail-closed: an unparseable diagnostic, a patch that does not
  apply, or a dirty tree after restore is an error, never a skip.

## Live boundary

GitHub Actions on the pushed head: the CI run must show the mutant step
executing (its summary line with nonzero DISCOVERED, REQUIRED, EXECUTED and
KILLED counts) and passing.

## Slices

| slice | content | tasks |
|---|---|---|
| S3 | runner, catalogue, ledger, renderings, recipe, CI step, controls, LEAN-CLARITY.md, OD74 comment | T301–T308 |

## Re-derivation (T309)

The accepted slice was derived on `890a74f`. Master gained the V-5/S-12
lifecycle model (#81) and the lake-root reach checker (#92) before the push.
History already pushed is not rewritten: master is merged into the branch and
the ledger is re-derived in one further commit under the same requirements.
`VoteError.notProposer` and `notDesignee` are now emitted, so R-9 requires guard
mutants for them.

## CI wall time (measured, T311)

The `Lean mutant ledger` job on `7a844dc` (CI run 36964267517) was cancelled at
its 60-minute timeout: about 23 min provisioning the Nix dev shell, 1 min
census, 34 min mutants, negative controls unfinished. The ceiling stands: every
job that carries the ledger finishes green within 60 minutes on GitHub, with
the measurement taken from the pushed head's CI run. The mechanism is the
implementer's. If the mutants are split across several jobs, a final check
proves the union of executed mutants equals the catalogue and that every
control ran, with a negative control showing a dropped shard fails.

Superseded (desk ruling, T311): the ledger stays one CI job with
`timeout-minutes: 120`, because a split pays the dev-shell provisioning per
job and adds transport and aggregation failure modes. Splitting is reconsidered
only if a measured full run on GitHub exceeds about 100 minutes.
