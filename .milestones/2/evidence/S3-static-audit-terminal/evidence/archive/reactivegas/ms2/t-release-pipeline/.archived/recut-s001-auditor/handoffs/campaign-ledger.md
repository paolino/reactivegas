# S001 audit campaign ledger

- Candidate: `bbda7a58adfab31d6ce3d98288975f81a9b2fa66`
- Mandate: `e2ef6d95699e5a50b0e34961977cbd5a91c487f30fa8f22b2595bba6de9d5347`
- Scope: publication readiness before the authorized post-merge O001 operation
- State: OPEN — O001/I006 cannot become terminal before an actual clean-directory GitHub Release download
- Building audits: 1/3 under the default commit-auditor budget; this audit used a fresh Cabal build tree and a warm Nix store

| Row | Severity | State | Named negative control / evidence |
| --- | --- | --- | --- |
| I001 | BLOCKING | KILLED-S001 | frozen gate rejects `9999.0.0.0` drift; wiring battery kills malformed manifest and exercises nine domain rows; C002 remains explicitly `DEFERRED-S002` |
| I002 | BLOCKING | KILLED-S001 | `ci-no-dispatch`, `sync-no-dispatch`, `sync-guard-pr-only`, `sync-checkout-head-only`, missing-command, and wrong-order mutants are killed; RED→sync→GREEN model passes |
| I003 | BLOCKING | KILLED-S001 | `release-mutable-ref` is killed; checkout and `TAG` are bound to the Release Please `tag_name` output |
| I004 | BLOCKING | KILLED-S001 | `prerelease-off`, `prerelease-versioning`, and `release-not-prerelease` are killed; staged bytes also carry `PROVISIONAL.md` |
| I005 | BLOCKING | KILLED-S001 | `post-smoke-entrypoint-tamper` is killed for digest mismatch and leaves zero uploadable archives; the real packager then passes |
| I006 | BLOCKING | OPEN — PENDING-POST-MERGE | local clean-directory validator and smoke pass, but the required GitHub Release download has not happened; C001 remains explicitly `DEFERRED-S002` |
| I007 | BLOCKING | KILLED-S001 | `release-from-pull-request` and `release-ungated` are killed; publication workflow has no PR trigger |
| I008 | BLOCKING | KILLED-S001 | `forbidden-state-machine-append` changes the bound blob and is detected; real state-machine and Lean identities match the base |

## Deferred rows outside S001 closure

- C001 exact artifact identity: `DEFERRED-S002`, open; predecessor instrument SHA-256 `f12f57422823484a19b2c1afe10364a0607846f510cae6270023ceedf0f07db1`.
- C002 non-vacuous PVP mutation coverage: `DEFERRED-S002`, open; predecessor instrument SHA-256 `d8097ee406f081f3c84f47a881a8e79065e6a306a229b2dadb1652f7af6e5c42`.
