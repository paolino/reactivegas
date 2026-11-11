# Audit build ledger — S57-A submission 1

Budget: 20 invocations. Packet correction A-001 occurred before invocation 1.

| # | Invocation | Cache | Result | Receipt / evidence |
|---:|---|---|---|---|
| 1 | Immutable v2 gate via `run-receipt` on the clean detached audit tree | cold | FAIL before candidate build: R-45 instrument could not import `KelGroups` | `evidence/independent-gate-v2.log`, SHA-256 `7b6c8f5d062b55db69b2220723fa20283c41ca62c42be91bca2af1a9c5826f06`; exit 1; 2439 ms; free space before `154889277440`, after `154889273344` bytes |
| 2 | `lake build KelGroups.Vote.Invariants KelGroups.Vote.Tests` through Lean 4.25.0 shell | cold | PASS, 9 jobs; contractual axiom lines printed | `evidence/focused-vote-build.log`, SHA-256 `3919030b7e298024cf371bff3e786c1c788a4a36363c511f7f6045f62cd1a368`; exit 0; 6556 ms; free space before `154889273344`, after `154885029888` bytes |
| 3 | Immutable v2 gate via `run-receipt` after focused build | warm | PASS: focused build, three green instruments, six named RED mutants, full repository CI | `evidence/independent-gate-v2-warm.log`, SHA-256 `5ac10a79342d78be62ea7ca5706097175904d25eafa054412dcb4be9a6451ffc`; exit 0; 99742 ms; free space before `154885029888`, after `154822176768` bytes |
| 4 | Runtime-only old-boundary stale-state probe | warm | Expected RED after `MUTATION-APPLIED:STALE-REJECTION-SWEEP`; old post-rejection sweep changes the input state | `evidence/stale-rejection-bypass-red.log`, SHA-256 `cd60312c92cc850421806468a2d10da376288fd916bb25e4dd550be23251cb4a`; exit 1 expected; 1567 ms; free space before `154822176768`, after `154822172672` bytes |
| 5 | Runtime-only candidate stale-state probe | warm | PASS: the state is sweep-ready to mutate, validation rejects, and candidate `applyVoteEvent` is exact state identity | `evidence/stale-rejection-candidate-green.log`, SHA-256 `e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`; exit 0; 1541 ms; free space before `154822172672`, after `154822184960` bytes |
| 6 | Frozen future-surface negative control | n/a | PASS: adding `auditBypass` plus an effect makes the checker fail at the authorization boundary | `evidence/future-surface-negative-control.log`, SHA-256 `60baa9cd30fa25d93600849695680a0d2e69a26aacde1f9ed3de9962275485f3`; exit 0; 168 ms; free space before/after `154822184960` bytes |
