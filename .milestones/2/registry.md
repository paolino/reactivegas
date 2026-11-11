# Contract registry — milestone 2 (2026-11-08)

| contract | parties | invariant | enforced |
|---|---|---|---|
| Lean model -> Haskell core | Lean (lean/), Haskell economic core (#67, #90) | Haskell follows Lean, no semantic fork | partial: #90 four money arms; no conformance report |
| Vote closure -> economic effect | #76 and #81 L-7 (landed), #67 | grant/deny/backdonate derive from closures; every negative closure is consumed by denyPermission | Lean composition tests (CompositionTests R76 rows) in CI |
| Lean -> simulator fidelity | Lean, simulator (#70) | simulator replays the merged model; claim-gate pins judged at HEAD | simulator gates in CI (PR94, PR99) |
| Lean -> mutant ledger | Lean theorems, #66 S3 ledger | every required theorem row killed, open or explained | CI job "Lean mutant ledger" on master since 67438a8 |
| Frozen corpora -> Haskell replay | exporter (#86), #75 | replay without resolving Lean symbols | partial: #86 landed; #75 not staffed |
| Pure core -> native/wasm -> coordinator/browser/CLI | #82, #83, #84 | one core, no semantic fork | NONE: not started |
| Kelgroups substrate -> coordinator | kelgroups, #73 | the substrate contract the Lean specifies | NONE: held on the kelgroups side |
| CI runner environment -> workflows | GitHub runner image, .github/workflows | runner node survives disk clean-up | the post-job steps of every CI run (#101) |
| Released coordinator + clients -> milestone outcome | all | end-to-end laws for one group | NONE: outcome audit only at completion |
