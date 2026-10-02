# Modules — #70 C1

| ID | Module | Responsibility change | Depends on |
|---|---|---|---|
| M-1 | `economics-simulator-core.mjs` | apply #81 closures/refusals as Lean does | Lean traces (M-3) |
| M-2 | `economics-simulator.html` (built by `economics-simulator-build.mjs`) | present M-1 outcomes; no refused event shown performed | M-1 |
| M-3 | `lean/TraceDriverV1.lean`, `lean/KelTraceDriverV1.lean` | emit traces covering the #81 rows from the merged model | `lean/Reactivegas`, `lean/KelGroups` (read-only) |
| M-4 | `economics-simulator-*-gate.mjs` | judge M-1/M-2 against M-3 and pinned sources | M-1, M-2, M-3 |
| M-5 | `justfile`, `flake.nix`, `.github/workflows/ci.yaml` | run M-4 in CI | M-4 |

Dependency direction: Lean → traces → core → page; gates read all, nothing
reads gates.
