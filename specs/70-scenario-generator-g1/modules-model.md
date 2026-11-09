# Modules — #70 G1

| Module | Change |
|---|---|
| Lean surface discovery (new module, or the claim gate's discovery made importable) | owns constructor discovery at the derived core pin; imported by the claim gate and the generator |
| `economics-simulator-generator-gate.mjs` (new) | owns generation, per-step checking, reporting, budget and its `--selftest` controls |
| `economics-simulator-core.mjs` | unchanged in behaviour; read through its exports |
| `economics-simulator-claim-gate.mjs` | imports the discovery instead of owning it; verdicts unchanged |

Dependency direction: generator -> discovery, core; claim gate -> discovery; nothing imports the generator.
