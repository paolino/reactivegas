# Modules — #70 G2

| Module | Change |
|---|---|
| `economics-simulator-generator-gate.mjs` | owns shrinking on a violation, emission, and the G2 `--selftest` controls |
| shrink module (optional, `economics-simulator-*shrink*.mjs`) | may hold the shrinker if the gate grows past readability |
| `economics-simulator-scenario-gate.mjs` | may export its loader/replay for the generator's control; verdicts unchanged |

Dependency direction: generator -> core, lean surface, scenario loader; nothing imports the generator.
