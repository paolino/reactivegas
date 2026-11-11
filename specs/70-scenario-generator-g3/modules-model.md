# Modules — #70 G3

| Module | Change |
|---|---|
| page check on generated states (new gate `economics-simulator-*-gate.mjs`, or the generator gate) | owns loading generated histories into the built page, comparing display with core state, and the display mutants |
| `economics-simulator-generator-gate.mjs` | may export its seeded histories for the page check; generation, laws, coverage, shrinking and budget unchanged |
| `economics-simulator.html`, `economics-simulator-core.mjs` | read only |

Dependency direction: page check -> generator, core, built page; nothing imports the page check.
