# Modules — #70 slice D

| Module | Change |
|---|---|
| `economics-simulator.html` (page UI, outside the `@@CORE` slices) | owns the regime label on each listed pledge, the owner correction and withdrawal controls on pending pledges, and signer offers that follow the core's guards |
| page check on pledge regimes (new gate `economics-simulator-*pledge*-gate.mjs`) | owns driving the built page over the stated histories, comparing labels and offers with the core state and the core's verdict, and the page mutants |
| `economics-simulator-ui-gate.mjs`, `economics-simulator-page-gate.mjs`, generator gate | may export helpers; their verdicts unchanged |
| `economics-simulator-core.mjs`, `@@CORE` slices of the page | read only |

Dependency direction: pledge check -> generator, core, built page; nothing imports the pledge check.
