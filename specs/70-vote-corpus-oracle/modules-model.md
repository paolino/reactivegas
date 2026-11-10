# Modules — #70 slice O

| Module | Change |
|---|---|
| vote-corpus oracle (new gate `economics-simulator-*-gate.mjs`) | owns sidecar validation, the table-backed threshold with point-of-query domain check, the step-by-step replay and comparison, the three outcome tags, the policy witness and the R3.1 §7 controls |
| `economics-simulator-core.mjs` | the integrated route accepts the threshold as an argument; its default stays the named threshold the page uses |
| `economics-simulator.html` | regenerated from the core by `economics-simulator-build.mjs` only |
| `lean/corpus/*` | read only |

Dependency direction: oracle -> core, corpus files; nothing imports the oracle.
