# Modules — #70 S5-sim-a

| Module | Change |
|---|---|
| `economics-simulator-claim-gate.mjs` | owns the core pin derivation (functions-model F-DERIVE-CORE-PIN); every reader of the core pin depends on it; no new module |

Dependency direction unchanged: the gate reads git history and Lean sources;
nothing reads the gate except `just simulator`.
