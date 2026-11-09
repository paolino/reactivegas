# Modules — #70 S5-sim-b

| Module | Change |
|---|---|
| simulator page (`economics-simulator.html`, built by `economics-simulator-build.mjs`) | renders bindings, live authorizations and closure causes from the core state; owns no rule |
| simulator core (`economics-simulator-core.mjs`) | unchanged in behaviour unless a story is unreachable; any change follows the Lean and is judged by the trace gates |
| simulator gates (`economics-simulator-*-gate.mjs`) | own the controls behind INV-S5B-CANFAIL |
| scenarios (`economics-simulator-scenarios/*.json`) | may gain L-7 scenarios judged by the scenario gate |

Dependency direction unchanged: page -> core; gates -> page and core; core -> Lean only through the trace drivers' committed output.
