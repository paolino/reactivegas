# Plan — #70 G1

One bisect-safe slice on `sim/70-generator-g1`, one draft PR with preview. Files:
simulator modules and gates (`economics-simulator*.mjs`) and this folder. The
constructor discovery moves into one module the claim gate and the generator
both import; the generator drives `applyIntegrated` and the core's law
checkers. CI gate: `just simulator`. Lean is read, never edited.
