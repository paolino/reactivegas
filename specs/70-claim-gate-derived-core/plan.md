# Plan — #70 S5-sim-a

One bisect-safe slice on branch `sim/70-derived-core-pin`, one draft PR.
Files: `economics-simulator-claim-gate.mjs` and this folder only.
Strategy: replace the literal core pin by one derivation from the history of
`HEAD` over the vocabulary roots; keep the resolvability and ancestry checks on
the derived value; route the existing controls through the derivation and add
the unresolvable and Lean-branch controls. CI gate: `just simulator` (gate and
`--selftest`). Lean is read, never edited.
