# Plan — #70 G2

One bisect-safe slice on `sim/70-generator-g2`, one draft PR with preview. Files:
the generator gate, an optional shrink module, the scenario gate only to expose
its loader, and this folder. Shrinking replays candidates through the production
core; the emitted envelope is the existing `reactivegas.trace` v1. CI gate:
`just simulator`. Lean is read, never edited.
