# Modules model — #75

All additive on `222ad9a`. Dependency direction: new Reactivegas modules may
import `Reactivegas.*` and `KelGroups.*`; nothing under `KelGroups` imports
them; no existing module imports them except the exporter.

- **vote-corpus module** (new, under `lean/Reactivegas/`). Owns the third
  corpus: initial `GroupState State`, the signed event list, the step snapshot
  with the vote-error annotation, the live emission, and the in-Lean coverage
  check over discovered constructor extents. Reuses the integrated `ToJson`
  instances and `s62b` helpers; restates none. Generation threshold is the
  named `KelGroups.Vote.legacyThreshold`; authorization is `probeAuth`.
- **replay-context module** (new). Owns the sidecar value, canonical table
  validation (R3.1 §3), the table-backed threshold with point-of-query domain
  check, the replayer's three outcomes, and the source-input digest
  computation. Does not import the vote-corpus module's emitter for its
  expected policy values.
- **policy re-derivation check** (new, separate from the sidecar writer path).
  Reads sidecar bytes, evaluates the named declaration per domain point.
- **exporter** (`lean/Reactivegas/CorpusExport.lean`). Gains modes for the
  third file and its sidecar (write, live check, replay). Existing two-file
  modes and bytes unchanged.
- **verify recipe** (`justfile` `lean-corpus-verify`, `lean-corpus-export`).
  Extended for the third file, the sidecar, controls, digest recomputation,
  hermetic regeneration and the added-input control. Nothing removed.
- **frozen files** (`lean/corpus/vote.json`, `lean/corpus/vote.context.json`,
  `lean/corpus/corpus.sha256` extended). Existing two files byte-identical.

Explicit non-goals: model modules, `Trace`, Haskell, simulator, docs.
