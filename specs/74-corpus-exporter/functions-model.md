# Functions model — #74 corpus exporter

Artifact ceiling: 60 lines / 5 KiB. New/changed signatures only. No bodies,
no imports, no control flow.

- `F74-VIEWJSON (new, in CorpusExport module)`: `KelGroups.GroupView → Lean.Json`
  via a `Lean.ToJson KelGroups.GroupView` instance declared in the new module.
  Constraint: total over `members`; must not alter or duplicate any existing
  instance.
- `F74-ECONWRAP (new)`: builds the D74-ECONWRAP wrapper value from
  (`seedView`, `seedAuth`-identity, `State.empty`, `seedCorpus`).
  Constraint: calls `seedCorpus`; takes no event list argument (a list
  argument would be a second corpus).
- `F74-INTWRAP (new)`: builds the D74-INTWRAP wrapper value from
  (`corpusInitial`, `emitIntegratedCorpus`).
  Constraint: calls `emitIntegratedCorpus`; takes no step-list argument.
- `F74-MAIN (new, `lean_exe` entry)`: `IO UInt32` (or `IO Unit` per lake
  convention) writing both wrapper files to their frozen paths. Constraint:
  the sole writer of the frozen files; writes bytes the verify target
  compares exactly.
- `F74-VERIFY (justfile, not Lean)`: verify target re-emits to temp and
  byte-compares against checked-in files + manifest. Constraint: fails closed
  (non-zero) on drift, missing file, or emitter failure.

Unchanged and called, never redefined: `Reactivegas.seedCorpus`,
`Reactivegas.emitIntegratedCorpus`, `Reactivegas.traceToJson`,
`Lean.toJson` for `Trace` / `IntegratedTraceStep` / `State`.
