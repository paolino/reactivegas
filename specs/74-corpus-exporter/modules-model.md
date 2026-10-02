# Modules model — #74 corpus exporter

Artifact ceiling: 60 lines / 5 KiB.

## New responsibility

- **M74-EXPORT (`lean/Reactivegas/CorpusExport*.lean`, new).** Owns: the
  `KelGroups.GroupView` JSON projection; the file-level wrapper object(s)
  carrying view + authorization identity + initial aggregate + traces; the
  `lean_exe` entry point that writes both corpus files by calling
  `Reactivegas.seedCorpus` and `Reactivegas.emitIntegratedCorpus`. Nothing
  else in the tree owns wrapper bytes.
  - Depends on: `Reactivegas.Trace` (hence `Invariants`, hence the machine).
  - Depended on by: nothing. No existing module imports it. One-way, mirroring
    the `Trace.lean` header rule.
  - Promotion: none. This stays a leaf emitter; a future Haskell replayer (D3)
    consumes its files, never imports it.

## Changed responsibilities

- **M74-BUILD (`lean/lakefile.lean`).** Gains one additive `lean_exe` stanza
  pointing at the new module's `main`. No existing stanza is edited.
- **M74-JUST (`justfile`).** Gains two additive targets: export (emit both
  files) and verify (re-emit + byte-compare, fail closed). No existing recipe
  is edited; `ci` gains one line invoking verify.
- **M74-CORPUS (new frozen files, e.g. `lean/corpus/*.json` + `.sha256`).**
  Owned bytes: the emitted wrappers. The emitter is the sole writer; humans
  never hand-edit. Location is fixed by the commit owner and recorded in the
  PR; the manifest lives beside the files.
- **M74-RECORD (NOTE-001: no `docs/` change).** The commit owner authors
  the coverage/hole/provisional prose as evidence in its own root
  (`handoffs/coverage.md`: what each corpus covers with exact counts, the
  vote hole with mechanism, the assenso consequence, the provisional-until-
  #68-#69 notice, the replayer field table). The ticket owner places it at
  ticket `handoffs/CORPUS-COVERAGE.md` for routing to #71. `docs/` stays
  byte-identical to base; the gate fails if it does not.

## Explicit non-goals

`Eventi/`, `Core/`, `Lib/`, `Voci/`, `Server/`, `paolino/kelgroups`, the
Haskell replayer (D3, blocked on #73), and any model edit per R74-05.
