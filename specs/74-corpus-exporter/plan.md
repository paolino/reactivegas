# Plan — #74 corpus exporter

Artifact ceiling: 120 lines / 9 KiB.

## Fixed decisions

Single OWNER slice S74. LIGHT is ineligible: the wrapper shape (what a
replayer must do with each field), the `GroupView` JSON projection, and the
claim that no model definition moved are semantic judgments no executable
gate alone entails. `draft=NONE`. Commit owner `muse`, auditor `grok`.

The exporter is a new Lean module plus a `lean_exe`. It imports the machine
(`Reactivegas.Trace`, hence `Invariants`) and calls the two existing
definitions. Dependency direction stays one-way: nothing existing imports the
new module back (same rule `Trace.lean` documents for itself).

Format lands now; content is provisional. #68 (proposer is not an assent) and
#69 (pledge sovereignty) change what the corpora emit. The checked-in files
are re-frozen after they land, before D3 consumes them as an oracle. Do not
wait for them; do not present the current content as final.

#71 is rewriting the design record. The entry here is additive prose in the
current `docs/en/design/` shape; if #71 relocates it, that is a forward move,
not a reason to wait.

## Source horizon and fence

Implementation may add/change only:

- `lean/Reactivegas/CorpusExport*.lean` (new module(s); the `GroupView`
  `ToJson` lives here);
- `lean/lakefile.lean` (one `lean_exe` stanza, additive);
- frozen corpus JSON + hash manifest (new checked-in files, location named in
  `modules-model.md`);
- `justfile` (export + verify targets, additive) and the CI step that runs
  the verify target;
- `docs/en/design/*` (one additive entry; no rewrite of existing prose);
- task stamps in `specs/74-corpus-exporter/tasks.md`.

Everything else is read-only, including all existing `lean/` content bytes,
`Eventi/`, `Core/`, `Lib/`, `Voci/`, `Server/`, `paolino/kelgroups`, and prior
`specs/` directories. The forbidden list in R74-05 is exact.

## Slice S74 — exporter, artifacts, gate, record

Delivers R74-01…R74-05 and tasks T7400…T7404.

- New module: `GroupView` JSON, file-level wrapper(s) for the economic corpus
  (`seedView` + refusing `seedAuth` identity + initial + traces) and the
  integrated corpus (`corpusInitial` + steps), `lean_exe` entry point writing
  both files.
- Frozen files + SHA-256 manifest beside them; `just` export target and
  `just` verify target (re-emit + byte-compare, fail closed); CI runs verify.
- Design-record entry: coverage, vote hole, provisional-until-#68-#69 notice.
- Verification: export → verify 0 clean; mutate one byte → non-zero; restore
  → zero (all three quoted); `just ci` green; additive-only proof by diff.

Frozen gate rows: `G74-CALLS-EXISTING`, `G74-ENVELOPE-CLOSED`,
`G74-VERIFY-FAILS-CLOSED`, `G74-RECORD-HONEST`, `G74-ADDITIVE-ONLY`.
