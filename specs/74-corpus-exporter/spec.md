# Spec — #74 corpus exporter

Issue: https://github.com/paolino/reactivegas/issues/74 (parent #67, under #72)

Authoritative inputs:

- issue body: exporter calls `seedCorpus` / `emitIntegratedCorpus`, closes the
  envelope gap at file level, additive only, records the vote hole, format now
  / content re-frozen after #68 + #69;
- ticket-owner brief `/tmp/reactivegas/ms2/e-haskell-impl/t74-corpus-exporter/brief.md`
  (base `e6c5924`, worktree `/code/reactivegas-issue-74`, branch
  `feat/74-corpus-exporter`, seat `muse`, auditor `grok`);
- epic map `/tmp/reactivegas/ms2/e-haskell-impl/EPIC-MAP.md` (D0c unblocked,
  D3 blocked on #73 + #68 + #69).

Artifact ceiling: 120 lines / 9 KiB.

## Observable outcome

A person who has never seen this repository can, from a clean checkout:

1. run one command and get both corpus files;
2. run one command that verifies the checked-in files match what the Lean
   emits, and see it exit non-zero when a byte is changed;
3. read the design record and learn what the corpora cover and what they do not.

## Requirements

- **R74-01 — call, do not restate.** The exporter calls
  `Reactivegas.seedCorpus : List Trace` (`lean/Reactivegas/Trace.lean`, 5
  traces / 32 events) and `Reactivegas.emitIntegratedCorpus :
  List IntegratedTraceStep` (`lean/Reactivegas/Invariants.lean`, 7 steps from
  `corpusInitial`). Both already have `Lean.ToJson`. No corpus content,
  `seedView`, `corpusInitial`, or `seedAuth` is restated in the new module.
- **R74-02 — file-level envelope.** `Trace` is frozen and untouched. The
  wrapper object at file level carries the view, the authorization identity,
  the initial aggregate, and the traces, so a consumer needs nothing but the
  file to replay. `KelGroups.GroupView` gets its `ToJson` in the new module
  only, never in an existing one. The PR states what a replayer must do with
  each field.
- **R74-03 — frozen artifacts + failing-closed gate.** A `lean_exe` writes
  both corpora; the files are checked in; a hash manifest is checked in beside
  them; a `just` verify target re-emits and byte-compares, failing closed on
  drift; a CI step runs that target. The negative control (mutate one byte →
  non-zero → restore → zero) is demonstrated and quoted.
- **R74-04 — honest record.** The design-record entry names what the corpora
  cover, names the vote hole (no `openQuestion`/`cast`/`renounce`; `step`
  returns `none` for them, they run via `voteApply`/`appFold` which neither
  corpus reaches), and states the checked-in files are provisional until #68
  and #69 land and must be re-frozen then. The PR body states the hole too.
- **R74-05 — additive only.** No theorem, `example`, `#guard`, proof, guard,
  `step`, `stepEvent`, `appFold`, `baseHook`, state type, event constructor,
  `Trace` structure, existing corpus content, `seedView`, `corpusInitial`, or
  `seedAuth` is changed. No change under `Eventi/`, `Core/`, `Lib/`, `Voci/`,
  `Server/`, or `paolino/kelgroups`. If the oracle appears to need one, stop
  and file a question instead of deciding.

## Rejection behavior

- `just <verify-target>` exits non-zero on any byte drift, missing file, or
  emitter failure. It never silently passes when it cannot resolve the Lean.
- A candidate that edits `Trace`, the model, or restates corpus content is
  rejected even if all gates are green.
- A gate that has never been seen to fail is not a gate; without the quoted
  negative control the slice is not accepted.
