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
3. read `handoffs/CORPUS-COVERAGE.md` (ticket runtime root, routed to #71)
   and learn what the corpora cover and what they do not.

NOTE-001 (epic parent correction, 2026-09-05) revokes the design-record
entry: `docs/en/design/` is read-only on this slice (#71 is rewriting it).
Coverage content goes to the handoff file instead.

## Requirements

- **R74-01 — call, do not restate.** The exporter calls
  `Reactivegas.seedCorpus : List Trace` (`lean/Reactivegas/Trace.lean`, 5
  traces / 32 events) and `Reactivegas.emitIntegratedCorpus :
  List IntegratedTraceStep` (`lean/Reactivegas/Invariants.lean`, 7 steps from
  `corpusInitial`). Both already have `Lean.ToJson`. No corpus content,
  `seedView`, `corpusInitial`, or `seedAuth` is restated in the new module.
- **R74-02 — file-level envelope, hard boundary (NOTE-001).** `Trace` is
  frozen and untouched. Each wrapper file carries the `GroupView` plus the
  authorization identity, and nothing else, without escalating first: if
  replaying turns out to need a third thing — any third thing — stop and
  file a question, because a wrapper that grows fields becomes the second
  format this slice exists to prevent. The integrated file's initial
  aggregate (`corpusInitial`) is the same shape of need as `GroupView`, not
  a third thing. (Each economic `Trace` already carries its own `initial`;
  the economic wrapper does not repeat it.) `KelGroups.GroupView` gets its
  `ToJson` in the new module only, never in an existing one. The PR states
  what a replayer must do with each field.
- **R74-03 — frozen artifacts + failing-closed gate.** A `lean_exe` writes
  both corpora; the files are checked in; a hash manifest is checked in beside
  them; a `just` verify target re-emits and byte-compares, failing closed on
  drift; a CI step runs that target. The negative control (mutate one byte →
  non-zero → restore → zero) is demonstrated and quoted.
- **R74-04 — honest record (NOTE-001).** No `docs/` file is written. The
  same content is delivered as `handoffs/CORPUS-COVERAGE.md` in the ticket
  runtime root (routed to #71): what each corpus covers (economic vs base
  channel, 5 traces / 32 events; 7 base steps), what neither covers —
  **votes**, with the mechanism (`step` returns `none` for
  `openQuestion`/`cast`/`renounce`, which run inside `appFold` via
  `voteApply`, which the economic corpus never reaches and the integrated
  corpus never emits) — the consequence (**assenso is named in the
  milestone's outcome test and has no oracle behind it**), and that the
  checked-in content is provisional until #68 and #69 land and must be
  re-frozen then. The PR body states the hole too.
- **R74-05 — additive only.** No theorem, `example`, `#guard`, proof, guard,
  `step`, `stepEvent`, `appFold`, `baseHook`, state type, event constructor,
  `Trace` structure, existing corpus content, `seedView`, `corpusInitial`, or
  `seedAuth` is changed. No change under `Eventi/`, `Core/`, `Lib/`, `Voci/`,
  `Server/`, `docs/`, or `paolino/kelgroups`. If the oracle appears to need
  one, stop and file a question instead of deciding.

## Rejection behavior

- `just <verify-target>` exits non-zero on any byte drift, missing file, or
  emitter failure. It never silently passes when it cannot resolve the Lean.
- A candidate that edits `Trace`, the model, or restates corpus content is
  rejected even if all gates are green.
- A gate that has never been seen to fail is not a gate; without the quoted
  negative control the slice is not accepted.
