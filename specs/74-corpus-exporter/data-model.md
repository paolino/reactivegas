# Data model — #74 corpus exporter

Artifact ceiling: 60 lines / 5 KiB.

## Wrapper principle

`Trace = { schema, version, initial, steps }` is frozen. The gap (evaluation
`GroupView`, backdonate authorization, initial aggregate for the integrated
corpus) closes one level up: each checked-in file is a wrapper object. A
consumer holding only the file can replay without out-of-band knowledge.
Hard boundary (NOTE-001): the wrapper carries `GroupView` plus the
authorization identity, and nothing else, without a filed question. The
integrated file's `corpusInitial` is the same shape of need as `GroupView`,
not a third thing.

## D74-ECONWRAP — economic corpus file

- `view`: the full `KelGroups.GroupView` under which the traces were
  evaluated (today `seedView`: two admins + one member). JSON projection
  owned by the new module.
- `auth`: the authorization identity the traces were evaluated under (today
  the refusing probe `seedAuth`, `fun _ _ => false`, containing no
  backdonate event). Rendered as an explicit identity string/flag the
  replayer can match, never as an opaque closure.
- `traces`: exactly `Reactivegas.seedCorpus` (5 traces, 32 events), via
  `Lean.toJson`, unedited. Each trace already carries its own `initial`
  (`State.empty`); the wrapper does not repeat it.

State invariant: `traces == seedCorpus` element for element; `view`/`auth`
  are the exact context `emitTrace` received. No further file-level field
  without a filed question.

## D74-INTWRAP — integrated corpus file

- `initial`: exactly `corpusInitial : KelGroups.GroupState State` (same
  shape of need as `GroupView`, not a third thing).
- `auth`: the authorization identity the steps were evaluated under (today
  the refusing `probeAuth`), rendered as an explicit identity the replayer
  can match.
- `steps`: exactly `Reactivegas.emitIntegratedCorpus` (7 steps), via the
  existing `ToJson` set, unedited.
- Anything beyond `initial` + `auth` + `steps` that replaying turns out to
  need (threshold, or equivalent): file a question first — do not add the
  field. The PR enumerates each carried field and the replayer action for
  it.

State invariant: `steps == emitIntegratedCorpus`; `initial == corpusInitial`.

## D74-MANIFEST — hash manifest

Checked in beside the corpus files. One SHA-256 per corpus file over exact
bytes. The verify target recomputes and byte-compares; any mismatch fails
closed (non-zero, naming the file).
