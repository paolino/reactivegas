# Data model — #74 corpus exporter

Artifact ceiling: 60 lines / 5 KiB.

## Wrapper principle

`Trace = { schema, version, initial, steps }` is frozen. The gap (evaluation
`GroupView`, backdonate authorization, initial aggregate for the integrated
corpus) closes one level up: each checked-in file is a wrapper object. A
consumer holding only the file can replay without out-of-band knowledge.

## D74-ECONWRAP — economic corpus file

- `schema`: the file-level envelope identity (names `reactivegas.trace`
  corpus + wrapper version; exact string fixed by the implementation and
  quoted in the PR).
- `view`: the full `KelGroups.GroupView` under which the traces were
  evaluated (today `seedView`: two admins + one member). JSON projection
  owned by the new module.
- `auth`: the authorization identity the traces were evaluated under (today
  the refusing probe `seedAuth`, `fun _ _ => false`, containing no
  backdonate event). Rendered as an explicit identity string/flag the
  replayer can match, never as an opaque closure.
- `initial`: the initial aggregate the traces run from (today `State.empty`).
- `traces`: exactly `Reactivegas.seedCorpus` (5 traces, 32 events), via
  `Lean.toJson`, unedited.

State invariant: `traces == seedCorpus` element for element; `view`/`auth`/
`initial` are the exact inputs `emitTrace` received.

## D74-INTWRAP — integrated corpus file

- `schema`: file-level envelope identity (same versioning rule as above).
- `initial`: exactly `corpusInitial : KelGroups.GroupState State`.
- `steps`: exactly `Reactivegas.emitIntegratedCorpus` (7 steps), via the
  existing `ToJson` set, unedited.
- Replay context the file must also carry (threshold, `probeAuth`-refusing
  identity, or equivalent): whatever a replayer needs beyond `initial` +
  `steps` to run `Reactivegas.apply` to the same verdicts. The PR enumerates
  each field and the replayer action for it.

State invariant: `steps == emitIntegratedCorpus`; `initial == corpusInitial`.

## D74-MANIFEST — hash manifest

Checked in beside the corpus files. One SHA-256 per corpus file over exact
bytes. The verify target recomputes and byte-compares; any mismatch fails
closed (non-zero, naming the file).
