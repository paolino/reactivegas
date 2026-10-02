# Functions model — #86 exporter successor (new/changed signatures only)

- `F86-CHECKCTX (extended, in CorpusExport module)`: bind wrapper context to
  live defs — economic `view` vs `seedView`, integrated `initial` vs
  `corpusInitial`, each `auth` vs its identity def — element/value equality,
  nonzero extent where applicable. Constraint: same call-site `ToJson` path
  as emission; bounded independence claim in the module comment (does not
  establish serializer-instance independence); no new encoder required.
- `F86-ARITY (changed, `main`)`: `["check", onePath]` (and any non-3-arg
  `check`-headed form) → usage to stderr, exit 1, zero filesystem mutation.
  Constraint: write arm never triggers when `args[0] == "check"`; existing
  `["check", econ, int]` + 2-path write arms unchanged; declared
  second-write-failure limit stands.
- `F86-VERIFY (justfile, extended)`: verify target runs re-emit + `cmp` +
  manifest + compiled `check` + both exact `jq` key-set programs (top level
  AND one level in, nonzero-extent guards). Constraint: fails closed naming
  the file/row; no existing `just lean` leg weakened.
- `F86-CIVERIFY (ci.yaml, additive)`: one committed step invoking corpus
  verification (exact command frozen in gate). Constraint: additive only.
- `F86-JQDECL (nix/, additive)`: `jq` in dev-shell inputs (exact attr frozen
  in gate). Constraint: clean-env resolution, correct misattribution on
  omission.

Unchanged and called: `seedCorpus`, `emitIntegratedCorpus`, `seedView`,
`corpusInitial`, `checkLiveArray`/`checkEconFile`/`checkIntFile` cores
(extended, not replaced), `Trace`/`ToJson` set.
