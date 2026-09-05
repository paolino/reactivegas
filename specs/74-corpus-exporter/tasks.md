# Tasks — #74 corpus exporter

Artifact ceiling: 60 lines / 5 KiB. Only the ticket owner checks behavior-task
boxes after a fresh audit passes the exact candidate.

## Planning and gate

- [ ] **T7400** Freeze mandate + immutable slice gate + falsification proof
      (gate RED on base: no exe, no corpus files, no verify target).
- [ ] **T7401** New `CorpusExport` module + `GroupView` JSON + wrappers +
      `lean_exe` calling `seedCorpus` / `emitIntegratedCorpus`. (R74-01, R74-02)
- [ ] **T7402** Frozen corpus files + SHA-256 manifest + `just` export/verify
      + CI step; quoted negative control (0 / non-zero / 0). (R74-03)
- [ ] **T7403** Design-record entry (coverage, vote hole, provisional until
      #68 + #69) + PR body hole statement + replayer field table. (R74-04)
- [ ] **T7404** Full verification: export, verify, negative control, `just ci`,
      additive-only diff proof, fresh `grok` audit, push + draft PR. (R74-05)
