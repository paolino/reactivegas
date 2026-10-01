# Tasks — #92

Only the ticket owner checks boxes, after the persistent auditor approves the
checkpoint and the frozen gate row is green at that SHA.

- [x] **T9200** Mandate + frozen gate; red run of the CI command on
      `890a74f` + PR94 drivers (`MIRROR-IMPORT-REACH-GAP` both drivers).
- [x] **T9201** Seed landed; declared roots imported from Lake evaluation.
      (R92-1, R92-5)
- [x] **T9202** Permanent negative control in `just lean` naming the omitted
      module; bypass makes it fail. (R92-2, R92-3)
- [x] **T9203** Setup-failure diagnostics distinct and named. (R92-4)
- [x] **T9204** Combined-tree demonstration green; remote CI green on the
      pushed head. (all)
- [x] **T9205** Second-exe control leg: non-zero via Lean duplicate-declaration
      error naming the clashing module, not a reach gap; limit recorded in
      spec non-goals and PR body. (R92-1x, desk A-001)
