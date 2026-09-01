# Tasks — #48 inversion coverage amendment

Artifact ceiling: 60 lines / 5 KiB. Only the ticket owner checks behavior-task
boxes, and only after a fresh independent audit passes the exact candidate.

## Planning and gate

- [x] **T4800** Freeze the re-scoped #48 mandate, derived 14/8/6 baseline,
      source fence, OWNER topology, model pins, and build budget.

## Slice S48-I — close inversion coverage

- [x] **T4810** Add successful-step inversion theorems for the mechanically
      derived six-constructor gap. (R48-I01…R48-I03)
- [x] **T4811** Ship constructor/theorem coverage derivation with no copied
      event registry and reject missing, duplicate, or phantom coverage.
      (R48-I01, R48-I04)
- [x] **T4812** Prove the permanent coverage detector can fail by dynamically
      removing derived coverage, and execute it from the normal Lean gate.
      (R48-I04, R48-I07)
- [x] **T4813** Print clean axiom sets for all six new theorems and retain zero
      `sorry`, `admit`, or `sorryAx`. (R48-I05)
- [x] **T4814** Preserve all existing theorem behavior and every forbidden
      #62/membership/departure/vote/composition/toolchain/pledge-agency surface;
      pass focused Lean and full repository CI. (R48-I06)
