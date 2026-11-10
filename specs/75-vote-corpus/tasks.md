# Tasks — #75 integrated vote corpus

Only the ticket owner checks boxes, after the auditor approves the line.

## Planning and gate

- [x] **freeze-mandate-and-gate** Mandate in this dir, gate frozen in the
      ticket runtime root with hash.

## Slice vote-corpus-with-replay-context

- [x] **red-bundle** RED proof: extended verify fails on base for the missing
      third file, sidecar, coverage, controls (one cited red per acceptance line).
- [x] **vote-corpus-emitted** V1, V2, V3, journey-lifted, lifecycle-rows,
      vote-error-identity-observed; frozen-and-verified; byte-control-in-gate.
- [x] **coverage-discovered** Coverage over discovered extents of `VoteEvent`,
      `VoteError`, `ClosureCause`, `Verdict` + `openBound`; added-constructor
      control fails.
- [x] **replay-context-sidecar** Sidecar bytes per R3.1 §5; three outcomes;
      point-of-query domain; R3.1 §7 rows 1–9 in the gate.
- [x] **policy-rederived-and-witnessed** Independent declaration check against
      sidecar bytes; precondition exhibits a discriminating collective query at
      `n >= 1` before the policy controls.
- [x] **source-inputs-honest** Digest encoding, reproducibility from committed
      inputs, hermetic regeneration, added-input omission control.
- [ ] **final-acceptance** `just ci` once on the final candidate; hosted CI
      green at the pushed SHA; every checkpoint approved; draft PR body factual.
