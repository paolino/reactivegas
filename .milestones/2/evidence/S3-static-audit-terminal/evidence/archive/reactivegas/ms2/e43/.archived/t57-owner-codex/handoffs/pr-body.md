Tracks #57

## Summary

- make `validateVoteEvent` the sole exhaustive authorization boundary for all
  six current `VoteEvent` constructors, including member and role events;
- place both event effects and the closure sweep behind that decision, so any
  rejected signer/event pair returns the complete arbitrary input state
  unchanged;
- express no-expiry through semantic preservation of the target question,
  current franchise, and proposer standing;
- keep event effects authorization-free after repairing audit finding F-001,
  which identified a redundant cast-local standing guard;
- freshly re-demonstrate PARTITION, DISJOINT, NOSTALE, FRANCHISE, and
  POLICYFREE with named negative controls.

## Verification

- final fresh audit: PASS, 10 KILLED / 0 RESIDUAL / 0 BLOCKED / 0 OPEN;
- immutable gate v3: Lean 4.25.0, exhaustive boundary and effect-structure
  checks, focused proof build, nine clean axiom reports, three positive
  instruments, six named mutants RED, and full repository CI (24 jobs);
- final commit: `13b44bcb89567596c8b0d953838b1500ece1f4ef`;
- final audit report SHA-256:
  `c3d54428eab8ad2e6b6a85f7e0feb2a19620a25c96fd31efc7ba6fef6981e3dd`;
- exact-final gate receipt SHA-256:
  `5d2bae3c5ae6ebe9bfde022e8ca9878663842e9a8bcf65f7a56adb6cb19ddcc5`.

## Scope

This branch deliberately starts from the previously local #54 Slice-A
candidate, so the PR carries the complete required Vote-machine history and
its #54 specification alongside the #57 structural re-cut. The final #57
implementation delta itself changes the four Vote validation/fold/proof/test
modules plus its checked task ledger. End-to-end Reactivegas/KelGroups
composition is intentionally outside this PR and remains separate follow-up
work.
