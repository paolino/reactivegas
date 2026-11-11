# Plan — #70 C1

One OWNER slice, S1, on branch `feat/70-c1-integrate` pushed to
`feat/economics-simulator-fable` (PR94) by fast-forward.

1. Merge `origin/master` `2bd9a20` into the PR94 head; resolve conflicts in
   simulator-owned files only.
2. Re-pin the claim gate's extent at the merged base (C1-PIN).
3. Re-emit the Lean traces through the two drivers against the merged model;
   extend the vote trace to the #81 rows (C1-81); repair core/page where they
   contradict.
4. Add the simulator gates to CI (C1-SIMCI): a `just` recipe and one CI step;
   the dev shell supplies node and a headless chromium.
5. Confirm F-01/F-02/F-03 rows hold on the merged candidate.
6. Push; CI and the preview job run on the head (C1-CI, C1-PREVIEW).

Live boundary: the PR preview URL; verified by bytes, never locally.
Lean: one locked build per batch (LEAN-BATCH-RULE-20261001).
